//! Helpers for decoding LEB128-encoded integers.
//!
//! <https://en.wikipedia.org/wiki/LEB128>
use crate::read_byte;
use std::io;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DecodeU32Error {
    #[error("uint32 too large")]
    TooLarge,

    #[error("uint32 representation too long")]
    RepresentationTooLong,

    #[error(transparent)]
    Io(#[from] io::Error),
}

pub(crate) fn decode_u32<R: io::Read + ?Sized>(reader: &mut R) -> Result<u32, DecodeU32Error> {
    let mut result: u32 = 0;
    let mut shift: u8 = 0;

    // 5 == ceil(32/7)
    for i in 1..=5 {
        let byte = read_byte(reader)?;

        result |= u32::from(byte & 0b0111_1111 /* 0x7F */) << shift;

        let continuation_bit = byte & 0b1000_0000 /* 0x80 */;
        if continuation_bit == 0 {
            if i == 5 && (byte & 0b1111_0000/* 0xF0 */) != 0 {
                // we're at byte 5, which means 4*7=28 bits have been
                // consumed by the payload at this point. This leaves no more
                // than 32-28=4 more bits available for the rest of the payload.
                //
                // Therefore, ensure that the rest of those bits do not carry
                // any payload.
                return Err(DecodeU32Error::TooLarge);
            }
            return Ok(result);
        }

        // payload is encoded in groups of 7 bits. We parsed a chunk, so move to
        // the next one
        shift += 7;
    }

    Err(DecodeU32Error::RepresentationTooLong)
}

#[derive(Error, Debug)]
pub enum DecodeI32Error {
    #[error("int32 too large")]
    TooLarge,

    #[error("int32 representation too long")]
    RepresentationTooLong,

    #[error(transparent)]
    Io(#[from] io::Error),
}

pub(crate) fn decode_i32<R: io::Read + ?Sized>(reader: &mut R) -> Result<i32, DecodeI32Error> {
    let mut result: i64 = 0;
    let mut shift: u8 = 0;

    const MIN: i64 = i32::MIN as i64;
    const MAX: i64 = i32::MAX as i64;

    // 5 == ceil(32/7)
    for _ in 1..=5 {
        let byte = read_byte(reader)?;

        result |= i64::from(byte & 0b0111_1111 /* 0x7F */) << shift;
        shift += 7;

        let continuation_bit = byte & 0b1000_0000 /* 0x80 */;
        if continuation_bit == 0 {
            if (byte & 0b0100_0000/* 0x40 */) != 0 {
                result |= !0 << shift;
            }

            if !(MIN..=MAX).contains(&result) {
                return Err(DecodeI32Error::TooLarge);
            }

            return Ok(i32::try_from(result).unwrap());
        }
    }

    Err(DecodeI32Error::RepresentationTooLong)
}

#[derive(Error, Debug)]
pub enum DecodeI64Error {
    #[error("int64 representation too long")]
    RepresentationTooLong,

    #[error("int64 incorrect sign extension")]
    IncorrectSignExtension,

    #[error(transparent)]
    Io(#[from] io::Error),
}

pub(crate) fn decode_i64<R: io::Read + ?Sized>(reader: &mut R) -> Result<i64, DecodeI64Error> {
    let mut result: i64 = 0;
    let mut shift: u8 = 0;

    // 10 == ceil(64/7)
    for _ in 1..=10 {
        let byte = read_byte(reader)?;

        result |= i64::from(byte & 0b0111_1111 /* 0x7F */) << shift;
        shift += 7;

        let continuation_bit = byte & 0b1000_0000 /* 0x80 */;
        if continuation_bit == 0 {
            let on_10th_byte = shift >= 64;
            let is_negative = (byte & 0b0100_0000/* 0x40 */) != 0;

            if on_10th_byte {
                let padding = byte & 0b0011_1111 /* 0x3F */;
                if is_negative && padding != 0b0011_1111 {
                    // six low-order bits must be all 1s
                    return Err(DecodeI64Error::IncorrectSignExtension);
                }
                if !is_negative && padding != 0b0000_0000 {
                    // six low-order bits must be all 0s
                    return Err(DecodeI64Error::IncorrectSignExtension);
                }
            } else if is_negative {
                // fill remaining high bits with ones, to sign-extend the
                // value
                result |= !0 << shift;
            }

            return Ok(result);
        }
    }

    Err(DecodeI64Error::RepresentationTooLong)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    fn encode_u32(mut value: u32) -> Vec<u8> {
        let mut out = Vec::new();
        loop {
            let mut byte = (value & 0x7F) as u8;
            value >>= 7;
            if value != 0 {
                byte |= 0x80;
                out.push(byte);
            } else {
                out.push(byte);
                break;
            }
        }
        out
    }

    fn encode_sleb64(mut value: i64) -> Vec<u8> {
        let mut out = Vec::new();
        loop {
            let mut byte = (value & 0x7F) as u8;
            value >>= 7;
            let done = (value == 0 && (byte & 0x40) == 0) || (value == -1 && (byte & 0x40) != 0);
            if done {
                out.push(byte);
                break;
            } else {
                byte |= 0x80;
                out.push(byte);
            }
        }
        out
    }

    fn read_u32_from(bytes: Vec<u8>) -> Result<u32, DecodeU32Error> {
        let mut cursor = Cursor::new(bytes);
        decode_u32(&mut cursor)
    }

    fn read_i32_from(bytes: Vec<u8>) -> Result<i32, DecodeI32Error> {
        let mut cursor = Cursor::new(bytes);
        decode_i32(&mut cursor)
    }

    fn read_i64_from(bytes: Vec<u8>) -> Result<i64, DecodeI64Error> {
        let mut cursor = Cursor::new(bytes);
        decode_i64(&mut cursor)
    }

    #[test]
    fn read_u32_decodes_simple_values() {
        assert_eq!(read_u32_from(encode_u32(0)).unwrap(), 0);
        assert_eq!(read_u32_from(encode_u32(127)).unwrap(), 127);
        assert_eq!(read_u32_from(encode_u32(128)).unwrap(), 128);
        assert_eq!(read_u32_from(encode_u32(u32::MAX)).unwrap(), u32::MAX);
    }

    #[test]
    fn read_u32_rejects_payload_bits_in_last_byte() {
        let err = read_u32_from(vec![0xFF, 0xFF, 0xFF, 0xFF, 0x10]).unwrap_err();
        assert!(matches!(err, DecodeU32Error::TooLarge));
    }

    #[test]
    fn read_u32_accepts_extended_zero() {
        assert_eq!(read_u32_from(vec![0x80, 0x00]).unwrap(), 0);
    }

    #[test]
    fn read_u32_rejects_representation_too_long() {
        let err = read_u32_from(vec![0x80, 0x80, 0x80, 0x80, 0x80]).unwrap_err();
        assert!(matches!(err, DecodeU32Error::RepresentationTooLong));
    }

    #[test]
    fn read_i32_decodes_edge_values() {
        for value in [0, 1, -1, i32::MAX, i32::MIN] {
            assert_eq!(read_i32_from(encode_sleb64(value.into())).unwrap(), value);
        }
    }

    #[test]
    fn read_i32_accepts_non_minimal_encoding_for_negative_one() {
        let err_or_value = read_i32_from(vec![0xFF, 0x7F]);
        assert_eq!(err_or_value.unwrap(), -1);
    }

    #[test]
    fn read_i32_rejects_out_of_range_positive() {
        let bytes = encode_sleb64(i64::from(i32::MAX) + 1);
        let err = read_i32_from(bytes).unwrap_err();
        assert!(matches!(err, DecodeI32Error::TooLarge));
    }

    #[test]
    fn read_i32_rejects_out_of_range_negative() {
        let bytes = encode_sleb64(i64::from(i32::MIN) - 1);
        let err = read_i32_from(bytes).unwrap_err();
        assert!(matches!(err, DecodeI32Error::TooLarge));
    }

    #[test]
    fn read_i32_rejects_representation_too_long() {
        let err = read_i32_from(vec![0x80, 0x80, 0x80, 0x80, 0x80]).unwrap_err();
        assert!(matches!(err, DecodeI32Error::RepresentationTooLong));
    }

    #[test]
    fn read_i64_decodes_edge_values() {
        for value in [0i64, 1, -1, i64::MAX, i64::MIN] {
            assert_eq!(read_i64_from(encode_sleb64(value)).unwrap(), value);
        }
    }

    #[test]
    fn read_i64_accepts_extended_negative_encoding() {
        let mut bytes = encode_sleb64(-1);
        bytes.insert(0, 0xFF);
        assert_eq!(read_i64_from(bytes).unwrap(), -1);
    }

    #[test]
    fn read_i64_accepts_extended_zero() {
        assert_eq!(read_i64_from(vec![0x80, 0x00]).unwrap(), 0);
    }

    #[test]
    fn read_i64_rejects_incorrect_negative_padding() {
        let mut bytes = encode_sleb64(i64::MIN);
        let last = bytes.last_mut().unwrap();
        *last &= !0x01; // flip one of the padding bits
        let err = read_i64_from(bytes).unwrap_err();
        assert!(matches!(err, DecodeI64Error::IncorrectSignExtension));
    }

    #[test]
    fn read_i64_rejects_incorrect_positive_padding() {
        let mut bytes = vec![0x80; 9];
        bytes.push(0x02);
        let err = read_i64_from(bytes).unwrap_err();
        assert!(matches!(err, DecodeI64Error::IncorrectSignExtension));
    }

    #[test]
    fn read_i64_rejects_representation_too_long() {
        let err = read_i64_from(vec![0x80; 10]).unwrap_err();
        assert!(matches!(err, DecodeI64Error::RepresentationTooLong));
    }

    #[test]
    fn read_i64_accepts_full_length_negative_min() {
        let mut bytes = vec![0x80; 9];
        bytes.push(0x7F);
        assert_eq!(read_i64_from(bytes).unwrap(), i64::MIN);
    }

    #[test]
    fn read_i64_accepts_full_length_negative_min_plus_one() {
        let mut bytes = vec![0x81];
        bytes.extend(std::iter::repeat_n(0x80, 8));
        bytes.push(0x7F);
        assert_eq!(read_i64_from(bytes).unwrap(), i64::MIN + 1);
    }

    #[test]
    fn read_i64_accepts_full_length_positive_max() {
        let mut bytes = vec![0xFF; 9];
        bytes.push(0x00);
        assert_eq!(read_i64_from(bytes).unwrap(), i64::MAX);
    }
}
