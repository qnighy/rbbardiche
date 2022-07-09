use once_cell::sync::Lazy;
use std::{
    collections::HashSet,
    fmt::{self, Write},
};
use unicode_general_category::{get_general_category, GeneralCategory};

use bstr::{BStr, ByteSlice};

pub(crate) fn rb_str_inspect<'a>(s: &'a [u8]) -> RubyStringInspector<'a> {
    RubyStringInspector(s)
}

pub(crate) struct RubyStringInspector<'a>(&'a [u8]);

impl<'a> fmt::Display for RubyStringInspector<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("\"")?;
        for chunk in self.0.utf8_chunks() {
            for (i, char) in chunk.valid().char_indices() {
                let esc = match char {
                    '\n' => Some("\\n"),
                    '\r' => Some("\\r"),
                    '\t' => Some("\\t"),
                    '\x0C' => Some("\\f"),
                    '\x0B' => Some("\\v"),
                    '\x08' => Some("\\b"),
                    '\x07' => Some("\\a"),
                    '\x1B' => Some("\\e"),
                    '"' => Some("\\\""),
                    '\\' => Some("\\\\"),
                    '#' => match chunk.valid().as_bytes().get(i + 1) {
                        Some(b'$' | b'@' | b'{') => Some("\\#"),
                        _ => None,
                    },
                    _ => None,
                };
                if let Some(esc) = esc {
                    f.write_str(esc)?;
                } else if is_printable(char) {
                    f.write_char(char)?;
                } else if (char as u32) < 0x10000 {
                    write!(f, "\\u{:04X}", char as u32)?;
                } else {
                    write!(f, "\\u{{{:X}}}", char as u32)?;
                }
            }
            for &byte in chunk.invalid() {
                write!(f, "\\x{:02X}", byte)?;
            }
        }
        f.write_str("\"")?;
        Ok(())
    }
}

fn is_printable(ch: char) -> bool {
    match get_general_category(ch) {
        GeneralCategory::UppercaseLetter
        | GeneralCategory::LowercaseLetter
        | GeneralCategory::TitlecaseLetter
        | GeneralCategory::ModifierLetter
        | GeneralCategory::OtherLetter => true,
        GeneralCategory::NonspacingMark
        | GeneralCategory::SpacingMark
        | GeneralCategory::EnclosingMark => true,
        GeneralCategory::DecimalNumber
        | GeneralCategory::LetterNumber
        | GeneralCategory::OtherNumber => true,
        GeneralCategory::ConnectorPunctuation
        | GeneralCategory::DashPunctuation
        | GeneralCategory::OpenPunctuation
        | GeneralCategory::ClosePunctuation
        | GeneralCategory::InitialPunctuation
        | GeneralCategory::FinalPunctuation
        | GeneralCategory::OtherPunctuation => true,
        GeneralCategory::MathSymbol
        | GeneralCategory::CurrencySymbol
        | GeneralCategory::ModifierSymbol
        | GeneralCategory::OtherSymbol => true,
        GeneralCategory::SpaceSeparator => true,
        GeneralCategory::LineSeparator => false,
        GeneralCategory::ParagraphSeparator => false,
        GeneralCategory::Control => false,
        GeneralCategory::Format => true,
        GeneralCategory::Surrogate => unreachable!(),
        GeneralCategory::PrivateUse => true,
        GeneralCategory::Unassigned => false,
    }
}

pub(crate) fn rb_sym_inspect<'a>(s: &'a [u8]) -> RubySymbolInspector<'a> {
    RubySymbolInspector(s)
}

pub(crate) struct RubySymbolInspector<'a>(&'a [u8]);

impl<'a> fmt::Display for RubySymbolInspector<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if is_plain_symbol(self.0.as_bstr()) && self.0.is_utf8() {
            write!(f, ":{}", self.0.as_bstr())?;
        } else {
            write!(f, ":{}", rb_str_inspect(self.0))?;
        }
        Ok(())
    }
}

fn is_plain_symbol(s: &BStr) -> bool {
    if s.starts_with(b"@@") {
        is_ident(&s[2..])
    } else if s.starts_with(b"@") {
        is_ident(&s[1..])
    } else if s.starts_with(b"$-") {
        let rest = s[2..].as_bstr();
        rest.is_utf8() && rest.len() > 0 && {
            let mut iter = rest.chars();
            if let Some(first) = iter.next() {
                iter.next().is_none() && (first.is_alphanumeric() || first == '_')
            } else {
                false
            }
        }
    } else if s.starts_with(b"$") {
        is_ident(&s[1..])
            || (s.len() >= 2
                && s[1].is_ascii_digit()
                && s[1] != b'0'
                && s[1..].iter().all(|&ch| ch.is_ascii_digit()))
            || SYMBOL_NAMES.contains(s)
    } else if is_ident_start(s) {
        if s.ends_with(b"!") || s.ends_with(b"?") || s.ends_with(b"=") {
            is_ident_cont(s[..s.len() - 1].as_bstr())
        } else {
            is_ident_cont(s)
        }
    } else {
        SYMBOL_NAMES.contains(s)
    }
}

fn is_ident(s: &BStr) -> bool {
    is_ident_start(s) && is_ident_cont(s)
}

fn is_ident_start(s: &BStr) -> bool {
    s.len() >= 1 && (s[0].is_ascii_alphabetic() || s[0] == b'_' || s[0] >= 0x80)
}

fn is_ident_cont(s: &BStr) -> bool {
    s[1..]
        .iter()
        .all(|&ch| ch.is_ascii_alphanumeric() || ch == b'_' || ch >= 0x80)
}

static SYMBOL_NAMES: Lazy<HashSet<&BStr>> = Lazy::new(|| {
    vec![
        &b"<"[..],
        b"<<",
        b"<=",
        b"<=>",
        b">",
        b">>",
        b">=",
        b"=~",
        b"==",
        b"===",
        b"*",
        b"**",
        b"+",
        b"+@",
        b"-",
        b"-@",
        b"|",
        b"^",
        b"&",
        b"/",
        b"%",
        b"~",
        b"`",
        b"[]",
        b"[]=",
        b"!",
        b"!=",
        b"!~",
        b"$~",
        b"$*",
        b"$$",
        b"$?",
        b"$!",
        b"$@",
        b"$/",
        b"$\\",
        b"$;",
        b"$,",
        b"$.",
        b"$=",
        b"$:",
        b"$<",
        b"$>",
        b"$\"",
        b"$&",
        b"$`",
        b"$'",
        b"$+",
        b"$0",
    ]
    .into_iter()
    .map(|sym| sym.as_bstr())
    .collect::<HashSet<_>>()
});

#[cfg(test)]
mod tests {
    use bstr::BStr;

    use super::*;

    #[test]
    fn test_rb_str_inspect() {
        let testcases: Vec<(&BStr, &str)> = vec![
            (b"".as_bstr(), "\"\""),
            (b"foo".as_bstr(), "\"foo\""),
            (b"\xE3\x81\x84\xE3\x82\x8D\xE3\x81\xAF".as_bstr(), "\"いろは\""),
            (
                b"\x00\x01\x02\x03\x04\x05\x06\x07\x08\t\n\x0B\0x0C\r\x0E\x0F".as_bstr(),
                "\"\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\a\\b\\t\\n\\v\\u0000x0C\\r\\u000E\\u000F\"",
            ),
            (
                b"\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F".as_bstr(),
                "\"\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017\\u0018\\u0019\\u001A\\e\\u001C\\u001D\\u001E\\u001F\"",
            ),
            (b" !\"#$%&'()*+,-./".as_bstr(), "\" !\\\"\\#$%&'()*+,-./\""),
            (b"0123456789:;<=>?".as_bstr(), "\"0123456789:;<=>?\""),
            (b"@ABCDEFGHIJKLMNO".as_bstr(), "\"@ABCDEFGHIJKLMNO\""),
            (b"PQRSTUVWXYZ[\\]^_".as_bstr(), "\"PQRSTUVWXYZ[\\\\]^_\""),
            (b"`abcdefghijklmno".as_bstr(), "\"`abcdefghijklmno\""),
            (b"pqrstuvwxyz{|}~\x7F".as_bstr(), "\"pqrstuvwxyz{|}~\\u007F\""),
            (b"pqrstuvwxyz{|}~\x7F".as_bstr(), "\"pqrstuvwxyz{|}~\\u007F\""),
            (b"\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8A\x8B\x8C\x8D\x8E\x8F".as_bstr(), "\"\\x80\\x81\\x82\\x83\\x84\\x85\\x86\\x87\\x88\\x89\\x8A\\x8B\\x8C\\x8D\\x8E\\x8F\""),
            (b"# #( #{ #[ #@ #$ ##".as_bstr(), "\"# #( \\#{ #[ \\#@ \\#$ ##\""),
            (b"\xEF\xBF\xBC\xEF\xBF\xBD\xEF\xBF\xBE\xEF\xBF\xBF".as_bstr(), "\"\u{FFFC}\u{FFFD}\\uFFFE\\uFFFF\""),
            (b"\xF0\x90\x80\x80\xF0\x90\x80\x81\xF0\x90\x80\x82\xF0\x90\x80\x83".as_bstr(), "\"\u{10000}\u{10001}\u{10002}\u{10003}\""),
        ];
        for (input, expected) in &testcases {
            let result = rb_str_inspect(input).to_string();
            assert_eq!(&*result, *expected);
        }
    }
}
