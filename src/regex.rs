use crate::*;
use ::regex;

serde! {
    pub struct Regex {
        pub pattern: String,
        pub unicode: Option<bool>,
        pub case_insensitive: Option<bool>,
        pub multi_line: Option<bool>,
        pub dot_matches_new_line: Option<bool>,
        pub crlf: Option<bool>,
        pub line_terminator: Option<u8>,
        pub swap_greed: Option<bool>,
        pub ignore_whitespace: Option<bool>,
        pub octal: Option<bool>,
        pub size_limit: Option<usize>,
        pub dfa_size_limit: Option<usize>,
        pub nest_limit: Option<u32>,
    }
}

impl Regex {
    pub fn builder(self) -> regex::RegexBuilder {
        let Self {
            pattern,
            unicode,
            case_insensitive,
            multi_line,
            dot_matches_new_line,
            crlf,
            line_terminator,
            swap_greed,
            ignore_whitespace,
            octal,
            size_limit,
            dfa_size_limit,
            nest_limit,
        } = self;
        let mut b = regex::RegexBuilder::new(&pattern);
        (&mut b)
            .tap_opt(unicode, |b, v| b.unicode(v))
            .tap_opt(case_insensitive, |b, v| b.case_insensitive(v))
            .tap_opt(multi_line, |b, v| b.multi_line(v))
            .tap_opt(dot_matches_new_line, |b, v| b.dot_matches_new_line(v))
            .tap_opt(crlf, |b, v| b.crlf(v))
            .tap_opt(line_terminator, |b, v| b.line_terminator(v))
            .tap_opt(swap_greed, |b, v| b.swap_greed(v))
            .tap_opt(ignore_whitespace, |b, v| b.ignore_whitespace(v))
            .tap_opt(octal, |b, v| b.octal(v))
            .tap_opt(size_limit, |b, v| b.size_limit(v))
            .tap_opt(dfa_size_limit, |b, v| b.dfa_size_limit(v))
            .tap_opt(nest_limit, |b, v| b.nest_limit(v));
        b
    }
}

serde! {
    pub struct RegexSet {
        pub patterns: Vec<String>,
        pub unicode: Option<bool>,
        pub case_insensitive: Option<bool>,
        pub multi_line: Option<bool>,
        pub dot_matches_new_line: Option<bool>,
        pub crlf: Option<bool>,
        pub line_terminator: Option<u8>,
        pub swap_greed: Option<bool>,
        pub ignore_whitespace: Option<bool>,
        pub octal: Option<bool>,
        pub size_limit: Option<usize>,
        pub dfa_size_limit: Option<usize>,
        pub nest_limit: Option<u32>,
    }
}
impl RegexSet {
    pub fn builder(self) -> regex::RegexSetBuilder {
        let Self {
            patterns,
            unicode,
            case_insensitive,
            multi_line,
            dot_matches_new_line,
            crlf,
            line_terminator,
            swap_greed,
            ignore_whitespace,
            octal,
            size_limit,
            dfa_size_limit,
            nest_limit,
        } = self;
        let mut b = regex::RegexSetBuilder::new(patterns);
        (&mut b)
            .tap_opt(unicode, |b, v| b.unicode(v))
            .tap_opt(case_insensitive, |b, v| b.case_insensitive(v))
            .tap_opt(multi_line, |b, v| b.multi_line(v))
            .tap_opt(dot_matches_new_line, |b, v| b.dot_matches_new_line(v))
            .tap_opt(crlf, |b, v| b.crlf(v))
            .tap_opt(line_terminator, |b, v| b.line_terminator(v))
            .tap_opt(swap_greed, |b, v| b.swap_greed(v))
            .tap_opt(ignore_whitespace, |b, v| b.ignore_whitespace(v))
            .tap_opt(octal, |b, v| b.octal(v))
            .tap_opt(size_limit, |b, v| b.size_limit(v))
            .tap_opt(dfa_size_limit, |b, v| b.dfa_size_limit(v))
            .tap_opt(nest_limit, |b, v| b.nest_limit(v));
        b
    }
}
