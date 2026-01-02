#![allow(clippy::must_use_candidate, clippy::return_self_not_must_use)]

use std::hash::{Hash, Hasher};
use std::ops::Deref;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Hash for Span {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.end.hash(state);
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn empty() -> Self {
        Self { start: 0, end: 0 }
    }

    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn test_span_new() {
        let span = Span::new(10, 20);
        assert_eq!(span.start, 10);
        assert_eq!(span.end, 20);
    }

    #[test]
    fn test_span_empty() {
        let span = Span::empty();
        assert_eq!(span.start, 0);
        assert_eq!(span.end, 0);
    }

    #[test]
    fn test_span_merge() {
        let s1 = Span::new(5, 10);
        let s2 = Span::new(15, 20);
        let merged = s1.merge(&s2);
        assert_eq!(merged.start, 5);
        assert_eq!(merged.end, 20);
    }

    #[test]
    fn test_span_hash() {
        let mut set = HashSet::new();
        set.insert(Span::new(0, 5));
        set.insert(Span::new(0, 5));
        set.insert(Span::new(5, 10));
        assert_eq!(set.len(), 2);
    }

    #[test]
    fn test_spanned_new() {
        let spanned = Spanned::new(42, Span::new(0, 2));
        assert_eq!(spanned.value, 42);
        assert_eq!(spanned.span.start, 0);
        assert_eq!(spanned.span.end, 2);
    }

    #[test]
    fn test_spanned_deref() {
        let spanned = Spanned::new(String::from("hello"), Span::new(0, 5));
        assert_eq!(spanned.len(), 5);
    }
}
