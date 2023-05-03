use crate::lexer::Loc;

pub type Spanned<T> = (T, Loc);

pub trait Span<T> {
    fn new<F>(from: Spanned<F>) -> Self
    where
        T: From<F>;
}

impl<T> Span<T> for Spanned<T> {
    fn new<F>((from, loc): Spanned<F>) -> Self
    where
        T: From<F>,
    {
        (from.into(), loc)
    }
}
