use ashfire_types::core::{Location, TokenType, Typed};
use firelib::lexer::Loc;

#[derive(Clone, Copy)]
pub struct TypeFrame(TokenType, Loc);

impl TypeFrame {
    pub fn new<T: Typed + Location>(tok: T) -> Self {
        Self(tok.get_type(), tok.loc())
    }
}

impl Typed for TypeFrame {
    fn get_type(&self) -> TokenType {
        self.0
    }
}

impl Location for TypeFrame {
    fn loc(&self) -> Loc {
        self.1
    }
}

impl<T: Typed, L: Location> From<(T, L)> for TypeFrame {
    fn from(tuple: (T, L)) -> Self {
        Self(tuple.0.get_type(), tuple.1.loc())
    }
}

impl From<TypeFrame> for TokenType {
    fn from(frame: TypeFrame) -> Self {
        frame.get_type()
    }
}
