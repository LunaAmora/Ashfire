use ashfire_types::core::{Location, TokenType, Typed};
use firelib::lexer::Loc;

#[derive(Clone, Copy)]
pub struct TypeFrame(pub TokenType, pub Loc);

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
