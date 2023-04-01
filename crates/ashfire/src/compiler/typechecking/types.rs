use ashfire_types::{
    core::{Location, TokenType, Typed},
    data::DataType,
};
use firelib::lexer::Loc;

#[derive(Clone, Copy)]
pub struct TypeFrame(pub DataType, pub Loc);

impl Typed for TypeFrame {
    fn get_type(&self) -> TokenType {
        self.0.get_type()
    }
}

impl Location for TypeFrame {
    fn loc(&self) -> Loc {
        self.1
    }
}
