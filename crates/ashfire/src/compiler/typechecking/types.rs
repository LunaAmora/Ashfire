use ashfire_types::{
    core::{Location, Typed},
    data::DataType,
};
use firelib::lexer::Loc;

#[derive(Clone, Copy)]
pub struct TypeFrame(pub DataType, pub Loc);

impl Typed for TypeFrame {
    fn get_type(&self) -> DataType {
        self.0
    }
}

impl Location for TypeFrame {
    fn loc(&self) -> Loc {
        self.1
    }
}
