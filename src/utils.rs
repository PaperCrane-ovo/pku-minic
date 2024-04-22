use koopa::ir::entities::{ValueData, ValueKind};

pub fn is_const(value_data: &ValueData) -> bool {
    matches!(value_data.kind(), ValueKind::Integer(_)|ValueKind::ZeroInit(_))
}