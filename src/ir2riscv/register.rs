#![allow(dead_code)]
use std::fmt::Display;
 // Add this import




#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]

pub enum RegId{
    X0,
    RA,
    SP,
    GP,
    TP,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    S0,
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
}

impl Display for RegId{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self{
            RegId::X0 => write!(f, "x0"),
            RegId::RA => write!(f, "ra"),
            RegId::SP => write!(f, "sp"),
            RegId::GP => write!(f, "gp"),
            RegId::TP => write!(f, "tp"),
            RegId::A0 => write!(f, "a0"),
            RegId::A1 => write!(f, "a1"),
            RegId::A2 => write!(f, "a2"),
            RegId::A3 => write!(f, "a3"),
            RegId::A4 => write!(f, "a4"),
            RegId::A5 => write!(f, "a5"),
            RegId::A6 => write!(f, "a6"),
            RegId::A7 => write!(f, "a7"),
            RegId::T0 => write!(f, "t0"),
            RegId::T1 => write!(f, "t1"),
            RegId::T2 => write!(f, "t2"),
            RegId::T3 => write!(f, "t3"),
            RegId::T4 => write!(f, "t4"),
            RegId::T5 => write!(f, "t5"),
            RegId::T6 => write!(f, "t6"),
            RegId::S0 => write!(f, "s0"),
            RegId::S1 => write!(f, "s1"),
            RegId::S2 => write!(f, "s2"),
            RegId::S3 => write!(f, "s3"),
            RegId::S4 => write!(f, "s4"),
            RegId::S5 => write!(f, "s5"),
            RegId::S6 => write!(f, "s6"),
            RegId::S7 => write!(f, "s7"),
            RegId::S8 => write!(f, "s8"),
            RegId::S9 => write!(f, "s9"),
            RegId::S10 => write!(f, "s10"),
            RegId::S11 => write!(f, "s11"),
        }
    }
}

impl From<String> for RegId{
    fn from(s:String) -> Self {
        match &s[..]{
            "x0" => RegId::X0,
            "ra" => RegId::RA,
            "sp" => RegId::SP,
            "gp" => RegId::GP,
            "tp" => RegId::TP,
            "a0" => RegId::A0,
            "a1" => RegId::A1,
            "a2" => RegId::A2,
            "a3" => RegId::A3,
            "a4" => RegId::A4,
            "a5" => RegId::A5,
            "a6" => RegId::A6,
            "a7" => RegId::A7,
            "t0" => RegId::T0,
            "t1" => RegId::T1,
            "t2" => RegId::T2,
            "t3" => RegId::T3,
            "t4" => RegId::T4,
            "t5" => RegId::T5,
            "t6" => RegId::T6,
            "s0" => RegId::S0,
            "s1" => RegId::S1,
            "s2" => RegId::S2,
            "s3" => RegId::S3,
            "s4" => RegId::S4,
            "s5" => RegId::S5,
            "s6" => RegId::S6,
            "s7" => RegId::S7,
            "s8" => RegId::S8,
            "s9" => RegId::S9,
            "s10" => RegId::S10,
            "s11" => RegId::S11,
            _ => panic!("Invalid register name"),
        }
    }
}