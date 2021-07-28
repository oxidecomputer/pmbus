#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Bitpos(pub u8);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Bitwidth(pub u8);

pub trait Field {
    fn bits(&self) -> (Bitpos, Bitwidth);
    fn name(&self) -> &'static str;
}

pub trait Value: core::fmt::Display {
    fn desc(&self) -> &'static str;
    fn raw(&self) -> u32;
}

pub trait CommandData {
    fn fields(&self, iter: impl FnMut(&dyn Field, &dyn Value));
    fn raw(&self) -> (u32, Bitwidth);
}

include!(concat!(env!("OUT_DIR"), "/databytes.rs"));
