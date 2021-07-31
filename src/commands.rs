#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Bitpos(pub u8);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Bitwidth(pub u8);

#[derive(Copy, Clone, Debug)]
pub enum Error {
    ShortData,
}

pub trait Field {
    fn bits(&self) -> (Bitpos, Bitwidth);
    fn name(&self) -> &'static str;
}

pub trait Value: core::fmt::Display {
    fn scalar(&self) -> bool;
    fn desc(&self) -> &'static str;
    fn raw(&self) -> u32;
}

pub trait Command {
    fn read_op(&self) -> Operation;
    fn write_op(&self) -> Operation;
    fn data(
        &self,
        payload: &[u8],
        iter: impl Fn(&dyn Field, &dyn Value),
    ) -> Result<(), Error>;
}

pub trait CommandData {
    fn fields(&self, iter: impl FnMut(&dyn Field, &dyn Value));
    fn raw(&self) -> (u32, Bitwidth);
}

pub use crate::operation::Operation;

//
// This includes all of our common commands and their data payloads.
//
include!(concat!(env!("OUT_DIR"), "/commands.rs"));

//
// This includes all device-specifics commands and their data payloads.
//
include!(concat!(env!("OUT_DIR"), "/devices.rs"));
