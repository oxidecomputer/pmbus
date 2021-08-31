#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Bitpos(pub u8);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Bitwidth(pub u8);

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Error {
    ShortData,
    InvalidCode,
    ValueOutOfRange,
    InvalidMode,
    InvalidSentinel,
    MissingCoefficients,
    InvalidReplacement,
    OverflowReplacement,
    InvalidField,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Replacement {
    Float(f32),
    Integer(u32),
    Boolean(bool),
}

pub trait Field: core::fmt::Debug {
    /// Returns true if this field is a bitfield. If this returns false,
    /// [`Field::bits`] will return 0 for the position and the entire value
    /// width for the width.
    fn bitfield(&self) -> bool;

    /// Returns the bits that the field covers
    fn bits(&self) -> (Bitpos, Bitwidth);

    /// Returns the name of the field
    fn name(&self) -> &'static str;

    /// Returns the description of the field
    fn desc(&self) -> &'static str;
}

pub trait Value: core::fmt::Display + core::fmt::Debug {
    /// Returns the name of this value
    fn name(&self) -> &'static str;

    /// Returns the description of this value
    fn desc(&self) -> &'static str;

    /// Returns the raw value for this value
    fn raw(&self) -> u32;

    fn scalar(&self) -> bool;
}

pub trait Command: core::fmt::Debug {
    fn name(&self) -> &'static str;
    fn read_op(&self) -> Operation;
    fn write_op(&self) -> Operation;
}

pub type VOutMode = VOUT_MODE::CommandData;

pub trait CommandData {
    fn interpret(
        &self,
        mode: impl Fn() -> VOutMode,
        iter: impl FnMut(&dyn Field, &dyn Value),
    ) -> Result<(), Error>;

    fn mutate(
        &mut self,
        mode: impl Fn() -> VOutMode,
        iter: impl FnMut(&dyn Field, &dyn Value) -> Option<Replacement>,
    ) -> Result<(), Error>;

    fn fields(iter: impl FnMut(&dyn Field)) -> Result<(), Error>;

    fn sentinels(
        field: Bitpos,
        iter: impl FnMut(&dyn Value),
    ) -> Result<(), Error>;

    fn raw(&self) -> (u32, Bitwidth);
    fn command(&self, cb: impl FnMut(&dyn Command));
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct WholeField(&'static str, Bitwidth);

impl Field for WholeField {
    fn bitfield(&self) -> bool {
        false
    }

    fn bits(&self) -> (Bitpos, Bitwidth) {
        (Bitpos(0), self.1)
    }

    fn name(&self) -> &'static str {
        "scalar"
    }

    fn desc(&self) -> &'static str {
        self.0
    }
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
