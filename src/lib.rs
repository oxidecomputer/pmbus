#![no_std]
//
// Note that the below rustdoc is used by `cargo readme` to generate the
// README, but due to https://github.com/livioribeiro/cargo-readme/issues/70,
// it must be cleaned up manually, e.g.:
//
// ```
// cargo readme | sed 's/\[\(`[^`]*`\)]/\1/g' > README.md
// ```
//

//! pmbus: A crate for PMBus manipulation
//!
//! This is a no_std crate that expresses the PMBus protocol, as described in
//! the PMBus 1.3 specifcation.  This crate is intended to be generic with
//! respect to implementation and usable by software that will directly
//! communicate with PMBus devices via SMBus/I2C as well as by software that
//! merely wishes to make sense of the PMBus protocol (e.g., debuggers or
//! analyzers running on a host).  For PMBus, this can be a bit of a
//! challenge, as much of the definition is left up to a particular device
//! (that is, much is implementation-defined).  Our two use cases are
//! therefore divergent in their needs:
//!
//! 1. The embedded system that is speaking to a particular PMBus device *in
//!    situ* is likely to know (and want to use) the special capabilities of a
//!    given device.  That is, these use cases know their target device at
//!    compile time, and have no need or desire to dynamically discover their
//!    device capabilities.
//!
//! 2. The host-based system that is trying to make sense of PMBus is *not*
//!    necessarily going to know the specifics of the attached devices at
//!    compile time; it is going to want to allow the device to be specified
//!    (or otherwise dynamically determined) and then discover that device's
//!    capabilities dynamically -- even if only to pass those capabilities on
//!    to the user.
//!
//! These use cases are in tension:  we want the first to be tight and
//! typesafe while still allowing for the more dynamic second use case.  We
//! balance these two cases by dynamically compiling the crate based on
//! per-device RON files that specify the commands and their corresponding
//! destructured data; each device is in its own module, with each PMBus
//! command further having its own module that contains the types for the
//! corresponding command data.
//!
//! As a concrete example, [`commands::OPERATION`] contains an implementation
//! of the [`CommandData`] trait for the fields for the common PMBus
//! `OPERATION` command.  For each device, there is a device-specific
//! `OPERATION` module -- e.g.  `[commands::adm1272::OPERATION]` -- that may
//! extend or override the common definition.  Further, the device may define
//! its own constants; for example, while PMBus defines the command code
//! `0xd4` to be [`CommandCode::MFR_SPECIFIC_D4`], the ADM1272 defines this to
//! be `PMON_CONFIG`, a device-specific power monitor configuration register.
//! There therefore exists a [`commands::adm1272::PMON_CONFIG`] module that
//! understands the full (ADM1272-specific) functionality.  For code that
//! wishes to be device agnostic but still be able to display contents, there
//! exists a [`Device::interpret`] that given a device, a code, and a payload,
//! calls the specified closure to iterate over fields and values.  
//!
//! A final (crucial) constraint is that this crate remains `no_std`; it
//! performs no dynamic allocation and in general relies on program text
//! rather than table lookups -- with the knowledge that the compiler is very
//! good about dead code elimination and will not include unused program text
//! in the embedded system.
//!
//! If it needs to be said:  all of this adds up to specifications almost
//! entirely via RON definitions -- and an absolutely unholy `build.rs` to
//! assemble it all at build time.  Paraphrasing [the late Roger
//! Faulker](https://www.usenix.org/memoriam-roger-faulkner),
//! terrible things are sometimes required for beautiful abstractions.
//!

pub use num_derive::{FromPrimitive, ToPrimitive};
pub use num_traits::float::FloatCore;
pub use num_traits::{FromPrimitive, ToPrimitive};

mod operation;
pub use crate::operation::Operation;

pub mod units;

pub mod commands;
pub use crate::commands::devices;
pub use crate::commands::CommandCode;
pub use crate::commands::Device;

/// The position, in bits, of a field.  If a field contains multiple bits, this
/// position represents the **least** significant bit of the multi-bit field.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Bitpos(pub u8);

/// The width, in bits, of a field.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Bitwidth(pub u8);

/// A PMBus error
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Error {
    /// Data payload was shorter than expected by the command
    ShortData,
    /// Command code is invalid
    InvalidCode,
    /// Value outside of range that can be represented
    ValueOutOfRange,
    /// Specified VOutMode is not valid
    InvalidMode,
    /// Value in the field did not correspond to a known sentinel value
    InvalidSentinel,
    /// VOutMode indicates Direct, but device has no known coefficients
    MissingCoefficients,
    /// Indicated replacement value is invalid
    InvalidReplacement,
    /// Indicated replacement value overflows
    OverflowReplacement,
    /// Specified bit position does not correspond to any field
    InvalidField,
}

/// A value used to replace a field when mutating command data.  In general,
/// this interface should not be used in an embedded environment, which
/// should in general select to explicitly set desired fields.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Replacement {
    Float(f32),
    Integer(u32),
    Boolean(bool),
}

/// A trait to express a field as part of the reflection interface.  As
/// with all of the reflection interfaces, this should generally not be
/// needed in an embedded environment where devices (and their capabilities)
/// are expected to be known.
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

/// A trait to express the value contained by a field as part of the
/// reflection interface.  It is expected that consumers of the reflection
/// interface are generally displaying contents; while this can return the
/// raw value, its use is discouraged.
pub trait Value: core::fmt::Display + core::fmt::Debug {
    /// Returns the name of this value
    fn name(&self) -> &'static str;

    /// Returns the description of this value
    fn desc(&self) -> &'static str;

    /// Returns the raw value for this value
    fn raw(&self) -> u32;

    fn scalar(&self) -> bool;
}

/// A trait to express a PMBus command
pub trait Command: core::fmt::Debug {
    /// Returns the name of this command
    fn name(&self) -> &'static str;

    /// Returns the operation for reading data with this command, if any
    fn read_op(&self) -> Operation;

    /// Returns the operation for writing data with this command, if any
    fn write_op(&self) -> Operation;
}

/// A regrettable complexity of PMBus is that the output of one command --
/// VOUT_MODE -- dictates how others are interpreted.  This command therefore
/// must be enshrined as special even for consumers of the reflection
/// interface.
pub type VOutMode = commands::VOUT_MODE::CommandData;

/// A trait to express PMBus command data.  This is part of the reflection
/// interfact, and allows for consumers to iterate over fields (via
/// [`Device::interpret`] or optionally mutate a particular field (via
/// [`Device::mutate`]).
pub trait CommandData {
    /// Interprets the contents of command data. This takes a closure `mode`
    /// that returns `VOutMode` if (and only if) needed, as well as a
    /// closure to be called for each field and its value.
    fn interpret(
        &self,
        mode: impl Fn() -> VOutMode,
        iter: impl FnMut(&dyn Field, &dyn Value),
    ) -> Result<(), Error>;

    /// Mutates the contents of command data.
    fn mutate(
        &mut self,
        mode: impl Fn() -> VOutMode,
        iter: impl FnMut(&dyn Field, &dyn Value) -> Option<Replacement>,
    ) -> Result<(), Error>;

    /// Iterates over the command data fields absent any data.
    fn fields(iter: impl FnMut(&dyn Field)) -> Result<(), Error>;

    /// Iterates over the sentinels for the field at a particular bit
    /// position.
    fn sentinels(
        field: Bitpos,
        iter: impl FnMut(&dyn Value),
    ) -> Result<(), Error>;

    /// Returns the raw value associated with this data.
    fn raw(&self) -> (u32, Bitwidth);

    /// Executes the specified closure in the context of the [`Command`]
    /// that corresponds to this command data
    fn command(&self, cb: impl FnMut(&dyn Command));
}

/// A [`Field`]-implementing structure that denotes that the entire command
/// data payload is a single, numeric field.
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

///
/// The coefficients spelled out by PMBus for use in the DIRECT data format
/// (Part II, Sec. 7.4). The actual values used will depend on the device and
/// the condition.
///
#[derive(Copy, Clone, PartialEq, Debug)]
#[allow(non_snake_case)]
pub struct Coefficients {
    /// Slope coefficient. Two byte signed off the wire (but potentially
    /// larger after adjustment).
    pub m: i32,
    /// Offset. Two-byte, signed.
    pub b: i16,
    /// Exponent. One-byte, signed.
    pub R: i8,
}

///
/// A datum in the DIRECT data format.
///
#[derive(Copy, Clone, Debug)]
pub struct Direct(pub u16, pub Coefficients);

impl Direct {
    #[allow(dead_code)]
    pub fn to_real(&self) -> f32 {
        let coefficients = &self.1;
        let m: f32 = coefficients.m as f32;
        let b: f32 = coefficients.b.into();
        let exp: i32 = coefficients.R.into();
        let y: f32 = (self.0 as i16).into();

        (y * f32::powi(10.0, -exp) - b) / m
    }

    #[allow(dead_code)]
    pub fn from_real(x: f32, coefficients: Coefficients) -> Self {
        let m: f32 = coefficients.m as f32;
        let b: f32 = coefficients.b.into();
        let exp: i32 = coefficients.R.into();
        let y: f32 = (m * x + b) * f32::powi(10.0, exp);

        Self(y.round() as u16, coefficients)
    }
}

///
/// A datum in the LINEAR11 data format.
///
#[derive(Copy, Clone, Debug)]
pub struct Linear11(pub u16);

//
// The LINEAR11 format is outlined in Section 7.3 of the PMBus specification.
// It consists of 5 bits of signed exponent (N), and 11 bits of signed mantissa
// (Y):
//
// |<------------ high byte ------------>|<--------- low byte ---------->|
// +---+---+---+---+---+     +---+---+---+---+---+---+---+---+---+---+---+
// | 7 | 6 | 5 | 4 | 3 |     | 2 | 1 | 0 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
// +---+---+---+---+---+     +---+---+---+---+---+---+---+---+---+---+---+
//
// |<------- N ------->|     |<------------------- Y ------------------->|
//
// The relation between these values and the real world value is:
//
//   X = Y * 2^N
//
const LINEAR11_Y_WIDTH: u16 = 11;
const LINEAR11_Y_MAX: i16 = (1 << (LINEAR11_Y_WIDTH - 1)) - 1;
const LINEAR11_Y_MIN: i16 = -(1 << (LINEAR11_Y_WIDTH - 1));
const LINEAR11_Y_MASK: i16 = (1 << LINEAR11_Y_WIDTH) - 1;

const LINEAR11_N_WIDTH: u16 = 5;
const LINEAR11_N_MAX: i16 = (1 << (LINEAR11_N_WIDTH - 1)) - 1;
const LINEAR11_N_MIN: i16 = -(1 << (LINEAR11_N_WIDTH - 1));
const LINEAR11_N_MASK: i16 = (1 << LINEAR11_N_WIDTH) - 1;

impl Linear11 {
    pub fn to_real(&self) -> f32 {
        let n = (self.0 as i16) >> LINEAR11_Y_WIDTH;
        let y = ((self.0 << LINEAR11_N_WIDTH) as i16) >> LINEAR11_N_WIDTH;

        y as f32 * f32::powi(2.0, n.into())
    }

    #[allow(dead_code)]
    pub fn from_real(x: f32) -> Option<Self> {
        //
        // We get our closest approximation when we have as many digits as
        // possible in Y; to determine the value of N that will satisfy this,
        // we pick a value of Y that is further away from 0 (more positive or
        // more negative) than our true Y and determine what N would be, taking
        // the ceiling of this value.  If this value exceeds our resolution for
        // N, we cannot represent the value.
        //
        let n = if x >= 0.0 {
            x / LINEAR11_Y_MAX as f32
        } else {
            x / LINEAR11_Y_MIN as f32
        };

        let n = f32::ceil(libm::log2f(n)) as i16;

        if n < LINEAR11_N_MIN || n > LINEAR11_N_MAX {
            None
        } else {
            let exp = f32::powi(2.0, n.into());
            let y = x / exp;

            let high = ((n & LINEAR11_N_MASK) as u16) << LINEAR11_Y_WIDTH;
            let low = ((y as i16) & LINEAR11_Y_MASK) as u16;

            Some(Linear11(high | low))
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ULinear16Exponent(pub i8);

///
/// A datum in the ULINEAR16 format.  ULINEAR16 is used only for voltage;
/// the exponent comes from VOUT_MODE.
///
pub struct ULinear16(pub u16, pub ULinear16Exponent);

impl ULinear16 {
    pub fn to_real(&self) -> f32 {
        let exp = self.1 .0;
        self.0 as f32 * f32::powi(2.0, exp.into())
    }

    pub fn from_real(x: f32, exp: ULinear16Exponent) -> Option<Self> {
        let val = (x / f32::powi(2.0, exp.0.into())).round();

        if val > core::u16::MAX as f32 {
            None
        } else {
            Some(Self(val as u16, exp))
        }
    }
}
