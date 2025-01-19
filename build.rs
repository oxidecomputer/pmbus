//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
use anyhow::{bail, Result};

//
// This is code that generates code, and it is therefore a bit of a mess.
// When trying to modify or extend this code, it is most helpful to look
// instead at the generated artifacts themselves:  this code has sacrificed
// its own aesthetics to make the generated code (relatively) clean.
//
use convert_case::{Case, Casing};
use ron::de::{from_reader, from_str};
use serde::Deserialize;
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fmt::Write;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::path::PathBuf;

#[derive(Debug, Deserialize)]
struct High(u8);

#[derive(Debug, Deserialize)]
struct Low(u8);

#[derive(Clone, Debug, Deserialize)]
struct Factor(f32);

#[derive(Clone, Debug, Deserialize)]
struct Base(i8);

#[derive(Debug, Deserialize)]
struct Value(u16, String);

//
// Each member of this enum must have a corresponding 1-tuple struct in
// crate::units::Units.
//
#[derive(
    Copy, Clone, Debug, Deserialize, Eq, PartialEq, Hash, Ord, PartialOrd,
)]
enum Units {
    Nanoseconds,
    Microseconds,
    Milliseconds,
    Seconds,
    Amperes,
    Volts,
    Celsius,
    Kilohertz,
    Rpm,
    Milliohms,
    VoltsPerMicrosecond,
    VoltsPerMillisecond,
    Watts,
    MillivoltsPerAmp,
    MillivoltsPerCelsius,
    Percent,
    Unitless,
}

impl Units {
    fn suffix(&self) -> &str {
        match self {
            Units::Nanoseconds => "ns",
            Units::Microseconds => "μs",
            Units::Milliseconds => "ms",
            Units::Seconds => "s",
            Units::Amperes => "A",
            Units::Milliohms => "mΩ",
            Units::Volts => "V",
            Units::Celsius => "°C",
            Units::Rpm => "RPM",
            Units::Watts => "W",
            Units::Kilohertz => "kHz",
            Units::VoltsPerMillisecond => "V/ms",
            Units::VoltsPerMicrosecond => "V/μs",
            Units::MillivoltsPerAmp => "mV/A",
            Units::MillivoltsPerCelsius => "mV/°C",
            Units::Percent => "%",
            Units::Unitless => "",
        }
    }
}

#[derive(Clone, Debug, Deserialize)]
enum Sign {
    Signed,
    Unsigned,
}

#[derive(Debug, Deserialize)]
enum Values<T> {
    /// Value is a scalar
    Scalar(Sign),
    /// Value is a sentinel
    Sentinels(T),
    /// Value is of form: real_value = value / Factor
    FixedPointUnits(Factor, Units),
    /// Value is of form: real_value = Base**value / Factor
    LogFactorUnits(Base, Factor, Units),
}

#[derive(Copy, Clone, Debug, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
#[allow(non_snake_case)]
struct Coefficients {
    m: i32,
    b: i16,
    R: i8,
}

/// The numerical format of a particular value
#[derive(Clone, Debug, Deserialize)]
enum Format {
    /// LINEAR11 as defined by PMBus (7.3)
    Linear11,

    /// DIRECT as defined by PMBus (7.4), with static coefficients
    Direct(Coefficients),

    /// DIRECT, but with coefficients that are determined at runtime
    RuntimeDirect,

    /// Output that depends on the VOUT_MODE command
    #[allow(unused)]
    VOutMode(Sign),

    /// Unsigned fixed point with the specified scaling factor
    FixedPoint(Factor),

    /// Signed fixed point with the specified scaling factor
    SignedFixedPoint(Factor),
    Raw,
}

#[derive(Debug, Deserialize)]
enum Bits {
    Bitrange(High, Low),
    Bit(u8),
}

#[derive(Debug, Deserialize)]
struct Field {
    name: String,
    bits: Bits,
    values: Values<HashMap<String, Value>>,
}

#[derive(Clone, Debug, Deserialize)]
enum Operation {
    ReadByte,
    WriteByte,
    SendByte,
    ReadWord,
    WriteWord,
    WriteWord32,
    ReadWord32,
    ReadBlock,
    WriteBlock,
    ProcessCall,
    MfrDefined,
    Extended,
    Illegal,
    Unknown,
}

/// A PMBus command, a tuple that consists of:
///
/// - A `u8` that uniquely denotes the command
/// - A `String` that names the command
/// - An `Operation` that denotes how values are written (or `Illegal` if
///   the command does not allow values to be written)
/// - An `Operation` that denotes how values are read (or `Illegal` if the
///   the command does not allow values to be read
///
#[derive(Clone, Debug, Deserialize)]
struct Command(u8, String, Operation, Operation);

#[derive(Debug, Deserialize)]
struct CommandNumericFormat(String, Format, Units);

#[derive(Debug, Deserialize)]
struct CommandSynonym(String, String);

#[derive(Clone, Debug, Deserialize)]
struct Auxiliary(String, Operation);

#[derive(Debug, Deserialize)]
struct AuxiliaryNumericFormat(String, Format, Units);

#[derive(Debug, Deserialize)]
#[serde(transparent)]
struct Fields(
    #[serde(with = "::serde_with::rust::maps_duplicate_key_is_error")]
    HashMap<String, Field>,
);

#[derive(Debug, Deserialize)]
struct Auxiliaries {
    all: Vec<Auxiliary>,
    numerics: Vec<AuxiliaryNumericFormat>,
    #[serde(with = "::serde_with::rust::maps_duplicate_key_is_error")]
    structured: HashMap<String, Fields>,
}

#[derive(Debug, Deserialize)]
struct Commands {
    all: Vec<Command>,
    numerics: Vec<CommandNumericFormat>,
    #[serde(with = "::serde_with::rust::maps_duplicate_key_is_error")]
    structured: HashMap<String, Fields>,
    synonyms: Option<Vec<CommandSynonym>>,
    auxiliaries: Option<Auxiliaries>,
}

#[derive(Debug, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
#[allow(dead_code)]
struct Device {
    manufacturer: String,
    part: String,
    description: String,
    coefficients: Option<Coefficients>,
}

enum OutputCommand<'a> {
    PMBus(&'a str),
    Auxiliary(&'a str),
}

fn reg_sizes(cmds: &[Command]) -> Result<HashMap<String, Option<usize>>> {
    let mut sizes = HashMap::new();

    //
    // Note that we always treat a ReadBlock as a 128-bit quantity, the
    // largest that we can fit into a primitive.  Any register that attempts
    // to use more than 128-bits won't be able to be defined.
    //
    for cmd in cmds {
        let size = match cmd.3 {
            Operation::ReadByte => Some(1),
            Operation::ReadWord => Some(2),
            Operation::ReadWord32 => Some(4),
            Operation::ReadBlock => Some(16),
            Operation::WriteByte
            | Operation::WriteWord
            | Operation::WriteWord32
            | Operation::WriteBlock => {
                bail!("illegal read operation {:?} on {}", cmd.3, cmd.1);
            }
            _ => None,
        };

        sizes.insert(cmd.1.clone(), size);
    }

    Ok(sizes)
}

fn aux_sizes(auxs: &[Auxiliary]) -> Result<HashMap<String, Option<usize>>> {
    let mut sizes = HashMap::new();

    for aux in auxs {
        let size = match aux.1 {
            Operation::ReadByte => Some(1),
            Operation::ReadWord => Some(2),
            Operation::ReadWord32 => Some(4),
            _ => {
                bail!("illegal operation {:?} on aux {}", aux.1, aux.0);
            }
        };

        sizes.insert(aux.0.clone(), size);
    }

    Ok(sizes)
}

#[rustfmt::skip::macros(writeln)]
fn output_commands(
    cmds: &Commands,
    shadowing: Option<&Commands>,
) -> Result<String> {
    let mut s = String::new();

    writeln!(&mut s, r##"pub use num_derive::{{FromPrimitive, ToPrimitive}};
pub use num_traits::{{FromPrimitive, ToPrimitive}};"##)?;

    if shadowing.is_some() {
        writeln!(&mut s, r##"
use crate::Replacement;
use crate::VOutModeCommandData;"##)?;
    } else {
        writeln!(&mut s, r##"
use crate::Error;"##)?;
    }

    writeln!(&mut s, r##"
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq, FromPrimitive)]
#[repr(u8)]
pub enum CommandCode {{"##)?;

    for cmd in &cmds.all {
        writeln!(&mut s, "    {} = 0x{:x},", cmd.1, cmd.0)?;
    }

    writeln!(&mut s, r##"}}

impl crate::Command for CommandCode {{
    fn name(&self) -> &'static str {{
        match self {{"##)?;

    for cmd in &cmds.all {
        writeln!(&mut s,
            "            CommandCode::{} => \"{}\",", cmd.1, cmd.1)?;
    }

    writeln!(&mut s, "        }}\n    }}")?;

    writeln!(&mut s, r##"
    fn read_op(&self) -> Operation {{
        match self {{"##)?;

    for cmd in &cmds.all {
        writeln!(&mut s,
            "            CommandCode::{} => Operation::{:?},", cmd.1, cmd.3)?;
    }

    writeln!(&mut s, "        }}\n    }}")?;

    writeln!(&mut s, r##"
    fn write_op(&self) -> Operation {{
        match self {{"##)?;

    for cmd in &cmds.all {
        writeln!(&mut s,
            "            CommandCode::{} => Operation::{:?},", cmd.1, cmd.2)?;
    }

    writeln!(&mut s, "        }}\n    }}\n}}")?;

    let mut numerics = HashSet::new();
    let mut synonyms = HashSet::new();

    for cmd in &cmds.numerics {
        numerics.insert(&cmd.0);
    }

    if let Some(ref syn) = cmds.synonyms {
        for cmd in syn {
            synonyms.insert(&cmd.0);
        }
    }

    //
    // If we are a device, we need to go through any numerics that we're
    // shadowing as well, as they will have a device-local definition.
    //
    if let Some(shadowing) = shadowing {
        for cmd in &shadowing.numerics {
            numerics.insert(&cmd.0);
        }
    }

    writeln!(&mut s, r##"
impl CommandCode {{
    pub fn interpret(
        &self,
        payload: &[u8],
        mode: impl Fn() -> VOutModeCommandData,
        iter: impl FnMut(&dyn Field, &dyn Value)
    ) -> Result<(), Error> {{
        match self {{"##)?;

    for cmd in &cmds.all {
        if !cmds.structured.contains_key(&cmd.1)
            && !numerics.contains(&cmd.1)
            && !synonyms.contains(&cmd.1)
        {
            continue;
        }

        writeln!(&mut s, r##"            CommandCode::{} => {{
                use {}::CommandData;
                if let Some(data) = CommandData::from_slice(payload) {{
                    data.interpret(mode, iter)
                }} else {{
                    Err(Error::ShortData)
                }}
            }}"##, cmd.1, cmd.1)?;
    }

    if shadowing.is_some() {
        //
        // For devices, we want to fallback to calling the common data
        // method.
        //
        writeln!(&mut s, r##"            _ => {{
                let code = *self as u8;
                match super::CommandCode::from_u8(code) {{
                    Some(cmd) => cmd.interpret(payload, mode, iter),
                    None => Ok(())
                }}
            }}"##)?;
    } else {
        writeln!(&mut s, "            _ => Ok(()),")?;
    }

    writeln!(&mut s, r##"        }}
    }}

    pub fn mutate(
        &self,
        payload: &mut [u8],
        mode: impl Fn() -> VOutModeCommandData,
        iter: impl FnMut(&dyn Field, &dyn Value) -> Option<Replacement>
    ) -> Result<(), Error> {{
        match self {{"##)?;

    for cmd in &cmds.all {
        if !cmds.structured.contains_key(&cmd.1)
            && !numerics.contains(&cmd.1)
            && !synonyms.contains(&cmd.1)
        {
            continue;
        }

        writeln!(&mut s, r##"            CommandCode::{} => {{
                use {}::CommandData;
                if let Some(mut data) = CommandData::from_slice(payload) {{
                    data.mutate(mode, iter)?;
                    data.to_slice(payload);
                    Ok(())
                }} else {{
                    Err(Error::ShortData)
                }}
            }}"##, cmd.1, cmd.1)?;
    }

    if shadowing.is_some() {
        //
        // For devices, we want to fallback to calling the common mutate
        // method.
        //
        writeln!(&mut s, r##"            _ => {{
                let code = *self as u8;
                match super::CommandCode::from_u8(code) {{
                    Some(cmd) => cmd.mutate(payload, mode, iter),
                    None => Ok(())
                }}
            }}"##)?;
    } else {
        writeln!(&mut s, "            _ => Ok(()),")?;
    }

    writeln!(&mut s, r##"        }}
    }}

    pub fn fields(
        &self,
        iter: impl FnMut(&dyn Field)
    ) -> Result<(), Error> {{
        match self {{"##)?;

    for cmd in &cmds.all {
        if !cmds.structured.contains_key(&cmd.1)
            && !numerics.contains(&cmd.1)
            && !synonyms.contains(&cmd.1)
        {
            continue;
        }

        writeln!(&mut s, r##"            CommandCode::{} => {{
                {}::CommandData::fields(iter)
            }}"##, cmd.1, cmd.1)?;
    }

    if shadowing.is_some() {
        //
        // For devices, we want to fallback to calling the common fields
        // method.
        //
        writeln!(&mut s, r##"            _ => {{
                let code = *self as u8;
                match super::CommandCode::from_u8(code) {{
                    Some(cmd) => cmd.fields(iter),
                    None => Ok(())
                }}
            }}"##)?;
    } else {
        writeln!(&mut s, "            _ => Ok(()),")?;
    }

    writeln!(&mut s, r##"        }}
    }}

    pub fn sentinels(
        &self,
        field: Bitpos,
        iter: impl FnMut(&dyn Value)
    ) -> Result<(), Error> {{
        match self {{"##)?;

    for cmd in &cmds.all {
        if !cmds.structured.contains_key(&cmd.1)
            && !numerics.contains(&cmd.1)
            && !synonyms.contains(&cmd.1)
        {
            continue;
        }

        writeln!(&mut s, r##"            CommandCode::{} => {{
                {}::CommandData::sentinels(field, iter)
            }}"##, cmd.1, cmd.1)?;
    }

    if shadowing.is_some() {
        //
        // For devices, we want to fallback to calling the common fields
        // method.
        //
        writeln!(&mut s, r##"            _ => {{
                let code = *self as u8;
                match super::CommandCode::from_u8(code) {{
                    Some(cmd) => cmd.sentinels(field, iter),
                    None => Ok(())
                }}
            }}"##)?;
    } else {
        writeln!(&mut s, "            _ => Ok(()),")?;
    }

    writeln!(&mut s, "        }}\n    }}\n}}")?;

    Ok(s)
}

fn bitrange(bits: &Bits) -> (u8, u8) {
    match bits {
        Bits::Bit(pos) => (*pos, *pos),
        Bits::Bitrange(High(high), Low(low)) => (*high, *low),
    }
}

#[rustfmt::skip::macros(bail)]
fn validate(
    cmd: &str,
    fields: &Fields,
    sizes: &HashMap<String, Option<usize>>,
    units: &mut HashSet<Units>,
) -> Result<(usize, usize)> {
    let mut highest = 0;
    let fields = &fields.0;

    let size = match sizes.get(cmd) {
        Some(Some(size)) => *size,
        Some(None) => {
            bail!("command {} does not allow a register", cmd);
        }
        None => {
            bail!("command {} does not exist", cmd);
        }
    };

    let bits = size * 8;
    let mut v: Vec<Option<&String>> = vec![None; bits];

    for (f, field) in fields {
        let (high, low) = bitrange(&field.bits);

        if high < low {
            bail!("{}: field \"{}\" has illegal bit range", cmd, f);
        }

        if high as usize >= bits {
            bail!("{}: field \"{}\" has high bit that exceeds size", cmd, f);
        }

        if high > highest {
            highest = high;
        }

        for bit in low..=high {
            match v[bit as usize] {
                None => {
                    v[bit as usize] = Some(f);
                }
                Some(o) => {
                    bail!(
                        "{}: field \"{}\" overlaps with \"{}\" at bit {}",
                        cmd, f, o, bit
                    );
                }
            }
        }

        if let Values::FixedPointUnits(_, unit) = field.values {
            units.insert(unit);
        }
    }

    //
    // If this a block read, we will trim our size to our highest known bit
    // to prevent a spurious short read.
    //
    if bits == 128 {
        let bits = (highest + 1).next_power_of_two();
        Ok((bits.into(), ((highest + 7) / 8).into()))
    } else {
        Ok((bits, size))
    }
}

#[rustfmt::skip::macros(writeln)]
fn output_scalar(name: &str, width: usize) -> Result<String> {
    let mut s = String::new();
    let bits = ((width + 7) / 8) * 8;

    writeln!(&mut s, r##"
    #[derive(Copy, Clone, Debug, PartialEq, FromPrimitive, ToPrimitive)]
    #[allow(non_camel_case_types)]
    pub struct {}(pub u{});

    impl {} {{
        fn name(&self) -> &'static str {{
            "scalar"
        }}

        fn desc(&self) -> &'static str {{
            "(scalar value)"
        }}
    }}"##, name, bits, name)?;

    Ok(s)
}

#[rustfmt::skip::macros(writeln)]
fn output_value(
    name: &str,
    desc: &str,
    values: &Values<HashMap<String, Value>>,
    width: usize,
) -> Result<String> {
    let mut s = String::new();

    let values = match values {
        Values::Sentinels(ref v) => v,
        Values::Scalar(_)
        | Values::FixedPointUnits(..)
        | Values::LogFactorUnits(..) => {
            return output_scalar(name, width);
        }
    };

    let mut sorted_values: Vec<_> = values.iter().collect();
    sorted_values.sort_by(|a, b| a.0.cmp(b.0));

    writeln!(&mut s, r##"
    /// Values that can be taken by the {} field
    #[derive(Copy, Clone, Debug, PartialEq, FromPrimitive, ToPrimitive)]
    #[allow(non_camel_case_types)]
    #[repr(C)]
    pub enum {} {{"##, desc, name)?;

    for (v, value) in &sorted_values {
        writeln!(&mut s, "        /// {}", value.1)?;
        writeln!(&mut s, "        {} = 0b{:0width$b},",
            v, value.0, width = width
        )?;
    }

    writeln!(&mut s, "    }}")?;

    writeln!(&mut s, r##"
    impl {} {{
        fn desc(&self) -> &'static str {{
            match self {{"##, name)?;

    for (v, value) in &sorted_values {
        writeln!(
            &mut s, "                {}::{} => \"{}\",",
            name, v, value.1
        )?;
    }

    writeln!(&mut s, "            }}\n        }}")?;

    writeln!(&mut s, r##"
        fn name(&self) -> &'static str {{
            match self {{"##)?;

    for v in &sorted_values {
        let v = v.0;
        writeln!(
            &mut s, "                {}::{} => \"{}\",",
            name, v, v
        )?;
    }

    writeln!(&mut s, "            }}\n        }}\n     }}")?;
    Ok(s)
}

#[rustfmt::skip::macros(writeln)]
fn output_command(
    cmd: OutputCommand,
    fields: &Fields,
    bits: usize,
    bytes: usize,
) -> Result<String> {
    let mut s = String::new();
    let fields = &fields.0;

    let mut sorted_fields: Vec<_> = fields.iter().collect();
    sorted_fields.sort_by(|a, b| a.0.cmp(b.0));

    let sorted_fields_keys: Vec<_> =
        sorted_fields.iter().map(|(a, _)| a).collect();

    let (cmd, auxiliary) = match cmd {
        OutputCommand::PMBus(str) => (str, false),
        OutputCommand::Auxiliary(str) => (str, true),
    };

    writeln!(&mut s, r##"
/// Types and structures associated with the `{}` PMBus command
#[allow(non_snake_case)]
#[allow(non_camel_case_types)]
pub mod {} {{
    use crate::Bitpos;
    use crate::Bitwidth;
    use crate::Error;
    use crate::VOutModeCommandData;
    use crate::Replacement;

    use num_derive::FromPrimitive;
    use num_derive::ToPrimitive;

    #[allow(unused_imports)]
    use num_traits::FromPrimitive;

    #[allow(unused_imports)]
    use num_traits::ToPrimitive;

    #[allow(unused_imports)]
    pub use num_traits::float::FloatCore;

    /// The data payload for the `{}` PMBus command
    #[derive(Copy, Clone, Debug, PartialEq)]
    pub struct CommandData(pub u{});

    /// An enum that captures all fields for the `{}` data payload
    #[derive(Copy, Clone, Debug, PartialEq)]
    #[repr(C)]
    pub enum Field {{"##, cmd, cmd, cmd, bits, cmd)?;

    for (f, field) in &sorted_fields {
        writeln!(&mut s, "        /// {}", field.name)?;
        writeln!(&mut s, "        {},", f)?;
    }

    writeln!(&mut s, r##"    }}

    impl core::fmt::Display for Field {{
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
            use crate::Field;
            write!(f, "{{}}", self.desc())
        }}
    }}

    impl crate::Field for Field {{
        fn bitfield(&self) -> bool {{
            true
        }}

        fn bits(&self) -> (Bitpos, Bitwidth) {{
            match self {{"##)?;

    let mut wholefield = false;

    for (f, field) in &sorted_fields {
        let (high, low) = bitrange(&field.bits);

        let pos = low;
        let width = high - low + 1;

        if usize::from(width) == bits {
            if fields.len() != 1 {
                bail!("illegal whole width field in {:?}", fields);
            }

            wholefield = true;
        }

        writeln!(&mut s, "                Field::{} => \
            (Bitpos({}), Bitwidth({})),", f, pos, width)?;
    }

    writeln!(&mut s, "            }}\n        }}")?;

    writeln!(&mut s, r##"
        fn name(&self) -> &'static str {{
            match self {{"##)?;

    for f in &sorted_fields_keys {
        writeln!(&mut s, "                Field::{} => \"{}\",", f, f)?;
    }

    writeln!(&mut s, "            }}\n        }}")?;

    writeln!(&mut s, r##"
        fn desc(&self) -> &'static str {{
            match self {{"##)?;

    for (f, field) in &sorted_fields {
        writeln!(
            &mut s, "                Field::{} => \"{}\",",
            f, field.name
        )?;
    }

    writeln!(&mut s, "            }}\n        }}\n    }}")?;

    writeln!(&mut s, r##"
    impl Field {{
        #[allow(unused_variables)]
        #[allow(unused_mut)]
        fn sentinels(&self, mut sentinel: impl FnMut(&dyn crate::Value)) {{
            match self {{"##)?;

    for (f, field) in &sorted_fields {
        if let Values::Sentinels(ref values) = &field.values {
            writeln!(&mut s, "                Field::{} => {{", f)?;

            let mut sorted = vec![];

            for (v, value) in values {
                sorted.push((value.0, v));
            }

            sorted.sort();

            for v in &sorted {
                writeln!(
                    &mut s,
                    r##"                    sentinel(
                        &Value::{}({}::{}),
                    );"##, f, f, v.1
                )?;
            }

            writeln!(&mut s, "                }}")?;
        } else {
            writeln!(&mut s, "                Field::{} => {{}}", f)?;
        }
    }

    writeln!(&mut s, "            }}\n        }}\n    }}")?;

    for (f, field) in &sorted_fields {
        let (high, low) = bitrange(&field.bits);
        let width = high - low + 1;
        write!(
            &mut s,
            "{}",
            output_value(f, &field.name, &field.values, width.into())?
        )?;
    }

    writeln!(&mut s, r##"
    /// An enum that captures all possible field values for all of the
    /// fields in the `{}` data payload
    #[derive(Copy, Clone, Debug, PartialEq)]
    #[repr(C)]
    pub enum Value {{"##, cmd)?;

    for f in &sorted_fields_keys {
        writeln!(&mut s, "        {}({}),", f, f)?;
    }

    writeln!(&mut s, "        Unknown(u{}),\n    }}", bits)?;

    writeln!(&mut s, r##"
    impl crate::Value for Value {{
        fn desc(&self) -> &'static str {{
            match self {{"##)?;

    for f in &sorted_fields_keys {
        writeln!(&mut s, "                Value::{}(v) => v.desc(),", f)?;
    }

    writeln!(&mut s, "                Value::Unknown(_) => \"<unknown>\",")?;
    writeln!(&mut s, "            }}\n        }}")?;

    writeln!(&mut s, r##"
        fn name(&self) -> &'static str {{
            match self {{"##)?;

    for f in &sorted_fields_keys {
        writeln!(&mut s, "                Value::{}(v) => v.name(),", f)?;
    }

    writeln!(&mut s, "                Value::Unknown(_) => \"<unknown>\",")?;
    writeln!(&mut s, "            }}\n        }}")?;

    writeln!(&mut s, r##"
        fn scalar(&self) -> bool {{
            #[allow(clippy::match_single_binding)]
            #[allow(clippy::match_like_matches_macro)]
            match self {{"##)?;

    for (f, field) in &sorted_fields {
        match field.values {
            Values::Scalar(_) | Values::FixedPointUnits(..) => {
                writeln!(&mut s, "                Value::{}(_) => true,", f)?;
            }
            _ => {}
        }
    }

    writeln!(&mut s, "                _ => false,")?;
    writeln!(&mut s, "            }}\n        }}")?;

    writeln!(&mut s, r##"
        fn raw(&self) -> u32 {{
            match self {{"##)?;

    for (f, field) in &sorted_fields {
        match &field.values {
            Values::Sentinels(_) => {
                writeln!(
                    &mut s,
                    "                Value::{f}(v) => v.to_u32().unwrap(),",
                )?;
            }
            Values::Scalar(_)
            | Values::FixedPointUnits(..)
            | Values::LogFactorUnits(..) => {
                writeln!(
                    &mut s,
                    "                Value::{f}(v) => v.0 as u32,",
                )?;
            }
        }
    }

    let cast = if bits != 32 { " as u32" } else { "" };

    writeln!(&mut s, "                Value::Unknown(v) => *v{cast},")?;
    writeln!(&mut s, "            }}\n        }}\n    }}")?;

    writeln!(&mut s, r##"
    impl core::fmt::Display for Value {{
        #[allow(clippy::match_single_binding)]
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
            match self {{"##)?;

    for (f, field) in &sorted_fields {
        match &field.values {
            Values::Scalar(_) => {
                writeln!(&mut s, r##"
                Value::{f}(_) => {{
                    write!(
                        f, "0x{{:x}}",
                        crate::Value::raw(self)
                    )
                }}"##)?;
            }

            Values::FixedPointUnits(Factor(factor), u) => {
                writeln!(&mut s, r##"
                Value::{f}(_) => {{
                    write!(
                        f, "{{:.3}}{}",
                        crate::Value::raw(self) as f32 / ({factor}_f32)
                    )
                }}"##, u.suffix())?;
            }

            Values::LogFactorUnits(Base(base), Factor(factor), u) => {
                writeln!(&mut s, r##"
                Value::{f}(_) => {{
                    write!(
                        f, "{{:.3}}{}",
                        ({base}_f32).powi(crate::Value::raw(self) as i32) /
                        ({factor}_f32)
                    )
                }}"##, u.suffix())?;
            }

            _ => {}
        }
    }

    writeln!(&mut s, r##"
                _ => {{
                    write!(
                        f, "0b{{:b}} = {{}}",
                        crate::Value::raw(self), crate::Value::desc(self)
                    )
                }}
            }}
        }}
    }}"##)?;

    writeln!(&mut s, r##"
    impl CommandData {{
        pub const fn len() -> usize {{
            {bytes}
        }}"##)?;

    if !auxiliary {
        writeln!(&mut s, r##"
        pub const fn code() -> u8 {{
            super::CommandCode::{cmd} as u8
        }}"##)?;
    }

    writeln!(&mut s, r##"
        pub fn from_slice(slice: &[u8]) -> Option<Self> {{"##)?;

    if bits == bytes * 8 {
        writeln!(&mut s, r##"
            use core::convert::TryInto;

            let v: Result<&[u8; {bytes}], _> = slice[0..{bytes}].try_into();

            match v {{
                Ok(v) => Some(Self(u{bits}::from_le_bytes(*v))),
                Err(_) => None,
            }}
        }}"##)?;
    } else {
        writeln!(&mut s, "            let v: u{} = ", bits)?;

        for i in 0..bytes {
            if i == 0 {
                writeln!(&mut s, "{:16}(slice[{}] as u{})", "", i, bits)?;
            } else {
                writeln!(&mut s,
                    "{:16}| ((slice[{}] as u{}) << {}){}", "",
                    i, bits, i * 8,
                    if i == bytes - 1 { ";" } else { "" }
                )?;
            }
        }

        writeln!(&mut s, r##"
            Some(Self(v))
        }}"##)?;
    }

    writeln!(&mut s, r##"
        #[allow(clippy::identity_op)]
        pub fn to_slice(&self, slice: &mut [u8]) {{"##)?;

    if bytes == 1 {
        writeln!(&mut s, "{:12}slice[0] = self.0;", "")?;
    } else {
        for i in 0..bytes {
            writeln!(&mut s,
                "{:12}slice[{i}] = ((self.0 >> {}) & 0xff) as u8;", "", i * 8
            )?;
        }
    }

    writeln!(&mut s, "        }}")?;

    writeln!(&mut s, r##"
        pub fn field(bit: Bitpos) -> Option<(Field, Bitwidth)> {{
            match bit.0 {{"##)?;

    for (f, field) in &sorted_fields {
        let (high, low) = bitrange(&field.bits);

        writeln!(&mut s,
            "                {low} => Some((Field::{f}, Bitwidth({}))),",
            high - low + 1
        )?;
    }

    writeln!(&mut s, "                _ => None,")?;
    writeln!(&mut s, "            }}\n        }}")?;

    if !wholefield {
        writeln!(&mut s, r##"
        pub fn get_val(&self, field: Field) -> u{bits} {{
            use crate::Field;
            let (pos, width) = field.bits();
            (self.0 >> pos.0) & ((1 << width.0) - 1)
        }}"##)?;
    } else {
        writeln!(&mut s, r##"
        pub fn get_val(&self, _field: Field) -> u{bits} {{
            self.0
        }}"##)?;
    }

    writeln!(&mut s, r##"
        pub fn get(&self, field: Field) -> Result<Value, Error> {{
            let raw = self.get_val(field);

            match field {{"##)?;

    for f in &sorted_fields_keys {
        writeln!(&mut s, r##"
                Field::{f} => {{
                    match {f}::from_u{bits}(raw) {{
                        Some(t) => Ok(Value::{f}(t)),
                        None => Err(Error::InvalidSentinel),
                    }}
                }}"##)?;
    }

    writeln!(&mut s, "            }}\n        }}")?;

    writeln!(&mut s, r##"
        #[allow(dead_code)]
        fn set_val(&mut self, field: Field, raw: u{bits}) -> Result<(), Error> {{
            use crate::Field;
            let (pos, width) = field.bits();

            if width.0 < {bits} {{
                let mask = (1 << width.0) - 1;

                if width.0 < {bits} && raw > mask {{
                    Err(Error::ValueOutOfRange)
                }} else {{
                    self.0 &= !(mask << pos.0);
                    self.0 |= (raw & mask) << pos.0;
                    Ok(())
                }}
            }} else {{
                self.0 = raw;
                Ok(())
            }}
        }}

        #[allow(dead_code)]
        fn set_val_signed(
            &mut self,
            field: Field,
            raw: i{bits},
        ) -> Result<(), Error> {{
            use crate::Field;
            let (pos, width) = field.bits();
            let mask = (1 << width.0) - 1;
            let max = (mask >> 1) as i{bits};
            let min = !(max as u{bits}) as i{bits};

            if width.0 < {bits} && (raw > max || raw < min) {{
                Err(Error::ValueOutOfRange)
            }} else {{
                self.0 &= !(mask << pos.0);
                self.0 |= ((raw as u{bits}) & mask) << pos.0;
                Ok(())
            }}
        }}
    
    "##)?;

    for (f, field) in &sorted_fields {
        let method = f.from_case(Case::Camel).to_case(Case::Snake);

        match &field.values {
            Values::Scalar(Sign::Unsigned) => {
                writeln!(&mut s, r##"
        pub fn get_{method}(&self) -> u{bits} {{
            self.get_val(Field::{f})
        }}"##)?;

                writeln!(&mut s, r##"
        pub fn set_{method}(&mut self, val: u{bits}) -> Result<(), Error> {{
            self.set_val(Field::{f}, val)
        }}"##)?;
            }

            Values::Scalar(Sign::Signed) => {
                let (high, _) = bitrange(&field.bits);
                let shift = bits - (high + 1) as usize;

                writeln!(&mut s, r##"
        pub fn get_{method}(&self) -> i{bits} {{
            ((self.get_val(Field::{f}) << {shift}) as i{bits}) >> {shift}
        }}"##)?;

                writeln!(&mut s, r##"
        pub fn set_{method}(&mut self, val: i{bits}) -> Result<(), Error> {{
            self.set_val_signed(Field::{f}, val)
        }}"##)?;
            }

            Values::FixedPointUnits(Factor(factor), unit) => {
                writeln!(&mut s, r##"
        pub fn get_{method}(&self) -> crate::units::{unit:?} {{
            crate::units::{unit:?}(
                self.get_val(Field::{f}) as f32 / ({factor}_f32)
            )
        }}"##)?;

                writeln!(&mut s, r##"
        pub fn set_{method}(
            &mut self,
            val: crate::units::{unit:?}
        ) -> Result<(), Error> {{
            self.set_val(Field::{f}, (val.0 * ({factor}_f32)) as u{bits})
        }}"##)?;
            }

            Values::LogFactorUnits(Base(base), Factor(factor), unit) => {
                writeln!(&mut s, r##"
        pub fn get_{method}(&self) -> crate::units::{unit:?} {{
            crate::units::{unit:?}(
                ({base}_f32).powi(self.get_val(Field::{f}) as i32) / ({factor}_f32)
            )
        }}"##)?;

                writeln!(&mut s, r##"
        pub fn set_{method}(
            &mut self,
            val: crate::units::{unit:?}
        ) -> Result<(), Error> {{
            self.set_val(Field::{f}, libm::log{base}f(val.0 * ({factor}_f32)) as u{bits})
        }}"##)?;
            }

            Values::Sentinels(_) => {
                writeln!(&mut s, r##"
        /// Return the value of the {} field as a [`Value::{f}`], or
        /// `None` if the field is corrupt or otherwise cannot be represented
        /// as a [`Value::{f}`].
        pub fn get_{method}(&self) -> Option<{f}> {{
            match self.get(Field::{f}) {{
                Ok(Value::{f}(v)) => Some(v),
                _ => None,
            }}
        }}

        /// Sets the value of the {} field to the specified value.
        pub fn set_{method}(&mut self, val: {f}) {{
            self.set_val(Field::{f}, val.to_u{bits}().unwrap()).unwrap();
        }}"##, field.name, field.name)?;
            }
        }
    }

    writeln!(&mut s, "    }}")?;

    writeln!(&mut s, r##"
    impl crate::CommandData for CommandData {{
        fn interpret(
            &self,
            _mode: impl Fn() -> VOutModeCommandData,
            mut iter: impl FnMut(&dyn crate::Field, &dyn crate::Value)
        ) -> Result<(), Error> {{
            let mut pos: u8 = {};

            loop {{
                if let Some((field, _)) = CommandData::field(Bitpos(pos)) {{
                    let val = self.get(field)?;
                    iter(&field, &val);
                }}

                if pos == 0 {{
                    break;
                }}

                pos -= 1;
            }}
            Ok(())
        }}

        fn mutate(
            &mut self,
            _mode: impl Fn() -> VOutModeCommandData,
            mut iter: impl FnMut(
                &dyn crate::Field, &dyn crate::Value
            ) -> Option<Replacement>
        ) -> Result<(), Error> {{
            let mut pos: u8 = {};

            loop {{
                if let Some((field, _)) = CommandData::field(Bitpos(pos)) {{
                    let val = self.get(field)?;
                    if let Some(replacement) = iter(&field, &val) {{
                        match replacement {{
                            Replacement::Boolean(b) => {{
                                let v = if b {{ 1 }} else {{ 0 }};
                                self.set_val(field, v).unwrap();
                            }}

                            Replacement::Integer(i) => {{
                                #[allow(clippy::unnecessary_cast)]
                                if self.set_val(field, i as u{}).is_err() {{
                                    return Err(Error::OverflowReplacement);
                                }}
                            }}

                            _ => {{
                                return Err(Error::InvalidReplacement);
                            }}
                        }}
                    }}
                }}

                if pos == 0 {{
                    break;
                }}

                pos -= 1;
            }}
            Ok(())
        }}

        fn fields(
            mut iter: impl FnMut(&dyn crate::Field)
        ) -> Result<(), Error> {{
            let mut pos: u8 = {};

            loop {{
                if let Some((field, _)) = CommandData::field(Bitpos(pos)) {{
                    iter(&field);
                }}

                if pos == 0 {{
                    break;
                }}

                pos -= 1;
            }}

            Ok(())
        }}

        fn sentinels(
            field: Bitpos,
            iter: impl FnMut(&dyn crate::Value) 
        ) -> Result<(), Error> {{
            if let Some((field, _)) = CommandData::field(field) {{
                field.sentinels(iter);
                Ok(())
            }} else {{
                Err(Error::InvalidField)
            }}
        }}

        fn raw(&self) -> (u32, Bitwidth) {{
            (self.0{cast}, Bitwidth({}))
        }}"##, bits - 1, bits - 1, bits, bits, bits)?;

    if !auxiliary {
        writeln!(&mut s, r##"
        fn command(
            &self,
            mut cb: impl FnMut(&dyn crate::Command)
        ) {{
            cb(&super::CommandCode::{})
        }}"##, cmd)?;
    } else {
        writeln!(&mut s, r##"
        fn command(
            &self,
            mut _cb: impl FnMut(&dyn crate::Command)
        ) {{
            panic!("command() call on auxiliary");
        }}"##)?;
    }

    writeln!(&mut s, "    }}\n}}")?;

    Ok(s)
}

fn output_command_data(
    cmd: &str,
    fields: &Fields,
    bits: usize,
    bytes: usize,
) -> Result<String> {
    output_command(OutputCommand::PMBus(cmd), fields, bits, bytes)
}

fn output_aux_data(
    aux: &str,
    fields: &Fields,
    bits: usize,
    bytes: usize,
) -> Result<String> {
    output_command(OutputCommand::Auxiliary(aux), fields, bits, bytes)
}

#[rustfmt::skip::macros(writeln)]
fn output_command_numeric(
    cmd: OutputCommand,
    format: &Format,
    u: &Units,
    bytes: usize,
    coeff: Option<Coefficients>,
) -> Result<String> {
    let (cmd, auxiliary) = match cmd {
        OutputCommand::PMBus(str) => (str, false),
        OutputCommand::Auxiliary(str) => (str, true),
    };

    let mut s = String::new();
    let bits = bytes * 8;

    let units = &format!("crate::units::{:?}", u);

    if !auxiliary {
        writeln!(&mut s, r##"
/// Types and structures associated with the `{}` B PMBus command
#[allow(non_snake_case)]
#[allow(non_camel_case_types)]
pub mod {} {{
    use crate::Bitwidth;

    /// The data payload for the `{}` PMBus command
    pub struct CommandData(pub u{});

    use crate::Error;
    use crate::VOutModeCommandData;
    use crate::Replacement;

    #[allow(unused_imports)]
    use crate::Coefficients;"##, cmd, cmd, cmd, bits)?;
    } else {
        writeln!(&mut s, r##"
/// Types and structures associated with the `{}` auxiliary structure
#[allow(non_snake_case)]
#[allow(non_camel_case_types)]
pub mod {} {{
    use crate::Bitwidth;

    /// The data payload for the `{}` auxiliary structure
    pub struct CommandData(pub u{});

    use crate::Error;
    use crate::VOutModeCommandData;
    use crate::Replacement;

    #[allow(unused_imports)]
    use crate::Coefficients;"##, cmd, cmd, cmd, bits)?;
    }

    if let Format::Raw = format {
        writeln!(&mut s, r##"
    #[derive(Copy, Clone, Debug, PartialEq)]
    pub struct Value(u32);

    impl core::fmt::Display for Value {{
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
            write!(f, "{{}}", self.0)
        }}
    }}

    impl crate::Value for Value {{
        fn name(&self) -> &'static str {{
            "{}"
        }}

        fn desc(&self) -> &'static str {{
            "{} raw value"
        }}

        fn scalar(&self) -> bool {{
            true
        }}

        fn raw(&self) -> u32 {{
            self.0
        }}
    }}"##, cmd, cmd)?;
    } else {
        writeln!(&mut s, r##"
    #[derive(Copy, Clone, Debug, PartialEq)]
    pub struct Value({}, u32);

    impl core::fmt::Display for Value {{
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
            write!(f, "{{:.3}}{}", self.0.0)
        }}
    }}

    impl crate::Value for Value {{
        fn name(&self) -> &'static str {{
            "{}"
        }}

        fn desc(&self) -> &'static str {{
            "{} measurement"
        }}

        fn scalar(&self) -> bool {{
            true
        }}

        fn raw(&self) -> u32 {{
            self.1
        }}
    }}"##, units, u.suffix(), cmd, cmd)?;
    }

    writeln!(&mut s, r##"
    impl CommandData {{
        pub const fn len() -> usize {{
            {}
        }}

        pub fn from_slice(slice: &[u8]) -> Option<Self> {{
            use core::convert::TryInto;

            let v: Result<&[u8; {}], _> = slice[0..{}].try_into();

            match v {{
                Ok(v) => Some(Self(u{}::from_le_bytes(*v))),
                Err(_) => None,
            }}
        }}"##, bytes, bytes, bytes, bits)?;

    if !auxiliary {
        writeln!(&mut s, r##"
        pub const fn code() -> u8 {{
            super::CommandCode::{} as u8
        }}"##, cmd)?;
    }

    writeln!(&mut s, r##"
        #[allow(clippy::identity_op)]
        pub fn to_slice(&self, slice: &mut [u8]) {{"##)?;

    if bytes == 1 {
        writeln!(&mut s, "{:12}slice[0] = self.0;", "")?;
    } else {
        for i in 0..bytes {
            writeln!(&mut s,
                "{:12}slice[{i}] = ((self.0 >> {}) & 0xff) as u8;", "", i * 8
            )?;
        }
    }

    writeln!(&mut s, "        }}")?;

    match format {
        Format::Linear11 => {
            writeln!(&mut s, r##"
        pub fn get(&self) -> Result<{}, Error> {{
            Ok({}(crate::Linear11(self.0).to_real()))
        }}

        pub fn set(&mut self, val: {}) -> Result<(), Error> {{
            if let Some(lin) = crate::Linear11::from_real(val.0) {{
                self.0 = lin.0;
                Ok(())
            }} else {{
                Err(Error::ValueOutOfRange)
            }}
        }}"##, units, units, units)?;
        }

        Format::VOutMode(_) => {
            writeln!(&mut s, r##"
        pub fn get(&self, mode: VOutModeCommandData) -> Result<{}, Error> {{
            match mode.get_mode() {{
                Some(crate::commands::VOUT_MODE::Mode::ULINEAR16) => {{
                    let exp = crate::ULinear16Exponent(mode.get_parameter());
                    Ok({}(
                        crate::ULinear16(self.0, exp).to_real()
                    ))
                }}
                Some(crate::commands::VOUT_MODE::Mode::Direct) => {{"##,
                units, units)?;

            match coeff {
                Some(coeff) => {
                    writeln!(&mut s, r##"
                    let coefficients = Coefficients {{
                        m: {}, R: {}, b: {},
                    }};

                    Ok({}(
                        crate::Direct(self.0, coefficients).to_real()
                    ))"##, coeff.m, coeff.R, coeff.b, units)?;
                }

                None => {
                    writeln!(&mut s, r##"
                    Err(Error::MissingCoefficients)"##)?;
                }
            }

            writeln!(&mut s, r##"                }}
                _ => {{
                    Err(Error::InvalidMode)
                }}
            }}
        }}

        pub fn set(
            &mut self, mode: VOutModeCommandData, val: {}
        ) -> Result<(), Error> {{
            match mode.get_mode() {{
                Some(crate::commands::VOUT_MODE::Mode::ULINEAR16) => {{
                    let exp = crate::ULinear16Exponent(mode.get_parameter());

                    self.0 = match crate::ULinear16::from_real(val.0, exp) {{
                        Some(val) => val.0,
                        None => return Err(Error::ValueOutOfRange)
                    }};

                    Ok(())
                }}
                Some(crate::commands::VOUT_MODE::Mode::Direct) => {{"##,
                units)?;

            match coeff {
                Some(coeff) => {
                    writeln!(&mut s, r##"
                    let coefficients = Coefficients {{
                        m: {}, R: {}, b: {},
                    }};

                    self.0 = crate::Direct::from_real(val.0, coefficients).0;

                    Ok(())"##, coeff.m, coeff.R, coeff.b)?;
                }

                None => {
                    writeln!(&mut s, r##"
                    Err(Error::MissingCoefficients)"##)?;
                }
            }

            writeln!(&mut s, r##"                }}
                _ => {{
                    Err(Error::InvalidMode)
                }}
            }}
        }}"##)?;
        }

        Format::Direct(c) => {
            if bits > 16 {
                bail!("{} has {} bits, but Direct can only have 16", cmd, bits);
            }

            writeln!(&mut s, r##"
        pub fn get(&self) -> Result<{}, Error> {{
            let coefficients = Coefficients {{
                m: {}, R: {}, b: {},
            }};

            Ok({}(crate::Direct(self.0, coefficients).to_real()))
        }}

        pub fn set(&mut self, val: {}) -> Result<(), Error> {{
            let coefficients = Coefficients {{
                m: {}, R: {}, b: {},
            }};

            self.0 = crate::Direct::from_real(val.0, coefficients).0;

            Ok(())
        }}"##, units, c.m, c.R, c.b, units, units, c.m, c.R, c.b)?;
        }

        Format::RuntimeDirect => {
            writeln!(&mut s, r##"
        pub fn get(&self, coefficients: &Coefficients) -> Result<{}, Error> {{
            Ok({}(crate::Direct(self.0, *coefficients).to_real()))
        }}

        pub fn set(
            &mut self,
            coefficients: &Coefficients,
            val: {}
        ) -> Result<(), Error> {{
            self.0 = crate::Direct::from_real(val.0, *coefficients).0;

            Ok(())
        }}"##, units, units, units)?;
        }

        Format::FixedPoint(Factor(factor)) => {
            writeln!(&mut s, r##"
        pub fn get(&self) -> Result<{}, Error> {{
            Ok({}((self.0 as f32) / ({}_f32)))
        }}

        pub fn set(&mut self, val: {}) -> Result<(), Error> {{
            self.0 = (val.0 * ({}_f32)) as u{};
            Ok(())
        }}"##, units, units, factor, units, factor, bits)?;
        }

        Format::SignedFixedPoint(Factor(factor)) => {
            writeln!(&mut s, r##"
        pub fn get(&self) -> Result<{}, Error> {{
            Ok({}(((self.0 as i{}) as f32) / ({} as f32)))
        }}

        pub fn set(&mut self, val: {}) -> Result<(), Error> {{
            self.0 = (val.0 * ({} as f32)) as u{};
            Ok(())
        }}"##, units, units, bits, factor, units, factor, bits)?;
        }

        Format::Raw => {
            writeln!(&mut s, r##"
        pub fn get(&self) -> Result<u{}, Error> {{
            Ok(self.0)
        }}

        pub fn set(&mut self, val: u{}) -> Result<(), Error> {{
            self.0 = val;
            Ok(())
        }}"##, bits, bits)?;
        }
    }

    writeln!(&mut s, "    }}")?;

    writeln!(&mut s, r##"
    #[allow(clippy::useless_conversion)]
    impl crate::CommandData for CommandData {{"##)?;

    if let Format::VOutMode(_) = format {
        writeln!(&mut s, r##"
        fn interpret(
            &self,
            mode: impl Fn() -> VOutModeCommandData,
            mut iter: impl FnMut(&dyn crate::Field, &dyn crate::Value)
        ) -> Result<(), Error> {{
            let field = crate::WholeField("{} measurement", Bitwidth({}));
            iter(&field, &Value(self.get(mode())?, self.0.into()));
            Ok(())
        }}"##, cmd, bits)?;
    } else if let Format::Raw = format {
        writeln!(&mut s, r##"
        fn interpret(
            &self,
            _mode: impl Fn() -> VOutModeCommandData,
            mut iter: impl FnMut(&dyn crate::Field, &dyn crate::Value)
        ) -> Result<(), Error> {{
            let field = crate::WholeField("{} value", Bitwidth({}));
            iter(&field, &Value(self.get()?.into()));
            Ok(())
        }}"##, cmd, bits)?;
    } else if let Format::RuntimeDirect = format {
        writeln!(&mut s, r##"
        fn interpret(
            &self,
            _mode: impl Fn() -> VOutModeCommandData,
            mut _iter: impl FnMut(&dyn crate::Field, &dyn crate::Value)
        ) -> Result<(), Error> {{
            Ok(())
        }}"##)?;
    } else {
        writeln!(&mut s, r##"
        fn interpret(
            &self,
            _mode: impl Fn() -> VOutModeCommandData,
            mut iter: impl FnMut(&dyn crate::Field, &dyn crate::Value)
        ) -> Result<(), Error> {{
            let field = crate::WholeField("{} measurement", Bitwidth({}));
            iter(&field, &Value(self.get()?, self.0.into()));
            Ok(())
        }}"##, cmd, bits)?;
    }

    if let Format::VOutMode(_) = format {
        writeln!(&mut s, r##"
        fn mutate(
            &mut self,
            mode: impl Fn() -> VOutModeCommandData,
            mut iter: impl FnMut(
                &dyn crate::Field, &dyn crate::Value
            ) -> Option<Replacement>
        ) -> Result<(), Error> {{
            let field = crate::WholeField("{} measurement", Bitwidth({}));

            let mode = mode();
            let val = Value(self.get(mode)?, self.0.into());

            if let Some(replacement) = iter(&field, &val) {{
                match replacement {{
                    Replacement::Float(f) => {{
                        self.set(mode, {}(f))
                    }}
                    Replacement::Integer(i) => {{
                        self.set(mode, {}(i as f32))
                    }}
                    _ => {{
                        Err(Error::InvalidReplacement)
                    }}
                }}
            }} else {{
                Ok(())
            }}
        }}"##, cmd, bits, units, units)?;
    } else if let Format::Raw = format {
        writeln!(&mut s, r##"
        fn mutate(
            &mut self,
            _mode: impl Fn() -> VOutModeCommandData,
            mut iter: impl FnMut(
                &dyn crate::Field, &dyn crate::Value
            ) -> Option<Replacement>
        ) -> Result<(), Error> {{
            let field = crate::WholeField("{} value", Bitwidth({}));
            let val = Value(self.get()?.into());

            if let Some(replacement) = iter(&field, &val) {{
                if let Replacement::Integer(i) = replacement {{
                    use core::convert::TryFrom;

                    match u{}::try_from(i) {{
                        Ok(i) => self.set(i),
                        Err(_) => Err(Error::OverflowReplacement)
                    }}
                }} else {{
                    Err(Error::InvalidReplacement)
                }}
            }} else {{
                Ok(())
            }}
        }}"##, cmd, bits, bits)?;
    } else if let Format::RuntimeDirect = format {
        writeln!(&mut s, r##"
        fn mutate(
            &mut self,
            _mode: impl Fn() -> VOutModeCommandData,
            mut _iter: impl FnMut(
                &dyn crate::Field, &dyn crate::Value
            ) -> Option<Replacement>
        ) -> Result<(), Error> {{
            Ok(())
        }}"##)?;
    } else {
        writeln!(&mut s, r##"
        fn mutate(
            &mut self,
            _mode: impl Fn() -> VOutModeCommandData,
            mut iter: impl FnMut(
                &dyn crate::Field, &dyn crate::Value
            ) -> Option<Replacement>
        ) -> Result<(), Error> {{
            let field = crate::WholeField("{cmd} measurement", Bitwidth({bits}));
            let val = Value(self.get()?, self.0.into());

            if let Some(replacement) = iter(&field, &val) {{
                if let Replacement::Float(f) = replacement {{
                    self.set({units}(f))
                }} else {{
                    Err(Error::InvalidReplacement)
                }}
            }} else {{
                Ok(())
            }}
        }}"##)?;
    }

    writeln!(&mut s, r##"
        fn fields(
            mut iter: impl FnMut(&dyn crate::Field) 
        ) -> Result<(), Error> {{
            iter(&crate::WholeField("{cmd} measurement", Bitwidth({bits})));

            Ok(())
        }}

        fn sentinels(
            _field: crate::Bitpos,
            mut _iter: impl FnMut(&dyn crate::Value) 
        ) -> Result<(), Error> {{
            Ok(())
        }}

        #[allow(clippy::unnecessary_cast)]
        fn raw(&self) -> (u32, Bitwidth) {{
            (self.0 as u32, Bitwidth({bits}))
        }}"##)?;

    if !auxiliary {
        writeln!(&mut s, r##"
        fn command(
            &self,
            mut cb: impl FnMut(&dyn crate::Command)
        ) {{
            cb(&super::CommandCode::{})
        }}"##, cmd)?;
    } else {
        writeln!(&mut s, r##"
        fn command(
            &self,
            mut _cb: impl FnMut(&dyn crate::Command)
        ) {{
            panic!("command() call on auxiliary");
        }}"##)?;
    }

    writeln!(&mut s, "    }}\n}}")?;

    Ok(s)
}

#[rustfmt::skip::macros(writeln)]
fn output_numerics(
    cmds: &[CommandNumericFormat],
    sizes: &HashMap<String, Option<usize>>,
    units: &mut HashSet<Units>,
    coeff: Option<Coefficients>,
) -> Result<String> {
    let mut out = String::new();

    for cmd in cmds {
        let bytes = match sizes.get(&cmd.0) {
            Some(Some(size)) => *size,
            Some(None) => {
                bail!("command {} does not allow a value", cmd.0);
            }
            None => {
                bail!("command {} does not exist", cmd.0);
            }
        };

        units.insert(cmd.2);
        out.push_str(&output_command_numeric(
            OutputCommand::PMBus(&cmd.0),
            &cmd.1,
            &cmd.2,
            bytes,
            coeff,
        )?);
    }

    Ok(out)
}

fn output_aux_numerics(
    auxs: &[AuxiliaryNumericFormat],
    sizes: &HashMap<String, Option<usize>>,
    units: &mut HashSet<Units>,
    coeff: Option<Coefficients>,
) -> Result<String> {
    let mut out = String::new();

    for aux in auxs {
        let bytes = match sizes.get(&aux.0) {
            Some(Some(size)) => *size,
            Some(None) => {
                bail!("auxiliary {} does not allow a value", aux.0);
            }
            None => {
                bail!("auxiliary {} does not exist", aux.0);
            }
        };

        units.insert(aux.2);
        out.push_str(&output_command_numeric(
            OutputCommand::Auxiliary(&aux.0),
            &aux.1,
            &aux.2,
            bytes,
            coeff,
        )?);
    }

    Ok(out)
}

#[rustfmt::skip::macros(writeln)]
#[rustfmt::skip::macros(write)]
fn output_devices(devices: &HashMap<String, Device>) -> Result<String> {
    let mut s = String::new();

    let name = |str: &str| str.to_case(Case::UpperCamel);

    let mut all: Vec<_> = devices.iter().collect();
    all.sort();

    writeln!(&mut s, r##"
#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(C)]
pub enum Device {{
    Common,"##)?;

    for dev in &all {
        writeln!(&mut s, "    {},", name(dev.0))?;
    }

    write!(&mut s, r##"
}}

impl Device {{
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(str: &str) -> Option<Self> {{
        "##)?;

    for dev in &all {
        write!(&mut s, r##"if str == Device::{}.name() {{
            Some(Device::{})
        }} else "##, name(dev.0), name(dev.0))?;
    }

    writeln!(&mut s, r##"{{
            None
        }}
    }}

    pub fn name(&self) -> &str {{
        match self {{
            Device::Common => "<common>","##)?;

    for dev in &all {
        writeln!(&mut s,
            "            Device::{} => \"{}\",", name(dev.0), dev.0)?;
    }

    writeln!(&mut s, "        }}\n    }}\n")?;

    writeln!(&mut s, r##"
    pub fn desc(&self) -> &str {{
        match self {{
            Device::Common => "<common>","##)?;

    for (dev, device) in &all {
        writeln!(&mut s,
            "            Device::{} => \"{}\",",
            name(dev), device.description)?;
    }

    writeln!(&mut s, "        }}\n    }}\n")?;

    writeln!(&mut s, r##"
    /// For this device and the given command code, iterates over the fields
    /// in the structured register (if any), calling the specified function
    /// for each field and its value.  The current VOUT_MODE is required to
    /// interpret some command data bytes; this must be provided as a
    /// paramater.  In general, this should only be used by agnostic code that
    /// is attmpting to make sense of PMBus data; *in situ* code that wishes
    /// to pull a particular value should use the direct accessor function
    /// instead.
    pub fn interpret(
        &self,
        code: u8,
        payload: &[u8],
        mode: impl Fn() -> VOutModeCommandData,
        iter: impl FnMut(&dyn Field, &dyn Value)
    ) -> Result<(), Error> {{
        match self {{
            Device::Common => match CommandCode::from_u8(code) {{
                Some(cmd) => {{
                    cmd.interpret(payload, mode, iter)
                }}
                None => {{
                    Err(Error::InvalidCode)
                }}
            }},"##)?;

    for dev in &all {
        writeln!(&mut s, r##"
            Device::{} => match {}::CommandCode::from_u8(code) {{
                Some(cmd) => {{
                    cmd.interpret(payload, mode, iter)
                }}
                None => {{
                    Err(Error::InvalidCode)
                }}
            }},"##, name(dev.0), dev.0)?;
    }

    writeln!(&mut s, "        }}\n    }}\n")?;

    writeln!(&mut s, r##"
    /// For this device and the given command code, iterates over the fields
    /// in the structured register (if any) for the purpose of mutating some
    /// individual field.  This will call the specified function for each
    /// field and its value, which should return a value that should serve as
    /// a replacement for the passed field.  The current VOUT_MODE is required
    /// to interpret some command data bytes; this must be provided via a
    /// closure that returns it.  In general -- as with `interpret` -- this
    /// should only be used by agnostic code that is attmpting to modify PMBus
    /// registers; *in situ* code that wishes to set a particular value
    /// should use the direct setter function instead.
    pub fn mutate(
        &self,
        code: u8,
        payload: &mut [u8],
        mode: impl Fn() -> VOutModeCommandData,
        iter: impl FnMut(&dyn Field, &dyn Value) -> Option<Replacement>
    ) -> Result<(), Error> {{
        match self {{
            Device::Common => match CommandCode::from_u8(code) {{
                Some(cmd) => {{
                    cmd.mutate(payload, mode, iter)
                }}
                None => {{
                    Err(Error::InvalidCode)
                }}
            }},"##)?;

    for dev in &all {
        writeln!(&mut s, r##"
            Device::{} => match {}::CommandCode::from_u8(code) {{
                Some(cmd) => {{
                    cmd.mutate(payload, mode, iter)
                }}
                None => {{
                    Err(Error::InvalidCode)
                }}
            }},"##, name(dev.0), dev.0)?;
    }

    writeln!(&mut s, "        }}\n    }}\n")?;

    writeln!(&mut s, r##"
    /// For this device and the given command code, iterates over the fields
    /// in the structured register, calling the specified function for each
    /// field.
    pub fn fields(
        &self,
        code: u8,
        iter: impl FnMut(&dyn Field)
    ) -> Result<(), Error> {{
        match self {{
            Device::Common => match CommandCode::from_u8(code) {{
                Some(cmd) => {{
                    cmd.fields(iter)
                }}
                None => {{
                    Err(Error::InvalidCode)
                }}
            }},"##)?;

    for dev in &all {
        writeln!(&mut s, r##"
            Device::{} => match {}::CommandCode::from_u8(code) {{
                Some(cmd) => {{
                    cmd.fields(iter)
                }}
                None => {{
                    Err(Error::InvalidCode)
                }}
            }},"##, name(dev.0), dev.0)?;
    }

    writeln!(&mut s, "        }}\n    }}\n")?;

    writeln!(&mut s, r##"
    /// For this device and the given command code and field position, iterates
    /// over the sentinels for the specified field in the structured register
    /// (if any), calling the specified function for each sentinel value.
    pub fn sentinels(
        &self,
        code: u8,
        field: Bitpos,
        iter: impl FnMut(&dyn Value)
    ) -> Result<(), Error> {{
        match self {{
            Device::Common => match CommandCode::from_u8(code) {{
                Some(cmd) => {{
                    cmd.sentinels(field, iter)
                }}
                None => {{
                    Err(Error::InvalidCode)
                }}
            }},"##)?;

    for dev in &all {
        writeln!(&mut s, r##"
            Device::{} => match {}::CommandCode::from_u8(code) {{
                Some(cmd) => {{
                    cmd.sentinels(field, iter)
                }}
                None => {{
                    Err(Error::InvalidCode)
                }}
            }},"##, name(dev.0), dev.0)?;
    }

    writeln!(&mut s, "        }}\n    }}\n")?;

    writeln!(&mut s, r##"
    pub fn command(
        &self,
        code: u8,
        mut cb: impl FnMut(&dyn Command)
    ) {{
        match self {{
            Device::Common => if let Some(cmd) = CommandCode::from_u8(code) {{
                    cb(&cmd);
            }},"##)?;

    for dev in &all {
        writeln!(&mut s, r##"
            Device::{} => if let Some(cmd) = {}::CommandCode::from_u8(code) {{
                    cb(&cmd);
            }},"##, name(dev.0), dev.0)?;
    }

    writeln!(&mut s, "        }}\n    }}\n}}")?;

    writeln!(&mut s, r##"
pub fn devices(mut dev: impl FnMut(Device)) {{"##)?;
    for dev in &all {
        writeln!(&mut s, "    dev(Device::{});", name(dev.0))?;
    }

    writeln!(&mut s, "}}")?;

    Ok(s)
}

#[rustfmt::skip::macros(writeln)]
fn output_device(device: &str) -> Result<String> {
    let mut s = String::new();

    writeln!(&mut s, r##"
pub mod {} {{
    pub use crate::Command;
    pub use crate::CommandData;
    pub use crate::Value;
    pub use crate::Field;
    pub use crate::Bitwidth;
    pub use crate::Bitpos;
    pub use crate::Operation;
    pub use crate::Error;

    include!(concat!(env!("OUT_DIR"), "/{}.rs"));
}}"##, device, device)?;

    Ok(s)
}

#[rustfmt::skip::macros(writeln)]
fn output_units(units: &HashSet<Units>) -> Result<String> {
    let mut s = String::new();

    let mut all: Vec<_> = units.iter().collect();
    all.sort();

    for u in all {
        writeln!(&mut s, r##"
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct {:?}(pub f32);"##, u)?;
    }

    Ok(s)
}

fn open_file(filename: &str) -> Result<File> {
    let mut dir = PathBuf::from(&env::var("CARGO_MANIFEST_DIR").unwrap());
    dir.push("src");
    dir.push(filename);

    match File::open(dir) {
        Ok(f) => {
            println!("cargo:rerun-if-changed=src/{}", filename);
            Ok(f)
        }
        Err(e) => {
            bail!("failed to open {}: {}", filename, e);
        }
    }
}

fn read_commands(filename: &str) -> Result<Commands> {
    let mut file = open_file(filename)?;
    let mut contents = String::new();

    file.read_to_string(&mut contents)?;

    match from_str::<Commands>(&contents) {
        Ok(r) => Ok(r),
        Err(e) => {
            bail!("failed to parse {}: {}", filename, e);
        }
    }
}

#[rustfmt::skip::macros(bail)]
fn codegen() -> Result<()> {
    use std::io::Write;

    //
    // First, consume our common commands.
    //
    let cmds = read_commands("commands.ron")?;

    let sizes = reg_sizes(&cmds.all)?;
    let dbs = &cmds.structured;

    let out_dir = env::var("OUT_DIR")?;
    let dest_path = Path::new(&out_dir).join("commands.rs");
    let mut file = File::create(dest_path)?;
    let mut units: HashSet<Units> = HashSet::new();

    let out = output_commands(&cmds, None)?;
    file.write_all(out.as_bytes())?;

    let mut all_dbs: Vec<_> = dbs.iter().collect();

    all_dbs.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());

    for (cmd, fields) in &all_dbs {
        let (bits, bytes) = validate(cmd, fields, &sizes, &mut units)?;
        let out = output_command_data(cmd, fields, bits, bytes)?;
        file.write_all(out.as_bytes())?;
    }

    if let Some(ref synonyms) = cmds.synonyms {
        for synonym in synonyms {
            let cmd = &synonym.0;

            //
            // We must have a structured definition for the command for
            // which we're a synonym.
            //
            if let Some(fields) = dbs.get(&synonym.1) {
                let (bits, bytes) = validate(cmd, fields, &sizes, &mut units)?;
                let out = output_command_data(cmd, fields, bits, bytes)?;
                file.write_all(out.as_bytes())?;
            } else {
                bail!(
                    "command {} is a synonym for {}, \
                    but {} lacks a structured definition",
                    cmd, synonym.1, synonym.1
                );
            }
        }
    }

    let out = output_numerics(&cmds.numerics, &sizes, &mut units, None)?;
    file.write_all(out.as_bytes())?;

    let f = open_file("devices.ron")?;

    let devices: HashMap<String, Device> = match from_reader(f) {
        Ok(devices) => devices,
        Err(e) => {
            bail!("failed to parse devices.ron: {}", e);
        }
    };

    let dest_path = Path::new(&out_dir).join("devices.rs");
    let mut dfile = File::create(dest_path)?;

    let out = output_devices(&devices)?;
    dfile.write_all(out.as_bytes())?;

    //
    // Now we need to iterate over our devices.  For each one, we'll generate
    // our flattened module, and then include it in our flattened file of
    // all devices.
    //
    let mut all: Vec<_> = devices.iter().collect();
    all.sort();

    for (name, device) in all {
        let dest_path = Path::new(&out_dir).join(format!("{}.rs", name));
        let mut file = File::create(dest_path)?;

        let fname = format!("{}.ron", &name);
        let mut dcmds = read_commands(&fname)?;

        //
        // Flatten our commands and output them
        //
        let mut h: HashSet<u8> = HashSet::new();

        for cmd in &dcmds.all {
            h.insert(cmd.0);
        }

        for cmd in &cmds.all {
            if !h.contains(&cmd.0) {
                dcmds.all.push(cmd.clone());
            }
        }

        let out = output_commands(&dcmds, Some(&cmds))?;
        file.write_all(out.as_bytes())?;

        let dsizes = reg_sizes(&dcmds.all)?;

        //
        // Now emit data payloads, allowing the device definition to
        // override any common payload.
        //
        for (cmd, fields) in &all_dbs {
            if let Some(fields) = dcmds.structured.get(*cmd) {
                let (bits, bytes) = validate(cmd, fields, &dsizes, &mut units)?;
                let out = output_command_data(cmd, fields, bits, bytes)?;
                file.write_all(out.as_bytes())?;
                dcmds.structured.remove(*cmd);
            } else {
                let (bits, bytes) = validate(cmd, fields, &sizes, &mut units)?;
                let out = output_command_data(cmd, fields, bits, bytes)?;
                file.write_all(out.as_bytes())?;
            }
        }

        let mut structured_sorted: Vec<_> = dcmds.structured.iter().collect();
        structured_sorted.sort_by(|a, b| a.0.cmp(b.0));

        for (cmd, fields) in &structured_sorted {
            let (bits, bytes) = validate(cmd, fields, &dsizes, &mut units)?;
            let out = output_command_data(cmd, fields, bits, bytes)?;
            file.write_all(out.as_bytes())?;
        }

        if let Some(ref synonyms) = dcmds.synonyms {
            for synonym in synonyms {
                let cmd = &synonym.0;
                let s = &dsizes;

                //
                // We must have a structured definition for the command for
                // which we're a synonym -- or there must be one in the common
                // definition.
                //
                let fields = match dcmds.structured.get(&synonym.1) {
                    Some(fields) => fields,
                    None => match dbs.get(&synonym.1) {
                        Some(fields) => fields,
                        None => {
                            bail!(
                                "command {} is a synonym for {}, \
                                but {} lacks a structured definition",
                                cmd, synonym.1, synonym.1
                            );
                        }
                    },
                };

                let (bits, bytes) = validate(cmd, fields, s, &mut units)?;
                let out = output_command_data(cmd, fields, bits, bytes)?;
                file.write_all(out.as_bytes())?;
            }
        }

        let coeff = device.coefficients;
        let out = output_numerics(&dcmds.numerics, &dsizes, &mut units, coeff)?;
        file.write_all(out.as_bytes())?;

        let out = output_numerics(&cmds.numerics, &sizes, &mut units, coeff)?;
        file.write_all(out.as_bytes())?;

        //
        // If we have auxiliary structures, we emit each of those in its
        // own module.
        //
        if let Some(aux) = dcmds.auxiliaries {
            let sizes = aux_sizes(&aux.all)?;

            let out =
                output_aux_numerics(&aux.numerics, &sizes, &mut units, coeff)?;
            file.write_all(out.as_bytes())?;

            let mut aux_structured_sorted: Vec<_> =
                aux.structured.iter().collect();
            aux_structured_sorted.sort_by(|a, b| a.0.cmp(b.0));

            for (aux, fields) in &aux_structured_sorted {
                let (bits, bytes) = validate(aux, fields, &sizes, &mut units)?;

                let out = output_aux_data(aux, fields, bits, bytes)?;
                file.write_all(out.as_bytes())?;
            }
        }

        let out = output_device(name)?;
        dfile.write_all(out.as_bytes())?;
    }

    let dest_path = Path::new(&out_dir).join("units.rs");
    let mut ufile = File::create(dest_path)?;

    let out = output_units(&units)?;
    ufile.write_all(out.as_bytes())?;

    Ok(())
}

fn main() {
    if let Err(e) = codegen() {
        println!("code generation failed: {}", e);
        std::process::exit(1);
    }

    println!("cargo:rerun-if-changed=build.rs");
}
