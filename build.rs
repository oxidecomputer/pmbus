use anyhow::{bail, Result};

use convert_case::{Case, Casing};
use ron::de::from_reader;
use serde::Deserialize;
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fmt::Write;
use std::fs::File;
use std::path::Path;
use std::path::PathBuf;

#[derive(Debug, Deserialize)]
struct High(u8);

#[derive(Debug, Deserialize)]
struct Low(u8);

#[derive(Debug, Deserialize)]
struct Factor(f32);

#[derive(Debug, Deserialize)]
struct Offset(f32);

#[derive(Debug, Deserialize)]
struct Value(u16, String);

#[derive(Debug, Deserialize)]
enum Units {
    Nanoseconds,
    Milliseconds,
    Seconds,
    Amperes,
    Milliamps,
    Volts,
    Millivolts,
    Celsius,
}

impl Units {
    fn suffix(&self) -> &str {
        match self {
            Units::Nanoseconds => "ns",
            Units::Milliseconds => "ms",
            Units::Seconds => "s",
            Units::Amperes => "A",
            Units::Milliamps => "mA",
            Units::Volts => "V",
            Units::Millivolts => "mV",
            Units::Celsius => "degrees C",
        }
    }
}

#[derive(Debug, Deserialize)]
enum Values<T> {
    Scalar,
    Sentinels(T),
    Units(Units),
    ScaledUnits(Units, Factor),
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
    ReadWord32,
    ReadBlock,
    WriteBlock,
    ProcessCall,
    MfrDefined,
    Extended,
    Illegal,
    Unknown,
}

#[derive(Clone, Debug, Deserialize)]
struct Command(u8, String, Operation, Operation);

#[derive(Debug, Deserialize)]
struct Commands(Vec<Command>, HashMap<String, HashMap<String, Field>>);

#[derive(Debug, Deserialize)]
struct Device(String, String);

fn reg_sizes(cmds: &Vec<Command>) -> Result<HashMap<String, Option<usize>>> {
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
            | Operation::WriteBlock => {
                bail!("illegal read operation {:?} on {}", cmd.3, cmd.1);
            }
            _ => None,
        };

        sizes.insert(cmd.1.clone(), size);
    }

    Ok(sizes)
}

#[rustfmt::skip::macros(writeln)]
fn output_commands(cmds: &Commands, device: bool) -> Result<String> {
    let mut s = String::new();

    writeln!(&mut s, r##"
pub use num_derive::{{FromPrimitive, ToPrimitive}};
pub use num_traits::{{FromPrimitive, ToPrimitive}};

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq, FromPrimitive)]
#[repr(u8)]
pub enum CommandCode {{"##)?;

    for cmd in &cmds.0 {
        writeln!(&mut s, "    {} = 0x{:x},", cmd.1, cmd.0)?;
    }

    writeln!(&mut s, r##"}}

impl Command for CommandCode {{
    fn read_op(&self) -> Operation {{
        match self {{"##)?;

    for cmd in &cmds.0 {
        writeln!(&mut s,
            "            CommandCode::{} => Operation::{:?},", cmd.1, cmd.3)?;
    }

    writeln!(&mut s, "        }}\n    }}")?;

    writeln!(&mut s, r##"
    fn write_op(&self) -> Operation {{
        match self {{"##)?;

    for cmd in &cmds.0 {
        writeln!(&mut s,
            "            CommandCode::{} => Operation::{:?},", cmd.1, cmd.2)?;
    }

    writeln!(&mut s, "        }}\n    }}\n}}")?;

    writeln!(&mut s, r##"
impl CommandCode {{
    pub fn fields(
        &self,
        payload: &[u8],
        iter: impl FnMut(&dyn Field, &dyn Value)
    ) -> Result<(), Error> {{
        match self {{"##)?;

    for cmd in &cmds.0 {
        if cmds.1.get(&cmd.1).is_none() {
            continue;
        }

        writeln!(&mut s, r##"            CommandCode::{} => {{
                use {}::CommandData;
                if let Some(data) = CommandData::from_slice(payload) {{
                    data.fields(iter);
                    Ok(())
                }} else {{
                    Err(Error::ShortData)
                }}
            }}"##, cmd.1, cmd.1)?;
    }

    if device {
        //
        // For devices, we want to fallback to calling the common data
        // method.
        //
        writeln!(&mut s, r##"            _ => {{
                let code = *self as u8;
                match super::CommandCode::from_u8(code) {{
                    Some(cmd) => cmd.fields(payload, iter),
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
    fields: &HashMap<String, Field>,
    sizes: &HashMap<String, Option<usize>>,
) -> Result<(usize, usize)> {
    let mut highest = 0;

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
        Values::Scalar | Values::Units(_) | Values::ScaledUnits(..) => {
            return output_scalar(name, width);
        }
    };

    writeln!(&mut s, r##"
    /// Values that can be taken by the {} field
    #[derive(Copy, Clone, Debug, PartialEq, FromPrimitive, ToPrimitive)]
    #[allow(non_camel_case_types)]
    pub enum {} {{"##, desc, name)?;

    for (v, value) in values {
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

    for (v, value) in values {
        writeln!(
            &mut s, "                {}::{} => \"{}\",",
            name, v, value.1
        )?;
    }

    writeln!(&mut s, "            }}\n        }}\n     }}")?;
    Ok(s)
}

#[rustfmt::skip::macros(writeln)]
fn output_command_data(
    cmd: &str,
    fields: &HashMap<String, Field>,
    bits: usize,
    bytes: usize,
) -> Result<String> {
    let mut s = String::new();

    writeln!(&mut s, r##"
#[allow(non_snake_case)]
#[allow(non_camel_case_types)]
pub mod {} {{
    use super::Bitpos;
    use super::Bitwidth;

    use num_derive::FromPrimitive;
    use num_derive::ToPrimitive;

    use num_traits::FromPrimitive;
    use num_traits::ToPrimitive;

    pub struct CommandData(pub u{});

    #[derive(Copy, Clone, Debug, PartialEq)]
    pub enum Field {{"##, cmd, bits)?;

    for f in fields.keys() {
        writeln!(&mut s, "        {},", f)?;
    }

    writeln!(&mut s, r##"    }}

    impl core::fmt::Display for Field {{
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
            use super::Field;
            write!(f, "{{}}", self.name())
        }}
    }}

    impl super::Field for Field {{
        fn bits(&self) -> (Bitpos, Bitwidth) {{
            match self {{"##)?;

    for (f, field) in fields {
        let (high, low) = bitrange(&field.bits);

        let pos = low;
        let width = high - low + 1;

        writeln!(&mut s, "                Field::{} => \
            (Bitpos({}), Bitwidth({})),", f, pos, width)?;
    }

    writeln!(&mut s, "            }}\n        }}")?;

    writeln!(&mut s, r##"
        fn name(&self) -> &'static str {{
            match self {{"##)?;

    for (f, field) in fields {
        writeln!(&mut s, "                Field::{} => \"{}\",", f, field.name)?;
    }

    writeln!(&mut s, "            }}\n        }}\n    }}")?;

    for (f, field) in fields {
        let (high, low) = bitrange(&field.bits);
        let width = high - low + 1;
        write!(
            &mut s,
            "{}",
            output_value(&f, &field.name, &field.values, width.into())?
        )?;
    }

    writeln!(&mut s, r##"
    #[derive(Copy, Clone, Debug, PartialEq)]
    pub enum Value {{"##)?;

    for (f, _) in fields {
        writeln!(&mut s, "        {}({}),", f, f)?;
    }

    writeln!(&mut s, "        Unknown(u{}),\n    }}", bits)?;

    writeln!(&mut s, r##"
    impl super::Value for Value {{
        fn desc(&self) -> &'static str {{
            match self {{"##)?;

    for (f, _) in fields {
        writeln!(&mut s, "                Value::{}(v) => v.desc(),", f)?;
    }

    writeln!(&mut s, "                Value::Unknown(_) => \"<unknown>\",")?;
    writeln!(&mut s, "            }}\n        }}")?;

    writeln!(&mut s, r##"
        fn scalar(&self) -> bool {{
            match self {{"##)?;

    for (f, field) in fields {
        match field.values {
            Values::Scalar | Values::Units(_) | Values::ScaledUnits(..) => {
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

    for (f, field) in fields {
        match &field.values {
            Values::Sentinels(_) => {
                writeln!(
                    &mut s,
                    "                Value::{}(v) => v.to_u32().unwrap(),",
                    f
                )?;
            }
            Values::Scalar | Values::Units(_) | Values::ScaledUnits(..) => {
                writeln!(
                    &mut s,
                    "                Value::{}(v) => v.0 as u32,",
                    f
                )?;
            }
        }
    }

    writeln!(&mut s, "                Value::Unknown(v) => *v as u32,")?;
    writeln!(&mut s, "            }}\n        }}\n    }}")?;

    writeln!(&mut s, r##"
    impl core::fmt::Display for Value {{
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
            match self {{"##)?;

    for (f, field) in fields {
        match &field.values {
            Values::Scalar => {
                writeln!(&mut s, r##"
                Value::{}(_) => {{
                    write!(
                        f, "0x{{:x}}",
                        super::Value::raw(self)
                    )
                }}"##, f)?;
            }

            Values::Units(u) => {
                writeln!(&mut s, r##"
                Value::{}(_) => {{
                    write!(
                        f, "{{}} {}",
                        super::Value::raw(self)
                    )
                }}"##, f, u.suffix())?;
            }

            Values::ScaledUnits(u, Factor(factor)) => {
                writeln!(&mut s, r##"
                Value::{}(_) => {{
                    write!(
                        f, "{{}}{}",
                        super::Value::raw(self) as f32 * {}
                    )
                }}"##, f, u.suffix(), factor)?;
            }

            _ => {}
        }
    }

    writeln!(&mut s, r##"
                _ => {{
                    write!(
                        f, "0b{{:b}} = {{}}",
                        super::Value::raw(self), super::Value::desc(self)
                    )
                }}
            }}
        }}
    }}"##)?;

    writeln!(&mut s, r##"
    impl CommandData {{
        pub fn from_slice(slice: &[u8]) -> Option<Self> {{"##)?;

    if bits == bytes * 8 {
        writeln!(&mut s, r##"
            use core::convert::TryInto;

            let v: Result<&[u8; {}], _> = slice[0..{}].try_into();

            match v {{
                Ok(v) => Some(Self(u{}::from_le_bytes(*v))),
                Err(_) => None,
            }}
        }}"##, bytes, bytes, bits)?;
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
        pub fn field(bit: Bitpos) -> Option<(Field, Bitwidth)> {{
            match bit.0 {{"##)?;

    for (f, field) in fields {
        let (high, low) = bitrange(&field.bits);

        writeln!(&mut s,
            "                {} => Some((Field::{}, Bitwidth({}))),",
            low, f, high - low + 1
        )?;
    }

    writeln!(&mut s, "                _ => None,")?;
    writeln!(&mut s, "            }}\n        }}")?;

    writeln!(&mut s, r##"
        pub fn get_val(&self, field: Field) -> u{} {{
            use super::Field;
            let (pos, width) = field.bits();
            (self.0 >> pos.0) & ((1 << width.0) - 1)
        }}
        
        pub fn get(&self, field: Field) -> Value {{
            let raw = self.get_val(field);

            match field {{"##, bits)?;

    for (f, _) in fields {
        writeln!(&mut s, r##"
                Field::{} => {{
                    match {}::from_u{}(raw) {{
                        Some(t) => Value::{}(t),
                        None => Value::Unknown(raw),
                    }}
                }}"##, f, f, bits, f)?;
    }

    writeln!(&mut s, "            }}\n        }}")?;

    writeln!(&mut s, r##"
        fn set_val(&mut self, field: Field, raw: u{}) {{
            use super::Field;
            let (pos, width) = field.bits();
            self.0 &= !(((1 << width.0) - 1) << pos.0);
            self.0 |= raw << pos.0;
        }}"##, bits)?;

    for (f, field) in fields {
        let method = f.from_case(Case::Camel).to_case(Case::Snake);

        match &field.values {
            Values::Scalar => {
                writeln!(&mut s, r##"
        pub fn get_{}(&self) -> u{} {{
            self.get_val(Field::{})
        }}"##, method, bits, f)?;
            }

            Values::Units(unit) => {
                writeln!(&mut s, r##"
        pub fn get_{}(&self) -> crate::{:?} {{
            crate::{:?}(self.get_val(Field::{}) as f32)
        }}"##, method, unit, unit, f)?;
            }

            Values::ScaledUnits(unit, Factor(factor)) => {
                writeln!(&mut s, r##"
        pub fn get_{}(&self) -> crate::{:?} {{
            crate::{:?}(self.get_val(Field::{}) as f32 * {})
        }}"##, method, unit, unit, f, factor)?;
            }

            Values::Sentinels(_) => {
                writeln!(&mut s, r##"
        /// Return the value of the {} field as a [`Value::{}`], or
        /// `None` if the field is corrupt or otherwise cannot be represented
        /// as a [`Value::{}`].
        pub fn get_{}(&self) -> Option<{}> {{
            match self.get(Field::{}) {{
                Value::{}(v) => Some(v),
                _ => None,
            }}
        }}"##, field.name, f, f,method, f, f, f)?;
            }
        }

        writeln!(&mut s, r##"
        pub fn set_{}(&mut self, val: {}) {{
            self.set_val(Field::{}, val.to_u{}().unwrap());
        }}"##, method, f, f, bits)?;
    }

    writeln!(&mut s, "    }}")?;

    writeln!(&mut s, r##"
    impl super::CommandData for CommandData {{
        fn fields(
            &self,
            mut iter: impl FnMut(&dyn super::Field, &dyn super::Value)
        ) {{
            let mut pos: u8 = {};

            loop {{
                if let Some((field, _)) = CommandData::field(Bitpos(pos)) {{
                    let val = self.get(field);
                    iter(&field, &val);
                }}

                if pos == 0 {{
                    break;
                }}

                pos -= 1;
            }}
        }}

        fn raw(&self) -> (u32, Bitwidth) {{
            (self.0 as u32, Bitwidth({}))
        }}

        fn command(
            &self,
            mut cb: impl FnMut(&dyn super::Command)
        ) {{
            cb(&super::CommandCode::{})
        }}

    }}"##, bits - 1, bits, cmd)?;

    writeln!(&mut s, "}}")?;

    Ok(s)
}

#[rustfmt::skip::macros(writeln)]
#[rustfmt::skip::macros(write)]
fn output_devices(devices: &Vec<Device>) -> Result<String> {
    let mut s = String::new();

    let name = |str: &str| str.to_case(Case::UpperCamel);

    writeln!(&mut s, r##"
#[derive(Copy, Clone, Debug)]
pub enum Device {{
    Common,"##)?;

    for dev in devices {
        writeln!(&mut s, "    {},", name(&dev.0))?;
    }

    write!(&mut s, r##"
}}

impl Device {{
    pub fn from_str(str: &str) -> Option<Self> {{
        "##)?;

    for dev in devices {
        write!(&mut s, r##"if str == Device::{}.name() {{
            Some(Device::{})
        }} else "##, name(&dev.0), name(&dev.0))?;
    }

    writeln!(&mut s, r##"{{
            None
        }}
    }}

    pub fn name(&self) -> &str {{
        match self {{
            Device::Common => "<common>","##)?;

    for dev in devices {
        writeln!(&mut s,
            "            Device::{} => \"{}\",", name(&dev.0), dev.0)?;
    }

    writeln!(&mut s, "        }}\n    }}\n")?;

    writeln!(&mut s, r##"
    pub fn desc(&self) -> &str {{
        match self {{
            Device::Common => "<common>","##)?;

    for dev in devices {
        writeln!(&mut s,
            "            Device::{} => \"{}\",", name(&dev.0), dev.1)?;
    }

    writeln!(&mut s, "        }}\n    }}\n")?;
    writeln!(&mut s, "}}")?;

    writeln!(&mut s, r##"
pub fn devices(mut dev: impl FnMut(Device)) {{"##)?;
    for dev in devices {
        writeln!(&mut s, "    dev(Device::{});", name(&dev.0))?;
    }
    writeln!(&mut s, r##"}}

/// For the given command code, iterates over the fields in the structured
/// register (if any), calling the specified function for each field and its
/// value.  In general, this should only be used by agnostic code that is
/// attmpting to make sense of PMBus data; *in situ* code that wishes to pull
/// a particular value should use the direct accessor function instead.
pub fn fields(
    device: Device,
    code: u8,
    payload: &[u8],
    iter: impl FnMut(&dyn Field, &dyn Value)
) -> Result<(), Error> {{
    match device {{
        Device::Common => match CommandCode::from_u8(code) {{
            Some(cmd) => {{
                cmd.fields(payload, iter)?;
                Ok(())
            }}
            None => {{
                Err(Error::InvalidCode)
            }}
        }},"##)?;

    for dev in devices {
        writeln!(&mut s, r##"
        Device::{} => match {}::CommandCode::from_u8(code) {{
            Some(cmd) => {{
                cmd.fields(payload, iter)?;
                Ok(())
            }}
            None => {{
                Err(Error::InvalidCode)
            }}
        }},"##, name(&dev.0), dev.0)?;
    }

    writeln!(&mut s, r##"    }}
}}

pub fn command(
    device: Device,
    code: u8,
    mut cb: impl FnMut(&dyn Command)
) {{
    match device {{
        Device::Common => match CommandCode::from_u8(code) {{
            Some(cmd) => {{
                cb(&cmd);
            }}
            None => {{}}
        }},"##)?;

    for dev in devices {
        writeln!(&mut s, r##"
        Device::{} => match {}::CommandCode::from_u8(code) {{
            Some(cmd) => {{
                cb(&cmd);
            }}
            None => {{}}
        }},"##, name(&dev.0), dev.0)?;
    }

    writeln!(&mut s, "    }}\n}}\n")?;

    Ok(s)
}

#[rustfmt::skip::macros(writeln)]
fn output_device(device: &str) -> Result<String> {
    let mut s = String::new();

    writeln!(&mut s, r##"
pub mod {} {{
    pub use super::Command;
    pub use super::CommandData;
    pub use super::Value;
    pub use super::Field;
    pub use super::Bitwidth;
    pub use super::Bitpos;
    pub use super::Operation;
    pub use super::Error;

    include!(concat!(env!("OUT_DIR"), "/{}.rs"));
}}"##, device, device)?;

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

fn codegen() -> Result<()> {
    use std::io::Write;

    //
    // First, consume our common commands.
    //
    let mut dir = PathBuf::from(&env::var("CARGO_MANIFEST_DIR")?);
    dir.push("src");

    let f = open_file("commands.ron")?;

    let cmds: Commands = match from_reader(f) {
        Ok(cmds) => cmds,
        Err(e) => {
            bail!("failed to parse commands.ron: {}", e);
        }
    };

    let sizes = reg_sizes(&cmds.0)?;
    let dbs = &cmds.1;

    let out_dir = env::var("OUT_DIR")?;
    let dest_path = Path::new(&out_dir).join("commands.rs");
    let mut file = File::create(&dest_path)?;

    let out = output_commands(&cmds, false)?;
    file.write_all(out.as_bytes())?;

    for (cmd, fields) in dbs {
        let (bits, bytes) = validate(cmd, fields, &sizes)?;
        let out = output_command_data(cmd, fields, bits, bytes)?;
        file.write_all(out.as_bytes())?;
    }

    let f = open_file("devices.ron")?;

    let devices: Vec<Device> = match from_reader(f) {
        Ok(devices) => devices,
        Err(e) => {
            bail!("failed to parse devices.ron: {}", e);
        }
    };

    let dest_path = Path::new(&out_dir).join("devices.rs");
    let mut dfile = File::create(&dest_path)?;

    let out = output_devices(&devices)?;
    dfile.write_all(out.as_bytes())?;

    //
    // Now we need to iterate over our devices.  For each one, we'll generate
    // our flattened module, and then include it in our flattened file of
    // all devices.
    //
    for dev in &devices {
        let dest_path = Path::new(&out_dir).join(format!("{}.rs", dev.0));
        let mut file = File::create(&dest_path)?;

        let fname = format!("{}.ron", &dev.0);
        let f = open_file(&fname)?;

        let mut dcmds: Commands = match from_reader(f) {
            Ok(dcmds) => dcmds,
            Err(e) => {
                bail!("failed to parse {}: {}", fname, e);
            }
        };

        //
        // Flatten our commands and output them
        //
        let mut h: HashSet<u8> = HashSet::new();

        for cmd in &dcmds.0 {
            h.insert(cmd.0);
        }

        for cmd in &cmds.0 {
            if h.get(&cmd.0).is_none() {
                dcmds.0.push(cmd.clone());
            }
        }

        let out = output_commands(&dcmds, true)?;
        file.write_all(out.as_bytes())?;

        let dsizes = reg_sizes(&dcmds.0)?;

        //
        // Now emit data payloads, allowing the device definition to
        // override any common payload.
        //
        for (cmd, fields) in dbs {
            if let Some(fields) = dcmds.1.get(cmd) {
                let (bits, bytes) = validate(&cmd, &fields, &dsizes)?;
                let out = output_command_data(cmd, fields, bits, bytes)?;
                file.write_all(out.as_bytes())?;
                dcmds.1.remove(cmd);
            } else {
                let (bits, bytes) = validate(&cmd, &fields, &sizes)?;
                let out = output_command_data(cmd, fields, bits, bytes)?;
                file.write_all(out.as_bytes())?;
            }
        }

        for (cmd, fields) in &dcmds.1 {
            let (bits, bytes) = validate(&cmd, &fields, &dsizes)?;
            let out = output_command_data(cmd, fields, bits, bytes)?;
            file.write_all(out.as_bytes())?;
        }

        let out = output_device(&dev.0)?;
        dfile.write_all(out.as_bytes())?;
    }

    Ok(())
}

fn main() {
    if let Err(e) = codegen() {
        println!("code generation failed: {}", e);
        std::process::exit(1);
    }

    println!("cargo:rerun-if-changed=build.rs");
}
