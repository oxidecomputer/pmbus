use anyhow::{bail, Result};
use convert_case::{Case, Casing};
use ron::de::from_reader;
use serde::Deserialize;
use std::collections::HashMap;
use std::env;
use std::fmt::Write;
use std::fs;
use std::fs::File;
use std::path::Path;
use std::path::PathBuf;

#[derive(Debug, Deserialize)]
struct High(u8);

#[derive(Debug, Deserialize)]
struct Low(u8);

#[derive(Debug, Deserialize)]
struct Value(u16, String);

#[derive(Debug, Deserialize)]
struct Field {
    name: String,
    bitrange: (High, Low),
    values: HashMap<String, Value>,
}

#[derive(Debug, Deserialize)]
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

#[derive(Debug, Deserialize)]
struct Command(u8, String, Operation, Operation);

fn reg_sizes(cmds: &Vec<Command>) -> Result<HashMap<String, Option<usize>>> {
    let mut sizes = HashMap::new();

    for cmd in cmds {
        let size = match cmd.3 {
            Operation::ReadByte => Some(1),
            Operation::ReadWord => Some(2),
            Operation::ReadWord32 => Some(4),
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

fn validate(
    cmd: &str,
    fields: &HashMap<String, Field>,
    sizes: &HashMap<String, Option<usize>>,
) -> Result<()> {
    let size = match sizes.get(cmd) {
        Some(Some(size)) => size,
        Some(None) => {
            bail!("command {} does not allow a register", cmd);
        }
        None => {
            bail!("command {} does not exist", cmd);
        }
    };

    // TODO: Check for overlapping ranges, illegal values
    for (f, field) in fields {
        if field.bitrange.0 .0 < field.bitrange.1 .0 {
            bail!("{}: field \"{}\" has illegal bit range", cmd, f);
        }

        if field.bitrange.0 .0 as usize >= size * 8 {
            bail!("{}: field \"{}\" has high bit that exceeds size", cmd, f);
        }
    }

    Ok(())
}

#[rustfmt::skip::macros(writeln)]
fn output_value(
    name: &str,
    values: &HashMap<String, Value>,
    width: usize,
) -> Result<String> {
    let mut s = String::new();

    writeln!(&mut s, r##"
    #[derive(Copy, Clone, Debug, PartialEq, FromPrimitive, ToPrimitive)]
    #[allow(non_camel_case_types)]
    pub enum {} {{"##, name)?;

    for (v, value) in values {
        writeln!(
            &mut s, "        {} = 0b{:0width$b},",
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
fn output_databytes(
    cmd: &str,
    fields: &HashMap<String, Field>,
    sizes: &HashMap<String, Option<usize>>,
) -> Result<String> {
    let mut s = String::new();
    let size = sizes.get(cmd).unwrap().unwrap();
    let bits = size * 8;

    writeln!(&mut s, r##"
pub mod {} {{
    use super::Bitpos;
    use super::Bitwidth;
    use num_derive::FromPrimitive;
    use num_derive::ToPrimitive;

    use num_traits::FromPrimitive;
    use num_traits::ToPrimitive;

    pub struct Data(pub u{});

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
        let pos = field.bitrange.1 .0;
        let width = field.bitrange.0 .0 - field.bitrange.1 .0 + 1;

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
        let width = field.bitrange.0 .0 - field.bitrange.1 .0 + 1;
        write!(&mut s, "{}", output_value(&f, &field.values, width.into())?)?;
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
        fn raw(&self) -> u32 {{
            match self {{"##)?;

    for (f, _) in fields {
        writeln!(
            &mut s,
            "                Value::{}(v) => v.to_u32().unwrap(),",
            f
        )?;
    }

    writeln!(&mut s, "                Value::Unknown(v) => *v as u32,")?;
    writeln!(&mut s, "            }}\n        }}\n    }}")?;

    writeln!(&mut s, r##"
    impl core::fmt::Display for Value {{
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {{
            write!(
                f, "0b{{:b}} ({{}})",
                super::Value::raw(self), super::Value::desc(self)
            )
        }}
    }}"##)?;

    writeln!(&mut s, r##"
    impl Data {{
        pub fn field(bit: Bitpos) -> Option<(Field, Bitwidth)> {{
            match bit.0 {{"##)?;

    for (f, field) in fields {
        writeln!(&mut s,
            "                {} => Some((Field::{}, Bitwidth({}))),",
            field.bitrange.1.0, f,
            field.bitrange.0.0 - field.bitrange.1.0 + 1
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

    for (f, _) in fields {
        let method = f.from_case(Case::Camel).to_case(Case::Snake);

        writeln!(&mut s, r##"
        pub fn get_{}(&mut self) -> Option<{}> {{
            match self.get(Field::{}) {{
                Value::{}(v) => Some(v),
                _ => None,
            }}
        }}"##, method, f, f, f)?;

        writeln!(&mut s, r##"
        pub fn set_{}(&mut self, val: {}) {{
            self.set_val(Field::{}, val.to_u{}().unwrap());
        }}"##, method, f, f, bits)?;
    }

    writeln!(&mut s, "    }}")?;

    writeln!(&mut s, r##"
    impl super::Data for Data {{
        fn fields(&self, iter: impl Fn(&dyn super::Field, &dyn super::Value)) {{
            let mut pos = 0;

            while pos < {} {{
                if let Some((field, width)) = Data::field(Bitpos(pos)) {{
                    let val = self.get(field);
                    iter(&field, &val);
                    pos += width.0;
                }} else {{
                    pos += 1;
                }}
            }}
        }}
    }}"##, bits)?;

    writeln!(&mut s, "}}")?;

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
    //
    // First, consume our commands.
    //
    let mut dir = PathBuf::from(&env::var("CARGO_MANIFEST_DIR")?);
    dir.push("src");

    let f = open_file("commands.ron")?;

    let cmds: Vec<Command> = match from_reader(f) {
        Ok(cmds) => cmds,
        Err(e) => {
            bail!("failed to parse commands.ron: {}", e);
        }
    };

    let sizes = reg_sizes(&cmds)?;

    let f = open_file("databytes.ron")?;

    let dbs: HashMap<String, HashMap<String, Field>> = match from_reader(f) {
        Ok(dbs) => dbs,
        Err(e) => {
            bail!("failed to parse databytes.ron: {}", e);
        }
    };

    let out_dir = env::var("OUT_DIR")?;
    let dest_path = Path::new(&out_dir).join("databytes.rs");

    for (cmd, fields) in dbs {
        validate(&cmd, &fields, &sizes)?;
        let out = output_databytes(&cmd, &fields, &sizes)?;
        fs::write(&dest_path, out)?;
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
