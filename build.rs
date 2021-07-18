use std::env;
use std::fs;
use std::fs::File;
use std::path::Path;
use std::path::PathBuf;
use std::fmt::Write;
use ron::de::from_reader;
use serde::Deserialize;
use std::collections::HashMap;
use anyhow::{bail, Result};

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
    values: HashMap<String, Value>
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
            _ => None
        };

        sizes.insert(cmd.1.clone(), size);
    }

    Ok(sizes)
}

fn validate(
    cmd: &str,
    fields: &HashMap<String, Field>,
    sizes: &HashMap<String, Option<usize>>
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
        if field.bitrange.0.0 < field.bitrange.1.0 {
            bail!("{}: field \"{}\" has illegal bit range", cmd, f);
        }

        if field.bitrange.0.0 as usize >= size * 8 {
            bail!("{}: field \"{}\" has high bit that exceeds size", cmd, f);
        }
    }

    Ok(())
}

fn output_value(
    name: &str,
    values: &HashMap<String, Value>,
    width: usize
) -> Result<String> {
    let mut s = String::new();

    write!(&mut s, r##"
    #[derive(Copy, Clone, Debug, PartialEq, FromPrimitive, ToPrimitive)]
    #[allow(non_camel_case_types)]
    pub enum {} {{
"##, name)?;

    for (v, value) in values {
        writeln!(&mut s, "        {} = 0b{:0width$b},", v, value.0, width = width);
    }

    writeln!(&mut s, "    }}");

    write!(&mut s, r##"
    impl {} {{
        fn desc(&self) -> &'static str {{
            match self {{
"##, name)?;

    for (v, value) in values {
        writeln!(&mut s, "                {}:{} => \"{}\",", name, v, value.1);
    }

    write!(&mut s, r##"            }}
        }} 
    }}
"##)?;

    Ok(s)
}

fn output_register(
    cmd: &str,
    fields: &HashMap<String, Field>,
    sizes: &HashMap<String, Option<usize>>
) -> Result<String> {
    let mut s = String::new();

    writeln!(&mut s, r##"
mod {} {{
    use super::Bitpos;
    use super::Bitwidth;
    use num_derive::FromPrimitive;
    use num_derive::ToPrimitive;

    use num_traits::FromPrimitive;
    use num_traits::ToPrimitive;

    pub struct Register(pub u8);

    #[derive(Copy, Clone, Debug, PartialEq)]
    pub enum Field {{"##, cmd)?;

    for f in fields.keys() {
        writeln!(&mut s, "        {},", f);
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
        let pos = field.bitrange.1.0;
        let width = field.bitrange.0.0 - field.bitrange.1.0 + 1;

        writeln!(&mut s, "                Field::{} => \
            (Bitpos({}), Bitwidth({})),", f, pos, width);
    }

    writeln!(&mut s, "            }}\n        }}")?;

    writeln!(&mut s, r##"
        fn name(&self) -> &'static str {{
            match self {{"##)?;

    for (f, field) in fields {
        writeln!(&mut s, "                Field::{} => \"{}\",", f, field.name);
    }

    writeln!(&mut s, "            }}\n        }}\n    }}")?;

    let size = sizes.get(cmd).unwrap().unwrap();

    for (f, field) in fields {
        let width = field.bitrange.0.0 - field.bitrange.1.0 + 1;
        write!(&mut s, "{}", output_value(&f, &field.values, width.into())?);
    }

    writeln!(&mut s, r##"
    #[derive(Copy, Clone, Debug, PartialEq)]
    pub enum Value {{"##)?;

    for (f, _) in fields {
        writeln!(&mut s, "        {}({}),", f, f)?;
    }

    writeln!(&mut s, "        Unknown(u{}),\n    }}", size * 8);

    writeln!(&mut s, r##"
    impl super::Value for Value {{
        fn desc(&self) -> &'static str {{
            match self {{"##)?;

    for (f, field) in fields {
        writeln!(&mut s, "                Value::{}(v) => v.desc(),", f);
    }

    writeln!(&mut s, "                Value::Unknown(_) => \"<unknown>\",");
    writeln!(&mut s, "            }}\n        }}\n    }}")?;

    writeln!(&mut s, r##"
    impl Register {{
        pub fn field(bit: Bitpos) -> Option<(Field, Bitwidth)> {{
            match bit.0 {{"##)?;

    for (f, field) in fields {
        writeln!(&mut s,
            "                {} => Some((Field::{}, Bitwidth({}))),",
            field.bitrange.1.0, f,
            field.bitrange.0.0 - field.bitrange.1.0 + 1
        )?;
    }

    writeln!(&mut s, "                _ => None,");
    writeln!(&mut s, "            }}\n        }}")?;

    writeln!(&mut s, r##"
        pub fn get_val(&self, field: Field) -> u{} {{
            use super::Field;
            let (pos, width) = field.bits();
            (self.0 >> pos.0) & ((1 << width.0) - 1)
        }}"##, size * 8)?;


                /*
    for (v, value) in fields.values {
        writeln!(&mut s, "{}", output_value(&v, &fields.values);
    }
    */


/** 
 * main.rs

        pub fn get(&self, field: Field) -> Value {
            let raw = self.get_val(field);

            match field {
                Field::TransitionControl => {
                    match TransitionControl::from_u8(raw) {
                        Some(t) => Value::TransitionControl(t),
                        None => Value::Unknown(raw),
                    }
                }
                Field::MarginFaultResponse => {
                    match MarginFaultResponse::from_u8(raw) {
                        Some(t) => Value::MarginFaultResponse(t),
                        None => Value::Unknown(raw),
                    }
                }
                Field::VoltageCommandSource => {
                    match VoltageCommandSource::from_u8(raw) {
                        Some(t) => Value::VoltageCommandSource(t),
                        None => Value::Unknown(raw),
                    }
                }
                Field::TurnOffBehavior => {
                    match TurnOffBehavior::from_u8(raw) {
                        Some(t) => Value::TurnOffBehavior(t),
                        None => Value::Unknown(raw),
                    }
                }
                Field::OnOffState => {
                    match OnOffState::from_u8(raw) {
                        Some(t) => Value::OnOffState(t),
                        None => Value::Unknown(raw),
                    }
                }
            }
        }
    }

    impl super::Register for Register {
        fn fields(&self, iter: impl Fn(&dyn super::Field, &dyn super::Value)) {
            let mut pos = 0;

            while pos < 8 {
                if let Some((field, width)) = field(Bitpos(pos)) {
                    let val = self.get(field);
                    iter(&field, &val);
                    pos += width.0;
                } else {
                    pos += 1;
                }
            }
        }
    }
    */
 
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

    let f = open_file("registers.ron")?;

    let regs: HashMap<String, HashMap<String, Field>> = match from_reader(f) {
        Ok(regs) => regs,
        Err(e) => {
            bail!("failed to parse registers.ron: {}", e);
        }
    };

    let out_dir = env::var("OUT_DIR")?;
    let dest_path = Path::new(&out_dir).join("registers.rs");

    for (cmd, fields) in regs {
        validate(&cmd, &fields, &sizes)?;
        let out = output_register(&cmd, &fields, &sizes)?;
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
