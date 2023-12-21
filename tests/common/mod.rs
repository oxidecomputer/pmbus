use pmbus::*;

#[allow(unused_macros)]
macro_rules! validate {
    ($cmd:tt, $raw:expr, $val:expr, $units:tt) => {
        let cmd = $cmd::CommandData::from_slice(&$raw).unwrap();
        let mut compare = $raw.clone();
        let c = stringify!($cmd);
        let r = stringify!($raw);

        match cmd.get() {
            Ok($units(val)) => {
                let delta = ($val as f32 - val).abs();

                if delta > 0.0001 {
                    panic!("{} failed: expected {}, found {}", c, $val, val);
                }
            }
            Err(err) => {
                panic!("{}({}) failed: {:?}", c, r, err);
            }
        }

        cmd.to_slice(&mut compare);
        assert_eq!($raw, compare);

        println!("{}({}) = {:?}", c, r, cmd.get());
    };

    ($cmd:tt, $vout:expr, $raw:expr, $val:expr, $units:tt) => {
        let cmd = $cmd::CommandData::from_slice(&$raw).unwrap();
        let mut compare = $raw.clone();
        let c = stringify!($cmd);
        let r = stringify!($raw);

        match cmd.get($vout) {
            Ok($units(val)) => {
                let delta = ($val as f32 - val).abs();

                if delta > 0.0001 {
                    panic!("{} failed: expected {}, found {}", c, $val, val);
                }
            }
            _ => {
                panic!("{}({}) failed", c, r);
            }
        }

        cmd.to_slice(&mut compare);
        assert_eq!($raw, compare);

        println!("{}({}) = {:?}", c, r, cmd.get($vout));
    };
}

#[allow(unused_imports)]
pub(crate) use validate;

pub fn mode() -> VOutModeCommandData {
    panic!("unexpected call to get VOutModeCommandData");
}

pub fn dump_data(
    val: u32,
    width: Bitwidth,
    v: &mut std::vec::Vec<((Bitpos, Bitwidth), &str, std::string::String)>,
) {
    let width = width.0 as usize;
    let nibble = 4;
    let maxwidth = 16;

    if width > maxwidth {
        std::println!("{:?}", v);
        return;
    }

    let indent = (maxwidth - width) + ((maxwidth - width) / nibble);

    std::print!("{:indent$}", "", indent = indent);
    std::print!("0b");

    for v in (0..width).step_by(nibble) {
        std::print!(
            "{:04b}{}",
            (val >> ((width - nibble) - v)) & 0xf,
            if v + nibble < width { "_" } else { "\n" }
        )
    }

    while v.len() > 0 {
        let mut cur = width - 1;

        std::print!("{:indent$}", "", indent = indent);
        std::print!("  ");

        for i in 0..v.len() {
            while cur > v[i].0 .0 .0 as usize {
                if cur % nibble == 0 {
                    std::print!(" ");
                }

                std::print!(" ");
                cur -= 1;
            }

            if i < v.len() - 1 {
                std::print!("|");

                if cur % nibble == 0 {
                    std::print!(" ");
                }

                cur -= 1;
            } else {
                std::print!("+--");

                while cur > 0 {
                    std::print!("-");

                    if cur % nibble == 0 {
                        std::print!("-");
                    }

                    cur -= 1;
                }

                std::println!(" {} = {}", v[i].1, v[i].2);
            }
        }

        v.pop();
    }
}

pub fn dump(data: &impl CommandData) {
    let (val, width) = data.raw();
    let mut v = std::vec![];

    data.command(|cmd| {
        std::println!("\n{:?}: ", cmd);
    });

    data.interpret(mode, |field, value| {
        v.push((field.bits(), field.desc(), std::format!("{}", value)));
    })
    .unwrap();

    dump_data(val, width, &mut v);
}
