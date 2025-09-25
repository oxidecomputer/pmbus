//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
use pmbus::*;

fn mode() -> VOutModeCommandData {
    panic!("unexpected call to get VOutModeCommandData");
}

macro_rules! validate {
    ($cmd:tt, $raw:expr, $val:expr, $units:tt) => {
        let cmd = $cmd::CommandData::from_slice(&$raw).unwrap();
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

        println!("{}({}) = {:?}", c, r, cmd.get());
    };

    ($cmd:tt, $vout:expr, $raw:expr, $val:expr, $units:tt) => {
        let cmd = $cmd::CommandData::from_slice(&$raw).unwrap();
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

        println!("{}({}) = {:?}", c, r, cmd.get($vout));
    };
}

#[test]
fn verify_cmds() {
    macro_rules! verify {
        ($val:expr, $cmd:tt, $write:tt, $read:tt) => {
            assert_eq!(CommandCode::$cmd as u8, $val);
            assert_eq!(CommandCode::$cmd.write_op(), Operation::$write);
            assert_eq!(CommandCode::$cmd.read_op(), Operation::$read);
        };
    }

    //
    // This is deliberately designed to allow one to read Table 31 in the
    // 1.3.1 specification and validate it.
    //
    verify!(0x00, PAGE, WriteByte, ReadByte);
    verify!(0x01, OPERATION, WriteByte, ReadByte);
    verify!(0x02, ON_OFF_CONFIG, WriteByte, ReadByte);
    verify!(0x03, CLEAR_FAULTS, SendByte, Illegal);
    verify!(0x04, PHASE, WriteByte, ReadByte);
    verify!(0x05, PAGE_PLUS_WRITE, WriteBlock, Illegal);
    verify!(0x06, PAGE_PLUS_READ, Illegal, ProcessCall);
    verify!(0x07, ZONE_CONFIG, WriteWord, ReadWord);
    verify!(0x08, ZONE_ACTIVE, WriteWord, ReadWord);
    verify!(0x10, WRITE_PROTECT, WriteByte, ReadByte);
    verify!(0x11, STORE_DEFAULT_ALL, SendByte, Illegal);
    verify!(0x12, RESTORE_DEFAULT_ALL, SendByte, Illegal);
    verify!(0x13, STORE_DEFAULT_CODE, WriteByte, Illegal);
    verify!(0x14, RESTORE_DEFAULT_CODE, WriteByte, Illegal);
    verify!(0x15, STORE_USER_ALL, SendByte, Illegal);
    verify!(0x16, RESTORE_USER_ALL, SendByte, Illegal);
    verify!(0x17, STORE_USER_CODE, WriteByte, Illegal);
    verify!(0x18, RESTORE_USER_CODE, WriteByte, Illegal);
    verify!(0x19, CAPABILITY, Illegal, ReadByte);
    verify!(0x1a, QUERY, Illegal, ProcessCall);
    verify!(0x1b, SMBALERT_MASK, WriteWord, ProcessCall);
    verify!(0x20, VOUT_MODE, WriteByte, ReadByte);
    verify!(0x21, VOUT_COMMAND, WriteWord, ReadWord);
    verify!(0x22, VOUT_TRIM, WriteWord, ReadWord);
    verify!(0x23, VOUT_CAL_OFFSET, WriteWord, ReadWord);
    verify!(0x24, VOUT_MAX, WriteWord, ReadWord);
    verify!(0x25, VOUT_MARGIN_HIGH, WriteWord, ReadWord);
    verify!(0x26, VOUT_MARGIN_LOW, WriteWord, ReadWord);
    verify!(0x27, VOUT_TRANSITION_RATE, WriteWord, ReadWord);
    verify!(0x28, VOUT_DROOP, WriteWord, ReadWord);
    verify!(0x29, VOUT_SCALE_LOOP, WriteWord, ReadWord);
    verify!(0x2a, VOUT_SCALE_MONITOR, WriteWord, ReadWord);
    verify!(0x2b, VOUT_MIN, WriteWord, ReadWord);
    verify!(0x30, COEFFICIENTS, Illegal, ProcessCall);
    verify!(0x31, POUT_MAX, WriteWord, ReadWord);
    verify!(0x32, MAX_DUTY, WriteWord, ReadWord);
    verify!(0x33, FREQUENCY_SWITCH, WriteWord, ReadWord);
    verify!(0x34, POWER_MODE, WriteByte, ReadByte);
    verify!(0x35, VIN_ON, WriteWord, ReadWord);
    verify!(0x36, VIN_OFF, WriteWord, ReadWord);
    verify!(0x37, INTERLEAVE, WriteWord, ReadWord);
    verify!(0x38, IOUT_CAL_GAIN, WriteWord, ReadWord);
    verify!(0x39, IOUT_CAL_OFFSET, WriteWord, ReadWord);
    verify!(0x3a, FAN_CONFIG_1_2, WriteByte, ReadByte);
    verify!(0x3b, FAN_COMMAND_1, WriteWord, ReadWord);
    verify!(0x3c, FAN_COMMAND_2, WriteWord, ReadWord);
    verify!(0x3d, FAN_CONFIG_3_4, WriteByte, ReadByte);
    verify!(0x3e, FAN_COMMAND_3, WriteWord, ReadWord);
    verify!(0x3f, FAN_COMMAND_4, WriteWord, ReadWord);
    verify!(0x40, VOUT_OV_FAULT_LIMIT, WriteWord, ReadWord);
    verify!(0x41, VOUT_OV_FAULT_RESPONSE, WriteByte, ReadByte);
    verify!(0x42, VOUT_OV_WARN_LIMIT, WriteWord, ReadWord);
    verify!(0x43, VOUT_UV_WARN_LIMIT, WriteWord, ReadWord);
    verify!(0x44, VOUT_UV_FAULT_LIMIT, WriteWord, ReadWord);
    verify!(0x45, VOUT_UV_FAULT_RESPONSE, WriteByte, ReadByte);
    verify!(0x46, IOUT_OC_FAULT_LIMIT, WriteWord, ReadWord);
    verify!(0x47, IOUT_OC_FAULT_RESPONSE, WriteByte, ReadByte);
    verify!(0x48, IOUT_OC_LV_FAULT_LIMIT, WriteWord, ReadWord);
    verify!(0x49, IOUT_OC_LV_FAULT_RESPONSE, WriteByte, ReadByte);
    verify!(0x4a, IOUT_OC_WARN_LIMIT, WriteWord, ReadWord);
    verify!(0x4b, IOUT_UC_FAULT_LIMIT, WriteWord, ReadWord);
    verify!(0x4c, IOUT_UC_FAULT_RESPONSE, WriteByte, ReadByte);
    verify!(0x4f, OT_FAULT_LIMIT, WriteWord, ReadWord);
    verify!(0x50, OT_FAULT_RESPONSE, WriteByte, ReadByte);
    verify!(0x51, OT_WARN_LIMIT, WriteWord, ReadWord);
    verify!(0x52, UT_WARN_LIMIT, WriteWord, ReadWord);
    verify!(0x53, UT_FAULT_LIMIT, WriteWord, ReadWord);
    verify!(0x54, UT_FAULT_RESPONSE, WriteByte, ReadByte);
    verify!(0x55, VIN_OV_FAULT_LIMIT, WriteWord, ReadWord);
    verify!(0x56, VIN_OV_FAULT_RESPONSE, WriteByte, ReadByte);
    verify!(0x57, VIN_OV_WARN_LIMIT, WriteWord, ReadWord);
    verify!(0x58, VIN_UV_WARN_LIMIT, WriteWord, ReadWord);
    verify!(0x59, VIN_UV_FAULT_LIMIT, WriteWord, ReadWord);
    verify!(0x5a, VIN_UV_FAULT_RESPONSE, WriteByte, ReadByte);
    verify!(0x5b, IIN_OC_FAULT_LIMIT, WriteWord, ReadWord);
    verify!(0x5c, IIN_OC_FAULT_RESPONSE, WriteByte, ReadByte);
    verify!(0x5d, IIN_OC_WARN_LIMIT, WriteWord, ReadWord);
    verify!(0x5e, POWER_GOOD_ON, WriteWord, ReadWord);
    verify!(0x5f, POWER_GOOD_OFF, WriteWord, ReadWord);
    verify!(0x60, TON_DELAY, WriteWord, ReadWord);
    verify!(0x61, TON_RISE, WriteWord, ReadWord);
    verify!(0x62, TON_MAX_FAULT_LIMIT, WriteWord, ReadWord);
    verify!(0x63, TON_MAX_FAULT_RESPONSE, WriteByte, ReadByte);
    verify!(0x64, TOFF_DELAY, WriteWord, ReadWord);
    verify!(0x65, TOFF_FALL, WriteWord, ReadWord);
    verify!(0x66, TOFF_MAX_WARN_LIMIT, WriteWord, ReadWord);
    verify!(0x67, Deprecated, Unknown, Unknown);
    verify!(0x68, POUT_OP_FAULT_LIMIT, WriteWord, ReadWord);
    verify!(0x69, POUT_OP_FAULT_RESPONSE, WriteByte, ReadByte);
    verify!(0x6a, POUT_OP_WARN_LIMIT, WriteWord, ReadWord);
    verify!(0x6b, PIN_OP_WARN_LIMIT, WriteWord, ReadWord);
    verify!(0x78, STATUS_BYTE, WriteByte, ReadByte);
    verify!(0x79, STATUS_WORD, WriteWord, ReadWord);
    verify!(0x7a, STATUS_VOUT, WriteByte, ReadByte);
    verify!(0x7b, STATUS_IOUT, WriteByte, ReadByte);
    verify!(0x7c, STATUS_INPUT, WriteByte, ReadByte);
    verify!(0x7d, STATUS_TEMPERATURE, WriteByte, ReadByte);
    verify!(0x7e, STATUS_CML, WriteByte, ReadByte);
    verify!(0x7f, STATUS_OTHER, WriteByte, ReadByte);
    verify!(0x80, STATUS_MFR_SPECIFIC, WriteByte, ReadByte);
    verify!(0x81, STATUS_FANS_1_2, WriteByte, ReadByte);
    verify!(0x82, STATUS_FANS_3_4, WriteByte, ReadByte);
    verify!(0x83, READ_KWH_IN, Illegal, ReadWord32);
    verify!(0x84, READ_KWH_OUT, Illegal, ReadWord32);
    verify!(0x85, READ_KWH_CONFIG, WriteWord, ReadWord);
    verify!(0x86, READ_EIN, Illegal, ReadBlock);
    verify!(0x87, READ_EOUT, Illegal, ReadBlock);
    verify!(0x88, READ_VIN, Illegal, ReadWord);
    verify!(0x89, READ_IIN, Illegal, ReadWord);
    verify!(0x8a, READ_VCAP, Illegal, ReadWord);
    verify!(0x8b, READ_VOUT, Illegal, ReadWord);
    verify!(0x8c, READ_IOUT, Illegal, ReadWord);
    verify!(0x8d, READ_TEMPERATURE_1, Illegal, ReadWord);
    verify!(0x8e, READ_TEMPERATURE_2, Illegal, ReadWord);
    verify!(0x8f, READ_TEMPERATURE_3, Illegal, ReadWord);
    verify!(0x90, READ_FAN_SPEED_1, Illegal, ReadWord);
    verify!(0x91, READ_FAN_SPEED_2, Illegal, ReadWord);
    verify!(0x92, READ_FAN_SPEED_3, Illegal, ReadWord);
    verify!(0x93, READ_FAN_SPEED_4, Illegal, ReadWord);
    verify!(0x94, READ_DUTY_CYCLE, Illegal, ReadWord);
    verify!(0x95, READ_FREQUENCY, Illegal, ReadWord);
    verify!(0x96, READ_POUT, Illegal, ReadWord);
    verify!(0x97, READ_PIN, Illegal, ReadWord);
    verify!(0x98, PMBUS_REVISION, Illegal, ReadByte);
    verify!(0x99, MFR_ID, WriteBlock, ReadBlock);
    verify!(0x9a, MFR_MODEL, WriteBlock, ReadBlock);
    verify!(0x9b, MFR_REVISION, WriteBlock, ReadBlock);
    verify!(0x9c, MFR_LOCATION, WriteBlock, ReadBlock);
    verify!(0x9d, MFR_DATE, WriteBlock, ReadBlock);
    verify!(0x9e, MFR_SERIAL, WriteBlock, ReadBlock);
    verify!(0x9f, APP_PROFILE_SUPPORT, Illegal, ReadBlock);
    verify!(0xa0, MFR_VIN_MIN, Illegal, ReadWord);
    verify!(0xa1, MFR_VIN_MAX, Illegal, ReadWord);
    verify!(0xa2, MFR_IIN_MAX, Illegal, ReadWord);
    verify!(0xa3, MFR_PIN_MAX, Illegal, ReadWord);
    verify!(0xa4, MFR_VOUT_MIN, Illegal, ReadWord);
    verify!(0xa5, MFR_VOUT_MAX, Illegal, ReadWord);
    verify!(0xa6, MFR_IOUT_MAX, Illegal, ReadWord);
    verify!(0xa7, MFR_POUT_MAX, Illegal, ReadWord);
    verify!(0xa8, MFR_TAMBIENT_MAX, Illegal, ReadWord);
    verify!(0xa9, MFR_TAMBIENT_MIN, Illegal, ReadWord);
    verify!(0xaa, MFR_EFFICIENCY_LL, Illegal, ReadBlock);
    verify!(0xab, MFR_EFFICIENCY_HL, Illegal, ReadBlock);
    verify!(0xac, MFR_PIN_ACCURACY, Illegal, ReadByte);
    verify!(0xad, IC_DEVICE_ID, Illegal, ReadBlock);
    verify!(0xae, IC_DEVICE_REV, Illegal, ReadBlock);
    verify!(0xb0, USER_DATA_00, WriteBlock, ReadBlock);
    verify!(0xb1, USER_DATA_01, WriteBlock, ReadBlock);
    verify!(0xb2, USER_DATA_02, WriteBlock, ReadBlock);
    verify!(0xb3, USER_DATA_03, WriteBlock, ReadBlock);
    verify!(0xb4, USER_DATA_04, WriteBlock, ReadBlock);
    verify!(0xb5, USER_DATA_05, WriteBlock, ReadBlock);
    verify!(0xb6, USER_DATA_06, WriteBlock, ReadBlock);
    verify!(0xb7, USER_DATA_07, WriteBlock, ReadBlock);
    verify!(0xb8, USER_DATA_08, WriteBlock, ReadBlock);
    verify!(0xb9, USER_DATA_09, WriteBlock, ReadBlock);
    verify!(0xba, USER_DATA_10, WriteBlock, ReadBlock);
    verify!(0xbb, USER_DATA_11, WriteBlock, ReadBlock);
    verify!(0xbc, USER_DATA_12, WriteBlock, ReadBlock);
    verify!(0xbd, USER_DATA_13, WriteBlock, ReadBlock);
    verify!(0xbe, USER_DATA_14, WriteBlock, ReadBlock);
    verify!(0xbf, USER_DATA_15, WriteBlock, ReadBlock);
    verify!(0xc0, MFR_MAX_TEMP_1, WriteWord, ReadWord);
    verify!(0xc1, MFR_MAX_TEMP_2, WriteWord, ReadWord);
    verify!(0xc2, MFR_MAX_TEMP_3, WriteWord, ReadWord);
    verify!(0xc4, MFR_SPECIFIC_C4, MfrDefined, MfrDefined);
    verify!(0xc4, MFR_SPECIFIC_C4, MfrDefined, MfrDefined);
    verify!(0xc5, MFR_SPECIFIC_C5, MfrDefined, MfrDefined);
    verify!(0xc6, MFR_SPECIFIC_C6, MfrDefined, MfrDefined);
    verify!(0xc7, MFR_SPECIFIC_C7, MfrDefined, MfrDefined);
    verify!(0xc8, MFR_SPECIFIC_C8, MfrDefined, MfrDefined);
    verify!(0xc9, MFR_SPECIFIC_C9, MfrDefined, MfrDefined);
    verify!(0xca, MFR_SPECIFIC_CA, MfrDefined, MfrDefined);
    verify!(0xcb, MFR_SPECIFIC_CB, MfrDefined, MfrDefined);
    verify!(0xcc, MFR_SPECIFIC_CC, MfrDefined, MfrDefined);
    verify!(0xcd, MFR_SPECIFIC_CD, MfrDefined, MfrDefined);
    verify!(0xce, MFR_SPECIFIC_CE, MfrDefined, MfrDefined);
    verify!(0xcf, MFR_SPECIFIC_CF, MfrDefined, MfrDefined);
    verify!(0xd0, MFR_SPECIFIC_D0, MfrDefined, MfrDefined);
    verify!(0xd1, MFR_SPECIFIC_D1, MfrDefined, MfrDefined);
    verify!(0xd2, MFR_SPECIFIC_D2, MfrDefined, MfrDefined);
    verify!(0xd3, MFR_SPECIFIC_D3, MfrDefined, MfrDefined);
    verify!(0xd4, MFR_SPECIFIC_D4, MfrDefined, MfrDefined);
    verify!(0xd5, MFR_SPECIFIC_D5, MfrDefined, MfrDefined);
    verify!(0xd6, MFR_SPECIFIC_D6, MfrDefined, MfrDefined);
    verify!(0xd7, MFR_SPECIFIC_D7, MfrDefined, MfrDefined);
    verify!(0xd8, MFR_SPECIFIC_D8, MfrDefined, MfrDefined);
    verify!(0xd9, MFR_SPECIFIC_D9, MfrDefined, MfrDefined);
    verify!(0xda, MFR_SPECIFIC_DA, MfrDefined, MfrDefined);
    verify!(0xdb, MFR_SPECIFIC_DB, MfrDefined, MfrDefined);
    verify!(0xdc, MFR_SPECIFIC_DC, MfrDefined, MfrDefined);
    verify!(0xdd, MFR_SPECIFIC_DD, MfrDefined, MfrDefined);
    verify!(0xde, MFR_SPECIFIC_DE, MfrDefined, MfrDefined);
    verify!(0xdf, MFR_SPECIFIC_DF, MfrDefined, MfrDefined);
    verify!(0xe0, MFR_SPECIFIC_E0, MfrDefined, MfrDefined);
    verify!(0xe1, MFR_SPECIFIC_E1, MfrDefined, MfrDefined);
    verify!(0xe2, MFR_SPECIFIC_E2, MfrDefined, MfrDefined);
    verify!(0xe3, MFR_SPECIFIC_E3, MfrDefined, MfrDefined);
    verify!(0xe4, MFR_SPECIFIC_E4, MfrDefined, MfrDefined);
    verify!(0xe5, MFR_SPECIFIC_E5, MfrDefined, MfrDefined);
    verify!(0xe6, MFR_SPECIFIC_E6, MfrDefined, MfrDefined);
    verify!(0xe7, MFR_SPECIFIC_E7, MfrDefined, MfrDefined);
    verify!(0xe8, MFR_SPECIFIC_E8, MfrDefined, MfrDefined);
    verify!(0xe9, MFR_SPECIFIC_E9, MfrDefined, MfrDefined);
    verify!(0xea, MFR_SPECIFIC_EA, MfrDefined, MfrDefined);
    verify!(0xeb, MFR_SPECIFIC_EB, MfrDefined, MfrDefined);
    verify!(0xec, MFR_SPECIFIC_EC, MfrDefined, MfrDefined);
    verify!(0xed, MFR_SPECIFIC_ED, MfrDefined, MfrDefined);
    verify!(0xee, MFR_SPECIFIC_EE, MfrDefined, MfrDefined);
    verify!(0xef, MFR_SPECIFIC_EF, MfrDefined, MfrDefined);
    verify!(0xf0, MFR_SPECIFIC_F0, MfrDefined, MfrDefined);
    verify!(0xf1, MFR_SPECIFIC_F1, MfrDefined, MfrDefined);
    verify!(0xf2, MFR_SPECIFIC_F2, MfrDefined, MfrDefined);
    verify!(0xf3, MFR_SPECIFIC_F3, MfrDefined, MfrDefined);
    verify!(0xf4, MFR_SPECIFIC_F4, MfrDefined, MfrDefined);
    verify!(0xf5, MFR_SPECIFIC_F5, MfrDefined, MfrDefined);
    verify!(0xf6, MFR_SPECIFIC_F6, MfrDefined, MfrDefined);
    verify!(0xf7, MFR_SPECIFIC_F7, MfrDefined, MfrDefined);
    verify!(0xf8, MFR_SPECIFIC_F8, MfrDefined, MfrDefined);
    verify!(0xf9, MFR_SPECIFIC_F9, MfrDefined, MfrDefined);
    verify!(0xfa, MFR_SPECIFIC_FA, MfrDefined, MfrDefined);
    verify!(0xfb, MFR_SPECIFIC_FB, MfrDefined, MfrDefined);
    verify!(0xfc, MFR_SPECIFIC_FC, MfrDefined, MfrDefined);
    verify!(0xfd, MFR_SPECIFIC_FD, MfrDefined, MfrDefined);
    verify!(0xfe, MFR_SPECIFIC_COMMAND_EXT, Extended, Extended);
    verify!(0xff, PMBUS_COMMAND_EXT, Extended, Extended);
    std::println!("{:?}", CommandCode::from_u8(0x9));
}

#[test]
fn verify_operation() {
    let data = commands::OPERATION::CommandData(0x4);

    data.interpret(mode, |field, value| {
        std::println!("{} = {}", field.desc(), value);
    })
    .unwrap();
}

#[test]
fn verify_operation_set() {
    use commands::OPERATION::*;
    let mut data = CommandData(0x4);

    dump(&data);

    assert_ne!(
        data.get_voltage_command_source(),
        Some(VoltageCommandSource::VOUT_MARGIN_HIGH)
    );

    data.set_voltage_command_source(VoltageCommandSource::VOUT_MARGIN_HIGH);

    dump(&data);

    assert_eq!(
        data.get_voltage_command_source(),
        Some(VoltageCommandSource::VOUT_MARGIN_HIGH)
    );
}

#[test]
fn raw_operation() {
    CommandCode::OPERATION
        .interpret(&[0x4], mode, |field, value| {
            std::println!("{} = {}", field.desc(), value);
        })
        .unwrap();
}

#[test]
fn bad_operation() {
    // We expect this to generate an Unknown value for the
    // margin fault response rather than an error
    CommandCode::OPERATION
        .interpret(&[0b0000_1100], mode, |field, value| {
            std::println!("{} = {}", field.desc(), value);
        })
        .unwrap();
}

#[test]
fn page() {
    use commands::PAGE::*;

    let mut data = CommandData(1);
    assert_eq!(data.0, 1);

    let rval = data.mutate(mode, |field, _| {
        assert_eq!(field.bitfield(), false);
        Some(Replacement::Integer(0xf00))
    });

    assert_eq!(rval, Err(Error::OverflowReplacement));

    let rval = data.mutate(mode, |field, _| {
        assert_eq!(field.bitfield(), false);
        Some(Replacement::Integer(0xde))
    });

    assert_eq!(rval, Ok(()));
    assert_eq!(data.0, 0xde);
}

#[test]
fn phase() {
    use commands::PHASE::*;

    let mut data = CommandData(1);
    assert_eq!(data.0, 1);

    let rval = data.mutate(mode, |field, _| {
        assert_eq!(field.bitfield(), false);
        Some(Replacement::Integer(0xf00))
    });

    assert_eq!(rval, Err(Error::OverflowReplacement));

    let rval = data.mutate(mode, |field, _| {
        assert_eq!(field.bitfield(), false);
        Some(Replacement::Integer(0xde))
    });

    assert_eq!(rval, Ok(()));
    assert_eq!(data.0, 0xde);
}

fn dump_data(
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

fn dump(data: &impl CommandData) {
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

#[test]
fn verify_status_word() {
    use commands::STATUS_WORD::*;

    let data = CommandData::from_slice(&[0x43, 0x18]).unwrap();
    dump(&data);

    data.interpret(mode, |field, value| {
        std::println!("{} = {}", field.desc(), value);
    })
    .unwrap();
}

#[test]
fn verify_on_off_config() {
    use commands::ON_OFF_CONFIG::*;

    let data = CommandData::from_slice(&[0x17]).unwrap();
    dump(&data);
}

#[test]
fn verify_capability() {
    use commands::CAPABILITY::*;

    let data = CommandData::from_slice(&[0xd0]).unwrap();
    dump(&data);

    let data = CommandData::from_slice(&[0xb0]).unwrap();
    dump(&data);
}

#[test]
fn verify_vout_mode() {
    use commands::VOUT_MODE::*;
    let data = CommandData::from_slice(&[0x97]).unwrap();
    dump(&data);
}

#[test]
fn verify_status_vout() {
    use commands::STATUS_VOUT::*;
    let data = CommandData::from_slice(&[0x0]).unwrap();
    dump(&data);
}

#[test]
fn verify_status_iout() {
    use commands::STATUS_IOUT::*;
    let data = CommandData::from_slice(&[0x0]).unwrap();
    dump(&data);
}

#[test]
fn verify_status_cml() {
    use commands::STATUS_CML::*;
    let data = CommandData::from_slice(&[0x82]).unwrap();
    dump(&data);
}

#[test]
fn verify_status_other() {
    use commands::STATUS_OTHER::*;
    let data = CommandData::from_slice(&[0x1]).unwrap();
    dump(&data);
}

#[test]
fn verify_status_adm127x() {
    use commands::adm127x::STATUS_MFR_SPECIFIC::*;
    let data = CommandData::from_slice(&[0x40]).unwrap();
    dump(&data);
}

#[test]
fn device_list() {
    let code = commands::CommandCode::STATUS_MFR_SPECIFIC as u8;

    std::println!("code is {:x}", code);

    devices(|d| {
        for i in 0..=0xff {
            d.command(i, |cmd| {
                std::println!(
                    "{:?}: {:2x} {} R={:?} W={:?}",
                    d,
                    i,
                    cmd.name(),
                    cmd.read_op(),
                    cmd.write_op()
                );
            });
        }
    });
}

fn synonyms(codes: &[commands::CommandCode], payload: &[u8]) {
    let mut bycode = vec![];

    for code in codes {
        let mut names = vec![];

        Device::Common
            .interpret(*code as u8, payload, mode, |f, v| {
                names.push((f.name(), v.name()));
            })
            .unwrap();

        let mut fields = vec![];

        Device::Common
            .fields(*code as u8, |f| {
                fields.push(f.name());
            })
            .unwrap();

        assert_eq!(fields.len(), names.len());
        bycode.push((*code, names));
    }

    println!("bycode: {:#?}", bycode);

    for c in 1..codes.len() {
        println!("comparing {:?} to {:?}", bycode[c].0, bycode[c - 1].0);
        assert_eq!(bycode[c].1, bycode[c - 1].1);
    }
}

#[test]
fn synonyms_ov() {
    use commands::*;

    let codes = [
        CommandCode::VOUT_OV_FAULT_RESPONSE,
        CommandCode::VOUT_UV_FAULT_RESPONSE,
        CommandCode::UT_FAULT_RESPONSE,
        CommandCode::VIN_OV_FAULT_RESPONSE,
        CommandCode::VIN_UV_FAULT_RESPONSE,
        CommandCode::TON_MAX_FAULT_RESPONSE,
        CommandCode::POUT_OP_FAULT_RESPONSE,
    ];

    synonyms(&codes, &[0x84]);
}

#[test]
fn synonyms_oc() {
    use commands::*;

    let codes = [
        CommandCode::IOUT_OC_FAULT_RESPONSE,
        CommandCode::IOUT_UC_FAULT_RESPONSE,
        CommandCode::IIN_OC_FAULT_RESPONSE,
    ];

    synonyms(&codes, &[0x04]);
}

#[test]
fn tps_read_all() {
    use commands::tps546b24a::READ_ALL::*;

    let data = CommandData::from_slice(&[
        0x02, 0x00, 0x63, 0x02, 0xee, 0xad, 0xd8, 0xdb, 0xfe, 0xd2, 0x00, 0x00,
        0x00, 0x00,
    ])
    .unwrap();

    assert_eq!(data.get_read_vin(), 0xd2fe);
    assert_eq!(data.get_read_vout(), 0x0263);
    assert_eq!(data.get_status_word(), 0x0002);
    assert_eq!(data.get_read_temperature_1(), 0xdbd8);
}

#[test]
fn tps_read_all_data() {
    let _code = commands::tps546b24a::CommandCode::READ_ALL as u8;
    let mode = || VOutModeCommandData::from_slice(&[0x97]).unwrap();

    let data = [
        0x02, 0x00, 0x63, 0x02, 0xee, 0xad, 0xd8, 0xdb, 0xfe, 0xd2, 0x00, 0x00,
        0x00, 0x00,
    ];

    for code in 0..=0xff {
        let _ =
            Device::Tps546B24A.interpret(code, &data[0..], mode, |f, _v| {
                std::println!("f is {}", f.desc());
            });
    }
}

#[test]
fn tps_passthrough() {
    //
    // This is a bit of a mouthful of a test to assure that common registers
    // are correctly passed through into device-specific modules.
    //
    use commands::tps546b24a::CAPABILITY::*;

    let code = commands::tps546b24a::CommandCode::CAPABILITY as u8;
    let payload = &[0xd0];
    let mut result = None;

    let name = Field::MaximumBusSpeed.name();

    let cap = CommandData::from_slice(payload).unwrap();
    let val = cap.get(Field::MaximumBusSpeed).unwrap();
    let target = std::format!("{}", val);

    Device::Tps546B24A
        .interpret(code, payload, mode, |f, v| {
            if f.name() == name {
                result = Some(std::format!("{}", v));
            }
        })
        .unwrap();

    assert_eq!(result, Some(target));
}

#[test]
fn bmr480_default() {
    use commands::bmr480::*;

    let data = MFR_FAST_OCP_CFG::CommandData::from_slice(&[0xe9, 0x02]);
    dump(&data.unwrap());

    let data = MFR_RESPONSE_UNIT_CFG::CommandData::from_slice(&[0x51]);
    dump(&data.unwrap());

    let data = MFR_ISHARE_THRESHOLD::CommandData::from_slice(&[
        0x10, 0x10, 0x00, 0x64, 0x00, 0x00, 0x00, 0x01,
    ])
    .unwrap();

    assert_eq!(data.get_trim_limit(), units::Volts(0.170));

    dump(&data);
}

#[test]
fn bmr491_default() {
    use commands::bmr491::*;

    let data = MFR_FAST_OCP_CFG::CommandData::from_slice(&[0xe9, 0x02]);
    dump(&data.unwrap());

    let data = MFR_RESPONSE_UNIT_CFG::CommandData::from_slice(&[0x51]);
    dump(&data.unwrap());

    let mut data = MFR_ISHARE_THRESHOLD::CommandData::from_slice(&[
        0x10, 0x10, 0x00, 0x64, 0x00, 0x00, 0x00, 0x01,
    ])
    .unwrap();

    assert_eq!(data.get_trim_limit(), units::Volts(0.170));

    assert_eq!(data.set_trim_limit(units::Volts(0.136)), Ok(()));
    assert_eq!(data.get_trim_limit(), units::Volts(0.136));

    dump(&data);
}

#[test]
fn bmr491_pgood_polarity() {
    use commands::bmr491::*;

    let data = MFR_PGOOD_POLARITY::CommandData::from_slice(&[0x01]);
    dump(&data.unwrap());
}

#[test]
fn bmr480_iout() {
    use commands::bmr480::*;

    let data = [
        (0xf028u16, 10.0),
        (0xf133, 76.75),
        (0xf040, 16.0),
        (0xf004, 1.0),
        (0xf051, 20.25),
        (0xf079, 30.25),
        (0xf00a, 2.5),
        (0xf0c9, 50.25),
        (0xf07d, 31.25),
        (0xf00b, 2.75),
        (0xf009, 2.25),
    ];

    for d in &data {
        let raw = d.0.to_le_bytes();
        let iout = READ_IOUT::CommandData::from_slice(&raw).unwrap();
        assert_eq!(iout.get(), Ok(units::Amperes(d.1)));

        iout.interpret(mode, |f, v| {
            assert_eq!(f.bitfield(), false);
            std::println!("{} 0x{:04x} = {}", f.name(), d.0, v);
        })
        .unwrap();
    }
}

#[test]
fn bmr480_vout() {
    use commands::bmr480::*;

    let mode = || VOutModeCommandData::from_slice(&[0x15]).unwrap();

    let data = [
        (0x0071u16, 0.05517578f32),
        (0x0754, 0.9160156),
        (0x5f72, 11.930664),
        (0x5f80, 11.9375),
        (0x5fd3, 11.978027),
        (0x5fdb, 11.981934),
        (0x5fe4, 11.986328),
        (0x5fe6, 11.987305),
        (0x5fec, 11.990234),
        (0x5fee, 11.991211),
        (0x5ff7, 11.995605),
        (0x6007, 12.003418),
        (0x6039, 12.027832),
        (0x603f, 12.030762),
        (0x6091, 12.070801),
        (0x65b7, 12.714355),
        (0x65d8, 12.730469),
        (0x670a, 12.879883),
        (0x68b0, 13.0859375),
        (0x69c1, 13.219238),
        (0x69e2, 13.235352),
    ];

    for d in &data {
        let raw = d.0.to_le_bytes();
        let vout = READ_VOUT::CommandData::from_slice(&raw).unwrap();
        assert_eq!(vout.get(mode()), Ok(units::Volts(d.1)));

        vout.interpret(mode, |f, v| {
            assert_eq!(f.bitfield(), false);
            std::println!("{} 0x{:04x} = {}", f.name(), d.0, v);
        })
        .unwrap();
    }
}

#[test]
fn bmr480_vin() {
    use commands::bmr480::*;

    let mode = || VOutModeCommandData::from_slice(&[0x15]).unwrap();

    let data = [
        (0x0a5cu16, 1208.0),
        (0x0a8c, 1304.0),
        (0xe9a0, 52.0),
        (0xe9a1, 52.125),
        (0xe9a2, 52.25),
        (0xe9a3, 52.375),
        (0xe9a4, 52.5),
        (0xe9a6, 52.75),
        (0xe9a7, 52.875),
    ];

    for d in &data {
        let raw = d.0.to_le_bytes();
        let vin = READ_VIN::CommandData::from_slice(&raw).unwrap();
        assert_eq!(vin.get(), Ok(units::Volts(d.1)));

        vin.interpret(mode, |f, v| {
            assert_eq!(f.bitfield(), false);
            std::println!("{} 0x{:04x} = {}", f.name(), d.0, v);
        })
        .unwrap();
    }
}

#[test]
fn bmr491_rc_level() {
    use commands::bmr491::*;
    let rc = MFR_RC_LEVEL::CommandData::from_slice(&[0xc8]).unwrap();
    assert_eq!(rc.get(), Ok(units::Volts(20.0)));
}

#[test]
fn bmr491_ks_pretrig() {
    use commands::bmr491::*;
    let ks = MFR_KS_PRETRIG::CommandData::from_slice(&[0x89]).unwrap();
    assert_eq!(ks.get(), Ok(units::Microseconds(61.649998)));
}

#[test]
fn bmr491_temp_compensation() {
    use commands::bmr491::*;
    let temp = MFR_TEMP_COMPENSATION::CommandData::from_slice(&[
        0x28, 0x33, 0x80, 0x85, 0x00, 0x90, 0x95, 0x00,
    ])
    .unwrap();

    assert_eq!(
        temp.get_edac_slope(),
        units::MillivoltsPerCelsius(0.3112793)
    );
    dump(&temp);
}

#[test]
fn bmr491_vin_offset() {
    use commands::bmr491::*;
    let offset =
        MFR_VIN_OFFSET::CommandData::from_slice(&[0x00, 0x04, 0x00, 0x09])
            .unwrap();

    dump(&offset);
}

#[test]
fn mutate_operation() {
    use commands::OPERATION::*;

    let mut data = CommandData(0x4);
    dump(&data);

    std::println!("{:?}", data.get_on_off_state());
    assert_eq!(data.get_on_off_state(), Some(OnOffState::Off));

    data.mutate(mode, |field, _| {
        if field.name() == "OnOffState" {
            Some(Replacement::Boolean(true))
        } else {
            None
        }
    })
    .unwrap();

    assert_eq!(data.get_on_off_state(), Some(OnOffState::On));

    dump(&data);
}

#[test]
fn mutate_overflow_replacement() {
    use commands::OPERATION::*;

    let mut data = CommandData(0x4);

    let rval = data.mutate(mode, |field, _| {
        if field.name() == "OnOffState" {
            Some(Replacement::Integer(3))
        } else {
            None
        }
    });

    assert_eq!(rval, Err(Error::OverflowReplacement));
}

#[test]
fn mutate_invalid() {
    use commands::OPERATION::*;

    let mut data = CommandData(0x4);

    let rval = data.mutate(mode, |field, _| {
        if field.name() == "OnOffState" {
            Some(Replacement::Float(3.1))
        } else {
            None
        }
    });

    assert_eq!(rval, Err(Error::InvalidReplacement));
}

#[test]
fn vout_command_set() {
    let mut vout = VOutModeCommandData::from_slice(&[0x97]).unwrap();
    use commands::VOUT_COMMAND::*;
    dump(&vout);

    std::println!("param is {}", vout.get_parameter());
    let mut data = CommandData::from_slice(&[0x63, 0x02]).unwrap();
    assert_eq!(data.get(vout), Ok(units::Volts(1.1933594)));

    data.set(vout, units::Volts(1.20)).unwrap();
    assert_eq!(data.0, 0x0266);
    assert_eq!(data.get(vout), Ok(units::Volts(1.1992188)));

    //
    // Now crank our resolution up
    //
    vout.set_parameter(-12).unwrap();
    assert_eq!(vout.get_parameter(), -12);
    data.set(vout, units::Volts(1.20)).unwrap();
    std::println!("{:?}", data.get(vout).unwrap());

    vout.set_parameter(-15).unwrap();
    assert_eq!(vout.get_parameter(), -15);
    data.set(vout, units::Volts(1.20)).unwrap();
    assert_eq!(data.get(vout), Ok(units::Volts(1.2000122)));

    //
    // With our exponent cranked to its maximum, there is no room
    // left for anything greater than 1.
    //
    vout.set_parameter(-16).unwrap();
    assert_eq!(vout.get_parameter(), -16);

    assert_eq!(vout.set_parameter(-101), Err(Error::ValueOutOfRange));
    std::println!("{:?}", vout.get_parameter());

    data.set(vout, units::Volts(0.20)).unwrap();
    assert_eq!(data.get(vout), Ok(units::Volts(0.19999695)));

    assert_eq!(
        data.set(vout, units::Volts(1.20)),
        Err(Error::ValueOutOfRange)
    );

    std::println!("{:?}", data.get(vout).unwrap());
}

#[test]
fn vout_command_mutate() {
    let vout = VOutModeCommandData::from_slice(&[0x97]).unwrap();
    use commands::VOUT_COMMAND::*;
    dump(&vout);

    let mut data = CommandData::from_slice(&[0x63, 0x02]).unwrap();
    assert_eq!(data.get(vout), Ok(units::Volts(1.1933594)));

    let rval = data.mutate(
        || vout,
        |field, _| {
            assert_eq!(field.bitfield(), false);
            assert_eq!(field.bits(), (Bitpos(0), Bitwidth(16)));
            Some(Replacement::Float(1.20))
        },
    );

    assert_eq!(rval, Ok(()));
    assert_eq!(data.0, 0x0266);
    assert_eq!(data.get(vout), Ok(units::Volts(1.1992188)));

    let rval = data.mutate(|| vout, |_, _| Some(Replacement::Integer(3)));

    assert_eq!(rval, Ok(()));
    assert_eq!(data.get(vout), Ok(units::Volts(3.0)));

    let rval = data.mutate(|| vout, |_, _| Some(Replacement::Boolean(true)));

    assert_eq!(rval, Err(Error::InvalidReplacement));

    let rval = data.mutate(|| vout, |_, _| Some(Replacement::Float(150.0)));

    assert_eq!(rval, Err(Error::ValueOutOfRange));
}

#[test]
fn device_vout_command_mutate() {
    let vout = VOutModeCommandData::from_slice(&[0x97]).unwrap();
    use commands::VOUT_COMMAND::*;
    dump(&vout);

    let mut payload = [0x63, 0x02];

    let data = CommandData::from_slice(&payload).unwrap();
    assert_eq!(data.get(vout), Ok(units::Volts(1.1933594)));

    let rval = Device::Common.mutate(
        commands::CommandCode::VOUT_COMMAND as u8,
        &mut payload[0..2],
        || vout,
        |field, _| {
            assert_eq!(field.bitfield(), false);
            assert_eq!(field.bits(), (Bitpos(0), Bitwidth(16)));
            Some(Replacement::Float(1.20))
        },
    );

    assert_eq!(rval, Ok(()));
    assert_eq!(payload[0], 0x66);
    assert_eq!(payload[1], 0x02);

    let data = CommandData::from_slice(&payload).unwrap();
    assert_eq!(data.0, 0x0266);
    assert_eq!(data.get(vout), Ok(units::Volts(1.1992188)));
}

#[test]
fn sentinels() {
    use commands::OPERATION::*;

    let data = CommandData::from_slice(&[0x88]).unwrap();
    dump(&data);

    CommandData::sentinels(Bitpos(4), |val| {
        match val.name() {
            "VOUT_COMMAND" => {
                assert_eq!(val.raw(), 0);
            }
            "VOUT_MARGIN_LOW" => {
                assert_eq!(val.raw(), 1);
            }
            "VOUT_MARGIN_HIGH" => {
                assert_eq!(val.raw(), 2);
            }
            "AVS_VOUT_COMMAND" => {
                assert_eq!(val.raw(), 3);
            }
            _ => {
                panic!("unrecognized sentinel");
            }
        }

        #[rustfmt::skip]
        std::println!(r##"{:16}"{}" => {{
                assert_eq!(val.raw(), {:?});
            }}"##, "", val.name(), val.raw());
    })
    .unwrap();

    assert_eq!(
        CommandData::sentinels(Bitpos(5), |_| {}),
        Err(Error::InvalidField)
    );
}

#[test]
fn device_sentinels() {
    Device::Common
        .sentinels(1, Bitpos(4), |val| {
            match val.name() {
                "VOUT_COMMAND" => {
                    assert_eq!(val.raw(), 0);
                }
                "VOUT_MARGIN_LOW" => {
                    assert_eq!(val.raw(), 1);
                }
                "VOUT_MARGIN_HIGH" => {
                    assert_eq!(val.raw(), 2);
                }
                "AVS_VOUT_COMMAND" => {
                    assert_eq!(val.raw(), 3);
                }
                _ => {
                    panic!("unrecognized sentinel");
                }
            }

            #[rustfmt::skip]
        std::println!(r##"{:16}"{}" => {{
                assert_eq!(val.raw(), {:?});
            }}"##, "", val.name(), val.raw());
        })
        .unwrap();

    assert_eq!(
        Device::Common.sentinels(1, Bitpos(5), |_| {}),
        Err(Error::InvalidField)
    );
}

#[test]
fn device_fields() {
    Device::Common
        .fields(1, |f| {
            let bits = f.bits();

            match f.name() {
                "OnOffState" => {
                    assert_eq!(bits, (Bitpos(7), Bitwidth(1)));
                }
                "TurnOffBehavior" => {
                    assert_eq!(bits, (Bitpos(6), Bitwidth(1)));
                }
                "VoltageCommandSource" => {
                    assert_eq!(bits, (Bitpos(4), Bitwidth(2)));
                }
                "MarginFaultResponse" => {
                    assert_eq!(bits, (Bitpos(2), Bitwidth(2)));
                }
                "TransitionControl" => {
                    assert_eq!(bits, (Bitpos(1), Bitwidth(1)));
                }
                _ => {
                    panic!("unrecognized field");
                }
            }

            #[rustfmt::skip]
        std::println!(r##"{:16}"{}" => {{
                assert_eq!(bits, {:?});
            }}"##, "", f.name(), f.bits());
        })
        .unwrap();
}

fn print_command(
    device: pmbus::Device,
    code: u8,
    command: &dyn pmbus::Command,
) {
    use std::*;

    println!("0x{:02x} {}", code, command.name());

    let mut bitfields = false;

    let fields = |field: &dyn Field| {
        let bits = field.bits();
        let nbits = bits.1 .0 as usize;

        let b = if nbits == 1 {
            format!("b{}", bits.0 .0)
        } else {
            format!("b{}:{}", bits.0 .0 + bits.1 .0 - 1, bits.0 .0)
        };

        if field.bitfield() {
            bitfields = true;

            println!("     | {:6} {:30} <= {}", b, field.name(), field.desc());

            let mut last = None;

            let sentinels = |val: &dyn Value| {
                let v =
                    format!("0b{:0w$b} = {}", val.raw(), val.name(), w = nbits);

                println!("     | {:6} {:30} <- {}", "", v, val.desc());

                if let Some(last) = last {
                    if last >= val.raw() {
                        panic!("values are out of order");
                    }
                }

                last = Some(val.raw());
            };

            device.sentinels(code, field.bits().0, sentinels).unwrap();
        }
    };

    device.fields(code, fields).unwrap();

    if bitfields {
        println!(
            "     +------------------------------------------\
            -----------------------------\n"
        );
    }
}

#[test]
fn device_commands() {
    devices(|d| {
        std::println!("==== {}: {} ====", d.name(), d.desc());
        for i in 0..=0xff {
            d.command(i, |cmd| {
                print_command(d, i, cmd);
            });
        }
    });
}

#[test]
fn adm127x_direct() {
    use commands::adm127x::*;
    use units::*;

    let voltage = Coefficients {
        m: 4062,
        b: 0,
        R: -2,
    };
    let current = Coefficients {
        m: 663,
        b: 20480,
        R: -1,
    };
    let power = Coefficients {
        m: 10535,
        b: 0,
        R: -3,
    };

    let vin = READ_VIN::CommandData::from_slice(&[0x6d, 0x07]).unwrap();
    assert_eq!(vin.get(&voltage), Ok(Volts(46.799606)));

    let vin = PEAK_VIN::CommandData::from_slice(&[0x04, 0x09]).unwrap();
    assert_eq!(vin.get(&voltage), Ok(Volts(56.8193)));

    let vout = READ_VOUT::CommandData::from_slice(&[0x51, 0x08]).unwrap();
    assert_eq!(vout.get(&voltage), Ok(Volts(52.412605)));

    let vout = PEAK_VOUT::CommandData::from_slice(&[0x03, 0x09]).unwrap();
    assert_eq!(vout.get(&voltage), Ok(Volts(56.79468)));

    let pin = READ_PIN::CommandData::from_slice(&[0x10, 0x01]).unwrap();
    assert_eq!(pin.get(&power), Ok(Watts(25.818699)));

    let pin = PEAK_PIN::CommandData::from_slice(&[0x3d, 0x01]).unwrap();
    assert_eq!(pin.get(&power), Ok(Watts(30.090176)));

    let iout = READ_IOUT::CommandData::from_slice(&[0x24, 0x08]).unwrap();
    assert_eq!(iout.get(&current), Ok(Amperes(0.54298645)));

    let iout = PEAK_IOUT::CommandData::from_slice(&[0x2b, 0x08]).unwrap();
    assert_eq!(iout.get(&current), Ok(Amperes(0.64856714)));
}

#[test]
fn raa228926_defaults() {
    use commands::raa228926::*;
    use units::*;

    let vout = VOutModeCommandData::from_slice(&[0x40]).unwrap();

    validate!(VOUT_COMMAND, vout, [0x84, 0x03], 0.9, Volts);
    validate!(VOUT_MAX, vout, [0xea, 0x0b], 3.05, Volts);
    validate!(VOUT_MARGIN_HIGH, vout, [0xb1, 0x03], 0.945, Volts);
    validate!(VOUT_MARGIN_LOW, vout, [0x57, 0x03], 0.855, Volts);
    validate!(
        VOUT_TRANSITION_RATE,
        [0xc4, 0x09],
        0.025,
        VoltsPerMicrosecond
    );
    validate!(FREQUENCY_SWITCH, [0x58, 0x02], 600, Kilohertz);
    validate!(VIN_ON, [0xbc, 0x02], 7, Volts);
    validate!(VIN_OFF, [0xf4, 0x01], 5, Volts);
    validate!(VOUT_OV_FAULT_LIMIT, vout, [0x1c, 0x0c], 3.1, Volts);
    validate!(IOUT_OC_FAULT_LIMIT, [0x2c, 0x01], 30, Amperes);
    validate!(OT_FAULT_LIMIT, [0x7d, 0x00], 125, Celsius);
    validate!(OT_WARN_LIMIT, [0x6e, 0x00], 110, Celsius);
    validate!(UT_FAULT_LIMIT, [0xd8, 0xff], -40, Celsius);
    validate!(VIN_OV_FAULT_LIMIT, [0x40, 0x06], 16, Volts);
    validate!(VIN_OV_WARN_LIMIT, [0x08, 0x07], 18, Volts);
    validate!(IIN_OC_FAULT_LIMIT, [0x98, 0x3a], 150, Amperes);

    validate!(TON_RISE, [0xf4, 0x01], 0.500, Milliseconds);
    validate!(TOFF_FALL, [0xf4, 0x01], 0.500, Milliseconds);

    validate!(PEAK_OC_LIMIT, [0x58, 0x02], 60, Amperes);
    validate!(PEAK_UC_LIMIT, [0xa8, 0xfd], -60, Amperes);
    validate!(HS_BUS_CURRENT_SCALE, [0x00, 0x40], 1.0, Unitless);
    validate!(IOUT_ALERT_THRESHOLD, [0xc8, 0x00], 20.0, Amperes);

    let ocuc = PEAK_OCUC_COUNT::CommandData::from_slice(&[0x06, 0x06]).unwrap();
    assert_eq!(ocuc.get_uc_limit(), 6);
    assert_eq!(ocuc.get_oc_limit(), 6);
    dump(&ocuc);
}

#[test]
fn raa228926_filt() {
    use commands::raa228926::*;
    use units::*;

    let filt =
        SUM_OC_FILT_COUNT::CommandData::from_slice(&[0x96, 0x06]).unwrap();

    dump(&filt);
    assert_eq!(filt.get_delay(), Microseconds(100.0));
    assert_eq!(filt.get_filter(), Microseconds(10.666667));

    let filt =
        IOUT_ALERT_FILT_COUNT::CommandData::from_slice(&[0x00, 0x06]).unwrap();

    assert_eq!(filt.get_filter(), Microseconds(10.666667));
    dump(&filt);
}

#[test]
fn raa229618_defaults() {
    use commands::raa229618::*;
    use units::*;

    let vout = VOutModeCommandData::from_slice(&[0x40]).unwrap();

    validate!(VOUT_COMMAND, vout, [0x84, 0x03], 0.9, Volts);
    validate!(VOUT_MAX, vout, [0xea, 0x0b], 3.05, Volts);
    validate!(VOUT_MARGIN_HIGH, vout, [0xb1, 0x03], 0.945, Volts);
    validate!(VOUT_MARGIN_LOW, vout, [0x57, 0x03], 0.855, Volts);
    validate!(
        VOUT_TRANSITION_RATE,
        [0xc4, 0x09],
        0.025,
        VoltsPerMicrosecond
    );
    validate!(FREQUENCY_SWITCH, [0x58, 0x02], 600, Kilohertz);
    validate!(VIN_ON, [0xbc, 0x02], 7, Volts);
    validate!(VIN_OFF, [0xf4, 0x01], 5, Volts);
    validate!(VOUT_OV_FAULT_LIMIT, vout, [0x1c, 0x0c], 3.1, Volts);
    validate!(IOUT_OC_FAULT_LIMIT, [0x2c, 0x01], 30, Amperes);
    validate!(OT_FAULT_LIMIT, [0x7d, 0x00], 125, Celsius);
    validate!(OT_WARN_LIMIT, [0x6e, 0x00], 110, Celsius);
    validate!(UT_FAULT_LIMIT, [0xd8, 0xff], -40, Celsius);
    validate!(VIN_OV_FAULT_LIMIT, [0x40, 0x06], 16, Volts);
    validate!(VIN_OV_WARN_LIMIT, [0x08, 0x07], 18, Volts);
    validate!(IIN_OC_FAULT_LIMIT, [0x98, 0x3a], 150, Amperes);

    // Either the value or the factor is incorrect in the datasheet!
    // validate!(TON_RISE, [0x32, 0x00], 0.500, Milliseconds);
    // validate!(TOFF_FALL, [0x32, 0x00], 0.500, Milliseconds);

    validate!(BOOTRATE, [0xf4, 0x01], 0.005, VoltsPerMicrosecond);
    validate!(PEAK_OC_LIMIT, [0x58, 0x02], 60, Amperes);
    validate!(PEAK_UC_LIMIT, [0xa8, 0xfd], -60, Amperes);
    validate!(HS_BUS_CURRENT_SCALE, [0x00, 0x40], 1.0, Unitless);

    let ocuc = PEAK_OCUC_COUNT::CommandData::from_slice(&[0x06, 0x06]).unwrap();
    assert_eq!(ocuc.get_uc_limit(), 6);
    assert_eq!(ocuc.get_oc_limit(), 6);
    dump(&ocuc);

    let comp =
        COMPPROP::CommandData::from_slice(&[0xc4, 0x07, 0x09, 0xd9]).unwrap();
    dump(&comp);
}

#[test]
fn raa229618_filt() {
    use commands::raa229618::*;
    use units::*;

    let fast =
        FAST_OC_FILT_COUNT::CommandData::from_slice(&[0x96, 0x06]).unwrap();

    dump(&fast);
    assert_eq!(fast.get_delay(), Microseconds(100.0));
    assert_eq!(fast.get_filter(), Microseconds(10.666667));

    let slow =
        SLOW_OC_FILT_COUNT::CommandData::from_slice(&[0x06, 0x06]).unwrap();

    assert_eq!(slow.get_delay(), Microseconds(1024.2));
    assert_eq!(slow.get_filter(), Microseconds(10.666667));
    dump(&slow);
}

#[test]
fn raa229618_loopcfg() {
    use commands::raa229618::LOOPCFG::*;

    let loopcfg = CommandData::from_slice(&[0xf6, 0x71, 0x20, 0x10]).unwrap();

    assert_eq!(
        loopcfg.get_diode_emulation_mode(),
        Some(DiodeEmulationMode::Enabled)
    );

    println!("{:?}", loopcfg.get_minimum_phase_count());
    println!("{:?}", loopcfg.get_lock_svid());
    println!("{:?}", loopcfg.get_zero_v_shutdown());

    dump(&loopcfg);
}
