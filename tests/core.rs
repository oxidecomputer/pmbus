//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
use pmbus::*;
mod common;
use common::*;

#[test]
fn core_verify_cmds() {
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
fn core_verify_operation() {
    let data = commands::OPERATION::CommandData(0x4);

    data.interpret(mode, |field, value| {
        std::println!("{} = {}", field.desc(), value);
    })
    .unwrap();
}

#[test]
fn core_verify_operation_set() {
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
fn core_raw_operation() {
    CommandCode::OPERATION
        .interpret(&[0x4], mode, |field, value| {
            std::println!("{} = {}", field.desc(), value);
        })
        .unwrap();
}

#[test]
fn core_page() {
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
fn core_phase() {
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

#[test]
fn core_verify_status_word() {
    use commands::STATUS_WORD::*;

    let data = CommandData::from_slice(&[0x43, 0x18]).unwrap();
    dump(&data);

    data.interpret(mode, |field, value| {
        std::println!("{} = {}", field.desc(), value);
    })
    .unwrap();
}

#[test]
fn core_verify_on_off_config() {
    use commands::ON_OFF_CONFIG::*;

    let data = CommandData::from_slice(&[0x17]).unwrap();
    dump(&data);
}

#[test]
fn core_verify_capability() {
    use commands::CAPABILITY::*;

    let data = CommandData::from_slice(&[0xd0]).unwrap();
    dump(&data);

    let data = CommandData::from_slice(&[0xb0]).unwrap();
    dump(&data);
}

#[test]
fn core_verify_vout_mode() {
    use commands::VOUT_MODE::*;
    let data = CommandData::from_slice(&[0x97]).unwrap();
    dump(&data);
}

#[test]
fn core_verify_status_vout() {
    use commands::STATUS_VOUT::*;
    let data = CommandData::from_slice(&[0x0]).unwrap();
    dump(&data);
}

#[test]
fn core_verify_status_iout() {
    use commands::STATUS_IOUT::*;
    let data = CommandData::from_slice(&[0x0]).unwrap();
    dump(&data);
}

#[test]
fn core_verify_status_cml() {
    use commands::STATUS_CML::*;
    let data = CommandData::from_slice(&[0x82]).unwrap();
    dump(&data);
}

#[test]
fn core_verify_status_other() {
    use commands::STATUS_OTHER::*;
    let data = CommandData::from_slice(&[0x1]).unwrap();
    dump(&data);
}

#[test]
fn core_device_list() {
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
fn core_synonyms_ov() {
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
fn core_synonyms_oc() {
    use commands::*;

    let codes = [
        CommandCode::IOUT_OC_FAULT_RESPONSE,
        CommandCode::IOUT_UC_FAULT_RESPONSE,
        CommandCode::IIN_OC_FAULT_RESPONSE,
    ];

    synonyms(&codes, &[0x04]);
}

#[test]
fn core_mutate_operation() {
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
fn core_mutate_overflow_replacement() {
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
fn core_mutate_invalid() {
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
fn core_vout_command_set() {
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
fn core_vout_command_mutate() {
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
fn core_device_vout_command_mutate() {
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
fn core_sentinels() {
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
fn core_device_sentinels() {
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
fn core_device_fields() {
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
fn core_device_commands() {
    devices(|d| {
        std::println!("==== {}: {} ====", d.name(), d.desc());
        for i in 0..=0xff {
            d.command(i, |cmd| {
                print_command(d, i, cmd);
            });
        }
    });
}
