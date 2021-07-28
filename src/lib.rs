#![no_std]

/// pmbus: A crate for PMBus manipulation
///
/// This is a no_std crate that expresses the PMBus protocol, as described in
/// the PMBus 1.3 specifcation.  This document is not proprietary, but
/// (regrettably) it is only available on request.  This crate is intended to
/// be generic with respect to implementation and usable by software that will
/// directly communicate with PMBus devices via SMBus/I2C as well as by
/// software that merely wishes to make sense of the PMBus protocol (e.g.,
/// debuggers or analyzers running on a host).
///
pub use num_traits::float::FloatCore;

pub use num_derive::{FromPrimitive, ToPrimitive};
pub use num_traits::{FromPrimitive, ToPrimitive};

mod operation;
pub use crate::operation::Operation;

mod commands;
pub use crate::commands::*;

#[derive(Copy, Clone, Debug)]
pub enum Error {
    ShortData,
}

#[allow(dead_code, non_camel_case_types)]
#[derive(Copy, Clone, PartialEq, Debug, FromPrimitive)]
#[repr(u8)]
pub enum Command {
    PAGE = 0x00,
    OPERATION = 0x01,
    ON_OFF_CONFIG = 0x02,
    CLEAR_FAULTS = 0x03,
    PHASE = 0x04,
    PAGE_PLUS_WRITE = 0x05,
    PAGE_PLUS_READ = 0x06,
    ZONE_CONFIG = 0x07,
    ZONE_ACTIVE = 0x08,
    WRITE_PROTECT = 0x10,
    STORE_DEFAULT_ALL = 0x11,
    RESTORE_DEFAULT_ALL = 0x12,
    STORE_DEFAULT_CODE = 0x13,
    RESTORE_DEFAULT_CODE = 0x14,
    STORE_USER_ALL = 0x15,
    RESTORE_USER_ALL = 0x16,
    STORE_USER_CODE = 0x17,
    RESTORE_USER_CODE = 0x18,
    CAPABILITY = 0x19,
    QUERY = 0x1a,
    SMBALERT_MASK = 0x1b,
    VOUT_MODE = 0x20,
    VOUT_COMMAND = 0x21,
    VOUT_TRIM = 0x22,
    VOUT_CAL_OFFSET = 0x23,
    VOUT_MAX = 0x24,
    VOUT_MARGIN_HIGH = 0x25,
    VOUT_MARGIN_LOW = 0x26,
    VOUT_TRANSITION_RATE = 0x27,
    VOUT_DROOP = 0x28,
    VOUT_SCALE_LOOP = 0x29,
    VOUT_SCALE_MONITOR = 0x2a,
    VOUT_MIN = 0x2b,
    COEFFICIENTS = 0x30,
    POUT_MAX = 0x31,
    MAX_DUTY = 0x32,
    FREQUENCY_SWITCH = 0x33,
    POWER_MODE = 0x34,
    VIN_ON = 0x35,
    VIN_OFF = 0x36,
    INTERLEAVE = 0x37,
    IOUT_CAL_GAIN = 0x38,
    IOUT_CAL_OFFSET = 0x39,
    FAN_CONFIG_1_2 = 0x3a,
    FAN_COMMAND_1 = 0x3b,
    FAN_COMMAND_2 = 0x3c,
    FAN_CONFIG_3_4 = 0x3d,
    FAN_COMMAND_3 = 0x3e,
    FAN_COMMAND_4 = 0x3f,
    VOUT_OV_FAULT_LIMIT = 0x40,
    VOUT_OV_FAULT_RESPONSE = 0x41,
    VOUT_OV_WARN_LIMIT = 0x42,
    VOUT_UV_WARN_LIMIT = 0x43,
    VOUT_UV_FAULT_LIMIT = 0x44,
    VOUT_UV_FAULT_RESPONSE = 0x45,
    IOUT_OC_FAULT_LIMIT = 0x46,
    IOUT_OC_FAULT_RESPONSE = 0x47,
    IOUT_OC_LV_FAULT_LIMIT = 0x48,
    IOUT_OC_LV_FAULT_RESPONSE = 0x49,
    IOUT_OC_WARN_LIMIT = 0x4a,
    IOUT_UC_FAULT_LIMIT = 0x4b,
    IOUT_UC_FAULT_RESPONSE = 0x4c,
    OT_FAULT_LIMIT = 0x4f,
    OT_FAULT_RESPONSE = 0x50,
    OT_WARN_LIMIT = 0x51,
    UT_WARN_LIMIT = 0x52,
    UT_FAULT_LIMIT = 0x53,
    UT_FAULT_RESPONSE = 0x54,
    VIN_OV_FAULT_LIMIT = 0x55,
    VIN_OV_FAULT_RESPONSE = 0x56,
    VIN_OV_WARN_LIMIT = 0x57,
    VIN_UV_WARN_LIMIT = 0x58,
    VIN_UV_FAULT_LIMIT = 0x59,
    VIN_UV_FAULT_RESPONSE = 0x5a,
    IIN_OC_FAULT_LIMIT = 0x5b,
    IIN_OC_FAULT_RESPONSE = 0x5c,
    IIN_OC_WARN_LIMIT = 0x5d,
    POWER_GOOD_ON = 0x5e,
    POWER_GOOD_OFF = 0x5f,
    TON_DELAY = 0x60,
    TON_RISE = 0x61,
    TON_MAX_FAULT_LIMIT = 0x62,
    TON_MAX_FAULT_RESPONSE = 0x63,
    TOFF_DELAY = 0x64,
    TOFF_FALL = 0x65,
    TOFF_MAX_WARN_LIMIT = 0x66,
    Deprecated = 0x67,
    POUT_OP_FAULT_LIMIT = 0x68,
    POUT_OP_FAULT_RESPONSE = 0x69,
    POUT_OP_WARN_LIMIT = 0x6a,
    PIN_OP_WARN_LIMIT = 0x6b,
    STATUS_BYTE = 0x78,
    STATUS_WORD = 0x79,
    STATUS_VOUT = 0x7a,
    STATUS_IOUT = 0x7b,
    STATUS_INPUT = 0x7c,
    STATUS_TEMPERATURE = 0x7d,
    STATUS_CML = 0x7e,
    STATUS_OTHER = 0x7f,
    STATUS_MFR_SPECIFIC = 0x80,
    STATUS_FANS_1_2 = 0x81,
    STATUS_FANS_3_4 = 0x82,
    READ_KWH_IN = 0x83,
    READ_KWH_OUT = 0x84,
    READ_KWH_CONFIG = 0x85,
    READ_EIN = 0x86,
    READ_EOUT = 0x87,
    READ_VIN = 0x88,
    READ_IIN = 0x89,
    READ_VCAP = 0x8a,
    READ_VOUT = 0x8b,
    READ_IOUT = 0x8c,
    READ_TEMPERATURE_1 = 0x8d,
    READ_TEMPERATURE_2 = 0x8e,
    READ_TEMPERATURE_3 = 0x8f,
    READ_FAN_SPEED_1 = 0x90,
    READ_FAN_SPEED_2 = 0x91,
    READ_FAN_SPEED_3 = 0x92,
    READ_FAN_SPEED_4 = 0x93,
    READ_DUTY_CYCLE = 0x94,
    READ_FREQUENCY = 0x95,
    READ_POUT = 0x96,
    READ_PIN = 0x97,
    PMBUS_REVISION = 0x98,
    MFR_ID = 0x99,
    MFR_MODEL = 0x9a,
    MFR_REVISION = 0x9b,
    MFR_LOCATION = 0x9c,
    MFR_DATE = 0x9d,
    MFR_SERIAL = 0x9e,
    APP_PROFILE_SUPPORT = 0x9f,
    MFR_VIN_MIN = 0xa0,
    MFR_VIN_MAX = 0xa1,
    MFR_IIN_MAX = 0xa2,
    MFR_PIN_MAX = 0xa3,
    MFR_VOUT_MIN = 0xa4,
    MFR_VOUT_MAX = 0xa5,
    MFR_IOUT_MAX = 0xa6,
    MFR_POUT_MAX = 0xa7,
    MFR_TAMBIENT_MAX = 0xa8,
    MFR_TAMBIENT_MIN = 0xa9,
    MFR_EFFICIENCY_LL = 0xaa,
    MFR_EFFICIENCY_HL = 0xab,
    MFR_PIN_ACCURACY = 0xac,
    IC_DEVICE_ID = 0xad,
    IC_DEVICE_REV = 0xae,
    USER_DATA_00 = 0xb0,
    USER_DATA_01 = 0xb1,
    USER_DATA_02 = 0xb2,
    USER_DATA_03 = 0xb3,
    USER_DATA_04 = 0xb4,
    USER_DATA_05 = 0xb5,
    USER_DATA_06 = 0xb6,
    USER_DATA_07 = 0xb7,
    USER_DATA_08 = 0xb8,
    USER_DATA_09 = 0xb9,
    USER_DATA_10 = 0xba,
    USER_DATA_11 = 0xbb,
    USER_DATA_12 = 0xbc,
    USER_DATA_13 = 0xbd,
    USER_DATA_14 = 0xbe,
    USER_DATA_15 = 0xbf,
    MFR_MAX_TEMP_1 = 0xc0,
    MFR_MAX_TEMP_2 = 0xc1,
    MFR_MAX_TEMP_3 = 0xc2,
    MFR_SPECIFIC_C4 = 0xc4,
    MFR_SPECIFIC_C5 = 0xc5,
    MFR_SPECIFIC_C6 = 0xc6,
    MFR_SPECIFIC_C7 = 0xc7,
    MFR_SPECIFIC_C8 = 0xc8,
    MFR_SPECIFIC_C9 = 0xc9,
    MFR_SPECIFIC_CA = 0xca,
    MFR_SPECIFIC_CB = 0xcb,
    MFR_SPECIFIC_CC = 0xcc,
    MFR_SPECIFIC_CD = 0xcd,
    MFR_SPECIFIC_CE = 0xce,
    MFR_SPECIFIC_CF = 0xcf,
    MFR_SPECIFIC_D0 = 0xd0,
    MFR_SPECIFIC_D1 = 0xd1,
    MFR_SPECIFIC_D2 = 0xd2,
    MFR_SPECIFIC_D3 = 0xd3,
    MFR_SPECIFIC_D4 = 0xd4,
    MFR_SPECIFIC_D5 = 0xd5,
    MFR_SPECIFIC_D6 = 0xd6,
    MFR_SPECIFIC_D7 = 0xd7,
    MFR_SPECIFIC_D8 = 0xd8,
    MFR_SPECIFIC_D9 = 0xd9,
    MFR_SPECIFIC_DA = 0xda,
    MFR_SPECIFIC_DB = 0xdb,
    MFR_SPECIFIC_DC = 0xdc,
    MFR_SPECIFIC_DD = 0xdd,
    MFR_SPECIFIC_DE = 0xde,
    MFR_SPECIFIC_DF = 0xdf,
    MFR_SPECIFIC_E0 = 0xe0,
    MFR_SPECIFIC_E1 = 0xe1,
    MFR_SPECIFIC_E2 = 0xe2,
    MFR_SPECIFIC_E3 = 0xe3,
    MFR_SPECIFIC_E4 = 0xe4,
    MFR_SPECIFIC_E5 = 0xe5,
    MFR_SPECIFIC_E6 = 0xe6,
    MFR_SPECIFIC_E7 = 0xe7,
    MFR_SPECIFIC_E8 = 0xe8,
    MFR_SPECIFIC_E9 = 0xe9,
    MFR_SPECIFIC_EA = 0xea,
    MFR_SPECIFIC_EB = 0xeb,
    MFR_SPECIFIC_EC = 0xec,
    MFR_SPECIFIC_ED = 0xed,
    MFR_SPECIFIC_EE = 0xee,
    MFR_SPECIFIC_EF = 0xef,
    MFR_SPECIFIC_F0 = 0xf0,
    MFR_SPECIFIC_F1 = 0xf1,
    MFR_SPECIFIC_F2 = 0xf2,
    MFR_SPECIFIC_F3 = 0xf3,
    MFR_SPECIFIC_F4 = 0xf4,
    MFR_SPECIFIC_F5 = 0xf5,
    MFR_SPECIFIC_F6 = 0xf6,
    MFR_SPECIFIC_F7 = 0xf7,
    MFR_SPECIFIC_F8 = 0xf8,
    MFR_SPECIFIC_F9 = 0xf9,
    MFR_SPECIFIC_FA = 0xfa,
    MFR_SPECIFIC_FB = 0xfb,
    MFR_SPECIFIC_FC = 0xfc,
    MFR_SPECIFIC_FD = 0xfd,
    MFR_SPECIFIC_COMMAND_EXT = 0xfe,
    PMBUS_COMMAND_EXT = 0xff,
}

impl Command {
    pub fn read_op(&self) -> Operation {
        match self {
            Command::PAGE
            | Command::OPERATION
            | Command::ON_OFF_CONFIG
            | Command::PHASE
            | Command::WRITE_PROTECT
            | Command::CAPABILITY
            | Command::VOUT_MODE
            | Command::POWER_MODE
            | Command::FAN_CONFIG_1_2
            | Command::FAN_CONFIG_3_4
            | Command::VOUT_OV_FAULT_RESPONSE
            | Command::VOUT_UV_FAULT_RESPONSE
            | Command::IOUT_OC_FAULT_RESPONSE
            | Command::IOUT_OC_LV_FAULT_RESPONSE
            | Command::IOUT_UC_FAULT_RESPONSE
            | Command::OT_FAULT_RESPONSE
            | Command::UT_FAULT_RESPONSE
            | Command::VIN_OV_FAULT_RESPONSE
            | Command::VIN_UV_FAULT_RESPONSE
            | Command::IIN_OC_FAULT_RESPONSE
            | Command::TON_MAX_FAULT_RESPONSE
            | Command::POUT_OP_FAULT_RESPONSE
            | Command::STATUS_BYTE
            | Command::STATUS_VOUT
            | Command::STATUS_IOUT
            | Command::STATUS_INPUT
            | Command::STATUS_TEMPERATURE
            | Command::STATUS_CML
            | Command::STATUS_OTHER
            | Command::STATUS_MFR_SPECIFIC
            | Command::STATUS_FANS_1_2
            | Command::STATUS_FANS_3_4
            | Command::PMBUS_REVISION
            | Command::MFR_PIN_ACCURACY => Operation::ReadByte,

            Command::ZONE_CONFIG
            | Command::ZONE_ACTIVE
            | Command::VOUT_COMMAND
            | Command::VOUT_TRIM
            | Command::VOUT_CAL_OFFSET
            | Command::VOUT_MAX
            | Command::VOUT_MARGIN_HIGH
            | Command::VOUT_MARGIN_LOW
            | Command::VOUT_TRANSITION_RATE
            | Command::VOUT_DROOP
            | Command::VOUT_SCALE_LOOP
            | Command::VOUT_SCALE_MONITOR
            | Command::VOUT_MIN
            | Command::POUT_MAX
            | Command::MAX_DUTY
            | Command::FREQUENCY_SWITCH
            | Command::VIN_ON
            | Command::VIN_OFF
            | Command::INTERLEAVE
            | Command::IOUT_CAL_GAIN
            | Command::IOUT_CAL_OFFSET
            | Command::FAN_COMMAND_1
            | Command::FAN_COMMAND_2
            | Command::FAN_COMMAND_3
            | Command::FAN_COMMAND_4
            | Command::VOUT_OV_FAULT_LIMIT
            | Command::VOUT_OV_WARN_LIMIT
            | Command::VOUT_UV_WARN_LIMIT
            | Command::VOUT_UV_FAULT_LIMIT
            | Command::IOUT_OC_FAULT_LIMIT
            | Command::IOUT_OC_LV_FAULT_LIMIT
            | Command::IOUT_OC_WARN_LIMIT
            | Command::IOUT_UC_FAULT_LIMIT
            | Command::OT_FAULT_LIMIT
            | Command::OT_WARN_LIMIT
            | Command::UT_WARN_LIMIT
            | Command::UT_FAULT_LIMIT
            | Command::VIN_OV_FAULT_LIMIT
            | Command::VIN_OV_WARN_LIMIT
            | Command::VIN_UV_WARN_LIMIT
            | Command::VIN_UV_FAULT_LIMIT
            | Command::IIN_OC_FAULT_LIMIT
            | Command::IIN_OC_WARN_LIMIT
            | Command::POWER_GOOD_ON
            | Command::POWER_GOOD_OFF
            | Command::TON_DELAY
            | Command::TON_RISE
            | Command::TON_MAX_FAULT_LIMIT
            | Command::TOFF_DELAY
            | Command::TOFF_FALL
            | Command::TOFF_MAX_WARN_LIMIT
            | Command::POUT_OP_FAULT_LIMIT
            | Command::POUT_OP_WARN_LIMIT
            | Command::PIN_OP_WARN_LIMIT
            | Command::STATUS_WORD
            | Command::READ_KWH_CONFIG
            | Command::READ_VIN
            | Command::READ_IIN
            | Command::READ_VCAP
            | Command::READ_VOUT
            | Command::READ_IOUT
            | Command::READ_TEMPERATURE_1
            | Command::READ_TEMPERATURE_2
            | Command::READ_TEMPERATURE_3
            | Command::READ_FAN_SPEED_1
            | Command::READ_FAN_SPEED_2
            | Command::READ_FAN_SPEED_3
            | Command::READ_FAN_SPEED_4
            | Command::READ_DUTY_CYCLE
            | Command::READ_FREQUENCY
            | Command::READ_POUT
            | Command::READ_PIN
            | Command::MFR_VIN_MIN
            | Command::MFR_VIN_MAX
            | Command::MFR_IIN_MAX
            | Command::MFR_PIN_MAX
            | Command::MFR_VOUT_MIN
            | Command::MFR_VOUT_MAX
            | Command::MFR_IOUT_MAX
            | Command::MFR_POUT_MAX
            | Command::MFR_TAMBIENT_MAX
            | Command::MFR_TAMBIENT_MIN
            | Command::MFR_MAX_TEMP_1
            | Command::MFR_MAX_TEMP_2
            | Command::MFR_MAX_TEMP_3 => Operation::ReadWord,

            Command::PAGE_PLUS_READ
            | Command::QUERY
            | Command::SMBALERT_MASK
            | Command::COEFFICIENTS => Operation::ProcessCall,

            Command::READ_EIN
            | Command::READ_EOUT
            | Command::MFR_ID
            | Command::MFR_MODEL
            | Command::MFR_REVISION
            | Command::MFR_LOCATION
            | Command::MFR_DATE
            | Command::MFR_SERIAL
            | Command::APP_PROFILE_SUPPORT
            | Command::MFR_EFFICIENCY_LL
            | Command::MFR_EFFICIENCY_HL
            | Command::IC_DEVICE_ID
            | Command::IC_DEVICE_REV
            | Command::USER_DATA_00
            | Command::USER_DATA_01
            | Command::USER_DATA_02
            | Command::USER_DATA_03
            | Command::USER_DATA_04
            | Command::USER_DATA_05
            | Command::USER_DATA_06
            | Command::USER_DATA_07
            | Command::USER_DATA_08
            | Command::USER_DATA_09
            | Command::USER_DATA_10
            | Command::USER_DATA_11
            | Command::USER_DATA_12
            | Command::USER_DATA_13
            | Command::USER_DATA_14
            | Command::USER_DATA_15 => Operation::ReadBlock,

            Command::CLEAR_FAULTS
            | Command::PAGE_PLUS_WRITE
            | Command::STORE_DEFAULT_ALL
            | Command::RESTORE_DEFAULT_ALL
            | Command::STORE_DEFAULT_CODE
            | Command::RESTORE_DEFAULT_CODE
            | Command::STORE_USER_ALL
            | Command::RESTORE_USER_ALL
            | Command::STORE_USER_CODE
            | Command::RESTORE_USER_CODE => Operation::Illegal,

            Command::READ_KWH_IN | Command::READ_KWH_OUT => {
                Operation::ReadWord32
            }

            Command::MFR_SPECIFIC_C4
            | Command::MFR_SPECIFIC_C5
            | Command::MFR_SPECIFIC_C6
            | Command::MFR_SPECIFIC_C7
            | Command::MFR_SPECIFIC_C8
            | Command::MFR_SPECIFIC_C9
            | Command::MFR_SPECIFIC_CA
            | Command::MFR_SPECIFIC_CB
            | Command::MFR_SPECIFIC_CC
            | Command::MFR_SPECIFIC_CD
            | Command::MFR_SPECIFIC_CE
            | Command::MFR_SPECIFIC_CF
            | Command::MFR_SPECIFIC_D0
            | Command::MFR_SPECIFIC_D1
            | Command::MFR_SPECIFIC_D2
            | Command::MFR_SPECIFIC_D3
            | Command::MFR_SPECIFIC_D4
            | Command::MFR_SPECIFIC_D5
            | Command::MFR_SPECIFIC_D6
            | Command::MFR_SPECIFIC_D7
            | Command::MFR_SPECIFIC_D8
            | Command::MFR_SPECIFIC_D9
            | Command::MFR_SPECIFIC_DA
            | Command::MFR_SPECIFIC_DB
            | Command::MFR_SPECIFIC_DC
            | Command::MFR_SPECIFIC_DD
            | Command::MFR_SPECIFIC_DE
            | Command::MFR_SPECIFIC_DF
            | Command::MFR_SPECIFIC_E0
            | Command::MFR_SPECIFIC_E1
            | Command::MFR_SPECIFIC_E2
            | Command::MFR_SPECIFIC_E3
            | Command::MFR_SPECIFIC_E4
            | Command::MFR_SPECIFIC_E5
            | Command::MFR_SPECIFIC_E6
            | Command::MFR_SPECIFIC_E7
            | Command::MFR_SPECIFIC_E8
            | Command::MFR_SPECIFIC_E9
            | Command::MFR_SPECIFIC_EA
            | Command::MFR_SPECIFIC_EB
            | Command::MFR_SPECIFIC_EC
            | Command::MFR_SPECIFIC_ED
            | Command::MFR_SPECIFIC_EE
            | Command::MFR_SPECIFIC_EF
            | Command::MFR_SPECIFIC_F0
            | Command::MFR_SPECIFIC_F1
            | Command::MFR_SPECIFIC_F2
            | Command::MFR_SPECIFIC_F3
            | Command::MFR_SPECIFIC_F4
            | Command::MFR_SPECIFIC_F5
            | Command::MFR_SPECIFIC_F6
            | Command::MFR_SPECIFIC_F7
            | Command::MFR_SPECIFIC_F8
            | Command::MFR_SPECIFIC_F9
            | Command::MFR_SPECIFIC_FA
            | Command::MFR_SPECIFIC_FB
            | Command::MFR_SPECIFIC_FC
            | Command::MFR_SPECIFIC_FD => Operation::MfrDefined,

            Command::MFR_SPECIFIC_COMMAND_EXT | Command::PMBUS_COMMAND_EXT => {
                Operation::Extended
            }

            Command::Deprecated => Operation::Unknown,
        }
    }

    pub fn write_op(&self) -> Operation {
        match self {
            Command::PAGE
            | Command::OPERATION
            | Command::ON_OFF_CONFIG
            | Command::PHASE
            | Command::WRITE_PROTECT
            | Command::STORE_DEFAULT_CODE
            | Command::RESTORE_DEFAULT_CODE
            | Command::STORE_USER_CODE
            | Command::RESTORE_USER_CODE
            | Command::VOUT_MODE
            | Command::POWER_MODE
            | Command::FAN_CONFIG_1_2
            | Command::FAN_CONFIG_3_4
            | Command::VOUT_OV_FAULT_RESPONSE
            | Command::VOUT_UV_FAULT_RESPONSE
            | Command::IOUT_OC_FAULT_RESPONSE
            | Command::IOUT_OC_LV_FAULT_RESPONSE
            | Command::IOUT_UC_FAULT_RESPONSE
            | Command::OT_FAULT_RESPONSE
            | Command::UT_FAULT_RESPONSE
            | Command::VIN_OV_FAULT_RESPONSE
            | Command::VIN_UV_FAULT_RESPONSE
            | Command::IIN_OC_FAULT_RESPONSE
            | Command::TON_MAX_FAULT_RESPONSE
            | Command::POUT_OP_FAULT_RESPONSE
            | Command::STATUS_BYTE
            | Command::STATUS_VOUT
            | Command::STATUS_IOUT
            | Command::STATUS_INPUT
            | Command::STATUS_TEMPERATURE
            | Command::STATUS_CML
            | Command::STATUS_OTHER
            | Command::STATUS_MFR_SPECIFIC
            | Command::STATUS_FANS_1_2
            | Command::STATUS_FANS_3_4 => Operation::WriteByte,

            Command::ZONE_CONFIG
            | Command::ZONE_ACTIVE
            | Command::SMBALERT_MASK
            | Command::VOUT_COMMAND
            | Command::VOUT_TRIM
            | Command::VOUT_CAL_OFFSET
            | Command::VOUT_MAX
            | Command::VOUT_MARGIN_HIGH
            | Command::VOUT_MARGIN_LOW
            | Command::VOUT_TRANSITION_RATE
            | Command::VOUT_DROOP
            | Command::VOUT_SCALE_LOOP
            | Command::VOUT_SCALE_MONITOR
            | Command::VOUT_MIN
            | Command::POUT_MAX
            | Command::MAX_DUTY
            | Command::FREQUENCY_SWITCH
            | Command::VIN_ON
            | Command::VIN_OFF
            | Command::INTERLEAVE
            | Command::IOUT_CAL_GAIN
            | Command::IOUT_CAL_OFFSET
            | Command::FAN_COMMAND_1
            | Command::FAN_COMMAND_2
            | Command::FAN_COMMAND_3
            | Command::FAN_COMMAND_4
            | Command::VOUT_OV_FAULT_LIMIT
            | Command::VOUT_OV_WARN_LIMIT
            | Command::VOUT_UV_WARN_LIMIT
            | Command::VOUT_UV_FAULT_LIMIT
            | Command::IOUT_OC_FAULT_LIMIT
            | Command::IOUT_OC_LV_FAULT_LIMIT
            | Command::IOUT_OC_WARN_LIMIT
            | Command::IOUT_UC_FAULT_LIMIT
            | Command::OT_FAULT_LIMIT
            | Command::OT_WARN_LIMIT
            | Command::UT_WARN_LIMIT
            | Command::UT_FAULT_LIMIT
            | Command::VIN_OV_FAULT_LIMIT
            | Command::VIN_OV_WARN_LIMIT
            | Command::VIN_UV_WARN_LIMIT
            | Command::VIN_UV_FAULT_LIMIT
            | Command::IIN_OC_FAULT_LIMIT
            | Command::IIN_OC_WARN_LIMIT
            | Command::POWER_GOOD_ON
            | Command::POWER_GOOD_OFF
            | Command::TON_DELAY
            | Command::TON_RISE
            | Command::TON_MAX_FAULT_LIMIT
            | Command::TOFF_DELAY
            | Command::TOFF_FALL
            | Command::TOFF_MAX_WARN_LIMIT
            | Command::POUT_OP_FAULT_LIMIT
            | Command::POUT_OP_WARN_LIMIT
            | Command::PIN_OP_WARN_LIMIT
            | Command::STATUS_WORD
            | Command::READ_KWH_CONFIG
            | Command::MFR_MAX_TEMP_1
            | Command::MFR_MAX_TEMP_2
            | Command::MFR_MAX_TEMP_3 => Operation::WriteWord,

            Command::PAGE_PLUS_WRITE
            | Command::MFR_ID
            | Command::MFR_MODEL
            | Command::MFR_REVISION
            | Command::MFR_LOCATION
            | Command::MFR_DATE
            | Command::MFR_SERIAL
            | Command::USER_DATA_00
            | Command::USER_DATA_01
            | Command::USER_DATA_02
            | Command::USER_DATA_03
            | Command::USER_DATA_04
            | Command::USER_DATA_05
            | Command::USER_DATA_06
            | Command::USER_DATA_07
            | Command::USER_DATA_08
            | Command::USER_DATA_09
            | Command::USER_DATA_10
            | Command::USER_DATA_11
            | Command::USER_DATA_12
            | Command::USER_DATA_13
            | Command::USER_DATA_14
            | Command::USER_DATA_15 => Operation::WriteBlock,

            Command::CLEAR_FAULTS
            | Command::STORE_DEFAULT_ALL
            | Command::RESTORE_DEFAULT_ALL
            | Command::STORE_USER_ALL
            | Command::RESTORE_USER_ALL => Operation::SendByte,

            Command::PAGE_PLUS_READ
            | Command::CAPABILITY
            | Command::QUERY
            | Command::COEFFICIENTS
            | Command::READ_KWH_IN
            | Command::READ_KWH_OUT
            | Command::READ_EIN
            | Command::READ_EOUT
            | Command::READ_VIN
            | Command::READ_IIN
            | Command::READ_VCAP
            | Command::READ_VOUT
            | Command::READ_IOUT
            | Command::READ_TEMPERATURE_1
            | Command::READ_TEMPERATURE_2
            | Command::READ_TEMPERATURE_3
            | Command::READ_FAN_SPEED_1
            | Command::READ_FAN_SPEED_2
            | Command::READ_FAN_SPEED_3
            | Command::READ_FAN_SPEED_4
            | Command::READ_DUTY_CYCLE
            | Command::READ_FREQUENCY
            | Command::READ_POUT
            | Command::READ_PIN
            | Command::PMBUS_REVISION
            | Command::APP_PROFILE_SUPPORT
            | Command::MFR_VIN_MIN
            | Command::MFR_VIN_MAX
            | Command::MFR_IIN_MAX
            | Command::MFR_PIN_MAX
            | Command::MFR_VOUT_MIN
            | Command::MFR_VOUT_MAX
            | Command::MFR_IOUT_MAX
            | Command::MFR_POUT_MAX
            | Command::MFR_TAMBIENT_MAX
            | Command::MFR_TAMBIENT_MIN
            | Command::MFR_EFFICIENCY_LL
            | Command::MFR_EFFICIENCY_HL
            | Command::MFR_PIN_ACCURACY
            | Command::IC_DEVICE_ID
            | Command::IC_DEVICE_REV => Operation::Illegal,

            Command::MFR_SPECIFIC_C4
            | Command::MFR_SPECIFIC_C5
            | Command::MFR_SPECIFIC_C6
            | Command::MFR_SPECIFIC_C7
            | Command::MFR_SPECIFIC_C8
            | Command::MFR_SPECIFIC_C9
            | Command::MFR_SPECIFIC_CA
            | Command::MFR_SPECIFIC_CB
            | Command::MFR_SPECIFIC_CC
            | Command::MFR_SPECIFIC_CD
            | Command::MFR_SPECIFIC_CE
            | Command::MFR_SPECIFIC_CF
            | Command::MFR_SPECIFIC_D0
            | Command::MFR_SPECIFIC_D1
            | Command::MFR_SPECIFIC_D2
            | Command::MFR_SPECIFIC_D3
            | Command::MFR_SPECIFIC_D4
            | Command::MFR_SPECIFIC_D5
            | Command::MFR_SPECIFIC_D6
            | Command::MFR_SPECIFIC_D7
            | Command::MFR_SPECIFIC_D8
            | Command::MFR_SPECIFIC_D9
            | Command::MFR_SPECIFIC_DA
            | Command::MFR_SPECIFIC_DB
            | Command::MFR_SPECIFIC_DC
            | Command::MFR_SPECIFIC_DD
            | Command::MFR_SPECIFIC_DE
            | Command::MFR_SPECIFIC_DF
            | Command::MFR_SPECIFIC_E0
            | Command::MFR_SPECIFIC_E1
            | Command::MFR_SPECIFIC_E2
            | Command::MFR_SPECIFIC_E3
            | Command::MFR_SPECIFIC_E4
            | Command::MFR_SPECIFIC_E5
            | Command::MFR_SPECIFIC_E6
            | Command::MFR_SPECIFIC_E7
            | Command::MFR_SPECIFIC_E8
            | Command::MFR_SPECIFIC_E9
            | Command::MFR_SPECIFIC_EA
            | Command::MFR_SPECIFIC_EB
            | Command::MFR_SPECIFIC_EC
            | Command::MFR_SPECIFIC_ED
            | Command::MFR_SPECIFIC_EE
            | Command::MFR_SPECIFIC_EF
            | Command::MFR_SPECIFIC_F0
            | Command::MFR_SPECIFIC_F1
            | Command::MFR_SPECIFIC_F2
            | Command::MFR_SPECIFIC_F3
            | Command::MFR_SPECIFIC_F4
            | Command::MFR_SPECIFIC_F5
            | Command::MFR_SPECIFIC_F6
            | Command::MFR_SPECIFIC_F7
            | Command::MFR_SPECIFIC_F8
            | Command::MFR_SPECIFIC_F9
            | Command::MFR_SPECIFIC_FA
            | Command::MFR_SPECIFIC_FB
            | Command::MFR_SPECIFIC_FC
            | Command::MFR_SPECIFIC_FD => Operation::MfrDefined,

            Command::MFR_SPECIFIC_COMMAND_EXT | Command::PMBUS_COMMAND_EXT => {
                Operation::Extended
            }

            Command::Deprecated => Operation::Unknown,
        }
    }

    pub fn data(
        &self,
        payload: &[u8],
        iter: impl Fn(&dyn Field, &dyn Value)
    ) -> Result<(), Error> {
        match self {
            Command::OPERATION => {
                use commands::OPERATION::CommandData;
                if let Some(data) = CommandData::from_slice(payload) {
                    data.fields(iter);
                    Ok(())
                } else {
                    Err(Error::ShortData)
                }
            }
            _ => {
                Ok(())
            }
        }
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
        let y: f32 = self.0.into();

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

#[derive(Copy, Clone, Debug)]
pub enum VOutMode {
    ULinear16(ULinear16Exponent),
    VID(u8),
    Direct,
    HalfPrecision,
}

impl From<u8> for VOutMode {
    fn from(mode: u8) -> Self {
        match (mode >> 5) & 0b11 {
            0b00 => {
                let exp = ((mode << 3) as i8) >> 3;
                VOutMode::ULinear16(ULinear16Exponent(exp))
            }
            0b01 => {
                let code = mode & 0x1f;
                VOutMode::VID(code)
            }
            0b10 => VOutMode::Direct,
            0b11 => VOutMode::HalfPrecision,
            _ => unreachable!(),
        }
    }
}

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
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate std;

    #[test]
    fn verify_cmds() {
        macro_rules! verify {
            ($val:expr, $cmd:tt, $write:tt, $read:tt) => {
                assert_eq!(Command::$cmd as u8, $val);
                assert_eq!(Command::$cmd.write_op(), Operation::$write);
                assert_eq!(Command::$cmd.read_op(), Operation::$read);
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
        std::println!("{:?}", Command::from_u8(0x9));
    }

    #[test]
    fn verify_operation() {
        let data = commands::OPERATION::CommandData(0x4);

        data.fields(|field, value| {
            std::println!("{} = {}", field.name(), value);
        });
    }

    #[test]
    fn verify_operation_set() {
        use commands::OPERATION::*;
        let mut data = CommandData(0x4);

        std::println!("");
        dump(&data);

        assert_ne!(
            data.get_voltage_command_source(),
            Some(VoltageCommandSource::VOUT_MARGIN_HIGH)
        );

        data.set_voltage_command_source(VoltageCommandSource::VOUT_MARGIN_HIGH);

        std::println!("");
        dump(&data);

        assert_eq!(
            data.get_voltage_command_source(),
            Some(VoltageCommandSource::VOUT_MARGIN_HIGH)
        );
    }

    #[test]
    fn raw_operation() {
        Command::OPERATION.data(&[0x4], |field, value| {
            std::println!("{} = {}", field.name(), value);
        }).unwrap();
    }

    fn dump(data: &impl commands::CommandData) {
        let (val, width) = data.raw();
        let width = width.0 as usize;
        let nibble = 4;
        let maxwidth = 16;
        let indent = (maxwidth - width) + ((maxwidth - width) / nibble);

        let mut v = std::vec![];

        data.fields(|field, value| {
            v.push((field.bits(), field.name(), std::format!("{}", value)));
        });

        v.reverse();

        std::print!("\n{:indent$}", "", indent = indent);
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
                while cur > v[i].0.0.0 as usize {
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
    
    #[test]
    fn verify_status_word() {
        use commands::STATUS_WORD::*;

        let data = CommandData::from_slice(&[0x43, 0x18]).unwrap();
        dump(&data);

        data.fields(|field, value| {
            std::println!("{} = {}", field.name(), value);
        });
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

}
