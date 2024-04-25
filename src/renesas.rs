//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
///
/// Renesas-specific functionality.
///
// For purposes of our implementation, we use the auxiliary structures as
// in the ISL68224 definition
//
use crate::commands::isl68224::*;

///
/// Structure that defines per-rail blackbox information.  There are three
/// of these present in each blackbox entry.
///
pub struct BlackboxRail {
    pub uptime: UptimeCounter::CommandData,
    pub first_fault: RailFault::CommandData,
    pub status: STATUS_WORD::CommandData,
    pub vout_status: STATUS_VOUT::CommandData,
    pub iout_status: STATUS_IOUT::CommandData,
    pub temp_status: STATUS_TEMPERATURE::CommandData,
    pub input_status: STATUS_INPUT::CommandData,
    pub vin: READ_VIN::CommandData,
    pub vout: READ_VOUT::CommandData,
    pub iin: READ_IIN::CommandData,
    pub iout: READ_IOUT::CommandData,
}

///
/// Enum for a rail index in the blackbox recording.  Even though not
/// every Renesas part supports it, the blackbox records have room for
/// three rails.
///
enum RailIndex {
    Rail0,
    Rail1,
    Rail2,
}

macro_rules! bb_field {
    ($slice:expr, $cmd:tt, $word:expr, $offs:expr) => {
        $cmd::CommandData::from_slice(&$slice[($word * 4) + $offs..]).unwrap()
    };
}

impl BlackboxRail {
    fn from_slice(buf: &[u8], rail: RailIndex) -> Self {
        match rail {
            RailIndex::Rail0 => BlackboxRail {
                uptime: bb_field!(buf, UptimeCounter, 1, 0),
                first_fault: bb_field!(buf, RailFault, 5, 0),
                status: bb_field!(buf, STATUS_WORD, 11, 0),
                vout_status: bb_field!(buf, STATUS_VOUT, 13, 1),
                iout_status: bb_field!(buf, STATUS_IOUT, 14, 2),
                temp_status: bb_field!(buf, STATUS_TEMPERATURE, 15, 3),
                input_status: bb_field!(buf, STATUS_INPUT, 15, 3),
                vin: bb_field!(buf, READ_VIN, 16, 0),
                vout: bb_field!(buf, READ_VOUT, 18, 2),
                iin: bb_field!(buf, READ_IIN, 19, 0),
                iout: bb_field!(buf, READ_IOUT, 21, 2),
            },
            RailIndex::Rail1 => BlackboxRail {
                uptime: bb_field!(buf, UptimeCounter, 2, 0),
                first_fault: bb_field!(buf, RailFault, 6, 0),
                status: bb_field!(buf, STATUS_WORD, 12, 2),
                vout_status: bb_field!(buf, STATUS_VOUT, 13, 0),
                iout_status: bb_field!(buf, STATUS_IOUT, 14, 1),
                temp_status: bb_field!(buf, STATUS_TEMPERATURE, 15, 2),
                input_status: bb_field!(buf, STATUS_INPUT, 16, 3),
                vin: bb_field!(buf, READ_VIN, 17, 2),
                vout: bb_field!(buf, READ_VOUT, 18, 0),
                iin: bb_field!(buf, READ_IIN, 20, 2),
                iout: bb_field!(buf, READ_IOUT, 21, 0),
            },
            RailIndex::Rail2 => BlackboxRail {
                uptime: bb_field!(buf, UptimeCounter, 3, 0),
                first_fault: bb_field!(buf, RailFault, 7, 0),
                status: bb_field!(buf, STATUS_WORD, 12, 0),
                vout_status: bb_field!(buf, STATUS_VOUT, 14, 3),
                iout_status: bb_field!(buf, STATUS_IOUT, 14, 0),
                temp_status: bb_field!(buf, STATUS_TEMPERATURE, 15, 1),
                input_status: bb_field!(buf, STATUS_INPUT, 16, 2),
                vin: bb_field!(buf, READ_VIN, 17, 0),
                vout: bb_field!(buf, READ_VOUT, 19, 2),
                iin: bb_field!(buf, READ_IIN, 20, 0),
                iout: bb_field!(buf, READ_IOUT, 22, 2),
            },
        }
    }
}

///
/// A blackbox entry.  The Gen2 parts will record one of these in RAM, and
/// up to 10 in non-volatile memory.
///
pub struct Blackbox {
    pub controller_first_fault: ControllerFault::CommandData,
    pub cml_status: STATUS_CML::CommandData,
    pub mfr_specific: STATUS_MFR_SPECIFIC::CommandData,
    pub rails: [BlackboxRail; 3],
}

impl Blackbox {
    pub fn from_slice(buf: &[u8]) -> Self {
        Self {
            controller_first_fault: bb_field!(buf, ControllerFault, 4, 0),
            cml_status: bb_field!(buf, STATUS_CML, 13, 3),
            mfr_specific: bb_field!(buf, STATUS_MFR_SPECIFIC, 13, 2),
            rails: [
                BlackboxRail::from_slice(buf, RailIndex::Rail0),
                BlackboxRail::from_slice(buf, RailIndex::Rail1),
                BlackboxRail::from_slice(buf, RailIndex::Rail2),
            ],
        }
    }
}

///
/// The Gen2 multiphase parts have a `DMAADDR` interface for reading memory.
/// This interfaces takes a 32-bit word offset into memory rather than an
/// absolute address.  We therefore two types: [`DMAAddress`] represents the
/// value to sent to the `DMAADDR` command, where [`Address`] represents an
/// absolute address.
///
pub struct Address(pub u32);
pub struct DMAAddress(pub u16);

/// The configuration ID, as WW.XX.YY.ZZ
pub const DMAADDR_CONFIG_ID: DMAAddress = DMAAddress(0x00c1);

/// Firmware revision, as WW.XX.YY.ZZ. Should match result of IC_DEVICE_ID.
pub const DMAADDR_FIRMWARE_REV: DMAAddress = DMAAddress(0x00c3);

/// Address of blackbox in RAM. Note that this is an [`Address`], not a
/// a [`DMAAddress`]; it will need to be converted before being read.
pub const DMAADDR_BLACKBOX_ADDR: DMAAddress = DMAAddress(0x00c5);

pub const DMAADDR_READ_CONTROL: DMAAddress = DMAAddress(0x0069);
pub const DMAADDR_READ_LOWER: DMAAddress = DMAAddress(0x006a);
pub const DMAADDR_READ_UPPER: DMAAddress = DMAAddress(0x006b);

impl DMAAddress {}
