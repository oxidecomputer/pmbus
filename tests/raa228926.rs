//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
use commands::raa228926::*;
use pmbus::*;
use units::*;
mod common;
use common::*;

#[test]
fn raa228926_defaults() {
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
