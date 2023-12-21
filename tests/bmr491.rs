//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
use commands::bmr491::*;
use pmbus::*;
use units::*;
mod common;
use common::*;

#[test]
fn bmr491_default() {
    use commands::bmr491::*;

    validate!(MFR_FAST_VIN_OFF_OFFSET, [0x20], 0.5, Volts);
    validate!(MFR_IOUT_CAL_GAIN, [0x32, 0xb7], 44.7254, MilliampsPerLSB);

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
    let data = MFR_PGOOD_POLARITY::CommandData::from_slice(&[0x01]);
    dump(&data.unwrap());
}

#[test]
fn bmr491_temp_compensation() {
    let data = [0x28, 0x33, 0xe2, 0xe4, 0x1e, 0xf6, 0xf9, 0x0f];
    let temp = MFR_TEMP_COMPENSATION::CommandData::from_slice(&data).unwrap();

    assert_eq!(temp.get_edac_slope(), MillivoltsPerCelsius(0.3112793));

    assert_eq!(temp.get_temp_threshold_1(), Celsius(-30.0));
    assert_eq!(temp.get_temp_threshold_1_hysteresis(), Celsius(-28.0));
    assert_eq!(temp.get_temp_threshold_1_deadtime(), Nanoseconds(30.0));

    assert_eq!(temp.get_temp_threshold_2(), Celsius(-10.0));
    assert_eq!(temp.get_temp_threshold_2_hysteresis(), Celsius(-7.0));
    assert_eq!(temp.get_temp_threshold_2_deadtime(), Nanoseconds(15.0));

    let mut compare = data.clone();
    temp.to_slice(&mut compare);
    assert_eq!(data, compare);

    dump(&temp);
}

#[test]
fn bmr491_vin_monitor() {
    let data = [0x52, 0x07, 0x52, 0x6a];
    let vin = MFR_VIN_SCALE_MONITOR::CommandData::from_slice(&data).unwrap();
    dump(&vin);

    assert_eq!(vin.get_idle_factor(), MillivoltsPerLSB(20.026207));
    assert_eq!(vin.get_operating_factor(), MillivoltsPerLSB(20.120619));

    let mut compare = data.clone();
    vin.to_slice(&mut compare);
    assert_eq!(data, compare);
}

#[test]
fn bmr491_rc_level() {
    use commands::bmr491::*;
    let rc = MFR_RC_LEVEL::CommandData::from_slice(&[0xc8]).unwrap();
    assert_eq!(rc.get(), Ok(units::Volts(1.953125)));
}

#[test]
fn bmr491_ks_pretrig() {
    use commands::bmr491::*;
    let ks = MFR_KS_PRETRIG::CommandData::from_slice(&[0x89]).unwrap();
    assert_eq!(ks.get(), Ok(units::Microseconds(61.649998)));
}

#[test]
fn bmr491_vin_offset() {
    use commands::bmr491::*;
    let data = [0x00, 0x04, 0x00, 0x09];
    let offset = MFR_VIN_OFFSET::CommandData::from_slice(&data).unwrap();
    dump(&offset);

    assert_eq!(offset.get_v_in_offset_on(), Volts(0.5));

    let mut compare = data.clone();
    offset.to_slice(&mut compare);
    assert_eq!(data, compare);
}
