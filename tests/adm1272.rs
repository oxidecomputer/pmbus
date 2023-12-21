//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
use commands::adm1272::*;
use pmbus::*;
mod common;
use common::*;
use units::*;

#[test]
fn adm1272_verify_status() {
    use commands::adm1272::STATUS_MFR_SPECIFIC::*;
    let data = CommandData::from_slice(&[0x40]).unwrap();
    dump(&data);
}

#[test]
fn adm1272_direct() {
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
