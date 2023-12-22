//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
use commands::bmr480::*;
use pmbus::*;
mod common;
use common::*;

#[test]
fn bmr480_default() {
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
fn bmr480_iout() {
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
