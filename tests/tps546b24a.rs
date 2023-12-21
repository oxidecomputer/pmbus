//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
use commands::tps546b24a::*;
use pmbus::*;
use units::*;
mod common;
use common::*;

#[test]
fn tps546b24a_read_all() {
    let data = READ_ALL::CommandData::from_slice(&[
        0x02, 0x00, 0x63, 0x02, 0xee, 0xad, 0xd8, 0xdb, 0xfe, 0xd2, 0x00, 0x00,
        0x00, 0x00,
    ])
    .unwrap();

    dump(&data);
    assert_eq!(data.get_read_vin(), 0xd2fe);
    assert_eq!(data.get_read_vout(), 0x0263);
    assert_eq!(data.get_status_word(), 0x0002);
    assert_eq!(data.get_read_temperature_1(), 0xdbd8);
}

#[test]
fn tps546b24a_read_all_data() {
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
fn tps546b24a_passthrough() {
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
