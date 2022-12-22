//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
use commands::mwocp68::*;
use pmbus::*;

#[test]
fn status_mfr_specific() {
    use STATUS_MFR_SPECIFIC::*;
    let status = CommandData::from_slice(&[0x18]).unwrap();
    assert_eq!(
        status.get_current_sensor_read_error(),
        Some(CurrentSensorReadError::Error)
    );
    assert_eq!(
        status.get_temperature_sensor_read_error(),
        Some(TemperatureSensorReadError::Error)
    );
}
