//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
pub use crate::operation::Operation;

use crate::Bitpos;
use crate::Command;
use crate::CommandData;
use crate::Field;
use crate::Replacement;
use crate::VOutModeCommandData;
use crate::Value;

//
// This includes all of our common commands and their data payloads.
//
include!(concat!(env!("OUT_DIR"), "/commands.rs"));

//
// This includes all device-specifics commands and their data payloads.
//
include!(concat!(env!("OUT_DIR"), "/devices.rs"));
