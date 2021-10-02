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
