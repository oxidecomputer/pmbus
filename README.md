# pmbus

pmbus: A crate for PMBus manipulation

This is a no_std crate that expresses the PMBus protocol, as described in
the PMBus 1.3 specification.  This crate is intended to be generic with
respect to implementation and usable by software that will directly
communicate with PMBus devices via SMBus/I2C as well as by software that
merely wishes to make sense of the PMBus protocol (e.g., debuggers or
analyzers running on a host).  For PMBus, this can be a bit of a
challenge, as much of the definition is left up to a particular device
(that is, much is implementation-defined).  Our two use cases are
therefore divergent in their needs:

1. The embedded system that is speaking to a particular PMBus device *in
   situ* is likely to know (and want to use) the special capabilities of a
   given device.  That is, these use cases know their target device at
   compile time, and have no need or desire to dynamically discover their
   device capabilities.

2. The host-based system that is trying to make sense of PMBus is *not*
   necessarily going to know the specifics of the attached devices at
   compile time; it is going to want to allow the device to be specified
   (or otherwise dynamically determined) and then discover that device's
   capabilities dynamically -- even if only to pass those capabilities on
   to the user.

These use cases are in tension:  we want the first to be tight and
typesafe while still allowing for the more dynamic second use case.  We
balance these two cases by dynamically compiling the crate based on
per-device RON files that specify the commands and their corresponding
destructured data; each device is in its own module, with each PMBus
command further having its own module that contains the types for the
corresponding command data.

As a concrete example, `commands::OPERATION` contains an implementation
of the `CommandData` trait for the fields for the common PMBus
`OPERATION` command.  For each device, there is a device-specific
`OPERATION` module -- e.g.  `[commands::adm1272::OPERATION]` -- that may
extend or override the common definition.  Further, the device may define
its own constants; for example, while PMBus defines the command code
`0xd4` to be `CommandCode::MFR_SPECIFIC_D4`, the ADM1272 defines this to
be `PMON_CONFIG`, a device-specific power monitor configuration register.
There therefore exists a `commands::adm1272::PMON_CONFIG` module that
understands the full (ADM1272-specific) functionality.  For code that
wishes to be device agnostic but still be able to display contents, there
exists a `Device::interpret` that given a device, a code, and a payload,
calls the specified closure to iterate over fields and values.

A final (crucial) constraint is that this crate remains `no_std`; it
performs no dynamic allocation and in general relies on program text
rather than table lookups -- with the knowledge that the compiler is very
good about dead code elimination and will not include unused program text
in the embedded system.

If it needs to be said:  all of this adds up to specifications almost
entirely via RON definitions -- and an absolutely unholy `build.rs` to
assemble it all at build time.  Paraphrasing [the late Roger
Faulker](https://www.usenix.org/memoriam-roger-faulkner),
terrible things are sometimes required for beautiful abstractions.


License: MPL-2.0
