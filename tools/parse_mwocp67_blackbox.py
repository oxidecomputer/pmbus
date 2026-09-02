#!/usr/bin/env python3
"""Parse an MWOCP67 PSU MFR_BLACKBOX (0xD5) record.

A full black box record is 300 bytes (150 x 16-bit words). It is read out in
two halves via two consecutive SMBus block reads of MFR_BLACKBOX. Per ACAN-157
each read returns:

    Block Count (== 151) + File Offset (0 or 1) + 150 bytes of black box data

The first read carries File Offset 0 (words 0..74) and the second File Offset 1
(words 75..149). Reading again just repeats the two halves.

This script takes the raw bytes of the two reads, stitches them back into the
300-byte record, verifies the trailing checksum, and decodes every field
according to the record layout / encodings in ACAN-157 section 12.

Encoding note: fields are stored as "Unsigned N=x" / "Signed N=x", a fixed-point
format where the real value = raw / 2**N (raw is a 16-bit int, two's complement
when signed). N=0 therefore means a plain integer.

Word order for multi-word (32-bit) fields and byte order within a word are both
little-endian, matching PMBus convention.
"""

from __future__ import annotations

import sys

WORDS_PER_RECORD = 150
WORDS_PER_HALF = 75    # words 0..74 in half 0, words 75..149 in half 1
BYTES_PER_HALF = 150   # data bytes per half (2 x WORDS_PER_HALF)
BLOCK_COUNT = 151      # File Offset byte + 150 data bytes

# Bit -> label maps for the five bit-mapped alarm status words (words 103-107).
# These mirror the verified decodes in src/mwocp67.ron.
GENERAL_ALARM = {
    11: "Fan Alarm", 10: "Temp Alarm", 9: "DC/DC converter failure",
    8: "PFC Converter Fail", 3: "Communication", 2: "Temperature",
    1: "DCDC", 0: "PFC",
}
PFC_ALARM = {
    11: "PFC Fail", 10: "Input Relay Off", 9: "Bulk Not Ok", 8: "AC Not OK",
    5: "Freq high", 4: "Freq low", 1: "AC OVP", 0: "AC UVP",
}
DCDC_ALARM = {
    10: "Oring Fail", 9: "secondary MCU fail", 8: "DC/DC fail",
    3: "MAIN_SCKT", 2: "MAIN_OCP", 1: "MAIN_OVP", 0: "MAIN_UVP",
}
TEMP_ALARM = {
    8: "Fan Failure", 5: "PFC temp alarm", 4: "LLC temp alarm",
    3: "Sync temp alarm", 2: "Oring temp alarm", 1: "Inlet temp alarm",
    0: "Outlet temp alarm",
}
COMM_ALARM = {
    1: "Secondary - Logic MCU Fault", 0: "Primary - Secondary MCU Fault",
}

# Record layout from ACAN-157 12.3. Each entry:
#   (start_word, end_word, name, form, signed, N, unit)
# form is one of:
#   "samples" - array of end-start+1 sample values (each raw/2**N)
#   "u16"     - single 16-bit scalar (raw/2**N)
#   "u32"     - single 32-bit scalar spanning two words, low word first
#   "reserved"- ignored
#   "hex"     - show raw hex (bit-mapped/undocumented)
#   "bitmap:X"- decode set bits using the named alarm map
#   "checksum"- trailing checksum word
FIELDS = [
    (0,   19,  "ac_input_voltage_rectified",   "samples", False, 6,  "V"),
    (20,  39,  "ac_input_current_rectified",   "samples", False, 10, "A"),
    (40,  59,  "main_output_voltage",          "samples", False, 10, "V"),
    (60,  79,  "main_output_current",          "samples", False, 6,  "A"),
    (80,  84,  "temp0_inlet",                  "samples", True,  7,  "C"),
    (85,  89,  "temp1_outlet",                 "samples", True,  7,  "C"),
    (90,  94,  "rpm_fan0",                     "samples", False, 0,  "rpm"),
    (95,  96,  "total_up_time",                "u32",     False, 0,  "s"),
    (97,  98,  "time_since_last_on",           "u32",     False, 0,  "s"),
    (99,  99,  "ac_power_cycle_counter",       "u16",     False, 0,  ""),
    (100, 100, "ac_outage_counter",            "u16",     False, 0,  ""),
    (101, 102, "reserved_101_102",             "reserved",False, 0,  ""),
    (103, 103, "general_alarm_status",         "bitmap:general", False, 0, ""),
    (104, 104, "pfc_alarm_status",             "bitmap:pfc",     False, 0, ""),
    (105, 105, "dcdc_alarm_status",            "bitmap:dcdc",    False, 0, ""),
    (106, 106, "temp_alarm_status",            "bitmap:temp",    False, 0, ""),
    (107, 107, "comm_alarm_status",            "bitmap:comm",    False, 0, ""),
    (108, 108, "psu_input_power_inst",         "u16",     False, 3,  "W"),
    (109, 109, "psu_input_current_ac_inst",    "u16",     False, 10, "A"),
    (110, 110, "psu_rpm_fan0",                 "u16",     False, 0,  "rpm"),
    (111, 111, "psu_rpm_fan1",                 "u16",     False, 0,  "rpm"),
    (112, 112, "psu_temp0_inlet",              "u16",     True,  7,  "C"),
    (113, 113, "psu_temp1_outlet",             "u16",     True,  7,  "C"),
    (114, 114, "psu_max_temp",                 "u16",     True,  7,  "C"),
    (115, 115, "psu_min_temp",                 "u16",     True,  7,  "C"),
    (116, 117, "psu_position_number",          "u32",     False, 0,  ""),
    (118, 119, "crc_error_counter",            "u32",     False, 0,  ""),
    (120, 121, "timeout_error_counter",        "u32",     False, 0,  ""),
    (122, 122, "psu_output_voltage",           "u16",     False, 10, "V"),
    (123, 123, "psu_output_current",           "u16",     False, 6,  "A"),
    (124, 124, "i_share_current_value",        "u16",     False, 6,  "A"),
    (125, 125, "psu_output_power",             "u16",     False, 3,  "W"),
    (126, 126, "psu_bulk_cap_voltage",         "u16",     False, 6,  "V"),
    (127, 127, "psu_input_frequency_ac",       "u16",     False, 0,  "Hz"),
    (128, 128, "psu_ithd",                     "u16",     False, 9,  ""),
    (129, 129, "psu_power_factor",             "u16",     False, 9,  ""),
    (130, 130, "psu_input_power",              "u16",     False, 3,  "W"),
    (131, 131, "psu_input_voltage_ac",         "u16",     False, 6,  "V"),
    (132, 132, "psu_input_current_ac",         "u16",     False, 10, "A"),
    (133, 133, "psu_fault_counter",            "u16",     False, 0,  ""),
    (134, 136, "reserved_134_136",             "reserved",False, 0,  ""),
    (137, 137, "psu_setting_register",         "hex",     False, 0,  ""),
    (138, 138, "communication_baud_rate",      "u16",     False, 0,  ""),
    (139, 139, "fan_override_rpm",             "u16",     False, 0,  "rpm"),
    (140, 140, "led_override",                 "hex",     False, 0,  ""),
    (141, 142, "unix_time",                    "u32",     False, 0,  "s"),
    (143, 143, "configurable_pls_timing",      "u16",     False, 0,  ""),
    (144, 144, "vin_min",                      "u16",     False, 6,  "V"),
    (145, 145, "vin_max",                      "u16",     False, 6,  "V"),
    (146, 146, "vout_setpoint_h",              "u16",     False, 10, "V"),
    (147, 147, "vout_setpoint_l",              "u16",     False, 10, "V"),
    (148, 148, "vout_change_timer",            "u16",     False, 0,  ""),
    (149, 149, "checksum",                     "checksum",False, 0,  ""),
]

BITMAP_TABLES = {
    "general": GENERAL_ALARM,
    "pfc": PFC_ALARM,
    "dcdc": DCDC_ALARM,
    "temp": TEMP_ALARM,
    "comm": COMM_ALARM,
}


def _signed16(v: int) -> int:
    return v - 0x10000 if v & 0x8000 else v


def _scale(raw: int, signed: bool, n: int) -> float:
    val = _signed16(raw) if signed else raw
    return val / (1 << n)


def strip_framing(read):
    """Return (file_offset, data) for a well-formed MFR_BLACKBOX read.

    A well-formed read is:  [Block Count] [File Offset (0/1)] [150 data bytes]
    The Block Count byte is optional (its value equals the number of bytes that
    follow it); any trailing PEC byte past the 150 data bytes is ignored. If no
    File Offset byte is present (some captures drop it) file_offset is None and
    the whole read is treated as data.

    NOTE: a read that is exactly 150 bytes is ambiguous -- it could be 150 data
    bytes with no offset byte, or a File Offset byte plus 149 (truncated) data
    bytes. This function treats a leading 0x00/0x01 as an offset byte only when
    the read is long enough (>=151) to still leave a full 150-byte payload.
    Otherwise pass the two halves to decode_record() explicitly.
    """
    b = list(read)
    if len(b) >= 2 and b[0] == len(b) - 1:   # drop leading block-count byte
        b = b[1:]
    if len(b) >= BYTES_PER_HALF + 1 and b[0] in (0, 1):
        return b[0], b[1:1 + BYTES_PER_HALF]
    return None, b[:BYTES_PER_HALF]


def decode_record(half0_data, half1_data):
    """Decode two 150-byte data halves into 150 words + any warnings.

    half0_data supplies words 0..74, half1_data supplies words 75..149. Each
    half is decoded independently on its own word boundary, so a framing byte in
    one read can never shift the other half. Short halves decode the missing
    tail bytes as zero (and produce a warning).
    """
    words = [0] * WORDS_PER_RECORD
    warnings = []
    for data, base, label in ((half0_data, 0, "half 0"),
                              (half1_data, WORDS_PER_HALF, "half 1")):
        if len(data) != BYTES_PER_HALF:
            warnings.append(
                f"{label}: got {len(data)} data bytes, expected {BYTES_PER_HALF} "
                "(missing bytes decoded as zero; checksum cannot be verified)")
        for k in range(WORDS_PER_HALF):
            lo = data[2 * k] if 2 * k < len(data) else 0
            hi = data[2 * k + 1] if 2 * k + 1 < len(data) else 0
            words[base + k] = lo | (hi << 8)
    return words, warnings


def assemble_record(read_a, read_b):
    """Convenience wrapper for two well-formed reads (each framed as
    [count?][offset][150 data]). For reads with inconsistent or missing framing,
    strip the framing yourself and call decode_record() with the two data halves.
    """
    halves = {}
    for r in (read_a, read_b):
        offset, data = strip_framing(r)
        if offset is None:
            raise ValueError("read has no file-offset byte; call decode_record() "
                             "with the two data halves explicitly")
        halves[offset] = data
    if set(halves) != {0, 1}:
        raise ValueError(f"expected file offsets 0 and 1, got {sorted(halves)}")
    return decode_record(halves[0], halves[1])

def verify_checksum(words):
    """Word 149 is the two's complement of the sum of all other 16-bit words,
    so the sum of the whole record must be 0 (mod 2**16)."""
    total = sum(words) & 0xFFFF
    return total == 0, total


def set_bits(word, table):
    """Return a list of (bit, label) for asserted bits; unknown bits labeled."""
    out = []
    for bit in range(16):
        if word & (1 << bit):
            out.append((bit, table.get(bit, "reserved/undocumented")))
    return out


def decode(words):
    """Decode a 150-word record into an ordered list of (name, value) items."""
    if len(words) != WORDS_PER_RECORD:
        raise ValueError(f"expected {WORDS_PER_RECORD} words, got {len(words)}")

    ok, total = verify_checksum(words)
    result = {"_checksum_ok": ok, "_checksum_sum": total, "fields": []}

    for start, end, name, form, signed, n, unit in FIELDS:
        chunk = words[start:end + 1]
        if form == "samples":
            value = [_scale(w, signed, n) for w in chunk]
        elif form == "u16":
            value = _scale(chunk[0], signed, n)
        elif form == "u32":
            combined = chunk[0] | (chunk[1] << 16)  # little-endian word order
            value = combined / (1 << n)
        elif form == "reserved":
            value = None
        elif form == "hex":
            value = " ".join(f"0x{w:04x}" for w in chunk)
        elif form == "checksum":
            value = f"0x{chunk[0]:04x}"
        elif form.startswith("bitmap:"):
            table = BITMAP_TABLES[form.split(":", 1)[1]]
            value = set_bits(chunk[0], table)
        else:
            raise ValueError(f"unknown form {form!r}")
        result["fields"].append((name, form, unit, chunk, value))
    return result


def format_report(result) -> str:
    lines = []
    ok = result["_checksum_ok"]
    lines.append(f"Checksum: {'OK' if ok else 'MISMATCH'} "
                 f"(record sum = 0x{result['_checksum_sum']:04x}, "
                 f"expected 0x0000)")
    lines.append("")
    for name, form, unit, raw, value in result["fields"]:
        raw_hex = " ".join(f"{w:04x}" for w in raw)
        if form == "reserved":
            continue
        if form == "samples":
            nums = ", ".join(f"{v:g}" for v in value)
            lines.append(f"{name:32s} [{nums}] {unit}".rstrip())
        elif form.startswith("bitmap:"):
            if value:
                flags = ", ".join(f"{label} (bit {bit})" for bit, label in value)
            else:
                flags = "none"
            lines.append(f"{name:32s} 0x{raw[0]:04x} -> {flags}")
        elif form in ("hex", "checksum"):
            lines.append(f"{name:32s} {value}")
        else:  # u16 / u32 scalar
            u = f" {unit}" if unit else ""
            lines.append(f"{name:32s} {value:g}{u}  (raw {raw_hex})")
    return "\n".join(lines)


# --------------------------------------------------------------------------- #
# CLI / self-test
# --------------------------------------------------------------------------- #
def _parse_hex_bytes(text):
    """Parse a whitespace/comma separated list of hex (or 0x) byte values."""
    toks = text.replace(",", " ").split()
    return [int(t, 16) for t in toks]


def main():
    read0 = _parse_hex_bytes("0x00 0x47 0x48 0x2f 0x69 0x17 0x78 0x09 0xf2 0x28 0xa3 0x3f 0xef 0x50 0x0e 0x54 0xe1 0x52 0x14 0x39 0x82 0x20 0xca 0x05 0x86 0x20 0x0a 0x37 0xc2 0x49 0xae 0x53 0xba 0x55 0x2d 0x42 0xd5 0x2b 0x36 0x12 0x58 0x00 0x7a 0x00 0x7a 0x00 0x28 0x00 0x7a 0x00 0x51 0x00 0x28 0x00 0x05 0x00 0x28 0x00 0x7a 0x00 0x23 0x00 0x05 0x00 0x51 0x00 0x2e 0x00 0x51 0x00 0x7a 0x00 0xad 0x00 0x05 0x00 0x23 0x00 0x05 0x00 0x2e 0x02 0xe6 0x02 0xe6 0x02 0xf3 0x02 0xe6 0x02 0xe6 0x02 0xe6 0x02 0xe6 0x02 0xe6 0x02 0xe6 0x02 0xf3 0x02 0xe6 0x02 0xe6 0x02 0xe6 0x02 0xe6 0x02 0xe6 0x02 0xe6 0x02 0xf3 0x02 0xe6 0x02 0xe6 0x02 0xe6 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00")
    read1 = _parse_hex_bytes("0x01 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x09 0xa0 0x09 0x80 0x09 0xa0 0x09 0xa0 0x09 0xa0 0x0e 0x70 0x0e 0x70 0x0e 0x70 0x0e 0x80 0x0e 0x80 0x2d 0xa2 0x2e 0xcb 0x00 0xc6 0x00 0x00 0x00 0x00 0x00 0x8f 0xcd 0xac 0x00 0x00 0x00 0x00 0x00 0x41 0x00 0xf4 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x09 0xa0 0x0e 0x80 0x0e 0x80 0x09 0xa0 0x00 0x00 0x00 0x07 0x00 0x00 0x00 0x00 0x00 0x03 0x92 0xde 0x00 0x00 0x00 0x00 0x00 0x79 0x00 0x00 0x6f 0x05 0x00 0x5d 0x01 0xff 0x56 0x4c 0x04 0x29 0x10 0x4d 0x02 0x61 0x00 0x22 0x00 0x00 0x00 0x00 0x00 0x00 0x0a 0x01 0x00 0x01 0x00 0x00 0x00 0x00 0x00 0x6c 0x72 0x03 0x00 0x2d 0x2c 0x80 0x66 0x00 0xc8 0x00 0xc0 0x00 0x00 0x00")

    # NOTE: these two hardcoded reads are framed inconsistently:
    #   * read0 is 150 pure data bytes (words 0-74) -- its File Offset byte is
    #     absent (its leading 0x00 is word 0's low byte; the AC-input-voltage
    #     waveform only decodes sanely when read0 is NOT stripped).
    #   * read1 is a File Offset byte (0x01) + 149 data bytes (words 75-149) --
    #     the offset byte is present and the final data byte (checksum high byte)
    #     is missing.
    # Because a 150-byte read starting with 0x00/0x01 is ambiguous, we frame the
    # two halves explicitly here rather than letting strip_framing() guess.
    half0 = read0            # already 150 data bytes
    half1 = read1[1:]        # drop the leading File Offset byte (0x01)

    words, warnings = decode_record(half0, half1)
    for w in warnings:
        print(f"WARNING: {w}")
    if warnings:
        print()
    print(format_report(decode(words)))


if __name__ == "__main__":
    main()
