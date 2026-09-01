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
BYTES_PER_HALF = 150
BLOCK_COUNT = 151  # File Offset byte + 150 data bytes

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


def unpack_read(read):
    """Strip the optional block-count byte, the file-offset byte, and any
    trailing PEC from a single MFR_BLACKBOX read. Returns (file_offset, data)
    where data is exactly 150 bytes."""
    b = list(read)
    # Drop a leading SMBus block-count byte if present (its value is 151).
    if len(b) >= BLOCK_COUNT + 1 and b[0] == BLOCK_COUNT:
        b = b[1:]
    if len(b) < BYTES_PER_HALF + 1:
        raise ValueError(
            f"read too short: need at least {BYTES_PER_HALF + 1} bytes "
            f"(file offset + {BYTES_PER_HALF} data), got {len(b)}"
        )
    file_offset = b[0]
    data = b[1:1 + BYTES_PER_HALF]  # ignore any trailing PEC
    if file_offset not in (0, 1):
        raise ValueError(f"unexpected file offset {file_offset}, expected 0 or 1")
    return file_offset, data


def assemble_record(read_a, read_b):
    """Stitch two consecutive reads (in either order) into 150 words."""
    halves = {}
    for r in (read_a, read_b):
        offset, data = unpack_read(r)
        if offset in halves:
            raise ValueError(f"both reads have file offset {offset}; "
                             "expected one with 0 and one with 1")
        halves[offset] = data
    if set(halves) != {0, 1}:
        raise ValueError(f"missing a half; got offsets {sorted(halves)}")

    raw = bytes(halves[0]) + bytes(halves[1])  # 300 bytes, words 0..149
    words = [raw[2 * i] | (raw[2 * i + 1] << 8) for i in range(WORDS_PER_RECORD)]
    return words


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


def _build_demo():
    """Construct a valid two-read pair for round-trip self-testing."""
    words = [0] * WORDS_PER_RECORD

    def put_scalar(idx, real, signed, n):
        raw = round(real * (1 << n))
        if signed and raw < 0:
            raw += 0x10000
        words[idx] = raw & 0xFFFF

    for i in range(0, 20):
        put_scalar(i, 230.0, False, 6)       # AC input voltage ~230 V
    for i in range(40, 60):
        put_scalar(i, 50.0, False, 10)       # Main output ~50 V
    for i in range(80, 85):
        put_scalar(i, 27.5, True, 7)         # Inlet temp 27.5 C
    put_scalar(95, 0, False, 0); words[95] = 123456 & 0xFFFF
    words[96] = (123456 >> 16) & 0xFFFF      # total_up_time = 123456 s
    words[99] = 5                            # AC power cycle counter
    words[103] = (1 << 11) | (1 << 2)        # general alarm: Fan Alarm + Temperature
    put_scalar(122, 50.0, False, 10)         # PSU output voltage
    put_scalar(127, 60, False, 0)            # 60 Hz
    words[141] = 0xABCD; words[142] = 0x0000  # unix time

    words[149] = (-sum(words[:149])) & 0xFFFF  # checksum

    raw = b"".join(int(w).to_bytes(2, "little") for w in words)
    half0, half1 = raw[:150], raw[150:]
    read0 = bytes([BLOCK_COUNT, 0]) + half0
    read1 = bytes([BLOCK_COUNT, 1]) + half1
    return read0, read1


def main(argv):
    if len(argv) == 2 and argv[1] == "--demo":
        read0, read1 = _build_demo()
    elif len(argv) == 3:
        # Two files, each containing one read as hex bytes.
        with open(argv[1]) as f:
            read0 = _parse_hex_bytes(f.read())
        with open(argv[2]) as f:
            read1 = _parse_hex_bytes(f.read())
    elif len(argv) == 1 and not sys.stdin.isatty():
        # Two reads on stdin, one per line.
        lines = [ln for ln in sys.stdin.read().splitlines() if ln.strip()]
        if len(lines) != 2:
            sys.exit("stdin must contain exactly two lines (one per read)")
        read0 = _parse_hex_bytes(lines[0])
        read1 = _parse_hex_bytes(lines[1])
    else:
        sys.exit(
            "usage:\n"
            f"  {argv[0]} read0.hex read1.hex   # two files of hex bytes\n"
            f"  {argv[0]} --demo                # decode a synthetic record\n"
            f"  ... | {argv[0]}                 # two reads on stdin, one per line"
        )

    words = assemble_record(read0, read1)
    print(format_report(decode(words)))


if __name__ == "__main__":
    main(sys.argv)
