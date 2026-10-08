let expect label expected actual =
  if actual <> expected then failwith (label ^ " did not match expected value")

let () =
  expect "epoch" (Some 0L)
    (Option.map Int64.of_int (Timestamp.epoch_milliseconds "1970-01-01 00:00:00"));
  expect "benchmark UTC milliseconds" (Some 1772465400000L)
    (Option.map Int64.of_int (Timestamp.epoch_milliseconds "2026-03-02 15:30:00"));
  expect "fractional milliseconds" (Some 1772465400123L)
    (Option.map Int64.of_int (Timestamp.epoch_milliseconds "2026-03-02 15:30:00.123456"));
  expect "ISO8601 format" (Some "2026-03-02T15:30:00.123Z")
    (Timestamp.iso8601_utc "2026-03-02 15:30:00.123456");
  if Timestamp.epoch_milliseconds "2025-02-29 00:00:00" <> None then
    failwith "invalid leap day should be rejected"
