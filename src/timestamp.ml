let parse value =
  try
    let part offset length = int_of_string (String.sub value offset length) in
    let year = part 0 4 and month = part 5 2 and day = part 8 2 in
    let hour = part 11 2 and minute = part 14 2 and second = part 17 2 in
    if value.[4] <> '-' || value.[7] <> '-' || (value.[10] <> ' ' && value.[10] <> 'T')
       || value.[13] <> ':' || value.[16] <> ':' || month < 1 || month > 12
       || hour > 23 || minute > 59 || second > 59
    then None
    else
      let leap = year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0) in
      let days_in_month =
        [| 31; (if leap then 29 else 28); 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]
      in
      if day < 1 || day > days_in_month.(month - 1) then None
      else
        let milliseconds =
          if String.length value <= 19 || value.[19] <> '.' then 0
          else
            let digits = Buffer.create 3 in
            let index = ref 20 in
            while !index < String.length value && value.[!index] >= '0' && value.[!index] <= '9' do
              if Buffer.length digits < 3 then Buffer.add_char digits value.[!index];
              incr index
            done;
            let fraction = Buffer.contents digits in
            if fraction = "" then 0
            else int_of_string (fraction ^ String.make (3 - String.length fraction) '0')
        in
        Some (year, month, day, hour, minute, second, milliseconds)
  with _ -> None

let epoch_milliseconds value =
  Option.map
    (fun (year, month, day, hour, minute, second, milliseconds) ->
      let adjusted_year = year - if month <= 2 then 1 else 0 in
      let era = adjusted_year / 400 in
      let year_of_era = adjusted_year - (era * 400) in
      let adjusted_month = month + if month > 2 then -3 else 9 in
      let day_of_year = ((153 * adjusted_month + 2) / 5) + day - 1 in
      let day_of_era = (365 * year_of_era) + (year_of_era / 4) - (year_of_era / 100) + day_of_year in
      let days = (era * 146097) + day_of_era - 719468 in
      (((days * 24 + hour) * 60 + minute) * 60 + second) * 1000 + milliseconds)
    (parse value)

let iso8601_utc value =
  Option.map
    (fun (year, month, day, hour, minute, second, milliseconds) ->
      Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ" year month day hour minute second
        milliseconds)
    (parse value)
