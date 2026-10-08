let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected result")

let () =
  let secret = "independent-active-storage-vector" in
  let sgid =
    "eyJfcmFpbHMiOnsiZGF0YSI6ImdpZDovL2NhbXBmaXJlL1VzZXIvNDI_ZXhwaXJlc19pbiIsInB1ciI6ImF0dGFjaGFibGUifX0=--e48ee53fbcf4307dbeaa9af5a09733f29768a579"
  in
  let body =
    "<div>Hey <action-text-attachment sgid=\"" ^ sgid
    ^ "\" content-type=\"application/vnd.campfire.mention\"></action-text-attachment> "
    ^ "<action-text-attachment sgid='" ^ sgid
    ^ "'></action-text-attachment><span sgid=\"" ^ sgid ^ "\"></span></div>"
  in
  check "Action Text mentions are verified, attachment-scoped and deduplicated" [ 42 ]
    (Action_text.mentioned_user_ids ~secret body);
  let single_mention =
    "<div>Hey <action-text-attachment sgid=\"" ^ sgid
    ^ "\" content-type=\"application/vnd.campfire.mention\"></action-text-attachment></div>"
  in
  let stored = Action_text.sanitize ~secret single_mention in
  if not (String.starts_with ~prefix:"<div>Hey <action-text-attachment sgid=\"" stored)
  then failwith ("verified mention was not preserved: " ^ stored);
  let rendered =
    Action_text.sanitize ~secret
      ~resolve_user:(fun id -> if id = 42 then Some "Ada Lovelace" else None)
      stored
  in
  if rendered <> "<div>Hey <span class=\"mention\">Ada Lovelace</span></div>" then
    failwith ("stored mention render mismatch: " ^ rendered);
  let stripped = Action_text.sanitize single_mention in
  if stripped <> "<div>Hey </div>" then
    failwith ("mention without secret was retained: " ^ stripped);
  check "invalid Action Text mention signatures are ignored" []
    (Action_text.mentioned_user_ids ~secret ("<action-text-attachment sgid=\"" ^ sgid ^ "x\"></action-text-attachment>"))
