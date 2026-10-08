let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected result")

let () =
  check "landscape previews fit the Rails 1200x800 box without distortion"
    (Some (1200.0, 675.0))
    (Attachment_presentation.preview_dimensions ~width:(Some 3840) ~height:(Some 2160));
  check "portrait previews fit the Rails 1200x800 box without distortion"
    (Some (400.0, 800.0))
    (Attachment_presentation.preview_dimensions ~width:(Some 1200) ~height:(Some 2400));
  check "small previews are not enlarged"
    (Some (640.0, 480.0))
    (Attachment_presentation.preview_dimensions ~width:(Some 640) ~height:(Some 480));
  check "unknown preview dimensions remain unknown" None
    (Attachment_presentation.preview_dimensions ~width:None ~height:(Some 480));
  check "known previews get Rails inline media constraints"
    "<div class=\"max-inline-size center flex overflow-clip\" style=\"width: 600px; aspect-ratio: 1.777778;\"><img></div>"
    (Attachment_presentation.wrap_preview ~width:(Some 3840) ~height:(Some 2160) "<img>");
  check "unknown previews keep the unconstrained Rails wrapper"
    "<div class=\"max-inline-size center overflow-clip\"><img></div>"
    (Attachment_presentation.wrap_preview ~width:None ~height:None "<img>")
