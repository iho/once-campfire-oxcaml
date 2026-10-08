let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected dimensions")

let () =
  let png =
    "\137PNG\r\n\026\n\000\000\000\013IHDR"
    ^ "\000\000\007\128\000\000\004\176"
  in
  check "PNG dimensions" (Some (1920, 1200)) (Media_dimensions.of_bytes png);
  check "JPEG dimensions from SOF marker" (Some (8, 4))
    (Media_dimensions.of_bytes
       "\255\216\255\192\000\011\008\000\004\000\008\003\001\017\000");
  check "GIF dimensions" (Some (256, 128))
    (Media_dimensions.of_bytes "GIF89a\000\001\128\000\000\000");
  check "WebP extended dimensions" (Some (1024, 512))
    (Media_dimensions.of_bytes
       ("RIFF\000\000\000\000WEBPVP8X\000\000\000\000"
        ^ "\000\000\000\000\255\003\000\255\001\000"));
  check "unknown media has no dimensions" None
    (Media_dimensions.of_bytes "not an image");
  check "TIFF upload signature" (Some "image/tiff")
    (Media_dimensions.image_content_type_of_bytes "II*\000\008\000\000\000");
  check "AVIF upload signature" (Some "image/avif")
    (Media_dimensions.image_content_type_of_bytes
       "\000\000\000\020ftypavif\000\000\000\000");
  check "HEIC compatible brand" (Some "image/heic")
    (Media_dimensions.image_content_type_of_bytes
       "\000\000\000\024ftypmif1\000\000\000\000heic");
  check "generic HEIF brand" (Some "image/heif")
    (Media_dimensions.image_content_type_of_bytes
       "\000\000\000\020ftypmif1\000\000\000\000");
  check "JPEG 2000 upload signature" (Some "image/jp2")
    (Media_dimensions.image_content_type_of_bytes
       "\000\000\000\012jP  \r\n\135\n");
  check "icon upload signature" (Some "image/vnd.microsoft.icon")
    (Media_dimensions.image_content_type_of_bytes "\000\000\001\000\001\000");
  check "image hints do not promote non-image bytes" None
    (Media_dimensions.image_content_type_of_bytes "not an image");
  check "vipsheader dimensions" (Some (4032, 3024))
    (Media_dimensions.vipsheader_output
       "/tmp/photo.tif: 4032x3024 uchar, 3 bands, srgb, tiffload\nwidth: 4032\nheight: 3024\nbands: 3\nvips-loader: tiffload\n");
  check "vipsheader rejects incomplete dimensions" None
    (Media_dimensions.vipsheader_output "width: 4032\nheight: 0\n");
  check "pdfinfo first page dimensions" (Some (596, 842))
    (Media_dimensions.pdfinfo_output
       "Title: sample\nPage 1 size: 595.28 x 841.89 pts (A4)\n");
  check "pdfinfo missing size" None (Media_dimensions.pdfinfo_output "Title: sample\n")
