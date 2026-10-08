module OpenGraph = Opengraph

let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected result")

let expect label predicate value =
  if not (predicate value) then failwith (label ^ ": predicate failed")

let public_resolver _host _port = [ "8.8.8.8"; "2606:4700:4700::1111" ]

let () =
  let public_uri =
    OpenGraph.valid_public_uri ~resolve:public_resolver
      "https://example.test/path?q=1#fragment"
    |> Option.get |> Uri.to_string
  in
  check "public URL preserves the query and omits its fragment"
    "https://example.test/path?q=1" public_uri;
  check "credentialed URLs are rejected" None
    (OpenGraph.valid_public_uri ~resolve:public_resolver
       "https://user:secret@example.test/path");
  check "non-HTTP schemes are rejected" None
    (OpenGraph.valid_public_uri ~resolve:public_resolver "file:///etc/passwd");
  check "empty host is rejected" None
    (OpenGraph.valid_public_uri ~resolve:public_resolver "https:///path");
  check "private DNS answers reject a URL" None
    (OpenGraph.valid_public_uri ~resolve:(fun _ _ -> [ "8.8.8.8"; "10.0.0.4" ])
       "https://example.test/");
  check "empty DNS answers reject a URL" None
    (OpenGraph.valid_public_uri ~resolve:(fun _ _ -> []) "https://example.test/");
  check "Teredo IPv6 DNS answers are rejected" false
    (OpenGraph.public_address "2001:0::1");
  check "6to4 IPv6 DNS answers are rejected" false
    (OpenGraph.public_address "2002:c000:0201::1");
  check "6to4 relay anycast IPv4 addresses are rejected" false
    (OpenGraph.public_address "192.88.99.1");
  expect "media URLs skip document fetches"
    (fun url -> OpenGraph.media_path (Uri.path url))
    (Uri.of_string "https://example.test/image.JPEG?size=large");
  let html =
    "<HTML><head><meta property='og:title' content='A &amp; B &quot;title&quot;'><meta name=\"og:description\" content=\"Read &lt;strong&gt;this&lt;/strong&gt; &#x1F525;\"><meta property=og:url content=https://canonical.example.test/article><meta property=og:image content=https://cdn.example.test/card.png><meta property=og:title content='Last title &copy;'></head></HTML>"
  in
  let fields = OpenGraph.extract_open_graph html in
  check "later OpenGraph fields override earlier values"
    "Last title &copy;" (List.assoc "title" fields);
  check "description entities decode and tags strip"
    "Read this 🔥" (List.assoc "description" fields);
  check "canonical and image values parse"
    [ "https://canonical.example.test/article"; "https://cdn.example.test/card.png" ]
    [ List.assoc "url" fields; List.assoc "image" fields ];
  let metadata =
    OpenGraph.metadata ~resolve:public_resolver
      ~image_content_type:(fun _ -> Some "IMAGE/PNG")
      "https://origin.example.test/page"
      [ ("title", "Campfire"); ("url", "https://canonical.example.test/page");
        ("image", "https://cdn.example.test/cover.png");
        ("description", "A room for everyone") ]
    |> Option.get
  in
  check "metadata returns sanitized title, canonical URL, accepted image and description"
    ("Campfire", "https://canonical.example.test/page",
     Some "https://cdn.example.test/cover.png", "A room for everyone") metadata;
  let image_rejected =
    OpenGraph.metadata ~resolve:public_resolver
      ~image_content_type:(fun _ -> Some "image/svg+xml")
      "https://origin.example.test/page"
      [ ("title", "Campfire"); ("url", "https://origin.example.test/page");
        ("image", "https://cdn.example.test/cover.svg"); ("description", "Description") ]
    |> Option.get
  in
  check "unsafe image content types are omitted" None (let _, _, image, _ = image_rejected in image);
  check "metadata requires title and description" None
    (OpenGraph.metadata ~resolve:public_resolver ~image_content_type:(fun _ -> None)
       "https://origin.example.test/page" [ ("title", "Only title") ]);
  check "Twitter links use fxtwitter for metadata" "fxtwitter.com"
    (OpenGraph.twitter_proxy_uri (Uri.of_string "https://x.com/user/status/123")
    |> Uri.host |> Option.get);
  check "Twitter home does not change host" "x.com"
    (OpenGraph.twitter_proxy_uri (Uri.of_string "https://x.com/")
    |> Uri.host |> Option.get)
