let algorithm = "AWS4-HMAC-SHA256"

module Hash = struct
  let[@inline] _sha256 ?key str =
    match key with
    | Some key -> Digestif.SHA256.hmac_string ~key str
    | None -> Digestif.SHA256.digest_string str

  let sha256 ?key str = _sha256 ?key str |> Digestif.SHA256.to_raw_string

  let sha256_hex ?key str = _sha256 ?key str |> Digestif.SHA256.to_hex

  let sha256_base64 ?key str = Base64.encode_string @@ sha256 ?key str

  let content_sha256 = sha256_hex ""
end

let encode_query ps =
  (* NOTE(dbp 2015-03-13): We want just:
     A-Z, a-z, 0-9, hyphen ( - ), underscore ( _ ), period ( . ), and tilde ( ~ ).
            As per the docs:
            http://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html
            Uri has that as it's fall-through, which at least currently (and hopefully forever)
            ~component:`Authority causes it to use.
  *)
  let encoded =
    List.rev_map
      (fun (k, v) ->
        let key = Uri.pct_encode ~component:`Authority k in
        let value =
          match v with
          | [] -> ""
          | [ x ] -> Uri.pct_encode ~component:`Authority x
          | _ -> failwith "AWS query cannot have multiple values for same key"
        in
        key, value)
      ps
  in
  let sorted = List.sort (fun a b -> compare (fst b) (fst a)) encoded in
  let joined = List.rev_map (fun (k, v) -> k ^ "=" ^ v) sorted in
  String.concat "&" joined

let get_signature_key =
  let[@inline] sign msg key = Hash.sha256 ~key msg in
  fun key date region service ->
    String.concat "" [ "AWS4"; key ]
    |> sign date
    |> sign "aws4_request"
    |> sign service
    |> sign region

(* NOTE(dbp 2015-01-13): This is a direct translation of reference implementation at:
 * http://docs.aws.amazon.com/general/latest/gr/sigv4-signed-request-examples.html
 *)
let sign_request_header ~access_key ~secret_key ?token ~service ~region ~meth target =
  let uri = Endpoints.url_of service region |> Util.of_option_exn |> Uri.of_string in
  let host = Uri.host uri |> Util.of_option_exn in
  let now = Time.now_utc () in
  let amzdate = Time.date_time now in
  let datestamp = Time.date_yymmdd now in
  let canonical_uri = "/" in
  (* let canonical_querystring = params in *)
  let token_header, signed_headers =
    match token with
    | Some t ->
        let th = String.concat "" [ "x-amz-security-token:"; t; "\n" ] in
        let sh =
          "host;x-amz-content-sha256;x-amz-date;x-amz-target;x-amz-security-token"
        in
        th, sh
    | None -> "", "host;x-amz-content-sha256;x-amz-date;x-amz-target"
  in
  let canonical_headers =
    Format.sprintf
      "host:%s\nx-amz-content-sha256:%s\nx-amz-date:%s\nx-amz-target:%s\n%s"
      host
      Hash.content_sha256
      amzdate
      target
      token_header
  in
  let canonical_request_sign =
    Hash.sha256_hex
    @@ String.concat
         "\n"
         [ Request.string_of_meth meth
         ; canonical_uri (* ; canonical_querystring *)
         ; canonical_headers
         ; signed_headers
         ; Hash.content_sha256
         ]
  in
  let credential_scope =
    String.concat "/" [ datestamp; region; service; "aws4_request" ]
  in
  let string_to_sign =
    String.concat "\n" [ algorithm; amzdate; credential_scope; canonical_request_sign ]
  in
  let signing_key = get_signature_key secret_key datestamp region service in
  let signature = Hash.sha256_hex ~key:signing_key string_to_sign in
  let authorization_header =
    String.concat
      ""
      [ algorithm
      ; " "
      ; "Credential="
      ; access_key
      ; "/"
      ; credential_scope
      ; ", "
      ; "SignedHeaders="
      ; signed_headers
      ; ", "
      ; "Signature="
      ; signature
      ]
  in
  let headers =
    [ "x-amz-date", amzdate
    ; "x-amz-content-sha256", Hash.content_sha256
    ; "x-amz-target", target
    ; "Authorization", authorization_header
    ]
  in
  let full_headers =
    match token with
    | Some t -> ("X-Amz-Security-Token", t) :: headers
    | None -> headers
  in
  full_headers, uri

let sign_v2_request
    ~access_key
    ~secret_key
    ?token
    ~service
    ~region
    (meth, uri, headers, body) =
  let host = Util.of_option_exn (Endpoints.endpoint_of service region) in
  let amzdate = Time.date_time_iso8601 (Time.now_utc ()) in

  let query =
    Uri.add_query_params'
      uri
      ((match token with
       | Some t -> [ "SecurityToken", t ]
       | None -> [])
      @ [ "Timestamp", amzdate
        ; "AWSAccessKeyId", access_key
        ; "SignatureMethod", "HmacSHA256"
        ; "SignatureVersion", "2"
        ])
  in

  let params = encode_query (Uri.query query) in
  let canonical_uri = "/" in
  let string_to_sign =
    String.concat "\n" [ Request.string_of_meth meth; host; canonical_uri; params ]
  in
  let signature = Hash.sha256_base64 ~key:secret_key string_to_sign in
  let new_uri = Uri.add_query_param' query ("Signature", signature) in
  meth, new_uri, headers, body
