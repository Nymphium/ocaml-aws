type input = Types.ScanInput.t
type output = Aws.Json.t
type error = Errors_internal.t
let request_body_of_input input =
  Aws.Json.to_string (Types.ScanInput.to_json input)
let service = "dynamodb"
let target = "DynamoDB_20120810.Scan"
let signature_version = Aws.Request.V4
let headers = [("x-amz-target", target)]
let meth = `POST
let to_http service region req =
  let body = Some (Aws.Json.to_string (Types.ScanInput.to_json req)) in
  let uri =
    Uri.of_string
      (Aws.Util.of_option_exn (Aws.Endpoints.url_of service region)) in
  (meth, uri, headers, body)
let of_http body =
  try `Ok (Aws.Json.of_string body)
  with
  | Aws.Json.Parse_error msg ->
      `Error
        (Aws.Error.BadResponse
           {
             body;
             message =
               ("Error parsing ScanOutput - missing field in body or children: "
                  ^ msg)
           })
let errors = Errors_internal.common
let parse_error code err =
  match Errors_internal.of_string err with
  | Some var ->
      if
        (List.mem var errors) &&
          ((match Errors_internal.to_http_code var with
            | Some var -> var = code
            | None -> true))
      then Some var
      else None
  | None -> None