open OUnit2
open Aws_dynamodb

type config =
  { access_key : string
  ; secret_key : string
  ; region : string
  }
    
let ( @? ) = assert_bool

module TestSuite(Runtime : sig
    type 'a m
    val run_request :
         region:string
      -> access_key:string
      -> secret_key:string
      -> ?token:string
      -> (module Aws.Call with type input = 'input
                           and type output = 'output
                           and type error = 'error)
      -> 'input
      -> [`Ok of 'output | `Error of 'error Aws.Error.t] m
    val un_m : 'a m -> 'a
  end) = struct

  let noop_test config () =
    "Noop DYNAMODB test succeeds"
    @?false

  let suite config =
    "Test DYNAMODB" >::: [ "DYNAMODB noop" >:: noop_test config ]

  let () =
      let access_key =
        try Some (Unix.getenv "AWS_ACCESS_KEY_ID") with Not_found -> None
      in
      let secret_key =
        try Some (Unix.getenv "AWS_SECRET_ACCESS_KEY") with Not_found -> None
      in
      let region = try Some (Unix.getenv "AWS_DEFAULT_REGION") with Not_found -> None in
      
      match access_key, secret_key, region with
      | Some access_key, Some secret_key, Some region ->
          run_test_tt_main (suite { access_key; secret_key; region })
      | _, _, _ ->
          Printf.eprintf
            "Skipping running tests. Environment variables AWS_ACCESS_KEY_ID, \
             AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION not available. ";
          exit 0    
end
