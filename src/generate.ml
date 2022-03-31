(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

open Structures
open Util
open Graphlib.Std

module E : Regular.Std.Opaque.S with type t = Shape.t = struct
  type t = Shape.t

  include Regular.Std.Opaque.Make (struct
    type nonrec t = t

    let compare a b = compare a.Shape.name b.Shape.name

    let hash a = Hashtbl.hash a.Shape.name
  end)
end

module G = Graphlib.Make (E) (Core.Unit)

module Gf = Graph.Persistent.Digraph.ConcreteBidirectional (struct
  type t = E.t Group.t

  let compare g g' = E.compare (Group.top g) (Group.top g')

  let hash g = E.hash (Group.top g)

  let equal g g' = E.equal (Group.top g) (Group.top g')
end)

let is_list ~shapes ~shp =
  try
    match (StringTable.find shp shapes).Shape.content with
    | Shape.List _ -> true
    | _ -> false
  with Not_found -> false

let is_flat_list ~shapes ~shp =
  try
    match (StringTable.find shp shapes).Shape.content with
    | Shape.List (_, _, true) -> true
    | _ -> false
  with Not_found -> false

let members = function
  | Shape.Structure members -> List.map (fun mem -> mem.Structure.shape) members
  | Shape.List (s, _, _) -> [ s ]
  | Shape.Map ((ks, _), (vs, _)) -> [ ks; vs ]
  | Shape.Enum _ -> []

(** Make strongly-connected component to resolve module dependencies and recursivities *)
let scc (shapes : Shape.t StringTable.t) =
  let nodes = StringTable.fold (Fun.const List.cons) shapes [] in
  let edges =
    ListLabels.fold_left nodes ~init:[] ~f:(fun edges shp ->
        ListLabels.fold_left ~init:edges ~f:(fun edges mem ->
            StringTable.find_opt mem shapes
            |> function
            | Some shp' -> (shp, shp', ()) :: edges
            | None -> edges)
        @@ members shp.Shape.content)
  in
  let module Seq = Regular.Std.Seq in
  let graph =
    Graphlib.create (module G) ~edges ~nodes () |> Graphlib.strong_components (module G)
  in
  let graph' =
    Seq.fold (Partition.groups graph) ~init:Gf.empty ~f:(fun graph' g ->
        let top = Group.top g in
        ListLabels.fold_left
          (members top.Shape.content)
          ~init:(Gf.add_vertex graph' g)
          ~f:(fun graph' mem ->
            StringTable.find_opt mem shapes
            |> Fun.flip Option.bind (fun shp ->
                   Partition.group graph shp |> Option.map (Gf.add_edge graph' g))
            |> function
            | Some graph' -> graph'
            | None -> graph'))
  in
  let module T = Graph.Topological.Make (Gf) in
  T.fold
    (fun g acc ->
      let top = Group.top g in
      let is_rec =
        Group.enum g
        |> Seq.to_list
        |> ListLabels.fold_left ~init:false ~f:(fun b shp ->
               b
               || (List.exists (String.equal top.Shape.name) @@ members shp.Shape.content))
      in
      (if is_rec then `Rec (Group.enum g |> Seq.to_list) else `Nonrec (Group.top g))
      :: acc)
    graph'
    []

let types is_ec2 shapes =
  let option_type shp = function
    | true -> Syntax.ty0 shp
    | false -> Syntax.ty1 "option" shp
  in
  let build_module v =
    let mkrecty fs =
      Syntax.tyreclet' "t" (List.map (fun (nm, shp, req) -> nm, option_type shp req) fs)
    in
    let ty =
      match v.Shape.content with
      | Shape.Structure [] ->
          (* Hack for a unit type since empty records aren't yet valid *)
          Syntax.tyunit' "t"
      | Shape.Structure members ->
          mkrecty
            (List.map
               (fun m ->
                 ( m.Structure.field_name
                 , String.capitalize_ascii m.Structure.shape ^ ".t"
                 , m.Structure.required ))
               members)
      | Shape.List (shp, _, _flatten) ->
          Syntax.tylet' "t" (Syntax.ty1 "list" (shp ^ ".t"))
      | Shape.Map ((kshp, _loc), (vshp, _)) ->
          Syntax.tylet' "t" (Syntax.ty2 "Hashtbl.t" (kshp ^ ".t") (vshp ^ ".t"))
      | Shape.Enum opts ->
          Syntax.tyvariantlet' "t" (List.map (fun t -> Util.to_variant_name t, []) opts)
    in
    let make, tymake =
      match v.Shape.content with
      | Shape.Structure [] | Shape.List _ | Shape.Enum _ -> None, None
      | Shape.Structure members ->
          let rec mkfun (args : Structure.member list) body =
            match args with
            | [] -> body
            | x :: xs ->
                let fn = if x.Structure.required then Syntax.funlab else Syntax.funopt in
                fn x.Structure.field_name (mkfun xs body)
          in
          let body =
            Syntax.(
              fununit
                (record
                   (List.map
                      (fun a -> a.Structure.field_name, ident a.Structure.field_name)
                      members)))
          in
          let ty =
            List.fold_right
              Syntax.(
                fun Structure.{ required; field_name; shape; _ } ty ->
                  let label =
                    field_name |> if required then Syntax.label else Syntax.labelopt
                  in
                  let memty = Syntax.ty0 (String.capitalize_ascii shape ^ ".t") in
                  tyfun ~label memty ty)
              members
              Syntax.(tyfun (ty0 "unit") (ty0 "t"))
          in
          Some (Syntax.let_ "make" (mkfun members body)), Some ty
      (* TODO: maybe accept a list of tuples and create a Hashtbl *)
      | Shape.Map _ -> None, None
    in
    let extra, tyextra =
      let open Syntax in
      match v.Shape.content with
      | Shape.Enum opts ->
          ( [ let_
                "str_to_t"
                (list
                   (List.map
                      (fun o -> pair (str o) (ident (Util.to_variant_name o)))
                      opts))
            ; let_
                "t_to_str"
                (list
                   (List.map
                      (fun o -> pair (ident (Util.to_variant_name o)) (str o))
                      opts))
            ; let_
                "to_string"
                (fun_
                   "e"
                   (app1
                      "Aws.Util.of_option_exn"
                      (app2 "Aws.Util.list_find" (ident "t_to_str") (ident "e"))))
            ; let_
                "of_string"
                (fun_
                   "s"
                   (app1
                      "Aws.Util.of_option_exn"
                      (app2 "Aws.Util.list_find" (ident "str_to_t") (ident "s"))))
            ]
          , [ sigval "str_to_t" (ty0 "(string * t) list")
            ; sigval "t_to_str" (ty0 "(t * string) list")
            ; sigval "to_string" (tyfun (ty0 "t") (ty0 "string"))
            ; sigval "of_string" (tyfun (ty0 "string") (ty0 "t"))
            ] )
      | _ -> [], []
    in
    let typarse = Syntax.(tyfun (ty0 "Ezxmlm.nodes") (ty0 "t option")) in
    let parse =
      match v.Shape.content with
      | Shape.Structure [] -> Syntax.(let_ "parse" (fun_ "xml" (app1 "Some" (unit ()))))
      | Shape.Structure s ->
          let fields =
            List.map
              (fun (mem : Structure.member) ->
                let loc_name =
                  match mem.Structure.loc_name with
                  | Some name -> name
                  | None -> mem.Structure.name
                in
                let b =
                  if is_flat_list ~shapes ~shp:mem.Structure.shape
                  then
                    Syntax.(
                      app1
                        (String.capitalize_ascii mem.Structure.shape ^ ".parse")
                        (ident "xml"))
                  else
                    Syntax.(
                      app2
                        "Option.bind"
                        (app2 "Aws.Xml.member" (str loc_name) (ident "xml"))
                        (ident (String.capitalize_ascii mem.Structure.shape ^ ".parse")))
                in
                let op =
                  if mem.Structure.required
                  then Syntax.(app2 "Aws.Xml.required" (str loc_name) b)
                  else b
                in
                mem.Structure.field_name, op)
              s
          in
          Syntax.(let_ "parse" (fun_ "xml" (app1 "Some" (record fields))))
      | Shape.Map ((_shp, _loc_name), _) ->
          Syntax.(let_ "parse" (fun_ "xml" (ident "None")))
      | Shape.List (shp, loc_name, _flatten) ->
          let item_name =
            match loc_name with
            | None -> "member"
            | Some nm -> nm
          in
          Syntax.(
            let_
              "parse"
              (fun_
                 "xml"
                 (app1
                    "Aws.Util.option_all"
                    (app2
                       "List.map"
                       (ident (shp ^ ".parse"))
                       (app2 "Aws.Xml.members" (str item_name) (ident "xml"))))))
      | Shape.Enum _opts ->
          Syntax.(
            let_
              "parse"
              (fun_
                 "xml"
                 (app2
                    "Option.bind"
                    (app1 "String.parse" (ident "xml"))
                    (fun_ "s" (app2 "Aws.Util.list_find" (ident "str_to_t") (ident "s"))))))
    in
    let tyto_query = Syntax.(tyfun (ty0 "t") (ty0 "Aws.Query.t")) in
    let to_query =
      Syntax.(
        let_
          "to_query"
          (fun_
             "v"
             (match v.Shape.content with
             | Shape.Structure s ->
                 app1
                   "Aws.Query.List"
                   (app1
                      "Aws.Util.list_filter_opt"
                      (list
                         (List.map
                            (fun mem ->
                              let location =
                                match mem.Structure.loc_name with
                                | Some name -> name
                                | None ->
                                    mem.Structure.name
                                    ^
                                    if (not is_ec2)
                                       && is_list ~shapes ~shp:mem.Structure.shape
                                    then ".member"
                                    else ""
                              in
                              let location =
                                if is_ec2
                                then String.capitalize_ascii location
                                else location
                              in
                              let q arg =
                                app1
                                  "Aws.Query.Pair"
                                  (pair
                                     (str location)
                                     (app1
                                        (String.capitalize_ascii mem.Structure.shape
                                        ^ ".to_query")
                                        arg))
                              in
                              if mem.Structure.required
                              then
                                app1 "Some" (q (ident ("v." ^ mem.Structure.field_name)))
                              else
                                app2
                                  "Option.map"
                                  (fun_ "f" (q (ident "f")))
                                  (ident ("v." ^ mem.Structure.field_name)))
                            s)))
             | Shape.List (shp, _, _flatten) ->
                 app2 "Aws.Query.to_query_list" (ident (shp ^ ".to_query")) (ident "v")
             | Shape.Map ((key_shp, _), (val_shp, _)) ->
                 app3
                   "Aws.Query.to_query_hashtbl"
                   (ident (key_shp ^ ".to_string"))
                   (ident (val_shp ^ ".to_query"))
                   (ident "v")
             | Shape.Enum _ ->
                 app1
                   "Aws.Query.Value"
                   (app1
                      "Some"
                      (app1
                         "Aws.Util.of_option_exn"
                         (app2 "Aws.Util.list_find" (ident "t_to_str") (ident "v")))))))
    in
    let tyto_json = Syntax.(tyfun (ty0 "t") (ty0 "Aws.Json.t")) in
    let to_json =
      Syntax.(
        let_
          "to_json"
          (fun_
             "v"
             (match v.Shape.content with
             | Shape.Structure s ->
                 variant1
                   "Assoc"
                   (app1
                      "Aws.Util.list_filter_opt"
                      (list
                         (List.map
                            (fun mem ->
                              let location =
                                match mem.Structure.loc_name with
                                | Some name -> name
                                | None -> mem.Structure.name
                              in
                              let q arg =
                                pair
                                  (str location)
                                  (app1
                                     (String.capitalize_ascii mem.Structure.shape
                                     ^ ".to_json")
                                     arg)
                              in
                              if mem.Structure.required
                              then
                                app1 "Some" (q (ident ("v." ^ mem.Structure.field_name)))
                              else
                                app2
                                  "Option.map"
                                  (fun_ "f" (q (ident "f")))
                                  (ident ("v." ^ mem.Structure.field_name)))
                            s)))
             | Shape.List (shp, _, _flatten) ->
                 variant1 "List" (app2 "List.map" (ident (shp ^ ".to_json")) (ident "v"))
             | Shape.Map ((key_shp, _), (val_shp, _)) ->
                 variant1
                   "Assoc"
                   (app3
                      "Hashtbl.fold"
                      (fun3
                         "k"
                         "v"
                         "acc"
                         (list_expr
                            (pair
                               (app1 (key_shp ^ ".to_string") (ident "k"))
                               (app1 (val_shp ^ ".to_json") (ident "v")))
                            (ident "acc")))
                      (ident "v")
                      (list []))
             | Shape.Enum _ ->
                 app1
                   "String.to_json"
                   (app1
                      "Aws.Util.of_option_exn"
                      (app2 "Aws.Util.list_find" (ident "t_to_str") (ident "v"))))))
    in
    let tyof_json = Syntax.(tyfun (ty0 "Aws.Json.t") (ty0 "t")) in
    let of_json =
      Syntax.(
        let_
          "of_json"
          (fun_
             "j"
             (match v.Shape.content with
             | Shape.Structure [] ->
                 (* Hack for a unit type since empty records aren't yet valid *)
                 Syntax.unit ()
             | Shape.Structure s ->
                 record
                   (List.map
                      (fun mem ->
                        let location =
                          match mem.Structure.loc_name with
                          | Some name -> name
                          | None -> mem.Structure.name
                        in
                        ( mem.Structure.field_name
                        , (if mem.Structure.required
                          then
                            fun v ->
                            app1
                              (String.capitalize_ascii mem.Structure.shape ^ ".of_json")
                              (app1 "Aws.Util.of_option_exn" v)
                          else
                            fun v ->
                            app2
                              "Option.map"
                              (ident
                                 (String.capitalize_ascii mem.Structure.shape ^ ".of_json"))
                              v)
                            (app2 "Aws.Json.lookup" (ident "j") (str location)) ))
                      s)
             | Shape.List (shp, _, _flatten) ->
                 app2 "Aws.Json.to_list" (ident (shp ^ ".of_json")) (ident "j")
             | Shape.Map ((key_shp, _), (val_shp, _)) ->
                 app3
                   "Aws.Json.to_hashtbl"
                   (ident (key_shp ^ ".of_string"))
                   (ident (val_shp ^ ".of_json"))
                   (ident "j")
             | Shape.Enum _ ->
                 app1
                   "Aws.Util.of_option_exn"
                   (app2
                      "Aws.Util.list_find"
                      (ident "str_to_t")
                      (app1 "String.of_json" (ident "j"))))))
    in
    ( String.capitalize_ascii v.Shape.name
    , [ Syntax.ty_ ty ]
      @ extra
      @ List.filter_map
          (fun o -> o)
          [ make; Some parse; Some to_query; Some to_json; Some of_json ]
    , List.filter_map
        (fun o -> o)
        [ Some (Syntax.sty_ ty)
        ; Option.map Syntax.(sigval "make") tymake
        ; Some (Syntax.sigval "parse" typarse)
        ; Some (Syntax.sigval "to_query" tyto_query)
        ; Some (Syntax.sigval "to_json" tyto_json)
        ; Some (Syntax.sigval "of_json" tyof_json)
        ]
      @ tyextra
      |> Syntax.sig_ )
  in
  let header =
    [ Syntax.open_ "Aws.BaseTypes"
    ; Syntax.(tylet "calendar" (ty0 "CalendarLib.Calendar.t"))
    ]
  in
  scc shapes
  |> ListLabels.fold_left ~init:header ~f:(fun acc g ->
         match g with
         | `Rec group ->
             acc
             @ [ ListLabels.map group ~f:(fun m ->
                     let modname, m', sig_ = build_module m in
                     Syntax.module'_ modname m' sig_)
                 |> Syntax.rec_module_
               ]
         | `Nonrec m ->
             let modname, m', _ = build_module m in
             acc @ [ Syntax.module_ modname m' ])

let op service version target_prefix _shapes protocol op signature_version =
  let protocol =
    match protocol with
    | "query" | "rest-xml" | "ec2" -> `Query
    | "json" -> `Json
    | _ -> failwith (Format.sprintf "unsupported protocol: %s" protocol)
  in

  let open Syntax in
  let with_types shp fld = String.concat "." [ "Types"; shp; fld ] in
  let headers =
    list
      (match target_prefix with
      | Some _ -> [ tuple [ str "x-amz-target"; ident "target" ] ]
      | None -> [])
  in
  let default_params =
    list
      [ pair (str "Action") (list [ str op.Operation.name ])
      ; pair (str "Version") (list [ str version ])
      ]
  in
  let params, body =
    match op.Operation.input_shape with
    | None -> default_params, ident "None"
    | Some input_shape ->
        let params =
          app2
            "List.append"
            default_params
            (app1
               "Aws.Util.drop_empty"
               (app1
                  "Uri.query_of_encoded"
                  (app1
                     "Aws.Query.render"
                     (app1 (with_types input_shape "to_query") (ident "req")))))
        in
        let body =
          app1
            "Some"
            (app1
               "Aws.Json.to_string"
               (app1 (with_types input_shape "to_json") (ident "req")))
        in
        params, body
  in
  let to_body =
    letin
      "body"
      body
      (letin
         "uri"
         (app2
            "Uri.add_query_params"
            (app1
               "Uri.of_string"
               (app1
                  "Aws.Util.of_option_exn"
                  (app2 "Endpoints.url_of" (ident "service") (ident "region"))))
            params)
         (tuple [ variant op.Operation.http_meth; ident "uri"; headers; ident "body" ]))
  in
  (* XXX(serious): assume xml -> json by default  *)
  let of_body =
    match protocol with
    | `Query -> (
        match op.Operation.output_shape with
        | None -> variant1 "Ok" (ident "()")
        | Some shp ->
            tryfail
              (letin
                 "xml"
                 (app1 "Ezxmlm.from_string" (ident "body"))
                 (letin
                    "resp"
                    (let r =
                       app2
                         "Aws.Xml.member"
                         (str (op.Operation.name ^ "Response"))
                         (app1 "snd" (ident "xml"))
                     in
                     match op.Operation.output_wrapper with
                     | None -> r
                     | Some w -> app2 "Option.bind" r (app1 "Aws.Xml.member" (str w)))
                    (try_msg
                       "Aws.Xml.RequiredFieldMissing"
                       (app2
                          "Aws.Util.or_error"
                          (app2
                             "Option.bind"
                             (ident "resp")
                             (ident (with_types shp "parse")))
                          (letom
                             "Error"
                             (app1
                                "BadResponse"
                                (record
                                   [ "body", ident "body"
                                   ; ( "message"
                                     , str ("Could not find well formed " ^ shp ^ ".") )
                                   ]))))
                       (letom
                          "Error"
                          (variant1
                             "Error"
                             (app1
                                "BadResponse"
                                (record
                                   [ "body", ident "body"
                                   ; ( "message"
                                     , app2
                                         "^"
                                         (str
                                            ("Error parsing "
                                            ^ shp
                                            ^ " - missing field in body or children: "))
                                         (ident "msg") )
                                   ])))))))
              (variant1
                 "Error"
                 (letom
                    "Error"
                    (app1
                       "BadResponse"
                       (record
                          [ "body", ident "body"
                          ; "message", app2 "^" (str "Error parsing xml: ") (ident "msg")
                          ])))))
    | `Json -> (
        match op.Operation.output_shape with
        | None -> variant1 "Ok" (variant1 "Assoc" (list []))
        | Some shp ->
            try_msg
              "Aws.Json.Parse_error"
              (variant1 "Ok" (app1 "Aws.Json.of_string" (ident "body")))
              (letom
                 "Error"
                 (variant1
                    "Error"
                    (app1
                       "BadResponse"
                       (record
                          [ "body", ident "body"
                          ; ( "message"
                            , app2
                                "^"
                                (str
                                   ("Error parsing "
                                   ^ shp
                                   ^ " - missing field in body or children: "))
                                (ident "msg") )
                          ])))))
  in
  let op_error_parse =
    letin
      "errors"
      (app2
         "@"
         (list
            (List.map
               (fun name -> ident ("Errors_internal." ^ Util.to_variant_name name))
               op.Operation.errors))
         (ident "Errors_internal.common"))
      (matchoption
         (app1 "Errors_internal.of_string" (ident "err"))
         (ifthen
            (app2
               "&&"
               (app2 "List.mem" (ident "var") (ident "errors"))
               (matchoption
                  (app1 "Errors_internal.to_http_code" (ident "var"))
                  (app2 "=" (ident "var") (ident "code"))
                  (ident "true")))
            (app1 "Some" (ident "var"))
            (ident "None"))
         (ident "None"))
  in
  let unit = ty0 "unit" in
  let input_type =
    match op.Operation.input_shape with
    | Some i -> ty0 (with_types i "t")
    | None -> unit
  in
  let output_type =
    match protocol, op.Operation.output_shape with
    | `Query, Some shp -> ty0 (with_types shp "t")
    | `Query, None -> ty0 "unit"
    | `Json, _ -> ty0 "Aws.Json.t"
  in
  (* Tuple corresponding to (mli, ml) *)
  ( [ stylet "input" input_type
    ; stylet "output" output_type
    ; stylet "error" (ty0 "Errors_internal.t")
    ; sinclude_
        "Aws.Call"
        [ withty "input" "input"; withty "output" "output"; withty "error" "error" ]
    ]
  , [ open_ "Aws"
    ; tylet "input" input_type
    ; tylet "output" output_type
    ; tylet "error" (ty0 "Errors_internal.t")
    ; let_ "service" (str service)
    ]
    @ (match target_prefix with
      | Some target_prefix ->
          [ let_ "target" (str (String.concat "." [ target_prefix; op.Operation.name ])) ]
      | None -> [])
    @ [ let_
          "signature_version"
          (ident ("Request." ^ String.capitalize_ascii signature_version))
      ; let_ "to_http" (fun3 "service" "region" "req" to_body)
      ; let_ "of_http" (fun_ "body" of_body)
      ; let_ "parse_error" (fun2 "code" "err" op_error_parse)
      ] )

let errors errs common_errors =
  let errs =
    errs
    @ [ Error.
          { shape_name = "UninhabitedError"
          ; variant_name = "Uninhabited"
          ; string_name = "Uninhabited"
          ; http_code = None
          }
      ]
  in
  let open Syntax in
  [ tyvariantlet "t" (List.map (fun e -> e.Error.variant_name, []) errs)
  ; let_ "common" (list (List.map (fun e -> ident e.Error.variant_name) common_errors))
  ; let_
      "to_http_code"
      (fun_
         "e"
         (matchvar
            (ident "e")
            (List.map
               (fun e ->
                 ( e.Error.variant_name
                 , match e.Error.http_code with
                   | Some n -> app1 "Some" (int n)
                   | None -> ident "None" ))
               errs)))
  ; let_
      "to_string"
      (fun_
         "e"
         (matchvar
            (ident "e")
            (List.map (fun e -> e.Error.variant_name, str e.Error.string_name) errs)))
  ; let_
      "of_string"
      (fun_
         "e"
         (matchstrs
            (ident "e")
            (List.map
               (fun e -> e.Error.string_name, app1 "Some" (ident e.Error.variant_name))
               errs)
            (ident "None")))
  ]
