type t =
  [ `Assoc of (string * t) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of t list
  | `Null
  | `String of string
  ]

exception Casting_error of string * t

exception Parse_error of string

let to_yojson t = t

let of_yojson t = t

let to_list f = function
  | `List l -> List.map f l
  | t -> raise (Casting_error ("list", t))

let to_string t = Yojson.Basic.to_string t

let of_string str =
  try Yojson.Basic.from_string str with Yojson.Json_error err -> raise (Parse_error err)

let to_hashtbl key_f f = function
  | `Assoc m ->
      List.fold_left
        (fun acc (k, v) ->
          Hashtbl.add acc (key_f k) (f v);
          acc)
        (Hashtbl.create (List.length m))
        m
  | t -> raise (Casting_error ("map", t))

let lookup t s =
  try
    match t with
    | `Assoc l -> Some (List.assoc s l)
    | _ -> raise Not_found
  with Not_found -> None
