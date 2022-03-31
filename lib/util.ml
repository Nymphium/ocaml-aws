let drop_empty l = List.filter (fun (k, _) -> String.length (String.trim k) <> 0) l

let or_error v err =
  match v with
  | None -> `Error err
  | Some v -> `Ok v

let of_option default = function
  | None -> default
  | Some v -> v

let of_option_exn = function
  | Some v -> v
  | None -> failwith "Expected Some v, got None."

let list_find l key = List.find_map (fun (k, v) -> if key = k then Some v else None) l

let list_filter_opt l = List.filter_map (fun v -> v) l

let option_bind o f = Option.bind o f

let option_map v f = Option.map f v

let rec option_all = function
  | [] -> Some []
  | Some v :: xs -> Option.bind (option_all xs) (fun rest -> Some (v :: rest))
  | None :: _ -> None

let string_starts_with prefix s =
  let open String in
  let len_s = length s and len_pre = length prefix in
  let rec aux i =
    if i = len_pre
    then true
    else if unsafe_get s i <> unsafe_get prefix i
    then false
    else aux (i + 1)
  in
  len_s >= len_pre && aux 0
