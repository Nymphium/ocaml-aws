open Aws.BaseTypes
type calendar = CalendarLib.Calendar.t
module BinarySetAttributeValue =
  struct
    type t = Blob.t list
    let parse xml =
      Aws.Util.option_all
        (List.map Blob.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list Blob.to_query v
    let to_json v = `List (List.map Blob.to_json v)
    let of_json j = Aws.Json.to_list Blob.of_json j
  end
module NumberSetAttributeValue =
  struct
    type t = String.t list
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module StringSetAttributeValue =
  struct
    type t = String.t list
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module KeyType =
  struct
    type t =
      | HASH 
      | RANGE 
    let str_to_t = [("RANGE", RANGE); ("HASH", HASH)]
    let t_to_str = [(RANGE, "RANGE"); (HASH, "HASH")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let parse xml =
      Option.bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module rec
  AttributeValue:sig
                   type t =
                     {
                     s: String.t option ;
                     n: String.t option ;
                     b: Blob.t option ;
                     s_s: StringSetAttributeValue.t option ;
                     n_s: NumberSetAttributeValue.t option ;
                     b_s: BinarySetAttributeValue.t option ;
                     m: MapAttributeValue.t option ;
                     l: ListAttributeValue.t option ;
                     n_u_l_l: Boolean.t option ;
                     b_o_o_l: Boolean.t option }
                   val make :
                     ?s:String.t ->
                       ?n:String.t ->
                         ?b:Blob.t ->
                           ?s_s:StringSetAttributeValue.t ->
                             ?n_s:NumberSetAttributeValue.t ->
                               ?b_s:BinarySetAttributeValue.t ->
                                 ?m:MapAttributeValue.t ->
                                   ?l:ListAttributeValue.t ->
                                     ?n_u_l_l:Boolean.t ->
                                       ?b_o_o_l:Boolean.t -> unit -> t
                   val parse : Ezxmlm.nodes -> t option
                   val to_query : t -> Aws.Query.t
                   val to_json : t -> Aws.Json.t
                   val of_json : Aws.Json.t -> t
                 end =
  struct
    type t =
      {
      s: String.t option ;
      n: String.t option ;
      b: Blob.t option ;
      s_s: StringSetAttributeValue.t option ;
      n_s: NumberSetAttributeValue.t option ;
      b_s: BinarySetAttributeValue.t option ;
      m: MapAttributeValue.t option ;
      l: ListAttributeValue.t option ;
      n_u_l_l: Boolean.t option ;
      b_o_o_l: Boolean.t option }
    let make ?s  ?n  ?b  ?s_s  ?n_s  ?b_s  ?m  ?l  ?n_u_l_l  ?b_o_o_l  () =
      { s; n; b; s_s; n_s; b_s; m; l; n_u_l_l; b_o_o_l }
    let parse xml =
      Some
        {
          s = (Option.bind (Aws.Xml.member "S" xml) String.parse);
          n = (Option.bind (Aws.Xml.member "N" xml) String.parse);
          b = (Option.bind (Aws.Xml.member "B" xml) Blob.parse);
          s_s =
            (Option.bind (Aws.Xml.member "SS" xml)
               StringSetAttributeValue.parse);
          n_s =
            (Option.bind (Aws.Xml.member "NS" xml)
               NumberSetAttributeValue.parse);
          b_s =
            (Option.bind (Aws.Xml.member "BS" xml)
               BinarySetAttributeValue.parse);
          m = (Option.bind (Aws.Xml.member "M" xml) MapAttributeValue.parse);
          l = (Option.bind (Aws.Xml.member "L" xml) ListAttributeValue.parse);
          n_u_l_l = (Option.bind (Aws.Xml.member "NULL" xml) Boolean.parse);
          b_o_o_l = (Option.bind (Aws.Xml.member "BOOL" xml) Boolean.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f -> Aws.Query.Pair ("BOOL", (Boolean.to_query f)))
              v.b_o_o_l;
           Option.map
             (fun f -> Aws.Query.Pair ("NULL", (Boolean.to_query f)))
             v.n_u_l_l;
           Option.map
             (fun f ->
                Aws.Query.Pair ("L.member", (ListAttributeValue.to_query f)))
             v.l;
           Option.map
             (fun f -> Aws.Query.Pair ("M", (MapAttributeValue.to_query f)))
             v.m;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("BS.member", (BinarySetAttributeValue.to_query f))) v.b_s;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("NS.member", (NumberSetAttributeValue.to_query f))) v.n_s;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("SS.member", (StringSetAttributeValue.to_query f))) v.s_s;
           Option.map (fun f -> Aws.Query.Pair ("B", (Blob.to_query f))) v.b;
           Option.map (fun f -> Aws.Query.Pair ("N", (String.to_query f)))
             v.n;
           Option.map (fun f -> Aws.Query.Pair ("S", (String.to_query f)))
             v.s])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("BOOL", (Boolean.to_json f))) v.b_o_o_l;
           Option.map (fun f -> ("NULL", (Boolean.to_json f))) v.n_u_l_l;
           Option.map (fun f -> ("L", (ListAttributeValue.to_json f))) v.l;
           Option.map (fun f -> ("M", (MapAttributeValue.to_json f))) v.m;
           Option.map (fun f -> ("BS", (BinarySetAttributeValue.to_json f)))
             v.b_s;
           Option.map (fun f -> ("NS", (NumberSetAttributeValue.to_json f)))
             v.n_s;
           Option.map (fun f -> ("SS", (StringSetAttributeValue.to_json f)))
             v.s_s;
           Option.map (fun f -> ("B", (Blob.to_json f))) v.b;
           Option.map (fun f -> ("N", (String.to_json f))) v.n;
           Option.map (fun f -> ("S", (String.to_json f))) v.s])
    let of_json j =
      {
        s = (Option.map String.of_json (Aws.Json.lookup j "S"));
        n = (Option.map String.of_json (Aws.Json.lookup j "N"));
        b = (Option.map Blob.of_json (Aws.Json.lookup j "B"));
        s_s =
          (Option.map StringSetAttributeValue.of_json
             (Aws.Json.lookup j "SS"));
        n_s =
          (Option.map NumberSetAttributeValue.of_json
             (Aws.Json.lookup j "NS"));
        b_s =
          (Option.map BinarySetAttributeValue.of_json
             (Aws.Json.lookup j "BS"));
        m = (Option.map MapAttributeValue.of_json (Aws.Json.lookup j "M"));
        l = (Option.map ListAttributeValue.of_json (Aws.Json.lookup j "L"));
        n_u_l_l = (Option.map Boolean.of_json (Aws.Json.lookup j "NULL"));
        b_o_o_l = (Option.map Boolean.of_json (Aws.Json.lookup j "BOOL"))
      }
  end
 and
  ListAttributeValue:sig
                       type t = AttributeValue.t list
                       val parse : Ezxmlm.nodes -> t option
                       val to_query : t -> Aws.Query.t
                       val to_json : t -> Aws.Json.t
                       val of_json : Aws.Json.t -> t
                     end =
  struct
    type t = AttributeValue.t list
    let parse xml =
      Aws.Util.option_all
        (List.map AttributeValue.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list AttributeValue.to_query v
    let to_json v = `List (List.map AttributeValue.to_json v)
    let of_json j = Aws.Json.to_list AttributeValue.of_json j
  end and
       MapAttributeValue:sig
                           type t = (String.t, AttributeValue.t) Hashtbl.t
                           val parse : Ezxmlm.nodes -> t option
                           val to_query : t -> Aws.Query.t
                           val to_json : t -> Aws.Json.t
                           val of_json : Aws.Json.t -> t
                         end =
       struct
         type t = (String.t, AttributeValue.t) Hashtbl.t
         let parse xml = None
         let to_query v =
           Aws.Query.to_query_hashtbl String.to_string
             AttributeValue.to_query v
         let to_json v =
           `Assoc
             (Hashtbl.fold
                (fun k ->
                   fun v ->
                     fun acc ->
                       ((String.to_string k), (AttributeValue.to_json v)) ::
                       acc) v [])
         let of_json j =
           Aws.Json.to_hashtbl String.of_string AttributeValue.of_json j
       end
module KeySchemaElement =
  struct
    type t = {
      attribute_name: String.t ;
      key_type: KeyType.t }
    let make ~attribute_name  ~key_type  () = { attribute_name; key_type }
    let parse xml =
      Some
        {
          attribute_name =
            (Aws.Xml.required "AttributeName"
               (Option.bind (Aws.Xml.member "AttributeName" xml) String.parse));
          key_type =
            (Aws.Xml.required "KeyType"
               (Option.bind (Aws.Xml.member "KeyType" xml) KeyType.parse))
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Some (Aws.Query.Pair ("KeyType", (KeyType.to_query v.key_type)));
           Some
             (Aws.Query.Pair
                ("AttributeName", (String.to_query v.attribute_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Some ("KeyType", (KeyType.to_json v.key_type));
           Some ("AttributeName", (String.to_json v.attribute_name))])
    let of_json j =
      {
        attribute_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeName")));
        key_type =
          (KeyType.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyType")))
      }
  end
module NonKeyAttributeNameList =
  struct
    type t = String.t list
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module ProjectionType =
  struct
    type t =
      | ALL 
      | KEYS_ONLY 
      | INCLUDE 
    let str_to_t =
      [("INCLUDE", INCLUDE); ("KEYS_ONLY", KEYS_ONLY); ("ALL", ALL)]
    let t_to_str =
      [(INCLUDE, "INCLUDE"); (KEYS_ONLY, "KEYS_ONLY"); (ALL, "ALL")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let parse xml =
      Option.bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module Key =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module PutItemInputAttributeMap =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module ScalarAttributeType =
  struct
    type t =
      | S 
      | N 
      | B 
    let str_to_t = [("B", B); ("N", N); ("S", S)]
    let t_to_str = [(B, "B"); (N, "N"); (S, "S")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let parse xml =
      Option.bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module IndexStatus =
  struct
    type t =
      | CREATING 
      | UPDATING 
      | DELETING 
      | ACTIVE 
    let str_to_t =
      [("ACTIVE", ACTIVE);
      ("DELETING", DELETING);
      ("UPDATING", UPDATING);
      ("CREATING", CREATING)]
    let t_to_str =
      [(ACTIVE, "ACTIVE");
      (DELETING, "DELETING");
      (UPDATING, "UPDATING");
      (CREATING, "CREATING")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let parse xml =
      Option.bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module KeySchema =
  struct
    type t = KeySchemaElement.t list
    let parse xml =
      Aws.Util.option_all
        (List.map KeySchemaElement.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list KeySchemaElement.to_query v
    let to_json v = `List (List.map KeySchemaElement.to_json v)
    let of_json j = Aws.Json.to_list KeySchemaElement.of_json j
  end
module Projection =
  struct
    type t =
      {
      projection_type: ProjectionType.t option ;
      non_key_attributes: NonKeyAttributeNameList.t option }
    let make ?projection_type  ?non_key_attributes  () =
      { projection_type; non_key_attributes }
    let parse xml =
      Some
        {
          projection_type =
            (Option.bind (Aws.Xml.member "ProjectionType" xml)
               ProjectionType.parse);
          non_key_attributes =
            (Option.bind (Aws.Xml.member "NonKeyAttributes" xml)
               NonKeyAttributeNameList.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("NonKeyAttributes.member",
                     (NonKeyAttributeNameList.to_query f)))
              v.non_key_attributes;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ProjectionType", (ProjectionType.to_query f)))
             v.projection_type])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("NonKeyAttributes", (NonKeyAttributeNameList.to_json f)))
              v.non_key_attributes;
           Option.map
             (fun f -> ("ProjectionType", (ProjectionType.to_json f)))
             v.projection_type])
    let of_json j =
      {
        projection_type =
          (Option.map ProjectionType.of_json
             (Aws.Json.lookup j "ProjectionType"));
        non_key_attributes =
          (Option.map NonKeyAttributeNameList.of_json
             (Aws.Json.lookup j "NonKeyAttributes"))
      }
  end
module ProvisionedThroughputDescription =
  struct
    type t =
      {
      last_increase_date_time: DateTime.t option ;
      last_decrease_date_time: DateTime.t option ;
      number_of_decreases_today: Long.t option ;
      read_capacity_units: Long.t option ;
      write_capacity_units: Long.t option }
    let make ?last_increase_date_time  ?last_decrease_date_time 
      ?number_of_decreases_today  ?read_capacity_units  ?write_capacity_units
       () =
      {
        last_increase_date_time;
        last_decrease_date_time;
        number_of_decreases_today;
        read_capacity_units;
        write_capacity_units
      }
    let parse xml =
      Some
        {
          last_increase_date_time =
            (Option.bind (Aws.Xml.member "LastIncreaseDateTime" xml)
               DateTime.parse);
          last_decrease_date_time =
            (Option.bind (Aws.Xml.member "LastDecreaseDateTime" xml)
               DateTime.parse);
          number_of_decreases_today =
            (Option.bind (Aws.Xml.member "NumberOfDecreasesToday" xml)
               Long.parse);
          read_capacity_units =
            (Option.bind (Aws.Xml.member "ReadCapacityUnits" xml) Long.parse);
          write_capacity_units =
            (Option.bind (Aws.Xml.member "WriteCapacityUnits" xml) Long.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair ("WriteCapacityUnits", (Long.to_query f)))
              v.write_capacity_units;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ReadCapacityUnits", (Long.to_query f)))
             v.read_capacity_units;
           Option.map
             (fun f ->
                Aws.Query.Pair ("NumberOfDecreasesToday", (Long.to_query f)))
             v.number_of_decreases_today;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("LastDecreaseDateTime", (DateTime.to_query f)))
             v.last_decrease_date_time;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("LastIncreaseDateTime", (DateTime.to_query f)))
             v.last_increase_date_time])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("WriteCapacityUnits", (Long.to_json f)))
              v.write_capacity_units;
           Option.map (fun f -> ("ReadCapacityUnits", (Long.to_json f)))
             v.read_capacity_units;
           Option.map (fun f -> ("NumberOfDecreasesToday", (Long.to_json f)))
             v.number_of_decreases_today;
           Option.map
             (fun f -> ("LastDecreaseDateTime", (DateTime.to_json f)))
             v.last_decrease_date_time;
           Option.map
             (fun f -> ("LastIncreaseDateTime", (DateTime.to_json f)))
             v.last_increase_date_time])
    let of_json j =
      {
        last_increase_date_time =
          (Option.map DateTime.of_json
             (Aws.Json.lookup j "LastIncreaseDateTime"));
        last_decrease_date_time =
          (Option.map DateTime.of_json
             (Aws.Json.lookup j "LastDecreaseDateTime"));
        number_of_decreases_today =
          (Option.map Long.of_json
             (Aws.Json.lookup j "NumberOfDecreasesToday"));
        read_capacity_units =
          (Option.map Long.of_json (Aws.Json.lookup j "ReadCapacityUnits"));
        write_capacity_units =
          (Option.map Long.of_json (Aws.Json.lookup j "WriteCapacityUnits"))
      }
  end
module ProvisionedThroughput =
  struct
    type t = {
      read_capacity_units: Long.t ;
      write_capacity_units: Long.t }
    let make ~read_capacity_units  ~write_capacity_units  () =
      { read_capacity_units; write_capacity_units }
    let parse xml =
      Some
        {
          read_capacity_units =
            (Aws.Xml.required "ReadCapacityUnits"
               (Option.bind (Aws.Xml.member "ReadCapacityUnits" xml)
                  Long.parse));
          write_capacity_units =
            (Aws.Xml.required "WriteCapacityUnits"
               (Option.bind (Aws.Xml.member "WriteCapacityUnits" xml)
                  Long.parse))
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Some
              (Aws.Query.Pair
                 ("WriteCapacityUnits",
                   (Long.to_query v.write_capacity_units)));
           Some
             (Aws.Query.Pair
                ("ReadCapacityUnits", (Long.to_query v.read_capacity_units)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Some
              ("WriteCapacityUnits", (Long.to_json v.write_capacity_units));
           Some ("ReadCapacityUnits", (Long.to_json v.read_capacity_units))])
    let of_json j =
      {
        read_capacity_units =
          (Long.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "ReadCapacityUnits")));
        write_capacity_units =
          (Long.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "WriteCapacityUnits")))
      }
  end
module DeleteRequest =
  struct
    type t = {
      key: Key.t }
    let make ~key  () = { key }
    let parse xml =
      Some
        {
          key =
            (Aws.Xml.required "Key"
               (Option.bind (Aws.Xml.member "Key" xml) Key.parse))
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Some (Aws.Query.Pair ("Key", (Key.to_query v.key)))])
    let to_json v =
      `Assoc (List.filter_map Fun.id [Some ("Key", (Key.to_json v.key))])
    let of_json j =
      {
        key =
          (Key.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")))
      }
  end
module PutRequest =
  struct
    type t = {
      item: PutItemInputAttributeMap.t }
    let make ~item  () = { item }
    let parse xml =
      Some
        {
          item =
            (Aws.Xml.required "Item"
               (Option.bind (Aws.Xml.member "Item" xml)
                  PutItemInputAttributeMap.parse))
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Some
              (Aws.Query.Pair
                 ("Item", (PutItemInputAttributeMap.to_query v.item)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Some ("Item", (PutItemInputAttributeMap.to_json v.item))])
    let of_json j =
      {
        item =
          (PutItemInputAttributeMap.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Item")))
      }
  end
module Capacity =
  struct
    type t = {
      capacity_units: Double.t option }
    let make ?capacity_units  () = { capacity_units }
    let parse xml =
      Some
        {
          capacity_units =
            (Option.bind (Aws.Xml.member "CapacityUnits" xml) Double.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f -> Aws.Query.Pair ("CapacityUnits", (Double.to_query f)))
              v.capacity_units])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("CapacityUnits", (Double.to_json f)))
              v.capacity_units])
    let of_json j =
      {
        capacity_units =
          (Option.map Double.of_json (Aws.Json.lookup j "CapacityUnits"))
      }
  end
module ItemCollectionKeyAttributeMap =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module ItemCollectionSizeEstimateRange =
  struct
    type t = Double.t list
    let parse xml =
      Aws.Util.option_all
        (List.map Double.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list Double.to_query v
    let to_json v = `List (List.map Double.to_json v)
    let of_json j = Aws.Json.to_list Double.of_json j
  end
module AttributeDefinition =
  struct
    type t =
      {
      attribute_name: String.t ;
      attribute_type: ScalarAttributeType.t }
    let make ~attribute_name  ~attribute_type  () =
      { attribute_name; attribute_type }
    let parse xml =
      Some
        {
          attribute_name =
            (Aws.Xml.required "AttributeName"
               (Option.bind (Aws.Xml.member "AttributeName" xml) String.parse));
          attribute_type =
            (Aws.Xml.required "AttributeType"
               (Option.bind (Aws.Xml.member "AttributeType" xml)
                  ScalarAttributeType.parse))
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Some
              (Aws.Query.Pair
                 ("AttributeType",
                   (ScalarAttributeType.to_query v.attribute_type)));
           Some
             (Aws.Query.Pair
                ("AttributeName", (String.to_query v.attribute_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Some
              ("AttributeType",
                (ScalarAttributeType.to_json v.attribute_type));
           Some ("AttributeName", (String.to_json v.attribute_name))])
    let of_json j =
      {
        attribute_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeName")));
        attribute_type =
          (ScalarAttributeType.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AttributeType")))
      }
  end
module GlobalSecondaryIndexDescription =
  struct
    type t =
      {
      index_name: String.t option ;
      key_schema: KeySchema.t option ;
      projection: Projection.t option ;
      index_status: IndexStatus.t option ;
      backfilling: Boolean.t option ;
      provisioned_throughput: ProvisionedThroughputDescription.t option ;
      index_size_bytes: Long.t option ;
      item_count: Long.t option ;
      index_arn: String.t option }
    let make ?index_name  ?key_schema  ?projection  ?index_status 
      ?backfilling  ?provisioned_throughput  ?index_size_bytes  ?item_count 
      ?index_arn  () =
      {
        index_name;
        key_schema;
        projection;
        index_status;
        backfilling;
        provisioned_throughput;
        index_size_bytes;
        item_count;
        index_arn
      }
    let parse xml =
      Some
        {
          index_name =
            (Option.bind (Aws.Xml.member "IndexName" xml) String.parse);
          key_schema =
            (Option.bind (Aws.Xml.member "KeySchema" xml) KeySchema.parse);
          projection =
            (Option.bind (Aws.Xml.member "Projection" xml) Projection.parse);
          index_status =
            (Option.bind (Aws.Xml.member "IndexStatus" xml) IndexStatus.parse);
          backfilling =
            (Option.bind (Aws.Xml.member "Backfilling" xml) Boolean.parse);
          provisioned_throughput =
            (Option.bind (Aws.Xml.member "ProvisionedThroughput" xml)
               ProvisionedThroughputDescription.parse);
          index_size_bytes =
            (Option.bind (Aws.Xml.member "IndexSizeBytes" xml) Long.parse);
          item_count =
            (Option.bind (Aws.Xml.member "ItemCount" xml) Long.parse);
          index_arn =
            (Option.bind (Aws.Xml.member "IndexArn" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f -> Aws.Query.Pair ("IndexArn", (String.to_query f)))
              v.index_arn;
           Option.map
             (fun f -> Aws.Query.Pair ("ItemCount", (Long.to_query f)))
             v.item_count;
           Option.map
             (fun f -> Aws.Query.Pair ("IndexSizeBytes", (Long.to_query f)))
             v.index_size_bytes;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ProvisionedThroughput",
                    (ProvisionedThroughputDescription.to_query f)))
             v.provisioned_throughput;
           Option.map
             (fun f -> Aws.Query.Pair ("Backfilling", (Boolean.to_query f)))
             v.backfilling;
           Option.map
             (fun f ->
                Aws.Query.Pair ("IndexStatus", (IndexStatus.to_query f)))
             v.index_status;
           Option.map
             (fun f -> Aws.Query.Pair ("Projection", (Projection.to_query f)))
             v.projection;
           Option.map
             (fun f ->
                Aws.Query.Pair ("KeySchema.member", (KeySchema.to_query f)))
             v.key_schema;
           Option.map
             (fun f -> Aws.Query.Pair ("IndexName", (String.to_query f)))
             v.index_name])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("IndexArn", (String.to_json f)))
              v.index_arn;
           Option.map (fun f -> ("ItemCount", (Long.to_json f))) v.item_count;
           Option.map (fun f -> ("IndexSizeBytes", (Long.to_json f)))
             v.index_size_bytes;
           Option.map
             (fun f ->
                ("ProvisionedThroughput",
                  (ProvisionedThroughputDescription.to_json f)))
             v.provisioned_throughput;
           Option.map (fun f -> ("Backfilling", (Boolean.to_json f)))
             v.backfilling;
           Option.map (fun f -> ("IndexStatus", (IndexStatus.to_json f)))
             v.index_status;
           Option.map (fun f -> ("Projection", (Projection.to_json f)))
             v.projection;
           Option.map (fun f -> ("KeySchema", (KeySchema.to_json f)))
             v.key_schema;
           Option.map (fun f -> ("IndexName", (String.to_json f)))
             v.index_name])
    let of_json j =
      {
        index_name =
          (Option.map String.of_json (Aws.Json.lookup j "IndexName"));
        key_schema =
          (Option.map KeySchema.of_json (Aws.Json.lookup j "KeySchema"));
        projection =
          (Option.map Projection.of_json (Aws.Json.lookup j "Projection"));
        index_status =
          (Option.map IndexStatus.of_json (Aws.Json.lookup j "IndexStatus"));
        backfilling =
          (Option.map Boolean.of_json (Aws.Json.lookup j "Backfilling"));
        provisioned_throughput =
          (Option.map ProvisionedThroughputDescription.of_json
             (Aws.Json.lookup j "ProvisionedThroughput"));
        index_size_bytes =
          (Option.map Long.of_json (Aws.Json.lookup j "IndexSizeBytes"));
        item_count =
          (Option.map Long.of_json (Aws.Json.lookup j "ItemCount"));
        index_arn =
          (Option.map String.of_json (Aws.Json.lookup j "IndexArn"))
      }
  end
module LocalSecondaryIndexDescription =
  struct
    type t =
      {
      index_name: String.t option ;
      key_schema: KeySchema.t option ;
      projection: Projection.t option ;
      index_size_bytes: Long.t option ;
      item_count: Long.t option ;
      index_arn: String.t option }
    let make ?index_name  ?key_schema  ?projection  ?index_size_bytes 
      ?item_count  ?index_arn  () =
      {
        index_name;
        key_schema;
        projection;
        index_size_bytes;
        item_count;
        index_arn
      }
    let parse xml =
      Some
        {
          index_name =
            (Option.bind (Aws.Xml.member "IndexName" xml) String.parse);
          key_schema =
            (Option.bind (Aws.Xml.member "KeySchema" xml) KeySchema.parse);
          projection =
            (Option.bind (Aws.Xml.member "Projection" xml) Projection.parse);
          index_size_bytes =
            (Option.bind (Aws.Xml.member "IndexSizeBytes" xml) Long.parse);
          item_count =
            (Option.bind (Aws.Xml.member "ItemCount" xml) Long.parse);
          index_arn =
            (Option.bind (Aws.Xml.member "IndexArn" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f -> Aws.Query.Pair ("IndexArn", (String.to_query f)))
              v.index_arn;
           Option.map
             (fun f -> Aws.Query.Pair ("ItemCount", (Long.to_query f)))
             v.item_count;
           Option.map
             (fun f -> Aws.Query.Pair ("IndexSizeBytes", (Long.to_query f)))
             v.index_size_bytes;
           Option.map
             (fun f -> Aws.Query.Pair ("Projection", (Projection.to_query f)))
             v.projection;
           Option.map
             (fun f ->
                Aws.Query.Pair ("KeySchema.member", (KeySchema.to_query f)))
             v.key_schema;
           Option.map
             (fun f -> Aws.Query.Pair ("IndexName", (String.to_query f)))
             v.index_name])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("IndexArn", (String.to_json f)))
              v.index_arn;
           Option.map (fun f -> ("ItemCount", (Long.to_json f))) v.item_count;
           Option.map (fun f -> ("IndexSizeBytes", (Long.to_json f)))
             v.index_size_bytes;
           Option.map (fun f -> ("Projection", (Projection.to_json f)))
             v.projection;
           Option.map (fun f -> ("KeySchema", (KeySchema.to_json f)))
             v.key_schema;
           Option.map (fun f -> ("IndexName", (String.to_json f)))
             v.index_name])
    let of_json j =
      {
        index_name =
          (Option.map String.of_json (Aws.Json.lookup j "IndexName"));
        key_schema =
          (Option.map KeySchema.of_json (Aws.Json.lookup j "KeySchema"));
        projection =
          (Option.map Projection.of_json (Aws.Json.lookup j "Projection"));
        index_size_bytes =
          (Option.map Long.of_json (Aws.Json.lookup j "IndexSizeBytes"));
        item_count =
          (Option.map Long.of_json (Aws.Json.lookup j "ItemCount"));
        index_arn =
          (Option.map String.of_json (Aws.Json.lookup j "IndexArn"))
      }
  end
module StreamViewType =
  struct
    type t =
      | NEW_IMAGE 
      | OLD_IMAGE 
      | NEW_AND_OLD_IMAGES 
      | KEYS_ONLY 
    let str_to_t =
      [("KEYS_ONLY", KEYS_ONLY);
      ("NEW_AND_OLD_IMAGES", NEW_AND_OLD_IMAGES);
      ("OLD_IMAGE", OLD_IMAGE);
      ("NEW_IMAGE", NEW_IMAGE)]
    let t_to_str =
      [(KEYS_ONLY, "KEYS_ONLY");
      (NEW_AND_OLD_IMAGES, "NEW_AND_OLD_IMAGES");
      (OLD_IMAGE, "OLD_IMAGE");
      (NEW_IMAGE, "NEW_IMAGE")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let parse xml =
      Option.bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module CreateGlobalSecondaryIndexAction =
  struct
    type t =
      {
      index_name: String.t ;
      key_schema: KeySchema.t ;
      projection: Projection.t ;
      provisioned_throughput: ProvisionedThroughput.t }
    let make ~index_name  ~key_schema  ~projection  ~provisioned_throughput 
      () = { index_name; key_schema; projection; provisioned_throughput }
    let parse xml =
      Some
        {
          index_name =
            (Aws.Xml.required "IndexName"
               (Option.bind (Aws.Xml.member "IndexName" xml) String.parse));
          key_schema =
            (Aws.Xml.required "KeySchema"
               (Option.bind (Aws.Xml.member "KeySchema" xml) KeySchema.parse));
          projection =
            (Aws.Xml.required "Projection"
               (Option.bind (Aws.Xml.member "Projection" xml)
                  Projection.parse));
          provisioned_throughput =
            (Aws.Xml.required "ProvisionedThroughput"
               (Option.bind (Aws.Xml.member "ProvisionedThroughput" xml)
                  ProvisionedThroughput.parse))
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Some
              (Aws.Query.Pair
                 ("ProvisionedThroughput",
                   (ProvisionedThroughput.to_query v.provisioned_throughput)));
           Some
             (Aws.Query.Pair
                ("Projection", (Projection.to_query v.projection)));
           Some
             (Aws.Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Some
             (Aws.Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Some
              ("ProvisionedThroughput",
                (ProvisionedThroughput.to_json v.provisioned_throughput));
           Some ("Projection", (Projection.to_json v.projection));
           Some ("KeySchema", (KeySchema.to_json v.key_schema));
           Some ("IndexName", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "IndexName")));
        key_schema =
          (KeySchema.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeySchema")));
        projection =
          (Projection.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Projection")));
        provisioned_throughput =
          (ProvisionedThroughput.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "ProvisionedThroughput")))
      }
  end
module DeleteGlobalSecondaryIndexAction =
  struct
    type t = {
      index_name: String.t }
    let make ~index_name  () = { index_name }
    let parse xml =
      Some
        {
          index_name =
            (Aws.Xml.required "IndexName"
               (Option.bind (Aws.Xml.member "IndexName" xml) String.parse))
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Some
              (Aws.Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Some ("IndexName", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "IndexName")))
      }
  end
module UpdateGlobalSecondaryIndexAction =
  struct
    type t =
      {
      index_name: String.t ;
      provisioned_throughput: ProvisionedThroughput.t }
    let make ~index_name  ~provisioned_throughput  () =
      { index_name; provisioned_throughput }
    let parse xml =
      Some
        {
          index_name =
            (Aws.Xml.required "IndexName"
               (Option.bind (Aws.Xml.member "IndexName" xml) String.parse));
          provisioned_throughput =
            (Aws.Xml.required "ProvisionedThroughput"
               (Option.bind (Aws.Xml.member "ProvisionedThroughput" xml)
                  ProvisionedThroughput.parse))
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Some
              (Aws.Query.Pair
                 ("ProvisionedThroughput",
                   (ProvisionedThroughput.to_query v.provisioned_throughput)));
           Some
             (Aws.Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Some
              ("ProvisionedThroughput",
                (ProvisionedThroughput.to_json v.provisioned_throughput));
           Some ("IndexName", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "IndexName")));
        provisioned_throughput =
          (ProvisionedThroughput.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "ProvisionedThroughput")))
      }
  end
module AttributeAction =
  struct
    type t =
      | ADD 
      | PUT 
      | DELETE 
    let str_to_t = [("DELETE", DELETE); ("PUT", PUT); ("ADD", ADD)]
    let t_to_str = [(DELETE, "DELETE"); (PUT, "PUT"); (ADD, "ADD")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let parse xml =
      Option.bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module AttributeValueList =
  struct
    type t = AttributeValue.t list
    let parse xml =
      Aws.Util.option_all
        (List.map AttributeValue.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list AttributeValue.to_query v
    let to_json v = `List (List.map AttributeValue.to_json v)
    let of_json j = Aws.Json.to_list AttributeValue.of_json j
  end
module ComparisonOperator =
  struct
    type t =
      | EQ 
      | NE 
      | IN 
      | LE 
      | LT 
      | GE 
      | GT 
      | BETWEEN 
      | NOT_NULL 
      | NULL 
      | CONTAINS 
      | NOT_CONTAINS 
      | BEGINS_WITH 
    let str_to_t =
      [("BEGINS_WITH", BEGINS_WITH);
      ("NOT_CONTAINS", NOT_CONTAINS);
      ("CONTAINS", CONTAINS);
      ("NULL", NULL);
      ("NOT_NULL", NOT_NULL);
      ("BETWEEN", BETWEEN);
      ("GT", GT);
      ("GE", GE);
      ("LT", LT);
      ("LE", LE);
      ("IN", IN);
      ("NE", NE);
      ("EQ", EQ)]
    let t_to_str =
      [(BEGINS_WITH, "BEGINS_WITH");
      (NOT_CONTAINS, "NOT_CONTAINS");
      (CONTAINS, "CONTAINS");
      (NULL, "NULL");
      (NOT_NULL, "NOT_NULL");
      (BETWEEN, "BETWEEN");
      (GT, "GT");
      (GE, "GE");
      (LT, "LT");
      (LE, "LE");
      (IN, "IN");
      (NE, "NE");
      (EQ, "EQ")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let parse xml =
      Option.bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module WriteRequest =
  struct
    type t =
      {
      put_request: PutRequest.t option ;
      delete_request: DeleteRequest.t option }
    let make ?put_request  ?delete_request  () =
      { put_request; delete_request }
    let parse xml =
      Some
        {
          put_request =
            (Option.bind (Aws.Xml.member "PutRequest" xml) PutRequest.parse);
          delete_request =
            (Option.bind (Aws.Xml.member "DeleteRequest" xml)
               DeleteRequest.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair ("DeleteRequest", (DeleteRequest.to_query f)))
              v.delete_request;
           Option.map
             (fun f -> Aws.Query.Pair ("PutRequest", (PutRequest.to_query f)))
             v.put_request])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f -> ("DeleteRequest", (DeleteRequest.to_json f)))
              v.delete_request;
           Option.map (fun f -> ("PutRequest", (PutRequest.to_json f)))
             v.put_request])
    let of_json j =
      {
        put_request =
          (Option.map PutRequest.of_json (Aws.Json.lookup j "PutRequest"));
        delete_request =
          (Option.map DeleteRequest.of_json
             (Aws.Json.lookup j "DeleteRequest"))
      }
  end
module SecondaryIndexesCapacityMap =
  struct
    type t = (String.t, Capacity.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string Capacity.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (Capacity.to_json v)) ::
                  acc) v [])
    let of_json j = Aws.Json.to_hashtbl String.of_string Capacity.of_json j
  end
module ItemCollectionMetrics =
  struct
    type t =
      {
      item_collection_key: ItemCollectionKeyAttributeMap.t option ;
      size_estimate_range_g_b: ItemCollectionSizeEstimateRange.t option }
    let make ?item_collection_key  ?size_estimate_range_g_b  () =
      { item_collection_key; size_estimate_range_g_b }
    let parse xml =
      Some
        {
          item_collection_key =
            (Option.bind (Aws.Xml.member "ItemCollectionKey" xml)
               ItemCollectionKeyAttributeMap.parse);
          size_estimate_range_g_b =
            (Option.bind (Aws.Xml.member "SizeEstimateRangeGB" xml)
               ItemCollectionSizeEstimateRange.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("SizeEstimateRangeGB.member",
                     (ItemCollectionSizeEstimateRange.to_query f)))
              v.size_estimate_range_g_b;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ItemCollectionKey",
                    (ItemCollectionKeyAttributeMap.to_query f)))
             v.item_collection_key])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("SizeEstimateRangeGB",
                   (ItemCollectionSizeEstimateRange.to_json f)))
              v.size_estimate_range_g_b;
           Option.map
             (fun f ->
                ("ItemCollectionKey",
                  (ItemCollectionKeyAttributeMap.to_json f)))
             v.item_collection_key])
    let of_json j =
      {
        item_collection_key =
          (Option.map ItemCollectionKeyAttributeMap.of_json
             (Aws.Json.lookup j "ItemCollectionKey"));
        size_estimate_range_g_b =
          (Option.map ItemCollectionSizeEstimateRange.of_json
             (Aws.Json.lookup j "SizeEstimateRangeGB"))
      }
  end
module AttributeNameList =
  struct
    type t = String.t list
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module ExpressionAttributeNameMap =
  struct
    type t = (String.t, String.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string String.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (String.to_json v)) :: acc)
           v [])
    let of_json j = Aws.Json.to_hashtbl String.of_string String.of_json j
  end
module KeyList =
  struct
    type t = Key.t list
    let parse xml =
      Aws.Util.option_all (List.map Key.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list Key.to_query v
    let to_json v = `List (List.map Key.to_json v)
    let of_json j = Aws.Json.to_list Key.of_json j
  end
module AttributeMap =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module AttributeDefinitions =
  struct
    type t = AttributeDefinition.t list
    let parse xml =
      Aws.Util.option_all
        (List.map AttributeDefinition.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list AttributeDefinition.to_query v
    let to_json v = `List (List.map AttributeDefinition.to_json v)
    let of_json j = Aws.Json.to_list AttributeDefinition.of_json j
  end
module GlobalSecondaryIndexDescriptionList =
  struct
    type t = GlobalSecondaryIndexDescription.t list
    let parse xml =
      Aws.Util.option_all
        (List.map GlobalSecondaryIndexDescription.parse
           (Aws.Xml.members "member" xml))
    let to_query v =
      Aws.Query.to_query_list GlobalSecondaryIndexDescription.to_query v
    let to_json v =
      `List (List.map GlobalSecondaryIndexDescription.to_json v)
    let of_json j =
      Aws.Json.to_list GlobalSecondaryIndexDescription.of_json j
  end
module LocalSecondaryIndexDescriptionList =
  struct
    type t = LocalSecondaryIndexDescription.t list
    let parse xml =
      Aws.Util.option_all
        (List.map LocalSecondaryIndexDescription.parse
           (Aws.Xml.members "member" xml))
    let to_query v =
      Aws.Query.to_query_list LocalSecondaryIndexDescription.to_query v
    let to_json v = `List (List.map LocalSecondaryIndexDescription.to_json v)
    let of_json j = Aws.Json.to_list LocalSecondaryIndexDescription.of_json j
  end
module StreamSpecification =
  struct
    type t =
      {
      stream_enabled: Boolean.t option ;
      stream_view_type: StreamViewType.t option }
    let make ?stream_enabled  ?stream_view_type  () =
      { stream_enabled; stream_view_type }
    let parse xml =
      Some
        {
          stream_enabled =
            (Option.bind (Aws.Xml.member "StreamEnabled" xml) Boolean.parse);
          stream_view_type =
            (Option.bind (Aws.Xml.member "StreamViewType" xml)
               StreamViewType.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("StreamViewType", (StreamViewType.to_query f)))
              v.stream_view_type;
           Option.map
             (fun f -> Aws.Query.Pair ("StreamEnabled", (Boolean.to_query f)))
             v.stream_enabled])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f -> ("StreamViewType", (StreamViewType.to_json f)))
              v.stream_view_type;
           Option.map (fun f -> ("StreamEnabled", (Boolean.to_json f)))
             v.stream_enabled])
    let of_json j =
      {
        stream_enabled =
          (Option.map Boolean.of_json (Aws.Json.lookup j "StreamEnabled"));
        stream_view_type =
          (Option.map StreamViewType.of_json
             (Aws.Json.lookup j "StreamViewType"))
      }
  end
module TableStatus =
  struct
    type t =
      | CREATING 
      | UPDATING 
      | DELETING 
      | ACTIVE 
    let str_to_t =
      [("ACTIVE", ACTIVE);
      ("DELETING", DELETING);
      ("UPDATING", UPDATING);
      ("CREATING", CREATING)]
    let t_to_str =
      [(ACTIVE, "ACTIVE");
      (DELETING, "DELETING");
      (UPDATING, "UPDATING");
      (CREATING, "CREATING")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let parse xml =
      Option.bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module GlobalSecondaryIndexUpdate =
  struct
    type t =
      {
      update: UpdateGlobalSecondaryIndexAction.t option ;
      create: CreateGlobalSecondaryIndexAction.t option ;
      delete: DeleteGlobalSecondaryIndexAction.t option }
    let make ?update  ?create  ?delete  () = { update; create; delete }
    let parse xml =
      Some
        {
          update =
            (Option.bind (Aws.Xml.member "Update" xml)
               UpdateGlobalSecondaryIndexAction.parse);
          create =
            (Option.bind (Aws.Xml.member "Create" xml)
               CreateGlobalSecondaryIndexAction.parse);
          delete =
            (Option.bind (Aws.Xml.member "Delete" xml)
               DeleteGlobalSecondaryIndexAction.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("Delete", (DeleteGlobalSecondaryIndexAction.to_query f)))
              v.delete;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("Create", (CreateGlobalSecondaryIndexAction.to_query f)))
             v.create;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("Update", (UpdateGlobalSecondaryIndexAction.to_query f)))
             v.update])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("Delete", (DeleteGlobalSecondaryIndexAction.to_json f)))
              v.delete;
           Option.map
             (fun f ->
                ("Create", (CreateGlobalSecondaryIndexAction.to_json f)))
             v.create;
           Option.map
             (fun f ->
                ("Update", (UpdateGlobalSecondaryIndexAction.to_json f)))
             v.update])
    let of_json j =
      {
        update =
          (Option.map UpdateGlobalSecondaryIndexAction.of_json
             (Aws.Json.lookup j "Update"));
        create =
          (Option.map CreateGlobalSecondaryIndexAction.of_json
             (Aws.Json.lookup j "Create"));
        delete =
          (Option.map DeleteGlobalSecondaryIndexAction.of_json
             (Aws.Json.lookup j "Delete"))
      }
  end
module AttributeValueUpdate =
  struct
    type t =
      {
      value: AttributeValue.t option ;
      action: AttributeAction.t option }
    let make ?value  ?action  () = { value; action }
    let parse xml =
      Some
        {
          value =
            (Option.bind (Aws.Xml.member "Value" xml) AttributeValue.parse);
          action =
            (Option.bind (Aws.Xml.member "Action" xml) AttributeAction.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair ("Action", (AttributeAction.to_query f)))
              v.action;
           Option.map
             (fun f -> Aws.Query.Pair ("Value", (AttributeValue.to_query f)))
             v.value])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("Action", (AttributeAction.to_json f)))
              v.action;
           Option.map (fun f -> ("Value", (AttributeValue.to_json f)))
             v.value])
    let of_json j =
      {
        value =
          (Option.map AttributeValue.of_json (Aws.Json.lookup j "Value"));
        action =
          (Option.map AttributeAction.of_json (Aws.Json.lookup j "Action"))
      }
  end
module ExpectedAttributeValue =
  struct
    type t =
      {
      value: AttributeValue.t option ;
      exists: Boolean.t option ;
      comparison_operator: ComparisonOperator.t option ;
      attribute_value_list: AttributeValueList.t option }
    let make ?value  ?exists  ?comparison_operator  ?attribute_value_list  ()
      = { value; exists; comparison_operator; attribute_value_list }
    let parse xml =
      Some
        {
          value =
            (Option.bind (Aws.Xml.member "Value" xml) AttributeValue.parse);
          exists = (Option.bind (Aws.Xml.member "Exists" xml) Boolean.parse);
          comparison_operator =
            (Option.bind (Aws.Xml.member "ComparisonOperator" xml)
               ComparisonOperator.parse);
          attribute_value_list =
            (Option.bind (Aws.Xml.member "AttributeValueList" xml)
               AttributeValueList.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("AttributeValueList.member",
                     (AttributeValueList.to_query f))) v.attribute_value_list;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ComparisonOperator", (ComparisonOperator.to_query f)))
             v.comparison_operator;
           Option.map
             (fun f -> Aws.Query.Pair ("Exists", (Boolean.to_query f)))
             v.exists;
           Option.map
             (fun f -> Aws.Query.Pair ("Value", (AttributeValue.to_query f)))
             v.value])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("AttributeValueList", (AttributeValueList.to_json f)))
              v.attribute_value_list;
           Option.map
             (fun f -> ("ComparisonOperator", (ComparisonOperator.to_json f)))
             v.comparison_operator;
           Option.map (fun f -> ("Exists", (Boolean.to_json f))) v.exists;
           Option.map (fun f -> ("Value", (AttributeValue.to_json f)))
             v.value])
    let of_json j =
      {
        value =
          (Option.map AttributeValue.of_json (Aws.Json.lookup j "Value"));
        exists = (Option.map Boolean.of_json (Aws.Json.lookup j "Exists"));
        comparison_operator =
          (Option.map ComparisonOperator.of_json
             (Aws.Json.lookup j "ComparisonOperator"));
        attribute_value_list =
          (Option.map AttributeValueList.of_json
             (Aws.Json.lookup j "AttributeValueList"))
      }
  end
module Condition =
  struct
    type t =
      {
      attribute_value_list: AttributeValueList.t option ;
      comparison_operator: ComparisonOperator.t }
    let make ?attribute_value_list  ~comparison_operator  () =
      { attribute_value_list; comparison_operator }
    let parse xml =
      Some
        {
          attribute_value_list =
            (Option.bind (Aws.Xml.member "AttributeValueList" xml)
               AttributeValueList.parse);
          comparison_operator =
            (Aws.Xml.required "ComparisonOperator"
               (Option.bind (Aws.Xml.member "ComparisonOperator" xml)
                  ComparisonOperator.parse))
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Some
              (Aws.Query.Pair
                 ("ComparisonOperator",
                   (ComparisonOperator.to_query v.comparison_operator)));
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("AttributeValueList.member",
                    (AttributeValueList.to_query f))) v.attribute_value_list])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Some
              ("ComparisonOperator",
                (ComparisonOperator.to_json v.comparison_operator));
           Option.map
             (fun f -> ("AttributeValueList", (AttributeValueList.to_json f)))
             v.attribute_value_list])
    let of_json j =
      {
        attribute_value_list =
          (Option.map AttributeValueList.of_json
             (Aws.Json.lookup j "AttributeValueList"));
        comparison_operator =
          (ComparisonOperator.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "ComparisonOperator")))
      }
  end
module GlobalSecondaryIndex =
  struct
    type t =
      {
      index_name: String.t ;
      key_schema: KeySchema.t ;
      projection: Projection.t ;
      provisioned_throughput: ProvisionedThroughput.t }
    let make ~index_name  ~key_schema  ~projection  ~provisioned_throughput 
      () = { index_name; key_schema; projection; provisioned_throughput }
    let parse xml =
      Some
        {
          index_name =
            (Aws.Xml.required "IndexName"
               (Option.bind (Aws.Xml.member "IndexName" xml) String.parse));
          key_schema =
            (Aws.Xml.required "KeySchema"
               (Option.bind (Aws.Xml.member "KeySchema" xml) KeySchema.parse));
          projection =
            (Aws.Xml.required "Projection"
               (Option.bind (Aws.Xml.member "Projection" xml)
                  Projection.parse));
          provisioned_throughput =
            (Aws.Xml.required "ProvisionedThroughput"
               (Option.bind (Aws.Xml.member "ProvisionedThroughput" xml)
                  ProvisionedThroughput.parse))
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Some
              (Aws.Query.Pair
                 ("ProvisionedThroughput",
                   (ProvisionedThroughput.to_query v.provisioned_throughput)));
           Some
             (Aws.Query.Pair
                ("Projection", (Projection.to_query v.projection)));
           Some
             (Aws.Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Some
             (Aws.Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Some
              ("ProvisionedThroughput",
                (ProvisionedThroughput.to_json v.provisioned_throughput));
           Some ("Projection", (Projection.to_json v.projection));
           Some ("KeySchema", (KeySchema.to_json v.key_schema));
           Some ("IndexName", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "IndexName")));
        key_schema =
          (KeySchema.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeySchema")));
        projection =
          (Projection.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Projection")));
        provisioned_throughput =
          (ProvisionedThroughput.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "ProvisionedThroughput")))
      }
  end
module LocalSecondaryIndex =
  struct
    type t =
      {
      index_name: String.t ;
      key_schema: KeySchema.t ;
      projection: Projection.t }
    let make ~index_name  ~key_schema  ~projection  () =
      { index_name; key_schema; projection }
    let parse xml =
      Some
        {
          index_name =
            (Aws.Xml.required "IndexName"
               (Option.bind (Aws.Xml.member "IndexName" xml) String.parse));
          key_schema =
            (Aws.Xml.required "KeySchema"
               (Option.bind (Aws.Xml.member "KeySchema" xml) KeySchema.parse));
          projection =
            (Aws.Xml.required "Projection"
               (Option.bind (Aws.Xml.member "Projection" xml)
                  Projection.parse))
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Some
              (Aws.Query.Pair
                 ("Projection", (Projection.to_query v.projection)));
           Some
             (Aws.Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Some
             (Aws.Query.Pair ("IndexName", (String.to_query v.index_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Some ("Projection", (Projection.to_json v.projection));
           Some ("KeySchema", (KeySchema.to_json v.key_schema));
           Some ("IndexName", (String.to_json v.index_name))])
    let of_json j =
      {
        index_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "IndexName")));
        key_schema =
          (KeySchema.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeySchema")));
        projection =
          (Projection.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Projection")))
      }
  end
module WriteRequests =
  struct
    type t = WriteRequest.t list
    let parse xml =
      Aws.Util.option_all
        (List.map WriteRequest.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list WriteRequest.to_query v
    let to_json v = `List (List.map WriteRequest.to_json v)
    let of_json j = Aws.Json.to_list WriteRequest.of_json j
  end
module ConsumedCapacity =
  struct
    type t =
      {
      table_name: String.t option ;
      capacity_units: Double.t option ;
      table: Capacity.t option ;
      local_secondary_indexes: SecondaryIndexesCapacityMap.t option ;
      global_secondary_indexes: SecondaryIndexesCapacityMap.t option }
    let make ?table_name  ?capacity_units  ?table  ?local_secondary_indexes 
      ?global_secondary_indexes  () =
      {
        table_name;
        capacity_units;
        table;
        local_secondary_indexes;
        global_secondary_indexes
      }
    let parse xml =
      Some
        {
          table_name =
            (Option.bind (Aws.Xml.member "TableName" xml) String.parse);
          capacity_units =
            (Option.bind (Aws.Xml.member "CapacityUnits" xml) Double.parse);
          table = (Option.bind (Aws.Xml.member "Table" xml) Capacity.parse);
          local_secondary_indexes =
            (Option.bind (Aws.Xml.member "LocalSecondaryIndexes" xml)
               SecondaryIndexesCapacityMap.parse);
          global_secondary_indexes =
            (Option.bind (Aws.Xml.member "GlobalSecondaryIndexes" xml)
               SecondaryIndexesCapacityMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("GlobalSecondaryIndexes",
                     (SecondaryIndexesCapacityMap.to_query f)))
              v.global_secondary_indexes;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("LocalSecondaryIndexes",
                    (SecondaryIndexesCapacityMap.to_query f)))
             v.local_secondary_indexes;
           Option.map
             (fun f -> Aws.Query.Pair ("Table", (Capacity.to_query f)))
             v.table;
           Option.map
             (fun f -> Aws.Query.Pair ("CapacityUnits", (Double.to_query f)))
             v.capacity_units;
           Option.map
             (fun f -> Aws.Query.Pair ("TableName", (String.to_query f)))
             v.table_name])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("GlobalSecondaryIndexes",
                   (SecondaryIndexesCapacityMap.to_json f)))
              v.global_secondary_indexes;
           Option.map
             (fun f ->
                ("LocalSecondaryIndexes",
                  (SecondaryIndexesCapacityMap.to_json f)))
             v.local_secondary_indexes;
           Option.map (fun f -> ("Table", (Capacity.to_json f))) v.table;
           Option.map (fun f -> ("CapacityUnits", (Double.to_json f)))
             v.capacity_units;
           Option.map (fun f -> ("TableName", (String.to_json f)))
             v.table_name])
    let of_json j =
      {
        table_name =
          (Option.map String.of_json (Aws.Json.lookup j "TableName"));
        capacity_units =
          (Option.map Double.of_json (Aws.Json.lookup j "CapacityUnits"));
        table = (Option.map Capacity.of_json (Aws.Json.lookup j "Table"));
        local_secondary_indexes =
          (Option.map SecondaryIndexesCapacityMap.of_json
             (Aws.Json.lookup j "LocalSecondaryIndexes"));
        global_secondary_indexes =
          (Option.map SecondaryIndexesCapacityMap.of_json
             (Aws.Json.lookup j "GlobalSecondaryIndexes"))
      }
  end
module ItemCollectionMetricsMultiple =
  struct
    type t = ItemCollectionMetrics.t list
    let parse xml =
      Aws.Util.option_all
        (List.map ItemCollectionMetrics.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list ItemCollectionMetrics.to_query v
    let to_json v = `List (List.map ItemCollectionMetrics.to_json v)
    let of_json j = Aws.Json.to_list ItemCollectionMetrics.of_json j
  end
module KeysAndAttributes =
  struct
    type t =
      {
      keys: KeyList.t ;
      attributes_to_get: AttributeNameList.t option ;
      consistent_read: Boolean.t option ;
      projection_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option }
    let make ~keys  ?attributes_to_get  ?consistent_read 
      ?projection_expression  ?expression_attribute_names  () =
      {
        keys;
        attributes_to_get;
        consistent_read;
        projection_expression;
        expression_attribute_names
      }
    let parse xml =
      Some
        {
          keys =
            (Aws.Xml.required "Keys"
               (Option.bind (Aws.Xml.member "Keys" xml) KeyList.parse));
          attributes_to_get =
            (Option.bind (Aws.Xml.member "AttributesToGet" xml)
               AttributeNameList.parse);
          consistent_read =
            (Option.bind (Aws.Xml.member "ConsistentRead" xml) Boolean.parse);
          projection_expression =
            (Option.bind (Aws.Xml.member "ProjectionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Option.bind (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ExpressionAttributeNames",
                     (ExpressionAttributeNameMap.to_query f)))
              v.expression_attribute_names;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ProjectionExpression", (String.to_query f)))
             v.projection_expression;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ConsistentRead", (Boolean.to_query f)))
             v.consistent_read;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("AttributesToGet.member", (AttributeNameList.to_query f)))
             v.attributes_to_get;
           Some (Aws.Query.Pair ("Keys.member", (KeyList.to_query v.keys)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("ExpressionAttributeNames",
                   (ExpressionAttributeNameMap.to_json f)))
              v.expression_attribute_names;
           Option.map (fun f -> ("ProjectionExpression", (String.to_json f)))
             v.projection_expression;
           Option.map (fun f -> ("ConsistentRead", (Boolean.to_json f)))
             v.consistent_read;
           Option.map
             (fun f -> ("AttributesToGet", (AttributeNameList.to_json f)))
             v.attributes_to_get;
           Some ("Keys", (KeyList.to_json v.keys))])
    let of_json j =
      {
        keys =
          (KeyList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Keys")));
        attributes_to_get =
          (Option.map AttributeNameList.of_json
             (Aws.Json.lookup j "AttributesToGet"));
        consistent_read =
          (Option.map Boolean.of_json (Aws.Json.lookup j "ConsistentRead"));
        projection_expression =
          (Option.map String.of_json
             (Aws.Json.lookup j "ProjectionExpression"));
        expression_attribute_names =
          (Option.map ExpressionAttributeNameMap.of_json
             (Aws.Json.lookup j "ExpressionAttributeNames"))
      }
  end
module ItemList =
  struct
    type t = AttributeMap.t list
    let parse xml =
      Aws.Util.option_all
        (List.map AttributeMap.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list AttributeMap.to_query v
    let to_json v = `List (List.map AttributeMap.to_json v)
    let of_json j = Aws.Json.to_list AttributeMap.of_json j
  end
module TableDescription =
  struct
    type t =
      {
      attribute_definitions: AttributeDefinitions.t option ;
      table_name: String.t option ;
      key_schema: KeySchema.t option ;
      table_status: TableStatus.t option ;
      creation_date_time: DateTime.t option ;
      provisioned_throughput: ProvisionedThroughputDescription.t option ;
      table_size_bytes: Long.t option ;
      item_count: Long.t option ;
      table_arn: String.t option ;
      local_secondary_indexes: LocalSecondaryIndexDescriptionList.t option ;
      global_secondary_indexes: GlobalSecondaryIndexDescriptionList.t option ;
      stream_specification: StreamSpecification.t option ;
      latest_stream_label: String.t option ;
      latest_stream_arn: String.t option }
    let make ?attribute_definitions  ?table_name  ?key_schema  ?table_status 
      ?creation_date_time  ?provisioned_throughput  ?table_size_bytes 
      ?item_count  ?table_arn  ?local_secondary_indexes 
      ?global_secondary_indexes  ?stream_specification  ?latest_stream_label 
      ?latest_stream_arn  () =
      {
        attribute_definitions;
        table_name;
        key_schema;
        table_status;
        creation_date_time;
        provisioned_throughput;
        table_size_bytes;
        item_count;
        table_arn;
        local_secondary_indexes;
        global_secondary_indexes;
        stream_specification;
        latest_stream_label;
        latest_stream_arn
      }
    let parse xml =
      Some
        {
          attribute_definitions =
            (Option.bind (Aws.Xml.member "AttributeDefinitions" xml)
               AttributeDefinitions.parse);
          table_name =
            (Option.bind (Aws.Xml.member "TableName" xml) String.parse);
          key_schema =
            (Option.bind (Aws.Xml.member "KeySchema" xml) KeySchema.parse);
          table_status =
            (Option.bind (Aws.Xml.member "TableStatus" xml) TableStatus.parse);
          creation_date_time =
            (Option.bind (Aws.Xml.member "CreationDateTime" xml)
               DateTime.parse);
          provisioned_throughput =
            (Option.bind (Aws.Xml.member "ProvisionedThroughput" xml)
               ProvisionedThroughputDescription.parse);
          table_size_bytes =
            (Option.bind (Aws.Xml.member "TableSizeBytes" xml) Long.parse);
          item_count =
            (Option.bind (Aws.Xml.member "ItemCount" xml) Long.parse);
          table_arn =
            (Option.bind (Aws.Xml.member "TableArn" xml) String.parse);
          local_secondary_indexes =
            (Option.bind (Aws.Xml.member "LocalSecondaryIndexes" xml)
               LocalSecondaryIndexDescriptionList.parse);
          global_secondary_indexes =
            (Option.bind (Aws.Xml.member "GlobalSecondaryIndexes" xml)
               GlobalSecondaryIndexDescriptionList.parse);
          stream_specification =
            (Option.bind (Aws.Xml.member "StreamSpecification" xml)
               StreamSpecification.parse);
          latest_stream_label =
            (Option.bind (Aws.Xml.member "LatestStreamLabel" xml)
               String.parse);
          latest_stream_arn =
            (Option.bind (Aws.Xml.member "LatestStreamArn" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair ("LatestStreamArn", (String.to_query f)))
              v.latest_stream_arn;
           Option.map
             (fun f ->
                Aws.Query.Pair ("LatestStreamLabel", (String.to_query f)))
             v.latest_stream_label;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("StreamSpecification", (StreamSpecification.to_query f)))
             v.stream_specification;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("GlobalSecondaryIndexes.member",
                    (GlobalSecondaryIndexDescriptionList.to_query f)))
             v.global_secondary_indexes;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("LocalSecondaryIndexes.member",
                    (LocalSecondaryIndexDescriptionList.to_query f)))
             v.local_secondary_indexes;
           Option.map
             (fun f -> Aws.Query.Pair ("TableArn", (String.to_query f)))
             v.table_arn;
           Option.map
             (fun f -> Aws.Query.Pair ("ItemCount", (Long.to_query f)))
             v.item_count;
           Option.map
             (fun f -> Aws.Query.Pair ("TableSizeBytes", (Long.to_query f)))
             v.table_size_bytes;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ProvisionedThroughput",
                    (ProvisionedThroughputDescription.to_query f)))
             v.provisioned_throughput;
           Option.map
             (fun f ->
                Aws.Query.Pair ("CreationDateTime", (DateTime.to_query f)))
             v.creation_date_time;
           Option.map
             (fun f ->
                Aws.Query.Pair ("TableStatus", (TableStatus.to_query f)))
             v.table_status;
           Option.map
             (fun f ->
                Aws.Query.Pair ("KeySchema.member", (KeySchema.to_query f)))
             v.key_schema;
           Option.map
             (fun f -> Aws.Query.Pair ("TableName", (String.to_query f)))
             v.table_name;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("AttributeDefinitions.member",
                    (AttributeDefinitions.to_query f)))
             v.attribute_definitions])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("LatestStreamArn", (String.to_json f)))
              v.latest_stream_arn;
           Option.map (fun f -> ("LatestStreamLabel", (String.to_json f)))
             v.latest_stream_label;
           Option.map
             (fun f ->
                ("StreamSpecification", (StreamSpecification.to_json f)))
             v.stream_specification;
           Option.map
             (fun f ->
                ("GlobalSecondaryIndexes",
                  (GlobalSecondaryIndexDescriptionList.to_json f)))
             v.global_secondary_indexes;
           Option.map
             (fun f ->
                ("LocalSecondaryIndexes",
                  (LocalSecondaryIndexDescriptionList.to_json f)))
             v.local_secondary_indexes;
           Option.map (fun f -> ("TableArn", (String.to_json f))) v.table_arn;
           Option.map (fun f -> ("ItemCount", (Long.to_json f))) v.item_count;
           Option.map (fun f -> ("TableSizeBytes", (Long.to_json f)))
             v.table_size_bytes;
           Option.map
             (fun f ->
                ("ProvisionedThroughput",
                  (ProvisionedThroughputDescription.to_json f)))
             v.provisioned_throughput;
           Option.map (fun f -> ("CreationDateTime", (DateTime.to_json f)))
             v.creation_date_time;
           Option.map (fun f -> ("TableStatus", (TableStatus.to_json f)))
             v.table_status;
           Option.map (fun f -> ("KeySchema", (KeySchema.to_json f)))
             v.key_schema;
           Option.map (fun f -> ("TableName", (String.to_json f)))
             v.table_name;
           Option.map
             (fun f ->
                ("AttributeDefinitions", (AttributeDefinitions.to_json f)))
             v.attribute_definitions])
    let of_json j =
      {
        attribute_definitions =
          (Option.map AttributeDefinitions.of_json
             (Aws.Json.lookup j "AttributeDefinitions"));
        table_name =
          (Option.map String.of_json (Aws.Json.lookup j "TableName"));
        key_schema =
          (Option.map KeySchema.of_json (Aws.Json.lookup j "KeySchema"));
        table_status =
          (Option.map TableStatus.of_json (Aws.Json.lookup j "TableStatus"));
        creation_date_time =
          (Option.map DateTime.of_json (Aws.Json.lookup j "CreationDateTime"));
        provisioned_throughput =
          (Option.map ProvisionedThroughputDescription.of_json
             (Aws.Json.lookup j "ProvisionedThroughput"));
        table_size_bytes =
          (Option.map Long.of_json (Aws.Json.lookup j "TableSizeBytes"));
        item_count =
          (Option.map Long.of_json (Aws.Json.lookup j "ItemCount"));
        table_arn =
          (Option.map String.of_json (Aws.Json.lookup j "TableArn"));
        local_secondary_indexes =
          (Option.map LocalSecondaryIndexDescriptionList.of_json
             (Aws.Json.lookup j "LocalSecondaryIndexes"));
        global_secondary_indexes =
          (Option.map GlobalSecondaryIndexDescriptionList.of_json
             (Aws.Json.lookup j "GlobalSecondaryIndexes"));
        stream_specification =
          (Option.map StreamSpecification.of_json
             (Aws.Json.lookup j "StreamSpecification"));
        latest_stream_label =
          (Option.map String.of_json (Aws.Json.lookup j "LatestStreamLabel"));
        latest_stream_arn =
          (Option.map String.of_json (Aws.Json.lookup j "LatestStreamArn"))
      }
  end
module GlobalSecondaryIndexUpdateList =
  struct
    type t = GlobalSecondaryIndexUpdate.t list
    let parse xml =
      Aws.Util.option_all
        (List.map GlobalSecondaryIndexUpdate.parse
           (Aws.Xml.members "member" xml))
    let to_query v =
      Aws.Query.to_query_list GlobalSecondaryIndexUpdate.to_query v
    let to_json v = `List (List.map GlobalSecondaryIndexUpdate.to_json v)
    let of_json j = Aws.Json.to_list GlobalSecondaryIndexUpdate.of_json j
  end
module AttributeUpdates =
  struct
    type t = (String.t, AttributeValueUpdate.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string
        AttributeValueUpdate.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((String.to_string k), (AttributeValueUpdate.to_json v)) ::
                  acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string AttributeValueUpdate.of_json j
  end
module ConditionalOperator =
  struct
    type t =
      | AND 
      | OR 
    let str_to_t = [("OR", OR); ("AND", AND)]
    let t_to_str = [(OR, "OR"); (AND, "AND")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let parse xml =
      Option.bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ExpectedAttributeMap =
  struct
    type t = (String.t, ExpectedAttributeValue.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string
        ExpectedAttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((String.to_string k), (ExpectedAttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string ExpectedAttributeValue.of_json j
  end
module ExpressionAttributeValueMap =
  struct
    type t = (String.t, AttributeValue.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string AttributeValue.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (AttributeValue.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string AttributeValue.of_json j
  end
module ReturnConsumedCapacity =
  struct
    type t =
      | INDEXES 
      | TOTAL 
      | NONE 
    let str_to_t = [("NONE", NONE); ("TOTAL", TOTAL); ("INDEXES", INDEXES)]
    let t_to_str = [(NONE, "NONE"); (TOTAL, "TOTAL"); (INDEXES, "INDEXES")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let parse xml =
      Option.bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ReturnItemCollectionMetrics =
  struct
    type t =
      | SIZE 
      | NONE 
    let str_to_t = [("NONE", NONE); ("SIZE", SIZE)]
    let t_to_str = [(NONE, "NONE"); (SIZE, "SIZE")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let parse xml =
      Option.bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ReturnValue =
  struct
    type t =
      | NONE 
      | ALL_OLD 
      | UPDATED_OLD 
      | ALL_NEW 
      | UPDATED_NEW 
    let str_to_t =
      [("UPDATED_NEW", UPDATED_NEW);
      ("ALL_NEW", ALL_NEW);
      ("UPDATED_OLD", UPDATED_OLD);
      ("ALL_OLD", ALL_OLD);
      ("NONE", NONE)]
    let t_to_str =
      [(UPDATED_NEW, "UPDATED_NEW");
      (ALL_NEW, "ALL_NEW");
      (UPDATED_OLD, "UPDATED_OLD");
      (ALL_OLD, "ALL_OLD");
      (NONE, "NONE")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let parse xml =
      Option.bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module FilterConditionMap =
  struct
    type t = (String.t, Condition.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string Condition.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (Condition.to_json v)) ::
                  acc) v [])
    let of_json j = Aws.Json.to_hashtbl String.of_string Condition.of_json j
  end
module Select =
  struct
    type t =
      | ALL_ATTRIBUTES 
      | ALL_PROJECTED_ATTRIBUTES 
      | SPECIFIC_ATTRIBUTES 
      | COUNT 
    let str_to_t =
      [("COUNT", COUNT);
      ("SPECIFIC_ATTRIBUTES", SPECIFIC_ATTRIBUTES);
      ("ALL_PROJECTED_ATTRIBUTES", ALL_PROJECTED_ATTRIBUTES);
      ("ALL_ATTRIBUTES", ALL_ATTRIBUTES)]
    let t_to_str =
      [(COUNT, "COUNT");
      (SPECIFIC_ATTRIBUTES, "SPECIFIC_ATTRIBUTES");
      (ALL_PROJECTED_ATTRIBUTES, "ALL_PROJECTED_ATTRIBUTES");
      (ALL_ATTRIBUTES, "ALL_ATTRIBUTES")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let parse xml =
      Option.bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module KeyConditions =
  struct
    type t = (String.t, Condition.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string Condition.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (Condition.to_json v)) ::
                  acc) v [])
    let of_json j = Aws.Json.to_hashtbl String.of_string Condition.of_json j
  end
module TableNameList =
  struct
    type t = String.t list
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module GlobalSecondaryIndexList =
  struct
    type t = GlobalSecondaryIndex.t list
    let parse xml =
      Aws.Util.option_all
        (List.map GlobalSecondaryIndex.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list GlobalSecondaryIndex.to_query v
    let to_json v = `List (List.map GlobalSecondaryIndex.to_json v)
    let of_json j = Aws.Json.to_list GlobalSecondaryIndex.of_json j
  end
module LocalSecondaryIndexList =
  struct
    type t = LocalSecondaryIndex.t list
    let parse xml =
      Aws.Util.option_all
        (List.map LocalSecondaryIndex.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list LocalSecondaryIndex.to_query v
    let to_json v = `List (List.map LocalSecondaryIndex.to_json v)
    let of_json j = Aws.Json.to_list LocalSecondaryIndex.of_json j
  end
module BatchWriteItemRequestMap =
  struct
    type t = (String.t, WriteRequests.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string WriteRequests.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (WriteRequests.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string WriteRequests.of_json j
  end
module ConsumedCapacityMultiple =
  struct
    type t = ConsumedCapacity.t list
    let parse xml =
      Aws.Util.option_all
        (List.map ConsumedCapacity.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list ConsumedCapacity.to_query v
    let to_json v = `List (List.map ConsumedCapacity.to_json v)
    let of_json j = Aws.Json.to_list ConsumedCapacity.of_json j
  end
module ItemCollectionMetricsPerTable =
  struct
    type t = (String.t, ItemCollectionMetricsMultiple.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string
        ItemCollectionMetricsMultiple.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((String.to_string k),
                    (ItemCollectionMetricsMultiple.to_json v))
                  :: acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string
        ItemCollectionMetricsMultiple.of_json j
  end
module BatchGetRequestMap =
  struct
    type t = (String.t, KeysAndAttributes.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string KeysAndAttributes.to_query
        v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc ->
                  ((String.to_string k), (KeysAndAttributes.to_json v)) ::
                  acc) v [])
    let of_json j =
      Aws.Json.to_hashtbl String.of_string KeysAndAttributes.of_json j
  end
module BatchGetResponseMap =
  struct
    type t = (String.t, ItemList.t) Hashtbl.t
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string ItemList.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (ItemList.to_json v)) ::
                  acc) v [])
    let of_json j = Aws.Json.to_hashtbl String.of_string ItemList.of_json j
  end
module UpdateTableOutput =
  struct
    type t = {
      table_description: TableDescription.t option }
    let make ?table_description  () = { table_description }
    let parse xml =
      Some
        {
          table_description =
            (Option.bind (Aws.Xml.member "TableDescription" xml)
               TableDescription.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("TableDescription", (TableDescription.to_query f)))
              v.table_description])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f -> ("TableDescription", (TableDescription.to_json f)))
              v.table_description])
    let of_json j =
      {
        table_description =
          (Option.map TableDescription.of_json
             (Aws.Json.lookup j "TableDescription"))
      }
  end
module UpdateTableInput =
  struct
    type t =
      {
      attribute_definitions: AttributeDefinitions.t option ;
      table_name: String.t ;
      provisioned_throughput: ProvisionedThroughput.t option ;
      global_secondary_index_updates: GlobalSecondaryIndexUpdateList.t option ;
      stream_specification: StreamSpecification.t option }
    let make ?attribute_definitions  ~table_name  ?provisioned_throughput 
      ?global_secondary_index_updates  ?stream_specification  () =
      {
        attribute_definitions;
        table_name;
        provisioned_throughput;
        global_secondary_index_updates;
        stream_specification
      }
    let parse xml =
      Some
        {
          attribute_definitions =
            (Option.bind (Aws.Xml.member "AttributeDefinitions" xml)
               AttributeDefinitions.parse);
          table_name =
            (Aws.Xml.required "TableName"
               (Option.bind (Aws.Xml.member "TableName" xml) String.parse));
          provisioned_throughput =
            (Option.bind (Aws.Xml.member "ProvisionedThroughput" xml)
               ProvisionedThroughput.parse);
          global_secondary_index_updates =
            (Option.bind (Aws.Xml.member "GlobalSecondaryIndexUpdates" xml)
               GlobalSecondaryIndexUpdateList.parse);
          stream_specification =
            (Option.bind (Aws.Xml.member "StreamSpecification" xml)
               StreamSpecification.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("StreamSpecification", (StreamSpecification.to_query f)))
              v.stream_specification;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("GlobalSecondaryIndexUpdates.member",
                    (GlobalSecondaryIndexUpdateList.to_query f)))
             v.global_secondary_index_updates;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ProvisionedThroughput",
                    (ProvisionedThroughput.to_query f)))
             v.provisioned_throughput;
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)));
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("AttributeDefinitions.member",
                    (AttributeDefinitions.to_query f)))
             v.attribute_definitions])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("StreamSpecification", (StreamSpecification.to_json f)))
              v.stream_specification;
           Option.map
             (fun f ->
                ("GlobalSecondaryIndexUpdates",
                  (GlobalSecondaryIndexUpdateList.to_json f)))
             v.global_secondary_index_updates;
           Option.map
             (fun f ->
                ("ProvisionedThroughput", (ProvisionedThroughput.to_json f)))
             v.provisioned_throughput;
           Some ("TableName", (String.to_json v.table_name));
           Option.map
             (fun f ->
                ("AttributeDefinitions", (AttributeDefinitions.to_json f)))
             v.attribute_definitions])
    let of_json j =
      {
        attribute_definitions =
          (Option.map AttributeDefinitions.of_json
             (Aws.Json.lookup j "AttributeDefinitions"));
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        provisioned_throughput =
          (Option.map ProvisionedThroughput.of_json
             (Aws.Json.lookup j "ProvisionedThroughput"));
        global_secondary_index_updates =
          (Option.map GlobalSecondaryIndexUpdateList.of_json
             (Aws.Json.lookup j "GlobalSecondaryIndexUpdates"));
        stream_specification =
          (Option.map StreamSpecification.of_json
             (Aws.Json.lookup j "StreamSpecification"))
      }
  end
module UpdateItemOutput =
  struct
    type t =
      {
      attributes: AttributeMap.t option ;
      consumed_capacity: ConsumedCapacity.t option ;
      item_collection_metrics: ItemCollectionMetrics.t option }
    let make ?attributes  ?consumed_capacity  ?item_collection_metrics  () =
      { attributes; consumed_capacity; item_collection_metrics }
    let parse xml =
      Some
        {
          attributes =
            (Option.bind (Aws.Xml.member "Attributes" xml) AttributeMap.parse);
          consumed_capacity =
            (Option.bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse);
          item_collection_metrics =
            (Option.bind (Aws.Xml.member "ItemCollectionMetrics" xml)
               ItemCollectionMetrics.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ItemCollectionMetrics",
                     (ItemCollectionMetrics.to_query f)))
              v.item_collection_metrics;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ConsumedCapacity", (ConsumedCapacity.to_query f)))
             v.consumed_capacity;
           Option.map
             (fun f ->
                Aws.Query.Pair ("Attributes", (AttributeMap.to_query f)))
             v.attributes])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("ItemCollectionMetrics", (ItemCollectionMetrics.to_json f)))
              v.item_collection_metrics;
           Option.map
             (fun f -> ("ConsumedCapacity", (ConsumedCapacity.to_json f)))
             v.consumed_capacity;
           Option.map (fun f -> ("Attributes", (AttributeMap.to_json f)))
             v.attributes])
    let of_json j =
      {
        attributes =
          (Option.map AttributeMap.of_json (Aws.Json.lookup j "Attributes"));
        consumed_capacity =
          (Option.map ConsumedCapacity.of_json
             (Aws.Json.lookup j "ConsumedCapacity"));
        item_collection_metrics =
          (Option.map ItemCollectionMetrics.of_json
             (Aws.Json.lookup j "ItemCollectionMetrics"))
      }
  end
module UpdateItemInput =
  struct
    type t =
      {
      table_name: String.t ;
      key: Key.t ;
      attribute_updates: AttributeUpdates.t option ;
      expected: ExpectedAttributeMap.t option ;
      conditional_operator: ConditionalOperator.t option ;
      return_values: ReturnValue.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      return_item_collection_metrics: ReturnItemCollectionMetrics.t option ;
      update_expression: String.t option ;
      condition_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option }
    let make ~table_name  ~key  ?attribute_updates  ?expected 
      ?conditional_operator  ?return_values  ?return_consumed_capacity 
      ?return_item_collection_metrics  ?update_expression 
      ?condition_expression  ?expression_attribute_names 
      ?expression_attribute_values  () =
      {
        table_name;
        key;
        attribute_updates;
        expected;
        conditional_operator;
        return_values;
        return_consumed_capacity;
        return_item_collection_metrics;
        update_expression;
        condition_expression;
        expression_attribute_names;
        expression_attribute_values
      }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Option.bind (Aws.Xml.member "TableName" xml) String.parse));
          key =
            (Aws.Xml.required "Key"
               (Option.bind (Aws.Xml.member "Key" xml) Key.parse));
          attribute_updates =
            (Option.bind (Aws.Xml.member "AttributeUpdates" xml)
               AttributeUpdates.parse);
          expected =
            (Option.bind (Aws.Xml.member "Expected" xml)
               ExpectedAttributeMap.parse);
          conditional_operator =
            (Option.bind (Aws.Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          return_values =
            (Option.bind (Aws.Xml.member "ReturnValues" xml)
               ReturnValue.parse);
          return_consumed_capacity =
            (Option.bind (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          return_item_collection_metrics =
            (Option.bind (Aws.Xml.member "ReturnItemCollectionMetrics" xml)
               ReturnItemCollectionMetrics.parse);
          update_expression =
            (Option.bind (Aws.Xml.member "UpdateExpression" xml) String.parse);
          condition_expression =
            (Option.bind (Aws.Xml.member "ConditionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Option.bind (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Option.bind (Aws.Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ExpressionAttributeValues",
                     (ExpressionAttributeValueMap.to_query f)))
              v.expression_attribute_values;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)))
             v.expression_attribute_names;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ConditionExpression", (String.to_query f)))
             v.condition_expression;
           Option.map
             (fun f ->
                Aws.Query.Pair ("UpdateExpression", (String.to_query f)))
             v.update_expression;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ReturnItemCollectionMetrics",
                    (ReturnItemCollectionMetrics.to_query f)))
             v.return_item_collection_metrics;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)))
             v.return_consumed_capacity;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ReturnValues", (ReturnValue.to_query f)))
             v.return_values;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)))
             v.conditional_operator;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("Expected", (ExpectedAttributeMap.to_query f))) v.expected;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("AttributeUpdates", (AttributeUpdates.to_query f)))
             v.attribute_updates;
           Some (Aws.Query.Pair ("Key", (Key.to_query v.key)));
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("ExpressionAttributeValues",
                   (ExpressionAttributeValueMap.to_json f)))
              v.expression_attribute_values;
           Option.map
             (fun f ->
                ("ExpressionAttributeNames",
                  (ExpressionAttributeNameMap.to_json f)))
             v.expression_attribute_names;
           Option.map (fun f -> ("ConditionExpression", (String.to_json f)))
             v.condition_expression;
           Option.map (fun f -> ("UpdateExpression", (String.to_json f)))
             v.update_expression;
           Option.map
             (fun f ->
                ("ReturnItemCollectionMetrics",
                  (ReturnItemCollectionMetrics.to_json f)))
             v.return_item_collection_metrics;
           Option.map
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)))
             v.return_consumed_capacity;
           Option.map (fun f -> ("ReturnValues", (ReturnValue.to_json f)))
             v.return_values;
           Option.map
             (fun f ->
                ("ConditionalOperator", (ConditionalOperator.to_json f)))
             v.conditional_operator;
           Option.map
             (fun f -> ("Expected", (ExpectedAttributeMap.to_json f)))
             v.expected;
           Option.map
             (fun f -> ("AttributeUpdates", (AttributeUpdates.to_json f)))
             v.attribute_updates;
           Some ("Key", (Key.to_json v.key));
           Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        key =
          (Key.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        attribute_updates =
          (Option.map AttributeUpdates.of_json
             (Aws.Json.lookup j "AttributeUpdates"));
        expected =
          (Option.map ExpectedAttributeMap.of_json
             (Aws.Json.lookup j "Expected"));
        conditional_operator =
          (Option.map ConditionalOperator.of_json
             (Aws.Json.lookup j "ConditionalOperator"));
        return_values =
          (Option.map ReturnValue.of_json (Aws.Json.lookup j "ReturnValues"));
        return_consumed_capacity =
          (Option.map ReturnConsumedCapacity.of_json
             (Aws.Json.lookup j "ReturnConsumedCapacity"));
        return_item_collection_metrics =
          (Option.map ReturnItemCollectionMetrics.of_json
             (Aws.Json.lookup j "ReturnItemCollectionMetrics"));
        update_expression =
          (Option.map String.of_json (Aws.Json.lookup j "UpdateExpression"));
        condition_expression =
          (Option.map String.of_json
             (Aws.Json.lookup j "ConditionExpression"));
        expression_attribute_names =
          (Option.map ExpressionAttributeNameMap.of_json
             (Aws.Json.lookup j "ExpressionAttributeNames"));
        expression_attribute_values =
          (Option.map ExpressionAttributeValueMap.of_json
             (Aws.Json.lookup j "ExpressionAttributeValues"))
      }
  end
module ScanOutput =
  struct
    type t =
      {
      items: ItemList.t option ;
      count: Integer.t option ;
      scanned_count: Integer.t option ;
      last_evaluated_key: Key.t option ;
      consumed_capacity: ConsumedCapacity.t option }
    let make ?items  ?count  ?scanned_count  ?last_evaluated_key 
      ?consumed_capacity  () =
      { items; count; scanned_count; last_evaluated_key; consumed_capacity }
    let parse xml =
      Some
        {
          items = (Option.bind (Aws.Xml.member "Items" xml) ItemList.parse);
          count = (Option.bind (Aws.Xml.member "Count" xml) Integer.parse);
          scanned_count =
            (Option.bind (Aws.Xml.member "ScannedCount" xml) Integer.parse);
          last_evaluated_key =
            (Option.bind (Aws.Xml.member "LastEvaluatedKey" xml) Key.parse);
          consumed_capacity =
            (Option.bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ConsumedCapacity", (ConsumedCapacity.to_query f)))
              v.consumed_capacity;
           Option.map
             (fun f -> Aws.Query.Pair ("LastEvaluatedKey", (Key.to_query f)))
             v.last_evaluated_key;
           Option.map
             (fun f -> Aws.Query.Pair ("ScannedCount", (Integer.to_query f)))
             v.scanned_count;
           Option.map
             (fun f -> Aws.Query.Pair ("Count", (Integer.to_query f)))
             v.count;
           Option.map
             (fun f -> Aws.Query.Pair ("Items.member", (ItemList.to_query f)))
             v.items])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f -> ("ConsumedCapacity", (ConsumedCapacity.to_json f)))
              v.consumed_capacity;
           Option.map (fun f -> ("LastEvaluatedKey", (Key.to_json f)))
             v.last_evaluated_key;
           Option.map (fun f -> ("ScannedCount", (Integer.to_json f)))
             v.scanned_count;
           Option.map (fun f -> ("Count", (Integer.to_json f))) v.count;
           Option.map (fun f -> ("Items", (ItemList.to_json f))) v.items])
    let of_json j =
      {
        items = (Option.map ItemList.of_json (Aws.Json.lookup j "Items"));
        count = (Option.map Integer.of_json (Aws.Json.lookup j "Count"));
        scanned_count =
          (Option.map Integer.of_json (Aws.Json.lookup j "ScannedCount"));
        last_evaluated_key =
          (Option.map Key.of_json (Aws.Json.lookup j "LastEvaluatedKey"));
        consumed_capacity =
          (Option.map ConsumedCapacity.of_json
             (Aws.Json.lookup j "ConsumedCapacity"))
      }
  end
module ScanInput =
  struct
    type t =
      {
      table_name: String.t ;
      index_name: String.t option ;
      attributes_to_get: AttributeNameList.t option ;
      limit: Integer.t option ;
      select: Select.t option ;
      scan_filter: FilterConditionMap.t option ;
      conditional_operator: ConditionalOperator.t option ;
      exclusive_start_key: Key.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      total_segments: Integer.t option ;
      segment: Integer.t option ;
      projection_expression: String.t option ;
      filter_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option ;
      consistent_read: Boolean.t option }
    let make ~table_name  ?index_name  ?attributes_to_get  ?limit  ?select 
      ?scan_filter  ?conditional_operator  ?exclusive_start_key 
      ?return_consumed_capacity  ?total_segments  ?segment 
      ?projection_expression  ?filter_expression  ?expression_attribute_names
       ?expression_attribute_values  ?consistent_read  () =
      {
        table_name;
        index_name;
        attributes_to_get;
        limit;
        select;
        scan_filter;
        conditional_operator;
        exclusive_start_key;
        return_consumed_capacity;
        total_segments;
        segment;
        projection_expression;
        filter_expression;
        expression_attribute_names;
        expression_attribute_values;
        consistent_read
      }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Option.bind (Aws.Xml.member "TableName" xml) String.parse));
          index_name =
            (Option.bind (Aws.Xml.member "IndexName" xml) String.parse);
          attributes_to_get =
            (Option.bind (Aws.Xml.member "AttributesToGet" xml)
               AttributeNameList.parse);
          limit = (Option.bind (Aws.Xml.member "Limit" xml) Integer.parse);
          select = (Option.bind (Aws.Xml.member "Select" xml) Select.parse);
          scan_filter =
            (Option.bind (Aws.Xml.member "ScanFilter" xml)
               FilterConditionMap.parse);
          conditional_operator =
            (Option.bind (Aws.Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          exclusive_start_key =
            (Option.bind (Aws.Xml.member "ExclusiveStartKey" xml) Key.parse);
          return_consumed_capacity =
            (Option.bind (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          total_segments =
            (Option.bind (Aws.Xml.member "TotalSegments" xml) Integer.parse);
          segment =
            (Option.bind (Aws.Xml.member "Segment" xml) Integer.parse);
          projection_expression =
            (Option.bind (Aws.Xml.member "ProjectionExpression" xml)
               String.parse);
          filter_expression =
            (Option.bind (Aws.Xml.member "FilterExpression" xml) String.parse);
          expression_attribute_names =
            (Option.bind (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Option.bind (Aws.Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse);
          consistent_read =
            (Option.bind (Aws.Xml.member "ConsistentRead" xml) Boolean.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair ("ConsistentRead", (Boolean.to_query f)))
              v.consistent_read;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ExpressionAttributeValues",
                    (ExpressionAttributeValueMap.to_query f)))
             v.expression_attribute_values;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)))
             v.expression_attribute_names;
           Option.map
             (fun f ->
                Aws.Query.Pair ("FilterExpression", (String.to_query f)))
             v.filter_expression;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ProjectionExpression", (String.to_query f)))
             v.projection_expression;
           Option.map
             (fun f -> Aws.Query.Pair ("Segment", (Integer.to_query f)))
             v.segment;
           Option.map
             (fun f -> Aws.Query.Pair ("TotalSegments", (Integer.to_query f)))
             v.total_segments;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)))
             v.return_consumed_capacity;
           Option.map
             (fun f -> Aws.Query.Pair ("ExclusiveStartKey", (Key.to_query f)))
             v.exclusive_start_key;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)))
             v.conditional_operator;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ScanFilter", (FilterConditionMap.to_query f)))
             v.scan_filter;
           Option.map
             (fun f -> Aws.Query.Pair ("Select", (Select.to_query f)))
             v.select;
           Option.map
             (fun f -> Aws.Query.Pair ("Limit", (Integer.to_query f)))
             v.limit;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("AttributesToGet.member", (AttributeNameList.to_query f)))
             v.attributes_to_get;
           Option.map
             (fun f -> Aws.Query.Pair ("IndexName", (String.to_query f)))
             v.index_name;
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("ConsistentRead", (Boolean.to_json f)))
              v.consistent_read;
           Option.map
             (fun f ->
                ("ExpressionAttributeValues",
                  (ExpressionAttributeValueMap.to_json f)))
             v.expression_attribute_values;
           Option.map
             (fun f ->
                ("ExpressionAttributeNames",
                  (ExpressionAttributeNameMap.to_json f)))
             v.expression_attribute_names;
           Option.map (fun f -> ("FilterExpression", (String.to_json f)))
             v.filter_expression;
           Option.map (fun f -> ("ProjectionExpression", (String.to_json f)))
             v.projection_expression;
           Option.map (fun f -> ("Segment", (Integer.to_json f))) v.segment;
           Option.map (fun f -> ("TotalSegments", (Integer.to_json f)))
             v.total_segments;
           Option.map
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)))
             v.return_consumed_capacity;
           Option.map (fun f -> ("ExclusiveStartKey", (Key.to_json f)))
             v.exclusive_start_key;
           Option.map
             (fun f ->
                ("ConditionalOperator", (ConditionalOperator.to_json f)))
             v.conditional_operator;
           Option.map
             (fun f -> ("ScanFilter", (FilterConditionMap.to_json f)))
             v.scan_filter;
           Option.map (fun f -> ("Select", (Select.to_json f))) v.select;
           Option.map (fun f -> ("Limit", (Integer.to_json f))) v.limit;
           Option.map
             (fun f -> ("AttributesToGet", (AttributeNameList.to_json f)))
             v.attributes_to_get;
           Option.map (fun f -> ("IndexName", (String.to_json f)))
             v.index_name;
           Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        index_name =
          (Option.map String.of_json (Aws.Json.lookup j "IndexName"));
        attributes_to_get =
          (Option.map AttributeNameList.of_json
             (Aws.Json.lookup j "AttributesToGet"));
        limit = (Option.map Integer.of_json (Aws.Json.lookup j "Limit"));
        select = (Option.map Select.of_json (Aws.Json.lookup j "Select"));
        scan_filter =
          (Option.map FilterConditionMap.of_json
             (Aws.Json.lookup j "ScanFilter"));
        conditional_operator =
          (Option.map ConditionalOperator.of_json
             (Aws.Json.lookup j "ConditionalOperator"));
        exclusive_start_key =
          (Option.map Key.of_json (Aws.Json.lookup j "ExclusiveStartKey"));
        return_consumed_capacity =
          (Option.map ReturnConsumedCapacity.of_json
             (Aws.Json.lookup j "ReturnConsumedCapacity"));
        total_segments =
          (Option.map Integer.of_json (Aws.Json.lookup j "TotalSegments"));
        segment = (Option.map Integer.of_json (Aws.Json.lookup j "Segment"));
        projection_expression =
          (Option.map String.of_json
             (Aws.Json.lookup j "ProjectionExpression"));
        filter_expression =
          (Option.map String.of_json (Aws.Json.lookup j "FilterExpression"));
        expression_attribute_names =
          (Option.map ExpressionAttributeNameMap.of_json
             (Aws.Json.lookup j "ExpressionAttributeNames"));
        expression_attribute_values =
          (Option.map ExpressionAttributeValueMap.of_json
             (Aws.Json.lookup j "ExpressionAttributeValues"));
        consistent_read =
          (Option.map Boolean.of_json (Aws.Json.lookup j "ConsistentRead"))
      }
  end
module ResourceNotFoundException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        { message = (Option.bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))
              v.message])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("message", (String.to_json f))) v.message])
    let of_json j =
      { message = (Option.map String.of_json (Aws.Json.lookup j "message")) }
  end
module ResourceInUseException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        { message = (Option.bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))
              v.message])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("message", (String.to_json f))) v.message])
    let of_json j =
      { message = (Option.map String.of_json (Aws.Json.lookup j "message")) }
  end
module QueryOutput =
  struct
    type t =
      {
      items: ItemList.t option ;
      count: Integer.t option ;
      scanned_count: Integer.t option ;
      last_evaluated_key: Key.t option ;
      consumed_capacity: ConsumedCapacity.t option }
    let make ?items  ?count  ?scanned_count  ?last_evaluated_key 
      ?consumed_capacity  () =
      { items; count; scanned_count; last_evaluated_key; consumed_capacity }
    let parse xml =
      Some
        {
          items = (Option.bind (Aws.Xml.member "Items" xml) ItemList.parse);
          count = (Option.bind (Aws.Xml.member "Count" xml) Integer.parse);
          scanned_count =
            (Option.bind (Aws.Xml.member "ScannedCount" xml) Integer.parse);
          last_evaluated_key =
            (Option.bind (Aws.Xml.member "LastEvaluatedKey" xml) Key.parse);
          consumed_capacity =
            (Option.bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ConsumedCapacity", (ConsumedCapacity.to_query f)))
              v.consumed_capacity;
           Option.map
             (fun f -> Aws.Query.Pair ("LastEvaluatedKey", (Key.to_query f)))
             v.last_evaluated_key;
           Option.map
             (fun f -> Aws.Query.Pair ("ScannedCount", (Integer.to_query f)))
             v.scanned_count;
           Option.map
             (fun f -> Aws.Query.Pair ("Count", (Integer.to_query f)))
             v.count;
           Option.map
             (fun f -> Aws.Query.Pair ("Items.member", (ItemList.to_query f)))
             v.items])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f -> ("ConsumedCapacity", (ConsumedCapacity.to_json f)))
              v.consumed_capacity;
           Option.map (fun f -> ("LastEvaluatedKey", (Key.to_json f)))
             v.last_evaluated_key;
           Option.map (fun f -> ("ScannedCount", (Integer.to_json f)))
             v.scanned_count;
           Option.map (fun f -> ("Count", (Integer.to_json f))) v.count;
           Option.map (fun f -> ("Items", (ItemList.to_json f))) v.items])
    let of_json j =
      {
        items = (Option.map ItemList.of_json (Aws.Json.lookup j "Items"));
        count = (Option.map Integer.of_json (Aws.Json.lookup j "Count"));
        scanned_count =
          (Option.map Integer.of_json (Aws.Json.lookup j "ScannedCount"));
        last_evaluated_key =
          (Option.map Key.of_json (Aws.Json.lookup j "LastEvaluatedKey"));
        consumed_capacity =
          (Option.map ConsumedCapacity.of_json
             (Aws.Json.lookup j "ConsumedCapacity"))
      }
  end
module QueryInput =
  struct
    type t =
      {
      table_name: String.t ;
      index_name: String.t option ;
      select: Select.t option ;
      attributes_to_get: AttributeNameList.t option ;
      limit: Integer.t option ;
      consistent_read: Boolean.t option ;
      key_conditions: KeyConditions.t option ;
      query_filter: FilterConditionMap.t option ;
      conditional_operator: ConditionalOperator.t option ;
      scan_index_forward: Boolean.t option ;
      exclusive_start_key: Key.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      projection_expression: String.t option ;
      filter_expression: String.t option ;
      key_condition_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option }
    let make ~table_name  ?index_name  ?select  ?attributes_to_get  ?limit 
      ?consistent_read  ?key_conditions  ?query_filter  ?conditional_operator
       ?scan_index_forward  ?exclusive_start_key  ?return_consumed_capacity 
      ?projection_expression  ?filter_expression  ?key_condition_expression 
      ?expression_attribute_names  ?expression_attribute_values  () =
      {
        table_name;
        index_name;
        select;
        attributes_to_get;
        limit;
        consistent_read;
        key_conditions;
        query_filter;
        conditional_operator;
        scan_index_forward;
        exclusive_start_key;
        return_consumed_capacity;
        projection_expression;
        filter_expression;
        key_condition_expression;
        expression_attribute_names;
        expression_attribute_values
      }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Option.bind (Aws.Xml.member "TableName" xml) String.parse));
          index_name =
            (Option.bind (Aws.Xml.member "IndexName" xml) String.parse);
          select = (Option.bind (Aws.Xml.member "Select" xml) Select.parse);
          attributes_to_get =
            (Option.bind (Aws.Xml.member "AttributesToGet" xml)
               AttributeNameList.parse);
          limit = (Option.bind (Aws.Xml.member "Limit" xml) Integer.parse);
          consistent_read =
            (Option.bind (Aws.Xml.member "ConsistentRead" xml) Boolean.parse);
          key_conditions =
            (Option.bind (Aws.Xml.member "KeyConditions" xml)
               KeyConditions.parse);
          query_filter =
            (Option.bind (Aws.Xml.member "QueryFilter" xml)
               FilterConditionMap.parse);
          conditional_operator =
            (Option.bind (Aws.Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          scan_index_forward =
            (Option.bind (Aws.Xml.member "ScanIndexForward" xml)
               Boolean.parse);
          exclusive_start_key =
            (Option.bind (Aws.Xml.member "ExclusiveStartKey" xml) Key.parse);
          return_consumed_capacity =
            (Option.bind (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          projection_expression =
            (Option.bind (Aws.Xml.member "ProjectionExpression" xml)
               String.parse);
          filter_expression =
            (Option.bind (Aws.Xml.member "FilterExpression" xml) String.parse);
          key_condition_expression =
            (Option.bind (Aws.Xml.member "KeyConditionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Option.bind (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Option.bind (Aws.Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ExpressionAttributeValues",
                     (ExpressionAttributeValueMap.to_query f)))
              v.expression_attribute_values;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)))
             v.expression_attribute_names;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("KeyConditionExpression", (String.to_query f)))
             v.key_condition_expression;
           Option.map
             (fun f ->
                Aws.Query.Pair ("FilterExpression", (String.to_query f)))
             v.filter_expression;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ProjectionExpression", (String.to_query f)))
             v.projection_expression;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)))
             v.return_consumed_capacity;
           Option.map
             (fun f -> Aws.Query.Pair ("ExclusiveStartKey", (Key.to_query f)))
             v.exclusive_start_key;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ScanIndexForward", (Boolean.to_query f)))
             v.scan_index_forward;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)))
             v.conditional_operator;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("QueryFilter", (FilterConditionMap.to_query f)))
             v.query_filter;
           Option.map
             (fun f ->
                Aws.Query.Pair ("KeyConditions", (KeyConditions.to_query f)))
             v.key_conditions;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ConsistentRead", (Boolean.to_query f)))
             v.consistent_read;
           Option.map
             (fun f -> Aws.Query.Pair ("Limit", (Integer.to_query f)))
             v.limit;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("AttributesToGet.member", (AttributeNameList.to_query f)))
             v.attributes_to_get;
           Option.map
             (fun f -> Aws.Query.Pair ("Select", (Select.to_query f)))
             v.select;
           Option.map
             (fun f -> Aws.Query.Pair ("IndexName", (String.to_query f)))
             v.index_name;
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("ExpressionAttributeValues",
                   (ExpressionAttributeValueMap.to_json f)))
              v.expression_attribute_values;
           Option.map
             (fun f ->
                ("ExpressionAttributeNames",
                  (ExpressionAttributeNameMap.to_json f)))
             v.expression_attribute_names;
           Option.map
             (fun f -> ("KeyConditionExpression", (String.to_json f)))
             v.key_condition_expression;
           Option.map (fun f -> ("FilterExpression", (String.to_json f)))
             v.filter_expression;
           Option.map (fun f -> ("ProjectionExpression", (String.to_json f)))
             v.projection_expression;
           Option.map
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)))
             v.return_consumed_capacity;
           Option.map (fun f -> ("ExclusiveStartKey", (Key.to_json f)))
             v.exclusive_start_key;
           Option.map (fun f -> ("ScanIndexForward", (Boolean.to_json f)))
             v.scan_index_forward;
           Option.map
             (fun f ->
                ("ConditionalOperator", (ConditionalOperator.to_json f)))
             v.conditional_operator;
           Option.map
             (fun f -> ("QueryFilter", (FilterConditionMap.to_json f)))
             v.query_filter;
           Option.map (fun f -> ("KeyConditions", (KeyConditions.to_json f)))
             v.key_conditions;
           Option.map (fun f -> ("ConsistentRead", (Boolean.to_json f)))
             v.consistent_read;
           Option.map (fun f -> ("Limit", (Integer.to_json f))) v.limit;
           Option.map
             (fun f -> ("AttributesToGet", (AttributeNameList.to_json f)))
             v.attributes_to_get;
           Option.map (fun f -> ("Select", (Select.to_json f))) v.select;
           Option.map (fun f -> ("IndexName", (String.to_json f)))
             v.index_name;
           Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        index_name =
          (Option.map String.of_json (Aws.Json.lookup j "IndexName"));
        select = (Option.map Select.of_json (Aws.Json.lookup j "Select"));
        attributes_to_get =
          (Option.map AttributeNameList.of_json
             (Aws.Json.lookup j "AttributesToGet"));
        limit = (Option.map Integer.of_json (Aws.Json.lookup j "Limit"));
        consistent_read =
          (Option.map Boolean.of_json (Aws.Json.lookup j "ConsistentRead"));
        key_conditions =
          (Option.map KeyConditions.of_json
             (Aws.Json.lookup j "KeyConditions"));
        query_filter =
          (Option.map FilterConditionMap.of_json
             (Aws.Json.lookup j "QueryFilter"));
        conditional_operator =
          (Option.map ConditionalOperator.of_json
             (Aws.Json.lookup j "ConditionalOperator"));
        scan_index_forward =
          (Option.map Boolean.of_json (Aws.Json.lookup j "ScanIndexForward"));
        exclusive_start_key =
          (Option.map Key.of_json (Aws.Json.lookup j "ExclusiveStartKey"));
        return_consumed_capacity =
          (Option.map ReturnConsumedCapacity.of_json
             (Aws.Json.lookup j "ReturnConsumedCapacity"));
        projection_expression =
          (Option.map String.of_json
             (Aws.Json.lookup j "ProjectionExpression"));
        filter_expression =
          (Option.map String.of_json (Aws.Json.lookup j "FilterExpression"));
        key_condition_expression =
          (Option.map String.of_json
             (Aws.Json.lookup j "KeyConditionExpression"));
        expression_attribute_names =
          (Option.map ExpressionAttributeNameMap.of_json
             (Aws.Json.lookup j "ExpressionAttributeNames"));
        expression_attribute_values =
          (Option.map ExpressionAttributeValueMap.of_json
             (Aws.Json.lookup j "ExpressionAttributeValues"))
      }
  end
module PutItemOutput =
  struct
    type t =
      {
      attributes: AttributeMap.t option ;
      consumed_capacity: ConsumedCapacity.t option ;
      item_collection_metrics: ItemCollectionMetrics.t option }
    let make ?attributes  ?consumed_capacity  ?item_collection_metrics  () =
      { attributes; consumed_capacity; item_collection_metrics }
    let parse xml =
      Some
        {
          attributes =
            (Option.bind (Aws.Xml.member "Attributes" xml) AttributeMap.parse);
          consumed_capacity =
            (Option.bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse);
          item_collection_metrics =
            (Option.bind (Aws.Xml.member "ItemCollectionMetrics" xml)
               ItemCollectionMetrics.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ItemCollectionMetrics",
                     (ItemCollectionMetrics.to_query f)))
              v.item_collection_metrics;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ConsumedCapacity", (ConsumedCapacity.to_query f)))
             v.consumed_capacity;
           Option.map
             (fun f ->
                Aws.Query.Pair ("Attributes", (AttributeMap.to_query f)))
             v.attributes])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("ItemCollectionMetrics", (ItemCollectionMetrics.to_json f)))
              v.item_collection_metrics;
           Option.map
             (fun f -> ("ConsumedCapacity", (ConsumedCapacity.to_json f)))
             v.consumed_capacity;
           Option.map (fun f -> ("Attributes", (AttributeMap.to_json f)))
             v.attributes])
    let of_json j =
      {
        attributes =
          (Option.map AttributeMap.of_json (Aws.Json.lookup j "Attributes"));
        consumed_capacity =
          (Option.map ConsumedCapacity.of_json
             (Aws.Json.lookup j "ConsumedCapacity"));
        item_collection_metrics =
          (Option.map ItemCollectionMetrics.of_json
             (Aws.Json.lookup j "ItemCollectionMetrics"))
      }
  end
module PutItemInput =
  struct
    type t =
      {
      table_name: String.t ;
      item: PutItemInputAttributeMap.t ;
      expected: ExpectedAttributeMap.t option ;
      return_values: ReturnValue.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      return_item_collection_metrics: ReturnItemCollectionMetrics.t option ;
      conditional_operator: ConditionalOperator.t option ;
      condition_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option }
    let make ~table_name  ~item  ?expected  ?return_values 
      ?return_consumed_capacity  ?return_item_collection_metrics 
      ?conditional_operator  ?condition_expression 
      ?expression_attribute_names  ?expression_attribute_values  () =
      {
        table_name;
        item;
        expected;
        return_values;
        return_consumed_capacity;
        return_item_collection_metrics;
        conditional_operator;
        condition_expression;
        expression_attribute_names;
        expression_attribute_values
      }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Option.bind (Aws.Xml.member "TableName" xml) String.parse));
          item =
            (Aws.Xml.required "Item"
               (Option.bind (Aws.Xml.member "Item" xml)
                  PutItemInputAttributeMap.parse));
          expected =
            (Option.bind (Aws.Xml.member "Expected" xml)
               ExpectedAttributeMap.parse);
          return_values =
            (Option.bind (Aws.Xml.member "ReturnValues" xml)
               ReturnValue.parse);
          return_consumed_capacity =
            (Option.bind (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          return_item_collection_metrics =
            (Option.bind (Aws.Xml.member "ReturnItemCollectionMetrics" xml)
               ReturnItemCollectionMetrics.parse);
          conditional_operator =
            (Option.bind (Aws.Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          condition_expression =
            (Option.bind (Aws.Xml.member "ConditionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Option.bind (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Option.bind (Aws.Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ExpressionAttributeValues",
                     (ExpressionAttributeValueMap.to_query f)))
              v.expression_attribute_values;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)))
             v.expression_attribute_names;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ConditionExpression", (String.to_query f)))
             v.condition_expression;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)))
             v.conditional_operator;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ReturnItemCollectionMetrics",
                    (ReturnItemCollectionMetrics.to_query f)))
             v.return_item_collection_metrics;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)))
             v.return_consumed_capacity;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ReturnValues", (ReturnValue.to_query f)))
             v.return_values;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("Expected", (ExpectedAttributeMap.to_query f))) v.expected;
           Some
             (Aws.Query.Pair
                ("Item", (PutItemInputAttributeMap.to_query v.item)));
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("ExpressionAttributeValues",
                   (ExpressionAttributeValueMap.to_json f)))
              v.expression_attribute_values;
           Option.map
             (fun f ->
                ("ExpressionAttributeNames",
                  (ExpressionAttributeNameMap.to_json f)))
             v.expression_attribute_names;
           Option.map (fun f -> ("ConditionExpression", (String.to_json f)))
             v.condition_expression;
           Option.map
             (fun f ->
                ("ConditionalOperator", (ConditionalOperator.to_json f)))
             v.conditional_operator;
           Option.map
             (fun f ->
                ("ReturnItemCollectionMetrics",
                  (ReturnItemCollectionMetrics.to_json f)))
             v.return_item_collection_metrics;
           Option.map
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)))
             v.return_consumed_capacity;
           Option.map (fun f -> ("ReturnValues", (ReturnValue.to_json f)))
             v.return_values;
           Option.map
             (fun f -> ("Expected", (ExpectedAttributeMap.to_json f)))
             v.expected;
           Some ("Item", (PutItemInputAttributeMap.to_json v.item));
           Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        item =
          (PutItemInputAttributeMap.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Item")));
        expected =
          (Option.map ExpectedAttributeMap.of_json
             (Aws.Json.lookup j "Expected"));
        return_values =
          (Option.map ReturnValue.of_json (Aws.Json.lookup j "ReturnValues"));
        return_consumed_capacity =
          (Option.map ReturnConsumedCapacity.of_json
             (Aws.Json.lookup j "ReturnConsumedCapacity"));
        return_item_collection_metrics =
          (Option.map ReturnItemCollectionMetrics.of_json
             (Aws.Json.lookup j "ReturnItemCollectionMetrics"));
        conditional_operator =
          (Option.map ConditionalOperator.of_json
             (Aws.Json.lookup j "ConditionalOperator"));
        condition_expression =
          (Option.map String.of_json
             (Aws.Json.lookup j "ConditionExpression"));
        expression_attribute_names =
          (Option.map ExpressionAttributeNameMap.of_json
             (Aws.Json.lookup j "ExpressionAttributeNames"));
        expression_attribute_values =
          (Option.map ExpressionAttributeValueMap.of_json
             (Aws.Json.lookup j "ExpressionAttributeValues"))
      }
  end
module ProvisionedThroughputExceededException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        { message = (Option.bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))
              v.message])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("message", (String.to_json f))) v.message])
    let of_json j =
      { message = (Option.map String.of_json (Aws.Json.lookup j "message")) }
  end
module ListTablesOutput =
  struct
    type t =
      {
      table_names: TableNameList.t option ;
      last_evaluated_table_name: String.t option }
    let make ?table_names  ?last_evaluated_table_name  () =
      { table_names; last_evaluated_table_name }
    let parse xml =
      Some
        {
          table_names =
            (Option.bind (Aws.Xml.member "TableNames" xml)
               TableNameList.parse);
          last_evaluated_table_name =
            (Option.bind (Aws.Xml.member "LastEvaluatedTableName" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("LastEvaluatedTableName", (String.to_query f)))
              v.last_evaluated_table_name;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("TableNames.member", (TableNameList.to_query f)))
             v.table_names])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f -> ("LastEvaluatedTableName", (String.to_json f)))
              v.last_evaluated_table_name;
           Option.map (fun f -> ("TableNames", (TableNameList.to_json f)))
             v.table_names])
    let of_json j =
      {
        table_names =
          (Option.map TableNameList.of_json (Aws.Json.lookup j "TableNames"));
        last_evaluated_table_name =
          (Option.map String.of_json
             (Aws.Json.lookup j "LastEvaluatedTableName"))
      }
  end
module ListTablesInput =
  struct
    type t =
      {
      exclusive_start_table_name: String.t option ;
      limit: Integer.t option }
    let make ?exclusive_start_table_name  ?limit  () =
      { exclusive_start_table_name; limit }
    let parse xml =
      Some
        {
          exclusive_start_table_name =
            (Option.bind (Aws.Xml.member "ExclusiveStartTableName" xml)
               String.parse);
          limit = (Option.bind (Aws.Xml.member "Limit" xml) Integer.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f -> Aws.Query.Pair ("Limit", (Integer.to_query f)))
              v.limit;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ExclusiveStartTableName", (String.to_query f)))
             v.exclusive_start_table_name])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("Limit", (Integer.to_json f))) v.limit;
           Option.map
             (fun f -> ("ExclusiveStartTableName", (String.to_json f)))
             v.exclusive_start_table_name])
    let of_json j =
      {
        exclusive_start_table_name =
          (Option.map String.of_json
             (Aws.Json.lookup j "ExclusiveStartTableName"));
        limit = (Option.map Integer.of_json (Aws.Json.lookup j "Limit"))
      }
  end
module LimitExceededException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        { message = (Option.bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))
              v.message])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("message", (String.to_json f))) v.message])
    let of_json j =
      { message = (Option.map String.of_json (Aws.Json.lookup j "message")) }
  end
module ItemCollectionSizeLimitExceededException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        { message = (Option.bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))
              v.message])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("message", (String.to_json f))) v.message])
    let of_json j =
      { message = (Option.map String.of_json (Aws.Json.lookup j "message")) }
  end
module InternalServerError =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        { message = (Option.bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))
              v.message])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("message", (String.to_json f))) v.message])
    let of_json j =
      { message = (Option.map String.of_json (Aws.Json.lookup j "message")) }
  end
module GetItemOutput =
  struct
    type t =
      {
      item: AttributeMap.t option ;
      consumed_capacity: ConsumedCapacity.t option }
    let make ?item  ?consumed_capacity  () = { item; consumed_capacity }
    let parse xml =
      Some
        {
          item = (Option.bind (Aws.Xml.member "Item" xml) AttributeMap.parse);
          consumed_capacity =
            (Option.bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ConsumedCapacity", (ConsumedCapacity.to_query f)))
              v.consumed_capacity;
           Option.map
             (fun f -> Aws.Query.Pair ("Item", (AttributeMap.to_query f)))
             v.item])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f -> ("ConsumedCapacity", (ConsumedCapacity.to_json f)))
              v.consumed_capacity;
           Option.map (fun f -> ("Item", (AttributeMap.to_json f))) v.item])
    let of_json j =
      {
        item = (Option.map AttributeMap.of_json (Aws.Json.lookup j "Item"));
        consumed_capacity =
          (Option.map ConsumedCapacity.of_json
             (Aws.Json.lookup j "ConsumedCapacity"))
      }
  end
module GetItemInput =
  struct
    type t =
      {
      table_name: String.t ;
      key: Key.t ;
      attributes_to_get: AttributeNameList.t option ;
      consistent_read: Boolean.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      projection_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option }
    let make ~table_name  ~key  ?attributes_to_get  ?consistent_read 
      ?return_consumed_capacity  ?projection_expression 
      ?expression_attribute_names  () =
      {
        table_name;
        key;
        attributes_to_get;
        consistent_read;
        return_consumed_capacity;
        projection_expression;
        expression_attribute_names
      }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Option.bind (Aws.Xml.member "TableName" xml) String.parse));
          key =
            (Aws.Xml.required "Key"
               (Option.bind (Aws.Xml.member "Key" xml) Key.parse));
          attributes_to_get =
            (Option.bind (Aws.Xml.member "AttributesToGet" xml)
               AttributeNameList.parse);
          consistent_read =
            (Option.bind (Aws.Xml.member "ConsistentRead" xml) Boolean.parse);
          return_consumed_capacity =
            (Option.bind (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          projection_expression =
            (Option.bind (Aws.Xml.member "ProjectionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Option.bind (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ExpressionAttributeNames",
                     (ExpressionAttributeNameMap.to_query f)))
              v.expression_attribute_names;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ProjectionExpression", (String.to_query f)))
             v.projection_expression;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)))
             v.return_consumed_capacity;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ConsistentRead", (Boolean.to_query f)))
             v.consistent_read;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("AttributesToGet.member", (AttributeNameList.to_query f)))
             v.attributes_to_get;
           Some (Aws.Query.Pair ("Key", (Key.to_query v.key)));
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("ExpressionAttributeNames",
                   (ExpressionAttributeNameMap.to_json f)))
              v.expression_attribute_names;
           Option.map (fun f -> ("ProjectionExpression", (String.to_json f)))
             v.projection_expression;
           Option.map
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)))
             v.return_consumed_capacity;
           Option.map (fun f -> ("ConsistentRead", (Boolean.to_json f)))
             v.consistent_read;
           Option.map
             (fun f -> ("AttributesToGet", (AttributeNameList.to_json f)))
             v.attributes_to_get;
           Some ("Key", (Key.to_json v.key));
           Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        key =
          (Key.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        attributes_to_get =
          (Option.map AttributeNameList.of_json
             (Aws.Json.lookup j "AttributesToGet"));
        consistent_read =
          (Option.map Boolean.of_json (Aws.Json.lookup j "ConsistentRead"));
        return_consumed_capacity =
          (Option.map ReturnConsumedCapacity.of_json
             (Aws.Json.lookup j "ReturnConsumedCapacity"));
        projection_expression =
          (Option.map String.of_json
             (Aws.Json.lookup j "ProjectionExpression"));
        expression_attribute_names =
          (Option.map ExpressionAttributeNameMap.of_json
             (Aws.Json.lookup j "ExpressionAttributeNames"))
      }
  end
module DescribeTableOutput =
  struct
    type t = {
      table: TableDescription.t option }
    let make ?table  () = { table }
    let parse xml =
      Some
        {
          table =
            (Option.bind (Aws.Xml.member "Table" xml) TableDescription.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair ("Table", (TableDescription.to_query f)))
              v.table])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("Table", (TableDescription.to_json f)))
              v.table])
    let of_json j =
      {
        table =
          (Option.map TableDescription.of_json (Aws.Json.lookup j "Table"))
      }
  end
module DescribeTableInput =
  struct
    type t = {
      table_name: String.t }
    let make ~table_name  () = { table_name }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Option.bind (Aws.Xml.member "TableName" xml) String.parse))
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Some
              (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")))
      }
  end
module DeleteTableOutput =
  struct
    type t = {
      table_description: TableDescription.t option }
    let make ?table_description  () = { table_description }
    let parse xml =
      Some
        {
          table_description =
            (Option.bind (Aws.Xml.member "TableDescription" xml)
               TableDescription.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("TableDescription", (TableDescription.to_query f)))
              v.table_description])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f -> ("TableDescription", (TableDescription.to_json f)))
              v.table_description])
    let of_json j =
      {
        table_description =
          (Option.map TableDescription.of_json
             (Aws.Json.lookup j "TableDescription"))
      }
  end
module DeleteTableInput =
  struct
    type t = {
      table_name: String.t }
    let make ~table_name  () = { table_name }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Option.bind (Aws.Xml.member "TableName" xml) String.parse))
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Some
              (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")))
      }
  end
module DeleteItemOutput =
  struct
    type t =
      {
      attributes: AttributeMap.t option ;
      consumed_capacity: ConsumedCapacity.t option ;
      item_collection_metrics: ItemCollectionMetrics.t option }
    let make ?attributes  ?consumed_capacity  ?item_collection_metrics  () =
      { attributes; consumed_capacity; item_collection_metrics }
    let parse xml =
      Some
        {
          attributes =
            (Option.bind (Aws.Xml.member "Attributes" xml) AttributeMap.parse);
          consumed_capacity =
            (Option.bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacity.parse);
          item_collection_metrics =
            (Option.bind (Aws.Xml.member "ItemCollectionMetrics" xml)
               ItemCollectionMetrics.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ItemCollectionMetrics",
                     (ItemCollectionMetrics.to_query f)))
              v.item_collection_metrics;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ConsumedCapacity", (ConsumedCapacity.to_query f)))
             v.consumed_capacity;
           Option.map
             (fun f ->
                Aws.Query.Pair ("Attributes", (AttributeMap.to_query f)))
             v.attributes])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("ItemCollectionMetrics", (ItemCollectionMetrics.to_json f)))
              v.item_collection_metrics;
           Option.map
             (fun f -> ("ConsumedCapacity", (ConsumedCapacity.to_json f)))
             v.consumed_capacity;
           Option.map (fun f -> ("Attributes", (AttributeMap.to_json f)))
             v.attributes])
    let of_json j =
      {
        attributes =
          (Option.map AttributeMap.of_json (Aws.Json.lookup j "Attributes"));
        consumed_capacity =
          (Option.map ConsumedCapacity.of_json
             (Aws.Json.lookup j "ConsumedCapacity"));
        item_collection_metrics =
          (Option.map ItemCollectionMetrics.of_json
             (Aws.Json.lookup j "ItemCollectionMetrics"))
      }
  end
module DeleteItemInput =
  struct
    type t =
      {
      table_name: String.t ;
      key: Key.t ;
      expected: ExpectedAttributeMap.t option ;
      conditional_operator: ConditionalOperator.t option ;
      return_values: ReturnValue.t option ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      return_item_collection_metrics: ReturnItemCollectionMetrics.t option ;
      condition_expression: String.t option ;
      expression_attribute_names: ExpressionAttributeNameMap.t option ;
      expression_attribute_values: ExpressionAttributeValueMap.t option }
    let make ~table_name  ~key  ?expected  ?conditional_operator 
      ?return_values  ?return_consumed_capacity 
      ?return_item_collection_metrics  ?condition_expression 
      ?expression_attribute_names  ?expression_attribute_values  () =
      {
        table_name;
        key;
        expected;
        conditional_operator;
        return_values;
        return_consumed_capacity;
        return_item_collection_metrics;
        condition_expression;
        expression_attribute_names;
        expression_attribute_values
      }
    let parse xml =
      Some
        {
          table_name =
            (Aws.Xml.required "TableName"
               (Option.bind (Aws.Xml.member "TableName" xml) String.parse));
          key =
            (Aws.Xml.required "Key"
               (Option.bind (Aws.Xml.member "Key" xml) Key.parse));
          expected =
            (Option.bind (Aws.Xml.member "Expected" xml)
               ExpectedAttributeMap.parse);
          conditional_operator =
            (Option.bind (Aws.Xml.member "ConditionalOperator" xml)
               ConditionalOperator.parse);
          return_values =
            (Option.bind (Aws.Xml.member "ReturnValues" xml)
               ReturnValue.parse);
          return_consumed_capacity =
            (Option.bind (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          return_item_collection_metrics =
            (Option.bind (Aws.Xml.member "ReturnItemCollectionMetrics" xml)
               ReturnItemCollectionMetrics.parse);
          condition_expression =
            (Option.bind (Aws.Xml.member "ConditionExpression" xml)
               String.parse);
          expression_attribute_names =
            (Option.bind (Aws.Xml.member "ExpressionAttributeNames" xml)
               ExpressionAttributeNameMap.parse);
          expression_attribute_values =
            (Option.bind (Aws.Xml.member "ExpressionAttributeValues" xml)
               ExpressionAttributeValueMap.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ExpressionAttributeValues",
                     (ExpressionAttributeValueMap.to_query f)))
              v.expression_attribute_values;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ExpressionAttributeNames",
                    (ExpressionAttributeNameMap.to_query f)))
             v.expression_attribute_names;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ConditionExpression", (String.to_query f)))
             v.condition_expression;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ReturnItemCollectionMetrics",
                    (ReturnItemCollectionMetrics.to_query f)))
             v.return_item_collection_metrics;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)))
             v.return_consumed_capacity;
           Option.map
             (fun f ->
                Aws.Query.Pair ("ReturnValues", (ReturnValue.to_query f)))
             v.return_values;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ConditionalOperator", (ConditionalOperator.to_query f)))
             v.conditional_operator;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("Expected", (ExpectedAttributeMap.to_query f))) v.expected;
           Some (Aws.Query.Pair ("Key", (Key.to_query v.key)));
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("ExpressionAttributeValues",
                   (ExpressionAttributeValueMap.to_json f)))
              v.expression_attribute_values;
           Option.map
             (fun f ->
                ("ExpressionAttributeNames",
                  (ExpressionAttributeNameMap.to_json f)))
             v.expression_attribute_names;
           Option.map (fun f -> ("ConditionExpression", (String.to_json f)))
             v.condition_expression;
           Option.map
             (fun f ->
                ("ReturnItemCollectionMetrics",
                  (ReturnItemCollectionMetrics.to_json f)))
             v.return_item_collection_metrics;
           Option.map
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)))
             v.return_consumed_capacity;
           Option.map (fun f -> ("ReturnValues", (ReturnValue.to_json f)))
             v.return_values;
           Option.map
             (fun f ->
                ("ConditionalOperator", (ConditionalOperator.to_json f)))
             v.conditional_operator;
           Option.map
             (fun f -> ("Expected", (ExpectedAttributeMap.to_json f)))
             v.expected;
           Some ("Key", (Key.to_json v.key));
           Some ("TableName", (String.to_json v.table_name))])
    let of_json j =
      {
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        key =
          (Key.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        expected =
          (Option.map ExpectedAttributeMap.of_json
             (Aws.Json.lookup j "Expected"));
        conditional_operator =
          (Option.map ConditionalOperator.of_json
             (Aws.Json.lookup j "ConditionalOperator"));
        return_values =
          (Option.map ReturnValue.of_json (Aws.Json.lookup j "ReturnValues"));
        return_consumed_capacity =
          (Option.map ReturnConsumedCapacity.of_json
             (Aws.Json.lookup j "ReturnConsumedCapacity"));
        return_item_collection_metrics =
          (Option.map ReturnItemCollectionMetrics.of_json
             (Aws.Json.lookup j "ReturnItemCollectionMetrics"));
        condition_expression =
          (Option.map String.of_json
             (Aws.Json.lookup j "ConditionExpression"));
        expression_attribute_names =
          (Option.map ExpressionAttributeNameMap.of_json
             (Aws.Json.lookup j "ExpressionAttributeNames"));
        expression_attribute_values =
          (Option.map ExpressionAttributeValueMap.of_json
             (Aws.Json.lookup j "ExpressionAttributeValues"))
      }
  end
module CreateTableOutput =
  struct
    type t = {
      table_description: TableDescription.t option }
    let make ?table_description  () = { table_description }
    let parse xml =
      Some
        {
          table_description =
            (Option.bind (Aws.Xml.member "TableDescription" xml)
               TableDescription.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("TableDescription", (TableDescription.to_query f)))
              v.table_description])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f -> ("TableDescription", (TableDescription.to_json f)))
              v.table_description])
    let of_json j =
      {
        table_description =
          (Option.map TableDescription.of_json
             (Aws.Json.lookup j "TableDescription"))
      }
  end
module CreateTableInput =
  struct
    type t =
      {
      attribute_definitions: AttributeDefinitions.t ;
      table_name: String.t ;
      key_schema: KeySchema.t ;
      local_secondary_indexes: LocalSecondaryIndexList.t option ;
      global_secondary_indexes: GlobalSecondaryIndexList.t option ;
      provisioned_throughput: ProvisionedThroughput.t ;
      stream_specification: StreamSpecification.t option }
    let make ~attribute_definitions  ~table_name  ~key_schema 
      ?local_secondary_indexes  ?global_secondary_indexes 
      ~provisioned_throughput  ?stream_specification  () =
      {
        attribute_definitions;
        table_name;
        key_schema;
        local_secondary_indexes;
        global_secondary_indexes;
        provisioned_throughput;
        stream_specification
      }
    let parse xml =
      Some
        {
          attribute_definitions =
            (Aws.Xml.required "AttributeDefinitions"
               (Option.bind (Aws.Xml.member "AttributeDefinitions" xml)
                  AttributeDefinitions.parse));
          table_name =
            (Aws.Xml.required "TableName"
               (Option.bind (Aws.Xml.member "TableName" xml) String.parse));
          key_schema =
            (Aws.Xml.required "KeySchema"
               (Option.bind (Aws.Xml.member "KeySchema" xml) KeySchema.parse));
          local_secondary_indexes =
            (Option.bind (Aws.Xml.member "LocalSecondaryIndexes" xml)
               LocalSecondaryIndexList.parse);
          global_secondary_indexes =
            (Option.bind (Aws.Xml.member "GlobalSecondaryIndexes" xml)
               GlobalSecondaryIndexList.parse);
          provisioned_throughput =
            (Aws.Xml.required "ProvisionedThroughput"
               (Option.bind (Aws.Xml.member "ProvisionedThroughput" xml)
                  ProvisionedThroughput.parse));
          stream_specification =
            (Option.bind (Aws.Xml.member "StreamSpecification" xml)
               StreamSpecification.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("StreamSpecification", (StreamSpecification.to_query f)))
              v.stream_specification;
           Some
             (Aws.Query.Pair
                ("ProvisionedThroughput",
                  (ProvisionedThroughput.to_query v.provisioned_throughput)));
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("GlobalSecondaryIndexes.member",
                    (GlobalSecondaryIndexList.to_query f)))
             v.global_secondary_indexes;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("LocalSecondaryIndexes.member",
                    (LocalSecondaryIndexList.to_query f)))
             v.local_secondary_indexes;
           Some
             (Aws.Query.Pair
                ("KeySchema.member", (KeySchema.to_query v.key_schema)));
           Some
             (Aws.Query.Pair ("TableName", (String.to_query v.table_name)));
           Some
             (Aws.Query.Pair
                ("AttributeDefinitions.member",
                  (AttributeDefinitions.to_query v.attribute_definitions)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("StreamSpecification", (StreamSpecification.to_json f)))
              v.stream_specification;
           Some
             ("ProvisionedThroughput",
               (ProvisionedThroughput.to_json v.provisioned_throughput));
           Option.map
             (fun f ->
                ("GlobalSecondaryIndexes",
                  (GlobalSecondaryIndexList.to_json f)))
             v.global_secondary_indexes;
           Option.map
             (fun f ->
                ("LocalSecondaryIndexes",
                  (LocalSecondaryIndexList.to_json f)))
             v.local_secondary_indexes;
           Some ("KeySchema", (KeySchema.to_json v.key_schema));
           Some ("TableName", (String.to_json v.table_name));
           Some
             ("AttributeDefinitions",
               (AttributeDefinitions.to_json v.attribute_definitions))])
    let of_json j =
      {
        attribute_definitions =
          (AttributeDefinitions.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "AttributeDefinitions")));
        table_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TableName")));
        key_schema =
          (KeySchema.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeySchema")));
        local_secondary_indexes =
          (Option.map LocalSecondaryIndexList.of_json
             (Aws.Json.lookup j "LocalSecondaryIndexes"));
        global_secondary_indexes =
          (Option.map GlobalSecondaryIndexList.of_json
             (Aws.Json.lookup j "GlobalSecondaryIndexes"));
        provisioned_throughput =
          (ProvisionedThroughput.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "ProvisionedThroughput")));
        stream_specification =
          (Option.map StreamSpecification.of_json
             (Aws.Json.lookup j "StreamSpecification"))
      }
  end
module ConditionalCheckFailedException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        { message = (Option.bind (Aws.Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f -> Aws.Query.Pair ("message", (String.to_query f)))
              v.message])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map (fun f -> ("message", (String.to_json f))) v.message])
    let of_json j =
      { message = (Option.map String.of_json (Aws.Json.lookup j "message")) }
  end
module BatchWriteItemOutput =
  struct
    type t =
      {
      unprocessed_items: BatchWriteItemRequestMap.t option ;
      item_collection_metrics: ItemCollectionMetricsPerTable.t option ;
      consumed_capacity: ConsumedCapacityMultiple.t option }
    let make ?unprocessed_items  ?item_collection_metrics  ?consumed_capacity
       () = { unprocessed_items; item_collection_metrics; consumed_capacity }
    let parse xml =
      Some
        {
          unprocessed_items =
            (Option.bind (Aws.Xml.member "UnprocessedItems" xml)
               BatchWriteItemRequestMap.parse);
          item_collection_metrics =
            (Option.bind (Aws.Xml.member "ItemCollectionMetrics" xml)
               ItemCollectionMetricsPerTable.parse);
          consumed_capacity =
            (Option.bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacityMultiple.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ConsumedCapacity.member",
                     (ConsumedCapacityMultiple.to_query f)))
              v.consumed_capacity;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ItemCollectionMetrics",
                    (ItemCollectionMetricsPerTable.to_query f)))
             v.item_collection_metrics;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("UnprocessedItems", (BatchWriteItemRequestMap.to_query f)))
             v.unprocessed_items])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("ConsumedCapacity", (ConsumedCapacityMultiple.to_json f)))
              v.consumed_capacity;
           Option.map
             (fun f ->
                ("ItemCollectionMetrics",
                  (ItemCollectionMetricsPerTable.to_json f)))
             v.item_collection_metrics;
           Option.map
             (fun f ->
                ("UnprocessedItems", (BatchWriteItemRequestMap.to_json f)))
             v.unprocessed_items])
    let of_json j =
      {
        unprocessed_items =
          (Option.map BatchWriteItemRequestMap.of_json
             (Aws.Json.lookup j "UnprocessedItems"));
        item_collection_metrics =
          (Option.map ItemCollectionMetricsPerTable.of_json
             (Aws.Json.lookup j "ItemCollectionMetrics"));
        consumed_capacity =
          (Option.map ConsumedCapacityMultiple.of_json
             (Aws.Json.lookup j "ConsumedCapacity"))
      }
  end
module BatchWriteItemInput =
  struct
    type t =
      {
      request_items: BatchWriteItemRequestMap.t ;
      return_consumed_capacity: ReturnConsumedCapacity.t option ;
      return_item_collection_metrics: ReturnItemCollectionMetrics.t option }
    let make ~request_items  ?return_consumed_capacity 
      ?return_item_collection_metrics  () =
      {
        request_items;
        return_consumed_capacity;
        return_item_collection_metrics
      }
    let parse xml =
      Some
        {
          request_items =
            (Aws.Xml.required "RequestItems"
               (Option.bind (Aws.Xml.member "RequestItems" xml)
                  BatchWriteItemRequestMap.parse));
          return_consumed_capacity =
            (Option.bind (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse);
          return_item_collection_metrics =
            (Option.bind (Aws.Xml.member "ReturnItemCollectionMetrics" xml)
               ReturnItemCollectionMetrics.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ReturnItemCollectionMetrics",
                     (ReturnItemCollectionMetrics.to_query f)))
              v.return_item_collection_metrics;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("ReturnConsumedCapacity",
                    (ReturnConsumedCapacity.to_query f)))
             v.return_consumed_capacity;
           Some
             (Aws.Query.Pair
                ("RequestItems",
                  (BatchWriteItemRequestMap.to_query v.request_items)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("ReturnItemCollectionMetrics",
                   (ReturnItemCollectionMetrics.to_json f)))
              v.return_item_collection_metrics;
           Option.map
             (fun f ->
                ("ReturnConsumedCapacity",
                  (ReturnConsumedCapacity.to_json f)))
             v.return_consumed_capacity;
           Some
             ("RequestItems",
               (BatchWriteItemRequestMap.to_json v.request_items))])
    let of_json j =
      {
        request_items =
          (BatchWriteItemRequestMap.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "RequestItems")));
        return_consumed_capacity =
          (Option.map ReturnConsumedCapacity.of_json
             (Aws.Json.lookup j "ReturnConsumedCapacity"));
        return_item_collection_metrics =
          (Option.map ReturnItemCollectionMetrics.of_json
             (Aws.Json.lookup j "ReturnItemCollectionMetrics"))
      }
  end
module BatchGetItemOutput =
  struct
    type t =
      {
      responses: BatchGetResponseMap.t option ;
      unprocessed_keys: BatchGetRequestMap.t option ;
      consumed_capacity: ConsumedCapacityMultiple.t option }
    let make ?responses  ?unprocessed_keys  ?consumed_capacity  () =
      { responses; unprocessed_keys; consumed_capacity }
    let parse xml =
      Some
        {
          responses =
            (Option.bind (Aws.Xml.member "Responses" xml)
               BatchGetResponseMap.parse);
          unprocessed_keys =
            (Option.bind (Aws.Xml.member "UnprocessedKeys" xml)
               BatchGetRequestMap.parse);
          consumed_capacity =
            (Option.bind (Aws.Xml.member "ConsumedCapacity" xml)
               ConsumedCapacityMultiple.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ConsumedCapacity.member",
                     (ConsumedCapacityMultiple.to_query f)))
              v.consumed_capacity;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("UnprocessedKeys", (BatchGetRequestMap.to_query f)))
             v.unprocessed_keys;
           Option.map
             (fun f ->
                Aws.Query.Pair
                  ("Responses", (BatchGetResponseMap.to_query f)))
             v.responses])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("ConsumedCapacity", (ConsumedCapacityMultiple.to_json f)))
              v.consumed_capacity;
           Option.map
             (fun f -> ("UnprocessedKeys", (BatchGetRequestMap.to_json f)))
             v.unprocessed_keys;
           Option.map
             (fun f -> ("Responses", (BatchGetResponseMap.to_json f)))
             v.responses])
    let of_json j =
      {
        responses =
          (Option.map BatchGetResponseMap.of_json
             (Aws.Json.lookup j "Responses"));
        unprocessed_keys =
          (Option.map BatchGetRequestMap.of_json
             (Aws.Json.lookup j "UnprocessedKeys"));
        consumed_capacity =
          (Option.map ConsumedCapacityMultiple.of_json
             (Aws.Json.lookup j "ConsumedCapacity"))
      }
  end
module BatchGetItemInput =
  struct
    type t =
      {
      request_items: BatchGetRequestMap.t ;
      return_consumed_capacity: ReturnConsumedCapacity.t option }
    let make ~request_items  ?return_consumed_capacity  () =
      { request_items; return_consumed_capacity }
    let parse xml =
      Some
        {
          request_items =
            (Aws.Xml.required "RequestItems"
               (Option.bind (Aws.Xml.member "RequestItems" xml)
                  BatchGetRequestMap.parse));
          return_consumed_capacity =
            (Option.bind (Aws.Xml.member "ReturnConsumedCapacity" xml)
               ReturnConsumedCapacity.parse)
        }
    let to_query v =
      Aws.Query.List
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 Aws.Query.Pair
                   ("ReturnConsumedCapacity",
                     (ReturnConsumedCapacity.to_query f)))
              v.return_consumed_capacity;
           Some
             (Aws.Query.Pair
                ("RequestItems",
                  (BatchGetRequestMap.to_query v.request_items)))])
    let to_json v =
      `Assoc
        (List.filter_map Fun.id
           [Option.map
              (fun f ->
                 ("ReturnConsumedCapacity",
                   (ReturnConsumedCapacity.to_json f)))
              v.return_consumed_capacity;
           Some
             ("RequestItems", (BatchGetRequestMap.to_json v.request_items))])
    let of_json j =
      {
        request_items =
          (BatchGetRequestMap.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "RequestItems")));
        return_consumed_capacity =
          (Option.map ReturnConsumedCapacity.of_json
             (Aws.Json.lookup j "ReturnConsumedCapacity"))
      }
  end