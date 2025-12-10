open Std

type event_type =
  | Begin
  | End

module Event = struct
  type t = {
    name : string;
    categories : string list;
    ty : event_type;
    timestamp_ns : float;
    pid : int;
    tid : int;
    args : Yojson.t StringMap.t;
  }

  let yojson_of_t { name; categories; ty; timestamp_ns; pid; tid; args } :
      Yojson.t =
    `Assoc
      [
        ("name", `String name);
        ("cat", `String (String.concat "," categories));
        ( "ph",
          `String
            (match ty with
            | Begin -> "B"
            | End -> "E") );
        ("ts", `Float timestamp_ns);
        ("pid", `Int pid);
        ("tid", `Int tid);
        ("args", `Assoc (args |> StringMap.to_list));
      ]
end

let out = ref None
let first = ref true

let init (path : string) =
  let ch = open_out path in
  output_string ch "[";
  out := Some ch

let deinit () =
  match !out with
  | None -> ()
  | Some ch -> output_string ch "]"

let timestamp () : float = Sys.time () *. 1000000.0

let event (e : Event.t) =
  match !out with
  | None -> ()
  | Some ch ->
      if !first then first := false else output_string ch ",";
      e |> Event.yojson_of_t |> Yojson.to_channel ch

let begin_ name =
  match !out with
  | None -> ()
  | Some _ ->
      event
        {
          name = name ();
          categories = [];
          ty = Begin;
          timestamp_ns = timestamp ();
          pid = 1;
          tid = 1;
          args = StringMap.empty;
        }

let end_ name =
  match !out with
  | None -> ()
  | Some _ ->
      event
        {
          name = name ();
          categories = [];
          ty = End;
          timestamp_ns = timestamp ();
          pid = 1;
          tid = 1;
          args = StringMap.empty;
        }
