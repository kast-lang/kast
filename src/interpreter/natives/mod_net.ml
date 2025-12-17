open Common

type tcp_stream = {
  file : Unix.file_descr;
  mutable buffer : Bytes.t;
  mutable buf_pos : int;
}

type tcp_listener = { file : Unix.file_descr }

let tcp () =
  [
    native_fn "net.tcp.read_line" (fun _ty ~caller ~state:_ arg : value ->
        match arg |> Value.await_inferred with
        | V_Ref place -> (
            match read_place ~span place |> Value.await_inferred with
            | V_Opaque { ty = _; value = stream } ->
                let stream : tcp_stream = Obj.obj stream in
                let result =
                  ref (Bytes.sub_string stream.buffer 0 stream.buf_pos)
                in
                let rec read_loop () =
                  match String.index_opt !result '\n' with
                  | Some newline_idx ->
                      let after_newline =
                        String.length !result - newline_idx - 1
                      in
                      Bytes.blit_string !result (newline_idx + 1) stream.buffer
                        0 after_newline;
                      stream.buf_pos <- after_newline;
                      result := String.sub !result 0 newline_idx
                  | None ->
                      let bytes_read =
                        Unix.read stream.file stream.buffer stream.buf_pos
                          (Bytes.length stream.buffer - stream.buf_pos)
                      in
                      if bytes_read <= 0 then failwith "YO bytes read <= 0";
                      let read =
                        Bytes.sub_string stream.buffer stream.buf_pos bytes_read
                      in
                      result := !result ^ read;
                      stream.buf_pos <- 0;
                      read_loop ()
                in
                read_loop ();
                V_String !result |> Value.inferred ~span
            | _ ->
                Error.error caller "net.tcp.read_line expected the &tcpstream";
                V_Error |> Value.inferred ~span)
        | _ ->
            Error.error caller "net.tcp.read_line expected the &tcpstream";
            V_Error |> Value.inferred ~span);
    native_fn "net.tcp.write" (fun _ty ~caller:_ ~state:_ arg : value ->
        let args = arg |> Value.expect_tuple |> Option.get in
        let stream, data = args.tuple |> Tuple.unwrap_unnamed2 in
        let stream : tcp_stream =
          stream.place |> claim ~span |> Value.expect_ref |> Option.get
          |> read_place ~span |> Value.expect_opaque |> Option.get
        in
        let data =
          data.place |> claim ~span |> Value.expect_ref |> Option.get
          |> read_place ~span |> Value.expect_string |> Option.get
        in
        let wrote =
          Unix.write_substring stream.file data 0 (String.length data)
        in
        if String.length data <> wrote then failwith "SHORT WRITE";
        V_Unit |> Value.inferred ~span);
    native_fn "net.tcp.connect" (fun ty ~caller ~state:_ arg : value ->
        match Ty.await_inferred ty.result with
        | T_Opaque result_ty -> (
            match arg |> Value.await_inferred with
            | V_String uri ->
                Log.trace (fun log -> log "Connecting to %S" uri);
                let host, port =
                  match String.split_on_char ':' uri with
                  | [ host; port ] -> (host, port)
                  | _ -> failwith "Expected host:port"
                in
                let addr =
                  match Unix.getaddrinfo host port [] with
                  | addr :: _ -> addr
                  | [] -> failwith "could not resolve addr"
                in
                let stream : tcp_stream =
                  {
                    file =
                      Unix.socket addr.ai_family addr.ai_socktype
                        addr.ai_protocol;
                    buffer = Bytes.create 4096;
                    buf_pos = 0;
                  }
                in
                Unix.connect stream.file addr.ai_addr;
                V_Opaque { ty = result_ty; value = Obj.repr stream }
                |> Value.inferred ~span
            | _ ->
                Error.error caller "net.tcp.connect expected string arg";
                V_Error |> Value.inferred ~span)
        | _ ->
            Error.error caller "net.tcp.connect returns type TcpStream";
            V_Error |> Value.inferred ~span);
    native_fn "net.tcp.bind" (fun ty ~caller ~state:_ arg : value ->
        match Ty.await_inferred ty.result with
        | T_Opaque result_ty -> (
            match arg |> Value.await_inferred with
            | V_String uri ->
                Log.trace (fun log -> log "Binding to %S" uri);
                let host, port =
                  match String.split_on_char ':' uri with
                  | [ host; port ] -> (host, port)
                  | _ -> failwith "Expected host:port"
                in
                let addr =
                  match Unix.getaddrinfo host port [] with
                  | addr :: _ -> addr
                  | [] -> failwith "could not resolve addr"
                in
                (* Create socket *)
                let listener : tcp_listener =
                  {
                    file =
                      Unix.socket addr.ai_family addr.ai_socktype
                        addr.ai_protocol;
                  }
                in
                (* Bind socket *)
                Unix.bind listener.file addr.ai_addr;
                V_Opaque { ty = result_ty; value = Obj.repr listener }
                |> Value.inferred ~span
            | _ ->
                Error.error caller "net.tcp.bind expected string arg";
                V_Error |> Value.inferred ~span)
        | _ ->
            Error.error caller "net.tcp.bind returns type TcpStream";
            V_Error |> Value.inferred ~span);
    native_fn "net.tcp.listen" (fun _ty ~caller:_ ~state:_ arg : value ->
        let args = arg |> Value.expect_tuple |> Option.get in
        let stream, max_pending = args.tuple |> Tuple.unwrap_unnamed2 in
        let stream : tcp_stream =
          stream.place |> claim ~span |> Value.expect_ref |> Option.get
          |> read_place ~span |> Value.expect_opaque |> Option.get
        in
        let max_pending =
          max_pending.place |> claim ~span |> Value.expect_int32 |> Option.get
        in
        (* Native call *)
        Unix.listen stream.file (Int32.to_int max_pending);
        V_Unit |> Value.inferred ~span);
    native_fn "net.tcp.accept" (fun ty ~caller ~state:_ arg : value ->
        match Ty.await_inferred ty.result with
        (* Get return type *)
        | T_Tuple ({ tuple = result_ty_field; _ } as result_ty) -> (
            (* Destructure return type tuple *)
            let stream_ty, addr_ty =
              Tuple.unwrap_named2 [ "stream"; "addr" ] result_ty_field
            in
            match
              ( stream_ty.ty |> Ty.await_inferred,
                addr_ty.ty |> Ty.await_inferred )
            with
            (* Assert .stream :: TcpStream & .addr :: String)` *)
            | T_Opaque client_stream_ty, T_String ->
                (* Destructure input argument as expected type *)
                let args = arg |> Value.expect_tuple |> Option.get in
                let stream, close_on_exec =
                  args.tuple |> Tuple.unwrap_unnamed2
                in
                let stream : tcp_stream =
                  stream.place |> claim ~span |> Value.expect_ref |> Option.get
                  |> read_place ~span |> Value.expect_opaque |> Option.get
                in
                let close_on_exec =
                  close_on_exec.place |> claim ~span |> Value.expect_bool
                  |> Option.get
                in
                (* Native call *)
                let client_stream, client_addr =
                  Unix.accept ?cloexec:(Some close_on_exec) stream.file
                in
                let client_stream : tcp_stream =
                  {
                    file = client_stream;
                    buffer = Bytes.create 4096;
                    buf_pos = 0;
                  }
                in
                let client_addr =
                  match client_addr with
                  | Unix.ADDR_UNIX str -> str
                  | Unix.ADDR_INET (addr, port) ->
                      Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port
                in
                (* Make return data *)
                let client_stream_value : Types.value_tuple_field =
                  {
                    place =
                      V_Opaque
                        {
                          ty = client_stream_ty;
                          value = Obj.repr client_stream;
                        }
                      |> Value.inferred ~span |> Place.init;
                    span;
                    ty_field = stream_ty;
                  }
                in
                let client_addr_value : Types.value_tuple_field =
                  {
                    place =
                      V_String client_addr |> Value.inferred ~span |> Place.init;
                    span;
                    ty_field = addr_ty;
                  }
                in
                V_Tuple
                  {
                    ty = result_ty;
                    tuple =
                      Tuple.of_list
                        [
                          (Some "stream", client_stream_value);
                          (Some "addr", client_addr_value);
                        ];
                  }
                |> Value.inferred ~span
            | _ ->
                Error.error caller
                  "net.tcp.accept returns (.stream :: TcpStream, .addr :: String)";
                V_Error |> Value.inferred ~span)
        | _ ->
            Error.error caller "net.tcp.accept returns tuple";
            V_Error |> Value.inferred ~span);
    native_fn "net.tcp.stream.close" (fun _ty ~caller ~state:_ arg : value ->
        match arg |> Value.await_inferred with
        | V_Opaque { ty = _; value = stream } ->
            let stream : tcp_stream = Obj.obj stream in
            (* Native call *)
            Unix.close stream.file;
            V_Unit |> Value.inferred ~span
        | _ ->
            Error.error caller "net.tcp.stream.close expected the tcp.stream";
            V_Error |> Value.inferred ~span);
    native_fn "net.tcp.listener.close" (fun _ty ~caller ~state:_ arg : value ->
        match arg |> Value.await_inferred with
        | V_Opaque { ty = _; value = stream } ->
            let listener : tcp_listener = Obj.obj stream in
            (* Native call *)
            Unix.close listener.file;
            V_Unit |> Value.inferred ~span
        | _ ->
            Error.error caller
              "net.tcp.listener.close expected the tcp.listener";
            V_Error |> Value.inferred ~span);
  ]

let init () = tcp ()
