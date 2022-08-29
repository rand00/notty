open Notty
open Lwt.Infix

(* XXX General, common up, possibly Mirage-Types. *)
module type SFLOW = sig

  type +'a io

  type input
  type output
  type flow
  type error
  type write_error

  val error_message : error -> string
  val read   : flow -> ([`Input of input | `Eof], error) result io
  val write  : flow -> output -> (unit, write_error) result io
  val writev : flow -> output list -> (unit, write_error) result io
  val close  : flow -> unit io

end


module type SFLOW_LWT = SFLOW with type 'a io = 'a Lwt.t
(* /XXX *)

(** XXX This should die and go to Cstruct. *)
module Cstruct = struct
  include Cstruct
  let to_bytes cs = Bytes.unsafe_of_string (to_string cs)
end
(* /XXX *)

(* Seriously? It's 2016 and this isn't in the standard library?? *)
let rec fmap f = function
  | []    -> []
  | x::xs -> match f x with Some y -> y :: fmap f xs | _ -> fmap f xs


module type TERMINAL_LINK = SFLOW_LWT
  with type input  = [ `Data of Cstruct.t | `Resize of (int * int) ]
   and type output = [ `Data of Cstruct.t | `Line_edit of bool ]

module Term (L : TERMINAL_LINK) = struct

  type 'a io = 'a Lwt.t

  type flow = {
    flow : L.flow
  ; trm  : Tmachine.t
  ; iflt : Unescape.t
  }

  type error = L.error
  type write_error = L.write_error
  let error_message = L.error_message

  type input  = [ Unescape.event | `Resize of (int * int) ]
  type output = [ `Image of image | `Cursor of (int * int) option ]

  let output_buffer = Buffer.create 128_000
  
  let output t =
    let bytes =
      Tmachine.output t.trm output_buffer;
      Buffer.to_bytes output_buffer
    in
    L.write t.flow (`Data (Cstruct.of_bytes bytes)) >|= fun res ->
    Buffer.clear output_buffer;
    res

  let set_size t dim = Tmachine.set_size t.trm dim

  let writev t msgs =
    msgs |> List.iter (function
      | `Cursor curs -> Tmachine.cursor t.trm curs
      | `Image img   -> Tmachine.image t.trm img);
    output t

  let write t msg = writev t [msg]

  let rec read t =
    match Unescape.next t.iflt with
    | #Unescape.event as e -> Lwt_result.return @@ `Input e
    | `End -> Lwt_result.return `Eof
    | `Await ->
      L.read t.flow >>= function
      | Error _ as e -> Lwt.return e
      | Ok `Eof as v -> Lwt.return v
      | Ok (`Input (`Resize dim)) as v ->
        set_size t dim;
        Lwt.return v
      | Ok (`Input (`Data buf)) ->
        Unescape.input t.iflt (Cstruct.to_bytes buf) 0 (Cstruct.length buf);
        read t

  let init_size = 80, 24
  
  (*> goto - correct defaults?*)
  let create
      ?(init_size=init_size)
      ?(mouse=false)
      ?(bpaste=false)
      ?(cap=Cap.ansi)
      flow =
    let t = {
        trm  = Tmachine.create ~mouse ~bpaste cap
      ; flow = flow
      ; iflt = Unescape.create ()
    } in
    set_size t init_size;
    let open Lwt_result.Infix in
    L.write flow (`Line_edit false) >>= fun () ->
    output t >|= fun () ->
    t

  let close t =
    if Tmachine.release t.trm then
      output t >>= fun _ -> L.close t.flow
    else Lwt.return_unit

  let size t = Tmachine.size t.trm

end

module Terminal_link_of_console (C : Mirage_console.S) = struct

  type 'a io = 'a Lwt.t
  type input  = [ `Data of Cstruct.t | `Resize of (int * int) ]
  type output = [ `Data of Cstruct.t | `Line_edit of bool ]
  type flow  = C.t
  type error = C.error
  type write_error = C.write_error
  let error_message err = Fmt.str "%a" C.pp_error err
  let close = C.close
  let writev t xs =
    fmap (function `Data x -> Some x | _ -> None) xs |> C.writev t
  let write t = function
    | `Data buf -> C.write t buf
    | _         -> Lwt_result.return ()
  let read t =
    let open Lwt_result.Infix in
    C.read t >|= function
    | `Eof as v -> v
    | `Data _ as v -> `Input v

end
