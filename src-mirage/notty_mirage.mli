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

(*
 * Ideally, someone working on, say, _telnet_, would map this space out and we
 * would model the input/output types after that.
 *)
module type TERMINAL_LINK = SFLOW_LWT
  with type input  = [ `Data of Cstruct.t | `Resize of (int * int) ]
   and type output = [ `Data of Cstruct.t | `Line_edit of bool ]

open Notty

module Term (F : TERMINAL_LINK) : sig

  include SFLOW_LWT
    with type input  = [ Unescape.event | `Resize of (int * int) ]
     and type output = [ `Image of image | `Cursor of (int * int) option ]

  val create : ?mouse:bool -> ?bpaste:bool -> ?cap:Notty.Cap.t -> F.flow
    -> (flow, write_error) result Lwt.t
end

module Terminal_link_of_console (C : Mirage_console.S) :
  TERMINAL_LINK with type flow = C.t
