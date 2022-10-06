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

module type TERM = sig

  type input_flow
  
  include SFLOW_LWT
    with type input  = [ Unescape.event | `Resize of (int * int) ]
     and type output = [ `Image of image | `Cursor of (int * int) option ]

  val create :
    ?init_size:(int * int) -> ?mouse:bool -> ?bpaste:bool -> ?cap:Notty.Cap.t
    -> input_flow -> (flow, write_error) result Lwt.t

  val size : flow -> int * int

  (* val set_size : flow -> int * int -> unit *)
  
end

module Term (F : TERMINAL_LINK) : TERM with type input_flow = F.flow

module Terminal_link_of_console (C : Mirage_console.S) :
  TERMINAL_LINK with type flow = C.t
