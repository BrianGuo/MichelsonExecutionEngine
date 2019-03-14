open Script_interpreter 

module type State = sig
  val stack : ('b * 'c) stack
end
(* module State = struct
  type a
  type t = a ty
  let stack = Empty
end *)