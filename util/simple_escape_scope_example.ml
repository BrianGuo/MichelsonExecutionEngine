type annotation = string
type 'ty ty =
  | Unit : annotation -> unit ty
  | Int : annotation -> int ty

type ('arg, 'return) code =
  {
    argument : 'arg ty;
    return : 'return ty;
    code : string;
  }

type ('a, 'b) ty_escaped = 
  {
    arg: 'a;
    arg2: 'b;
  }
type ex_ty = Ex_ty : 'a ty -> ex_ty

(* wrapping the return values with ex_ty guarantees that
the return type is consistent, otherwise we would have unit ty 
and int ty return types in the same pass of the function *)
let generate_arg_ty lst =
  match lst with
  | [] -> Ex_ty (Unit "")
  | _ :: _ -> Ex_ty (Int "")

let generate_return_ty = generate_arg_ty

(* I want the return type of this function to be ('a, 'b) code *)