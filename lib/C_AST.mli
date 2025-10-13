type location = Lexing.position * Lexing.position

type type_decl = { name : string; typ : typ }

and typ =
  | TInt
  | TVoid
  | TPtr of typ
  | TStruct of (string * typ) list
  | TArray of typ * int
  | TFunPtr of typ * typ list
  | TName of string

type comp_op = Eq | Leq
type arith_op = Plus | Minus | Times | Div | Modulo
type bool_op = And | Or

type binop = Comp of comp_op | Arith of arith_op | Bool_op of bool_op
and 'a expr = { descr : 'a expr_descr; info : 'a }

and 'a expr_descr =
  | Const of int
  | Var of string
  | Not of 'a expr  (** !e *)
  | Neg of 'a expr  (** -e *)
  | Addr of string  (** &x *)
  | Ptr of 'a expr  (** *e *)
  | Binop of binop * 'a expr * 'a expr  (** e1 op e2 *)
  | Call of string * 'a expr list  (** f(e1,...,en) *)
  | Array of 'a expr * 'a expr  (** e1[e2]*)
  | Dot of 'a expr * string  (** e.x *)
  | Arrow of 'a expr * string  (** e->x *)

type 'a instruction =
  | Seq of 'a instruction * 'a instruction  (** i1;i2 *)
  | Return of 'a expr option
  | While of 'a expr * 'a instruction
  | IfTE of 'a expr * 'a instruction * 'a instruction
  | Set of string * 'a expr  (** x=e *)
  | Store of 'a expr * 'a expr  (** *e1=e2 *)
  | ArraySet of 'a expr * 'a expr * 'a expr  (**e1[e2] = e3*)
  | StructSet of 'a expr * string * 'a expr  (**e1.x = e2*)
  | ArrowSet of 'a expr * string * 'a expr  (**e1->x = e2*)
  | ICall of string * 'a expr list
      (** on peut appeler une fonction et ignorer sa valeur de retour *)
  | Skip

type 'a global_decl = { global : string; typ : typ; value : 'a expr option }

type 'a fun_decl = {
  f_name : string;
  return_type : typ;
  args : (string * typ) list;
  var_decl : (string * typ) list;
  body : 'a instruction;
}

type 'a decl =
  | DGlob of 'a global_decl
  | DFun of 'a fun_decl
  | DTyp of type_decl

type 'a prog = 'a decl list
type untyped_prog = location prog

type typed_info = { typ : typ; size : int; loc : location }
(** in the typed AST, every expression is annotated with its type and its size,
    every named type should be inlined *)

(*****************************************************************************)
(*below we specify a type of error, every error you encounter should be raised with 
[error loc err]*)

type error_kind =
  | TypeCheck of typ * typ  (**could not assert equivalence between two types *)
  | UndefinedVar of string  (**the variable was not initialised*)
  | UndefinedFunc of string  (** the called function is undefined*)
  | UndefinedTyp of string  (**undefined type name*)
  | NoReturn of string  (**a function has branch with no return*)
  | NoMain  (**The program has no main function*)
  | FunctionWrongArgNum of string
      (**a function is called/defined with wrong number of arguments, also raise
         this error if a function has more than 6 arguments*)
  | ArgumentTooBig of string * string * typ
      (**the argument of a function has a size larger than 8 bytes
         [ArgumentTooBig(x,f,typ)]*)
  | NoStructField of typ * string  (**struct has no such field*)

type generic_location = Location of location | WholeProgram

exception AST_Error of generic_location * error_kind

val error : generic_location -> error_kind -> 'a

type typed_prog = typed_info prog

(*****************************************************************************)
(**pretty printers and dumpers*)

val pp_typ : Format.formatter -> typ -> unit
val pp_location : Format.formatter -> location -> unit
val pp_error_kind : Format.formatter -> error_kind -> unit

val pp_prog :
  ((Format.formatter -> 'a expr_descr -> unit) ->
  Format.formatter ->
  'a expr_descr * 'a ->
  unit) ->
  Format.formatter ->
  'a prog ->
  unit

val dump_parsed_dot : basename:string -> untyped_prog -> unit
val dump_typed_dot : basename:string -> typed_prog -> unit
