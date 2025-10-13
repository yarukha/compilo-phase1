
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WHILE
    | VAR of (
# 33 "parser.mly"
       (string)
# 16 "parser.ml"
  )
    | TYPEVAR of (
# 34 "parser.mly"
       (string)
# 21 "parser.ml"
  )
    | TYPEDEF
    | TVOID
    | TINT
    | TIMES
    | STRUCT
    | SKIP
    | SEMICOLON
    | RSQR
    | RPAR
    | RETURN
    | RBRACE
    | PLUS
    | OR
    | NOT
    | MODULO
    | MINUS
    | LSQR
    | LPAR
    | LEQ
    | LBRACE
    | INT of (
# 31 "parser.mly"
       (int)
# 46 "parser.ml"
  )
    | IF
    | EQ
    | EOF
    | ELSE
    | DOT
    | DIV
    | COMMA
    | CHAR of (
# 32 "parser.mly"
       (char)
# 58 "parser.ml"
  )
    | ASSIGN
    | ARROW
    | AND
    | AMPERSAND
  
end

include MenhirBasics

# 1 "parser.mly"
  
[@@@warning "-32-37"]
  open C_AST
  type _ declarator = 
    | DVar : string  -> [`Var] declarator
    | DTVar : string -> [`Typ] declarator
    | Darray : 'a declarator * int -> 'a declarator
    | DFun : 'a declarator * (typ list) -> 'a declarator
    | DPtr : 'a declarator -> 'a declarator

  let extract_name _ = assert false

  let rec build_type : type a . typ -> a declarator -> (string*typ) 
    = fun  base decl ->
    match decl with 
    | DVar name -> (name,base)
    | DTVar name -> (name,base)
    | DPtr t -> build_type (TPtr base) t
    | Darray (d,n) -> build_type (TArray (base,n)) d
    | DFun (d,args) -> build_type (TFunPtr (base, args)) d

# 91 "parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_prog) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: prog. *)

  | MenhirState002 : (('s, _menhir_box_prog) _menhir_cell1_TYPEDEF, _menhir_box_prog) _menhir_state
    (** State 002.
        Stack shape : TYPEDEF.
        Start symbol: prog. *)

  | MenhirState009 : ((('s, _menhir_box_prog) _menhir_cell1_TYPEDEF, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_state
    (** State 009.
        Stack shape : TYPEDEF simple_typ.
        Start symbol: prog. *)

  | MenhirState011 : (('s, _menhir_box_prog) _menhir_cell1_TIMES, _menhir_box_prog) _menhir_state
    (** State 011.
        Stack shape : TIMES.
        Start symbol: prog. *)

  | MenhirState013 : (('s, _menhir_box_prog) _menhir_cell1_LPAR _menhir_cell0_TIMES, _menhir_box_prog) _menhir_state
    (** State 013.
        Stack shape : LPAR TIMES.
        Start symbol: prog. *)

  | MenhirState018 : (('s, _menhir_box_prog) _menhir_cell1_typedef_direct_declarator _menhir_cell0_LPAR, _menhir_box_prog) _menhir_state
    (** State 018.
        Stack shape : typedef_direct_declarator LPAR.
        Start symbol: prog. *)

  | MenhirState020 : (('s, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_state
    (** State 020.
        Stack shape : simple_typ.
        Start symbol: prog. *)

  | MenhirState031 : (('s, _menhir_box_prog) _menhir_cell1_STRUCT _menhir_cell0_TYPEVAR, _menhir_box_prog) _menhir_state
    (** State 031.
        Stack shape : STRUCT TYPEVAR.
        Start symbol: prog. *)

  | MenhirState033 : (('s, _menhir_box_prog) _menhir_cell1_typed_var, _menhir_box_prog) _menhir_state
    (** State 033.
        Stack shape : typed_var.
        Start symbol: prog. *)

  | MenhirState035 : (('s, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_state
    (** State 035.
        Stack shape : simple_typ.
        Start symbol: prog. *)

  | MenhirState037 : (('s, _menhir_box_prog) _menhir_cell1_TIMES, _menhir_box_prog) _menhir_state
    (** State 037.
        Stack shape : TIMES.
        Start symbol: prog. *)

  | MenhirState039 : (('s, _menhir_box_prog) _menhir_cell1_LPAR _menhir_cell0_TIMES, _menhir_box_prog) _menhir_state
    (** State 039.
        Stack shape : LPAR TIMES.
        Start symbol: prog. *)

  | MenhirState044 : (('s, _menhir_box_prog) _menhir_cell1_direct_declarator _menhir_cell0_LPAR, _menhir_box_prog) _menhir_state
    (** State 044.
        Stack shape : direct_declarator LPAR.
        Start symbol: prog. *)

  | MenhirState056 : (('s, _menhir_box_prog) _menhir_cell1_typed_var, _menhir_box_prog) _menhir_state
    (** State 056.
        Stack shape : typed_var.
        Start symbol: prog. *)

  | MenhirState058 : (('s, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR, _menhir_box_prog) _menhir_state
    (** State 058.
        Stack shape : VAR LPAR.
        Start symbol: prog. *)

  | MenhirState059 : (('s, _menhir_box_prog) _menhir_cell1_TIMES, _menhir_box_prog) _menhir_state
    (** State 059.
        Stack shape : TIMES.
        Start symbol: prog. *)

  | MenhirState060 : (('s, _menhir_box_prog) _menhir_cell1_NOT, _menhir_box_prog) _menhir_state
    (** State 060.
        Stack shape : NOT.
        Start symbol: prog. *)

  | MenhirState061 : (('s, _menhir_box_prog) _menhir_cell1_MINUS, _menhir_box_prog) _menhir_state
    (** State 061.
        Stack shape : MINUS.
        Start symbol: prog. *)

  | MenhirState062 : (('s, _menhir_box_prog) _menhir_cell1_LPAR, _menhir_box_prog) _menhir_state
    (** State 062.
        Stack shape : LPAR.
        Start symbol: prog. *)

  | MenhirState075 : (('s, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 075.
        Stack shape : expr.
        Start symbol: prog. *)

  | MenhirState089 : (('s, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_binop, _menhir_box_prog) _menhir_state
    (** State 089.
        Stack shape : expr binop.
        Start symbol: prog. *)

  | MenhirState099 : (('s, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 099.
        Stack shape : expr.
        Start symbol: prog. *)

  | MenhirState104 : (('s, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_state
    (** State 104.
        Stack shape : simple_typ.
        Start symbol: prog. *)

  | MenhirState106 : ((('s, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR, _menhir_box_prog) _menhir_state
    (** State 106.
        Stack shape : simple_typ VAR LPAR.
        Start symbol: prog. *)

  | MenhirState108 : (('s, _menhir_box_prog) _menhir_cell1_typed_var, _menhir_box_prog) _menhir_state
    (** State 108.
        Stack shape : typed_var.
        Start symbol: prog. *)

  | MenhirState112 : (((('s, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_loption_separated_nonempty_list_COMMA_typed_var__ _menhir_cell0_RPAR, _menhir_box_prog) _menhir_state
    (** State 112.
        Stack shape : simple_typ VAR LPAR loption(separated_nonempty_list(COMMA,typed_var)) RPAR.
        Start symbol: prog. *)

  | MenhirState114 : ((((('s, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_loption_separated_nonempty_list_COMMA_typed_var__ _menhir_cell0_RPAR, _menhir_box_prog) _menhir_cell1_LBRACE, _menhir_box_prog) _menhir_state
    (** State 114.
        Stack shape : simple_typ VAR LPAR loption(separated_nonempty_list(COMMA,typed_var)) RPAR LBRACE.
        Start symbol: prog. *)

  | MenhirState116 : (('s, _menhir_box_prog) _menhir_cell1_typed_var, _menhir_box_prog) _menhir_state
    (** State 116.
        Stack shape : typed_var.
        Start symbol: prog. *)

  | MenhirState118 : (('s, _menhir_box_prog) _menhir_cell1_typed_var, _menhir_box_prog) _menhir_state
    (** State 118.
        Stack shape : typed_var.
        Start symbol: prog. *)

  | MenhirState120 : ((('s, _menhir_box_prog) _menhir_cell1_typed_var, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 120.
        Stack shape : typed_var expr.
        Start symbol: prog. *)

  | MenhirState122 : (((((('s, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_loption_separated_nonempty_list_COMMA_typed_var__ _menhir_cell0_RPAR, _menhir_box_prog) _menhir_cell1_LBRACE, _menhir_box_prog) _menhir_cell1_local_declarations, _menhir_box_prog) _menhir_state
    (** State 122.
        Stack shape : simple_typ VAR LPAR loption(separated_nonempty_list(COMMA,typed_var)) RPAR LBRACE local_declarations.
        Start symbol: prog. *)

  | MenhirState124 : (('s, _menhir_box_prog) _menhir_cell1_WHILE _menhir_cell0_LPAR, _menhir_box_prog) _menhir_state
    (** State 124.
        Stack shape : WHILE LPAR.
        Start symbol: prog. *)

  | MenhirState127 : ((('s, _menhir_box_prog) _menhir_cell1_WHILE _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_RPAR, _menhir_box_prog) _menhir_state
    (** State 127.
        Stack shape : WHILE LPAR expr RPAR.
        Start symbol: prog. *)

  | MenhirState129 : (('s, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR, _menhir_box_prog) _menhir_state
    (** State 129.
        Stack shape : VAR LPAR.
        Start symbol: prog. *)

  | MenhirState132 : (('s, _menhir_box_prog) _menhir_cell1_VAR, _menhir_box_prog) _menhir_state
    (** State 132.
        Stack shape : VAR.
        Start symbol: prog. *)

  | MenhirState134 : (('s, _menhir_box_prog) _menhir_cell1_TIMES, _menhir_box_prog) _menhir_state
    (** State 134.
        Stack shape : TIMES.
        Start symbol: prog. *)

  | MenhirState136 : ((('s, _menhir_box_prog) _menhir_cell1_TIMES, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 136.
        Stack shape : TIMES expr.
        Start symbol: prog. *)

  | MenhirState138 : (('s, _menhir_box_prog) _menhir_cell1_RETURN, _menhir_box_prog) _menhir_state
    (** State 138.
        Stack shape : RETURN.
        Start symbol: prog. *)

  | MenhirState143 : (('s, _menhir_box_prog) _menhir_cell1_IF _menhir_cell0_LPAR, _menhir_box_prog) _menhir_state
    (** State 143.
        Stack shape : IF LPAR.
        Start symbol: prog. *)

  | MenhirState146 : ((('s, _menhir_box_prog) _menhir_cell1_IF _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_RPAR, _menhir_box_prog) _menhir_state
    (** State 146.
        Stack shape : IF LPAR expr RPAR.
        Start symbol: prog. *)

  | MenhirState148 : (('s, _menhir_box_prog) _menhir_cell1_instruction, _menhir_box_prog) _menhir_state
    (** State 148.
        Stack shape : instruction.
        Start symbol: prog. *)

  | MenhirState151 : (('s, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 151.
        Stack shape : expr.
        Start symbol: prog. *)

  | MenhirState154 : ((('s, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_RSQR, _menhir_box_prog) _menhir_state
    (** State 154.
        Stack shape : expr expr RSQR.
        Start symbol: prog. *)

  | MenhirState158 : (('s, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_VAR, _menhir_box_prog) _menhir_state
    (** State 158.
        Stack shape : expr VAR.
        Start symbol: prog. *)

  | MenhirState162 : (('s, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_VAR, _menhir_box_prog) _menhir_state
    (** State 162.
        Stack shape : expr VAR.
        Start symbol: prog. *)

  | MenhirState166 : (((('s, _menhir_box_prog) _menhir_cell1_IF _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_RPAR, _menhir_box_prog) _menhir_cell1_instruction, _menhir_box_prog) _menhir_state
    (** State 166.
        Stack shape : IF LPAR expr RPAR instruction.
        Start symbol: prog. *)

  | MenhirState168 : ((((('s, _menhir_box_prog) _menhir_cell1_IF _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_RPAR, _menhir_box_prog) _menhir_cell1_instruction, _menhir_box_prog) _menhir_cell1_instruction, _menhir_box_prog) _menhir_state
    (** State 168.
        Stack shape : IF LPAR expr RPAR instruction instruction.
        Start symbol: prog. *)

  | MenhirState171 : (((('s, _menhir_box_prog) _menhir_cell1_WHILE _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_RPAR, _menhir_box_prog) _menhir_cell1_instruction, _menhir_box_prog) _menhir_state
    (** State 171.
        Stack shape : WHILE LPAR expr RPAR instruction.
        Start symbol: prog. *)

  | MenhirState174 : ((((((('s, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_loption_separated_nonempty_list_COMMA_typed_var__ _menhir_cell0_RPAR, _menhir_box_prog) _menhir_cell1_LBRACE, _menhir_box_prog) _menhir_cell1_local_declarations, _menhir_box_prog) _menhir_cell1_instruction, _menhir_box_prog) _menhir_state
    (** State 174.
        Stack shape : simple_typ VAR LPAR loption(separated_nonempty_list(COMMA,typed_var)) RPAR LBRACE local_declarations instruction.
        Start symbol: prog. *)

  | MenhirState182 : (('s, _menhir_box_prog) _menhir_cell1_decl, _menhir_box_prog) _menhir_state
    (** State 182.
        Stack shape : decl.
        Start symbol: prog. *)


and 's _menhir_cell0_binop = 
  | MenhirCell0_binop of 's * (C_AST.binop)

and ('s, 'r) _menhir_cell1_decl = 
  | MenhirCell1_decl of 's * ('s, 'r) _menhir_state * (C_AST.location C_AST.decl)

and ('s, 'r) _menhir_cell1_direct_declarator = 
  | MenhirCell1_direct_declarator of 's * ('s, 'r) _menhir_state * ([ `Var ] declarator)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (C_AST.location C_AST.expr) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_instruction = 
  | MenhirCell1_instruction of 's * ('s, 'r) _menhir_state * (C_AST.location C_AST.instruction)

and ('s, 'r) _menhir_cell1_local_declarations = 
  | MenhirCell1_local_declarations of 's * ('s, 'r) _menhir_state * ((string * C_AST.typ) list * C_AST.location C_AST.instruction)

and ('s, 'r) _menhir_cell1_loption_separated_nonempty_list_COMMA_typed_var__ = 
  | MenhirCell1_loption_separated_nonempty_list_COMMA_typed_var__ of 's * ('s, 'r) _menhir_state * ((string * C_AST.typ) list)

and ('s, 'r) _menhir_cell1_simple_typ = 
  | MenhirCell1_simple_typ of 's * ('s, 'r) _menhir_state * (C_AST.typ)

and ('s, 'r) _menhir_cell1_typed_var = 
  | MenhirCell1_typed_var of 's * ('s, 'r) _menhir_state * (string * C_AST.typ)

and ('s, 'r) _menhir_cell1_typedef_direct_declarator = 
  | MenhirCell1_typedef_direct_declarator of 's * ('s, 'r) _menhir_state * ([ `Typ ] declarator)

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LBRACE = 
  | MenhirCell1_LBRACE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAR = 
  | MenhirCell1_LPAR of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_LPAR = 
  | MenhirCell0_LPAR of 's * Lexing.position

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_NOT = 
  | MenhirCell1_NOT of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_RETURN = 
  | MenhirCell1_RETURN of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_RPAR = 
  | MenhirCell0_RPAR of 's * Lexing.position

and 's _menhir_cell0_RSQR = 
  | MenhirCell0_RSQR of 's * Lexing.position

and ('s, 'r) _menhir_cell1_STRUCT = 
  | MenhirCell1_STRUCT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TIMES = 
  | MenhirCell1_TIMES of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_TIMES = 
  | MenhirCell0_TIMES of 's * Lexing.position

and ('s, 'r) _menhir_cell1_TYPEDEF = 
  | MenhirCell1_TYPEDEF of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_TYPEVAR = 
  | MenhirCell0_TYPEVAR of 's * (
# 34 "parser.mly"
       (string)
# 418 "parser.ml"
)

and ('s, 'r) _menhir_cell1_VAR = 
  | MenhirCell1_VAR of 's * ('s, 'r) _menhir_state * (
# 33 "parser.mly"
       (string)
# 425 "parser.ml"
) * Lexing.position * Lexing.position

and 's _menhir_cell0_VAR = 
  | MenhirCell0_VAR of 's * (
# 33 "parser.mly"
       (string)
# 432 "parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_WHILE = 
  | MenhirCell1_WHILE of 's * ('s, 'r) _menhir_state

and _menhir_box_prog = 
  | MenhirBox_prog of (C_AST.untyped_prog) [@@unboxed]

let _menhir_action_01 =
  fun () ->
    (
# 178 "parser.mly"
       (Plus)
# 446 "parser.ml"
     : (C_AST.arith_op))

let _menhir_action_02 =
  fun () ->
    (
# 178 "parser.mly"
                      (Minus)
# 454 "parser.ml"
     : (C_AST.arith_op))

let _menhir_action_03 =
  fun () ->
    (
# 178 "parser.mly"
                                      (Times)
# 462 "parser.ml"
     : (C_AST.arith_op))

let _menhir_action_04 =
  fun () ->
    (
# 178 "parser.mly"
                                                    (Div)
# 470 "parser.ml"
     : (C_AST.arith_op))

let _menhir_action_05 =
  fun () ->
    (
# 178 "parser.mly"
                                                                  (Modulo)
# 478 "parser.ml"
     : (C_AST.arith_op))

let _menhir_action_06 =
  fun _1 ->
    (
# 170 "parser.mly"
            (Comp _1)
# 486 "parser.ml"
     : (C_AST.binop))

let _menhir_action_07 =
  fun _1 ->
    (
# 171 "parser.mly"
             (Arith _1)
# 494 "parser.ml"
     : (C_AST.binop))

let _menhir_action_08 =
  fun _1 ->
    (
# 172 "parser.mly"
            (Bool_op _1)
# 502 "parser.ml"
     : (C_AST.binop))

let _menhir_action_09 =
  fun () ->
    (
# 181 "parser.mly"
      (And)
# 510 "parser.ml"
     : (C_AST.bool_op))

let _menhir_action_10 =
  fun () ->
    (
# 181 "parser.mly"
                 (Or)
# 518 "parser.ml"
     : (C_AST.bool_op))

let _menhir_action_11 =
  fun () ->
    (
# 175 "parser.mly"
     (Eq)
# 526 "parser.ml"
     : (C_AST.comp_op))

let _menhir_action_12 =
  fun () ->
    (
# 175 "parser.mly"
                (Leq)
# 534 "parser.ml"
     : (C_AST.comp_op))

let _menhir_action_13 =
  fun _1 ->
    (
# 117 "parser.mly"
                (DGlob _1)
# 542 "parser.ml"
     : (C_AST.location C_AST.decl))

let _menhir_action_14 =
  fun _1 ->
    (
# 118 "parser.mly"
             (DFun _1)
# 550 "parser.ml"
     : (C_AST.location C_AST.decl))

let _menhir_action_15 =
  fun _1 ->
    (
# 119 "parser.mly"
              (DTyp _1)
# 558 "parser.ml"
     : (C_AST.location C_AST.decl))

let _menhir_action_16 =
  fun _2 ->
    (
# 87 "parser.mly"
                     (DPtr _2)
# 566 "parser.ml"
     : ([ `Var ] declarator))

let _menhir_action_17 =
  fun _1 ->
    (
# 88 "parser.mly"
                      (_1)
# 574 "parser.ml"
     : ([ `Var ] declarator))

let _menhir_action_18 =
  fun _1 ->
    (
# 91 "parser.mly"
        (DVar _1)
# 582 "parser.ml"
     : ([ `Var ] declarator))

let _menhir_action_19 =
  fun _3 ->
    (
# 92 "parser.mly"
                               (_3)
# 590 "parser.ml"
     : ([ `Var ] declarator))

let _menhir_action_20 =
  fun _1 _3 ->
    (
# 93 "parser.mly"
                                    (Darray(_1,_3))
# 598 "parser.ml"
     : ([ `Var ] declarator))

let _menhir_action_21 =
  fun _1 xs ->
    let _3 = 
# 241 "<standard.mly>"
    ( xs )
# 606 "parser.ml"
     in
    (
# 94 "parser.mly"
                                                                 (DFun (_1,_3))
# 611 "parser.ml"
     : ([ `Var ] declarator))

let _menhir_action_22 =
  fun _endpos_descr_ _startpos_descr_ descr ->
    let _endpos = _endpos_descr_ in
    let _startpos = _startpos_descr_ in
    (
# 151 "parser.mly"
                     ({descr;info = (_startpos,_endpos)})
# 621 "parser.ml"
     : (C_AST.location C_AST.expr))

let _menhir_action_23 =
  fun _1 ->
    (
# 154 "parser.mly"
        (Var _1)
# 629 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_24 =
  fun _1 ->
    (
# 155 "parser.mly"
        (Const _1)
# 637 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_25 =
  fun _1 ->
    (
# 156 "parser.mly"
         (Const (int_of_char _1))
# 645 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_26 =
  fun _1 _2 _3 ->
    (
# 157 "parser.mly"
                    (Binop(_2,_1,_3))
# 653 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_27 =
  fun _2 ->
    (
# 158 "parser.mly"
                  (Addr _2)
# 661 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_28 =
  fun _2 ->
    (
# 159 "parser.mly"
               (Ptr _2)
# 669 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_29 =
  fun _2 ->
    (
# 160 "parser.mly"
             (Not _2)
# 677 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_30 =
  fun _2 ->
    (
# 161 "parser.mly"
               (Neg _2)
# 685 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_31 =
  fun _2 ->
    (
# 162 "parser.mly"
               (Neg _2)
# 693 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_32 =
  fun _1 _3 ->
    (
# 163 "parser.mly"
                 (Dot(_1,_3))
# 701 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_33 =
  fun _1 _3 ->
    (
# 164 "parser.mly"
                   (Arrow(_1,_3))
# 709 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_34 =
  fun _1 xs ->
    let _3 = 
# 241 "<standard.mly>"
    ( xs )
# 717 "parser.ml"
     in
    (
# 165 "parser.mly"
                                             (Call(_1,_3))
# 722 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_35 =
  fun e1 e2 ->
    (
# 166 "parser.mly"
                              (Array(e1,e2))
# 730 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_36 =
  fun _2 ->
    (
# 167 "parser.mly"
                          (_2)
# 738 "parser.ml"
     : (C_AST.location C_AST.expr_descr))

let _menhir_action_37 =
  fun body f_name return_type vd xs ->
    let args = 
# 241 "<standard.mly>"
    ( xs )
# 746 "parser.ml"
     in
    (
# 131 "parser.mly"
      (let var_decl,decls = vd in {f_name;return_type;args;var_decl;body = Seq(decls,body)})
# 751 "parser.ml"
     : (C_AST.location C_AST.fun_decl))

let _menhir_action_38 =
  fun f_name return_type xs ->
    let args = 
# 241 "<standard.mly>"
    ( xs )
# 759 "parser.ml"
     in
    (
# 132 "parser.mly"
                                                                                                       ({f_name;return_type;args;var_decl=[];body=Skip})
# 764 "parser.ml"
     : (C_AST.location C_AST.fun_decl))

let _menhir_action_39 =
  fun _1 ->
    (
# 124 "parser.mly"
                        (let (global,typ) = _1 in {global ;typ;value=None})
# 772 "parser.ml"
     : (C_AST.location C_AST.global_decl))

let _menhir_action_40 =
  fun _1 _3 ->
    (
# 125 "parser.mly"
                                    (let (global,typ) = _1 in {global;typ;value = Some(_3)})
# 780 "parser.ml"
     : (C_AST.location C_AST.global_decl))

let _menhir_action_41 =
  fun e i next ->
    (
# 137 "parser.mly"
                                                                        (Seq(While(e,i),next))
# 788 "parser.ml"
     : (C_AST.location C_AST.instruction))

let _menhir_action_42 =
  fun e else_ if_ next ->
    (
# 138 "parser.mly"
                                                                                                            (Seq(IfTE(e,if_,else_),next))
# 796 "parser.ml"
     : (C_AST.location C_AST.instruction))

let _menhir_action_43 =
  fun _1 _3 ->
    (
# 139 "parser.mly"
                                      (Seq(_1,_3))
# 804 "parser.ml"
     : (C_AST.location C_AST.instruction))

let _menhir_action_44 =
  fun () ->
    (
# 140 "parser.mly"
                     (Return None)
# 812 "parser.ml"
     : (C_AST.location C_AST.instruction))

let _menhir_action_45 =
  fun _2 ->
    (
# 141 "parser.mly"
                          (Return(Some(_2)))
# 820 "parser.ml"
     : (C_AST.location C_AST.instruction))

let _menhir_action_46 =
  fun _1 xs ->
    let _3 = 
# 241 "<standard.mly>"
    ( xs )
# 828 "parser.ml"
     in
    (
# 142 "parser.mly"
                                             (ICall(_1,_3))
# 833 "parser.ml"
     : (C_AST.location C_AST.instruction))

let _menhir_action_47 =
  fun _1 _3 ->
    (
# 143 "parser.mly"
                    (Set(_1,_3))
# 841 "parser.ml"
     : (C_AST.location C_AST.instruction))

let _menhir_action_48 =
  fun _2 _4 ->
    (
# 144 "parser.mly"
                           (Store(_2,_4))
# 849 "parser.ml"
     : (C_AST.location C_AST.instruction))

let _menhir_action_49 =
  fun _1 _3 _5 ->
    (
# 145 "parser.mly"
                             (StructSet(_1,_3,_5))
# 857 "parser.ml"
     : (C_AST.location C_AST.instruction))

let _menhir_action_50 =
  fun _1 _3 _5 ->
    (
# 146 "parser.mly"
                               (ArrowSet(_1,_3,_5))
# 865 "parser.ml"
     : (C_AST.location C_AST.instruction))

let _menhir_action_51 =
  fun e1 e2 e3 ->
    (
# 147 "parser.mly"
                                             (ArraySet(e1,e2,e3))
# 873 "parser.ml"
     : (C_AST.location C_AST.instruction))

let _menhir_action_52 =
  fun () ->
    (
# 148 "parser.mly"
    (Skip)
# 881 "parser.ml"
     : (C_AST.location C_AST.instruction))

let _menhir_action_53 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 889 "parser.ml"
     : (C_AST.untyped_prog))

let _menhir_action_54 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 897 "parser.ml"
     : (C_AST.untyped_prog))

let _menhir_action_55 =
  fun _1 _3 ->
    (
# 109 "parser.mly"
                                           (let (type_decl,value_decl) = _3 in ((_1::type_decl),value_decl))
# 905 "parser.ml"
     : ((string * C_AST.typ) list * C_AST.location C_AST.instruction))

let _menhir_action_56 =
  fun e ld tv ->
    (
# 110 "parser.mly"
                                                               (
      let (x,_) = tv in
      let (type_decl,value_decl) = ld in
      ((tv::type_decl),Seq(Set(x,e),value_decl)))
# 916 "parser.ml"
     : ((string * C_AST.typ) list * C_AST.location C_AST.instruction))

let _menhir_action_57 =
  fun () ->
    (
# 114 "parser.mly"
    (([],Skip))
# 924 "parser.ml"
     : ((string * C_AST.typ) list * C_AST.location C_AST.instruction))

let _menhir_action_58 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 932 "parser.ml"
     : (C_AST.location C_AST.expr list))

let _menhir_action_59 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 940 "parser.ml"
     : (C_AST.location C_AST.expr list))

let _menhir_action_60 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 948 "parser.ml"
     : (C_AST.typ list))

let _menhir_action_61 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 956 "parser.ml"
     : (C_AST.typ list))

let _menhir_action_62 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 964 "parser.ml"
     : ((string * C_AST.typ) list))

let _menhir_action_63 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 972 "parser.ml"
     : ((string * C_AST.typ) list))

let _menhir_action_64 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 980 "parser.ml"
     : (unit option))

let _menhir_action_65 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 988 "parser.ml"
     : (unit option))

let _menhir_action_66 =
  fun _1 ->
    (
# 50 "parser.mly"
                 (_1)
# 996 "parser.ml"
     : (C_AST.untyped_prog))

let _menhir_action_67 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 1004 "parser.ml"
     : (C_AST.location C_AST.expr list))

let _menhir_action_68 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 1012 "parser.ml"
     : (C_AST.location C_AST.expr list))

let _menhir_action_69 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 1020 "parser.ml"
     : (C_AST.typ list))

let _menhir_action_70 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 1028 "parser.ml"
     : (C_AST.typ list))

let _menhir_action_71 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 1036 "parser.ml"
     : ((string * C_AST.typ) list))

let _menhir_action_72 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 1044 "parser.ml"
     : ((string * C_AST.typ) list))

let _menhir_action_73 =
  fun _1 ->
    (
# 78 "parser.mly"
            (TName _1)
# 1052 "parser.ml"
     : (C_AST.typ))

let _menhir_action_74 =
  fun () ->
    (
# 79 "parser.mly"
         (TInt)
# 1060 "parser.ml"
     : (C_AST.typ))

let _menhir_action_75 =
  fun () ->
    (
# 80 "parser.mly"
          (TVoid)
# 1068 "parser.ml"
     : (C_AST.typ))

let _menhir_action_76 =
  fun _2 ->
    (
# 81 "parser.mly"
                   (TName _2)
# 1076 "parser.ml"
     : (C_AST.typ))

let _menhir_action_77 =
  fun _1 _3 ->
    (
# 74 "parser.mly"
                                      (_1::_3)
# 1084 "parser.ml"
     : ((string * C_AST.typ) list))

let _menhir_action_78 =
  fun () ->
    (
# 75 "parser.mly"
    ([])
# 1092 "parser.ml"
     : ((string * C_AST.typ) list))

let _menhir_action_79 =
  fun _2 _4 ->
    (
# 99 "parser.mly"
                                                         ({name=_2;typ=TStruct _4})
# 1100 "parser.ml"
     : (C_AST.type_decl))

let _menhir_action_80 =
  fun _2 ->
    (
# 100 "parser.mly"
                                  (let (name,typ) = _2 in {name;typ})
# 1108 "parser.ml"
     : (C_AST.type_decl))

let _menhir_action_81 =
  fun _1 _2 ->
    (
# 84 "parser.mly"
                          (build_type _1 _2)
# 1116 "parser.ml"
     : (string * C_AST.typ))

let _menhir_action_82 =
  fun _2 ->
    (
# 59 "parser.mly"
                             (DPtr _2)
# 1124 "parser.ml"
     : ([ `Typ ] declarator))

let _menhir_action_83 =
  fun _1 ->
    (
# 60 "parser.mly"
                              (_1)
# 1132 "parser.ml"
     : ([ `Typ ] declarator))

let _menhir_action_84 =
  fun _1 ->
    (
# 64 "parser.mly"
            (DTVar _1)
# 1140 "parser.ml"
     : ([ `Typ ] declarator))

let _menhir_action_85 =
  fun _3 ->
    (
# 65 "parser.mly"
                                       (_3)
# 1148 "parser.ml"
     : ([ `Typ ] declarator))

let _menhir_action_86 =
  fun _1 _3 ->
    (
# 66 "parser.mly"
                                            (Darray(_1,_3))
# 1156 "parser.ml"
     : ([ `Typ ] declarator))

let _menhir_action_87 =
  fun _1 xs ->
    let _3 = 
# 241 "<standard.mly>"
    ( xs )
# 1164 "parser.ml"
     in
    (
# 67 "parser.mly"
                                                                         (DFun (_1,_3))
# 1169 "parser.ml"
     : ([ `Typ ] declarator))

let _menhir_action_88 =
  fun _1 _2 ->
    (
# 56 "parser.mly"
                                  (build_type _1 _2)
# 1177 "parser.ml"
     : (string * C_AST.typ))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AMPERSAND ->
        "AMPERSAND"
    | AND ->
        "AND"
    | ARROW ->
        "ARROW"
    | ASSIGN ->
        "ASSIGN"
    | CHAR _ ->
        "CHAR"
    | COMMA ->
        "COMMA"
    | DIV ->
        "DIV"
    | DOT ->
        "DOT"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | IF ->
        "IF"
    | INT _ ->
        "INT"
    | LBRACE ->
        "LBRACE"
    | LEQ ->
        "LEQ"
    | LPAR ->
        "LPAR"
    | LSQR ->
        "LSQR"
    | MINUS ->
        "MINUS"
    | MODULO ->
        "MODULO"
    | NOT ->
        "NOT"
    | OR ->
        "OR"
    | PLUS ->
        "PLUS"
    | RBRACE ->
        "RBRACE"
    | RETURN ->
        "RETURN"
    | RPAR ->
        "RPAR"
    | RSQR ->
        "RSQR"
    | SEMICOLON ->
        "SEMICOLON"
    | SKIP ->
        "SKIP"
    | STRUCT ->
        "STRUCT"
    | TIMES ->
        "TIMES"
    | TINT ->
        "TINT"
    | TVOID ->
        "TVOID"
    | TYPEDEF ->
        "TYPEDEF"
    | TYPEVAR _ ->
        "TYPEVAR"
    | VAR _ ->
        "VAR"
    | WHILE ->
        "WHILE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_178 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_prog =
    fun _menhir_stack _v ->
      let _1 = _v in
      let _v = _menhir_action_66 _1 in
      MenhirBox_prog _v
  
  let rec _menhir_run_183 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_decl -> _ -> _menhir_box_prog =
    fun _menhir_stack _v ->
      let MenhirCell1_decl (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_54 x xs in
      _menhir_goto_list_decl_ _menhir_stack _v _menhir_s
  
  and _menhir_goto_list_decl_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState182 ->
          _menhir_run_183 _menhir_stack _v
      | MenhirState000 ->
          _menhir_run_178 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_73 _1 in
      _menhir_goto_simple_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_simple_typ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState182 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState018 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState002 ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_104 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_simple_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAR ->
              let _menhir_stack = MenhirCell1_VAR (_menhir_stack, MenhirState104, _v_0, _startpos, _endpos) in
              let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
              let _menhir_stack = MenhirCell0_LPAR (_menhir_stack, _startpos) in
              let _menhir_s = MenhirState106 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TYPEVAR _v ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TVOID ->
                  _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | TINT ->
                  _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | STRUCT ->
                  _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | RPAR ->
                  let _v = _menhir_action_62 () in
                  _menhir_goto_loption_separated_nonempty_list_COMMA_typed_var__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | ASSIGN | LSQR | SEMICOLON ->
              let (_menhir_s, _1) = (MenhirState104, _v_0) in
              let _v = _menhir_action_18 _1 in
              _menhir_goto_direct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | TIMES ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | LPAR ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | _ ->
          _eRR ()
  
  and _menhir_run_003 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_75 () in
      _menhir_goto_simple_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_004 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_74 () in
      _menhir_goto_simple_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_005 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPEVAR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_76 _2 in
          _menhir_goto_simple_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_typed_var__ : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_loption_separated_nonempty_list_COMMA_typed_var__ (_menhir_stack, _menhir_s, _v) in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_RPAR (_menhir_stack, _endpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | LBRACE ->
          let _menhir_stack = MenhirCell1_LBRACE (_menhir_stack, MenhirState112) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEVAR _v_0 ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState114
          | TVOID ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
          | TINT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
          | STRUCT ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState114
          | AMPERSAND | CHAR _ | IF | INT _ | LPAR | MINUS | NOT | RBRACE | RETURN | SEMICOLON | TIMES | VAR _ | WHILE ->
              let _v_1 = _menhir_action_57 () in
              _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState114 _tok
          | _ ->
              _eRR ())
      | EOF | STRUCT | TINT | TVOID | TYPEDEF | TYPEVAR _ ->
          let _ = _menhir_action_64 () in
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_113 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = () in
      let _ = _menhir_action_65 x in
      _menhir_goto_option_SEMICOLON_ _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
  
  and _menhir_goto_option_SEMICOLON_ : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      match _menhir_s with
      | MenhirState112 ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState174 ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_176 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_loption_separated_nonempty_list_COMMA_typed_var__ _menhir_cell0_RPAR -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell0_RPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_loption_separated_nonempty_list_COMMA_typed_var__ (_menhir_stack, _, xs) = _menhir_stack in
      let MenhirCell0_LPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _, f_name, _, _) = _menhir_stack in
      let MenhirCell1_simple_typ (_menhir_stack, _menhir_s, return_type) = _menhir_stack in
      let _v = _menhir_action_38 f_name return_type xs in
      _menhir_goto_fun_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_fun_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_14 _1 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPEVAR _v_0 ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState182
      | TYPEDEF ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState182
      | TVOID ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState182
      | TINT ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState182
      | STRUCT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState182
      | EOF ->
          let _v_1 = _menhir_action_53 () in
          _menhir_run_183 _menhir_stack _v_1
      | _ ->
          _eRR ()
  
  and _menhir_run_002 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TYPEDEF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState002 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPEVAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TVOID ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TINT ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRUCT ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_029 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPEVAR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACE ->
              let _menhir_stack = MenhirCell1_STRUCT (_menhir_stack, _menhir_s) in
              let _menhir_stack = MenhirCell0_TYPEVAR (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TYPEVAR _v_0 ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState031
              | TVOID ->
                  _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
              | TINT ->
                  _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
              | STRUCT ->
                  _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
              | RBRACE ->
                  let _v_1 = _menhir_action_78 () in
                  _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
              | _ ->
                  _eRR ())
          | LPAR | TIMES | VAR _ ->
              let _2 = _v in
              let _v = _menhir_action_76 _2 in
              _menhir_goto_simple_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_051 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_STRUCT _menhir_cell0_TYPEVAR -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_TYPEVAR (_menhir_stack, _2) = _menhir_stack in
          let MenhirCell1_STRUCT (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_79 _2 _4 in
          _menhir_goto_type_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_type_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_15 _1 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_175 : type  ttv_stack. ((((((ttv_stack, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_loption_separated_nonempty_list_COMMA_typed_var__ _menhir_cell0_RPAR, _menhir_box_prog) _menhir_cell1_LBRACE, _menhir_box_prog) _menhir_cell1_local_declarations, _menhir_box_prog) _menhir_cell1_instruction -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_instruction (_menhir_stack, _, body) = _menhir_stack in
      let MenhirCell1_local_declarations (_menhir_stack, _, vd) = _menhir_stack in
      let MenhirCell1_LBRACE (_menhir_stack, _) = _menhir_stack in
      let MenhirCell0_RPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_loption_separated_nonempty_list_COMMA_typed_var__ (_menhir_stack, _, xs) = _menhir_stack in
      let MenhirCell0_LPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _, f_name, _, _) = _menhir_stack in
      let MenhirCell1_simple_typ (_menhir_stack, _menhir_s, return_type) = _menhir_stack in
      let _v = _menhir_action_37 body f_name return_type vd xs in
      _menhir_goto_fun_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_122 : type  ttv_stack. (((((ttv_stack, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_loption_separated_nonempty_list_COMMA_typed_var__ _menhir_cell0_RPAR, _menhir_box_prog) _menhir_cell1_LBRACE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_local_declarations (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState122
      | VAR _v_0 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState122
      | TIMES ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState122
      | RETURN ->
          _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState122
      | NOT ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState122
      | MINUS ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState122
      | LPAR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState122
      | INT _v_1 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState122
      | IF ->
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState122
      | CHAR _v_2 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState122
      | AMPERSAND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState122
      | RBRACE | SEMICOLON ->
          let _v_3 = _menhir_action_52 () in
          _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState122 _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_123 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WHILE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LPAR (_menhir_stack, _startpos) in
          let _menhir_s = MenhirState124 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TIMES ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CHAR _v ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | AMPERSAND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_057 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LPAR (_menhir_stack, _startpos_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_1 ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState058
          | TIMES ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
          | NOT ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
          | MINUS ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
          | LPAR ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
          | INT _v_2 ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState058
          | CHAR _v_3 ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState058
          | AMPERSAND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
          | RPAR ->
              let _v_4 = _menhir_action_58 () in
              _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
          | _ ->
              _eRR ())
      | AND | ARROW | ASSIGN | COMMA | DIV | DOT | EQ | LEQ | LSQR | MINUS | MODULO | OR | PLUS | RBRACE | RPAR | RSQR | SEMICOLON | TIMES ->
          let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_23 _1 in
          _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_TIMES (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState059 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TIMES ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | AMPERSAND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_060 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_NOT (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState060 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TIMES ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | AMPERSAND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_061 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState061 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TIMES ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | AMPERSAND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_062 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAR (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState062 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TIMES ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | AMPERSAND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_24 _1 in
      _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_expr_descr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState122 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState127 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState171 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState148 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState062 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_076 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_descr_, _startpos_descr_, descr) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_22 _endpos_descr_ _startpos_descr_ descr in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_descr_ _startpos_descr_ _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState162 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_155 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState127 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState171 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState148 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState062 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_163 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RBRACE | SEMICOLON ->
          let MenhirCell0_VAR (_menhir_stack, _3, _, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _, _) = _menhir_stack in
          let _5 = _v in
          let _v = _menhir_action_50 _1 _3 _5 in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_070 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_03 () in
      _menhir_goto_arith_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_arith_op : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_07 _1 in
      _menhir_goto_binop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_binop : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_binop (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState089
      | TIMES ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | NOT ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | MINUS ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | LPAR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | INT _v_1 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState089
      | CHAR _v_2 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState089
      | AMPERSAND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | _ ->
          _eRR ()
  
  and _menhir_run_064 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_25 _1 in
      _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_065 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_startpos__1_, _endpos__2_, _2) = (_startpos, _endpos, _v) in
          let _v = _menhir_action_27 _2 in
          _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_01 () in
      _menhir_goto_arith_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_072 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_10 () in
      _menhir_goto_bool_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_bool_op : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_08 _1 in
      _menhir_goto_binop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_073 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_05 () in
      _menhir_goto_arith_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_074 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_02 () in
      _menhir_goto_arith_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_075 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState075 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TIMES ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | AMPERSAND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_079 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_12 () in
      _menhir_goto_comp_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_comp_op : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_06 _1 in
      _menhir_goto_binop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_080 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_11 () in
      _menhir_goto_comp_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_081 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_endpos__3_, _3) = (_endpos, _v) in
          let _v = _menhir_action_32 _1 _3 in
          _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_083 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_04 () in
      _menhir_goto_arith_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_084 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_endpos__3_, _3) = (_endpos, _v) in
          let _v = _menhir_action_33 _1 _3 in
          _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_086 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_09 () in
      _menhir_goto_bool_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_instruction : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState122 ->
          _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState171 ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState127 ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState148 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_173 : type  ttv_stack. ((((((ttv_stack, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_loption_separated_nonempty_list_COMMA_typed_var__ _menhir_cell0_RPAR, _menhir_box_prog) _menhir_cell1_LBRACE, _menhir_box_prog) _menhir_cell1_local_declarations as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_instruction (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RBRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMICOLON ->
              _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
          | EOF | STRUCT | TINT | TVOID | TYPEDEF | TYPEVAR _ ->
              let _ = _menhir_action_64 () in
              _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_148 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_instruction -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | VAR _v ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState148
      | TIMES ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | RETURN ->
          _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | NOT ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | MINUS ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | LPAR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | INT _v ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState148
      | IF ->
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | CHAR _v ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState148
      | AMPERSAND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | RBRACE | SEMICOLON ->
          let _v = _menhir_action_52 () in
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState148 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_128 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LPAR (_menhir_stack, _startpos_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_1 ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState129
          | TIMES ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
          | NOT ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
          | MINUS ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
          | LPAR ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
          | INT _v_2 ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState129
          | CHAR _v_3 ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState129
          | AMPERSAND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
          | RPAR ->
              let _v_4 = _menhir_action_58 () in
              _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
          | _ ->
              _eRR ())
      | ASSIGN ->
          let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState132 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TIMES ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CHAR _v ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | AMPERSAND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND | ARROW | DIV | DOT | EQ | LEQ | LSQR | MINUS | MODULO | OR | PLUS | TIMES ->
          let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_23 _1 in
          _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_130 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | AND | ARROW | DIV | DOT | EQ | LEQ | LSQR | MINUS | MODULO | OR | PLUS | TIMES ->
          let MenhirCell0_LPAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_VAR (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (xs, _endpos__4_) = (_v, _endpos) in
          let _v = _menhir_action_34 _1 xs in
          _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _startpos__1_ _v _menhir_s _tok
      | RBRACE | SEMICOLON ->
          let MenhirCell0_LPAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_VAR (_menhir_stack, _menhir_s, _1, _, _) = _menhir_stack in
          let xs = _v in
          let _v = _menhir_action_46 _1 xs in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_134 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_TIMES (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState134 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TIMES ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | AMPERSAND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_138 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138
      | TIMES ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState138
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_44 () in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | NOT ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState138
      | MINUS ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState138
      | LPAR ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState138
      | INT _v ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138
      | CHAR _v ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138
      | AMPERSAND ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState138
      | _ ->
          _eRR ()
  
  and _menhir_run_142 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LPAR (_menhir_stack, _startpos) in
          let _menhir_s = MenhirState143 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TIMES ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CHAR _v ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | AMPERSAND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_149 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_instruction as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _menhir_stack = MenhirCell1_instruction (_menhir_stack, _menhir_s, _v) in
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RBRACE ->
          let MenhirCell1_instruction (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_43 _1 _3 in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_172 : type  ttv_stack. ((((ttv_stack, _menhir_box_prog) _menhir_cell1_WHILE _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_RPAR, _menhir_box_prog) _menhir_cell1_instruction as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _menhir_stack = MenhirCell1_instruction (_menhir_stack, _menhir_s, _v) in
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RBRACE ->
          let MenhirCell1_instruction (_menhir_stack, _, i) = _menhir_stack in
          let MenhirCell0_RPAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, e, _, _) = _menhir_stack in
          let MenhirCell0_LPAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_WHILE (_menhir_stack, _menhir_s) = _menhir_stack in
          let next = _v in
          let _v = _menhir_action_41 e i next in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_170 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_WHILE _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_RPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_instruction (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RBRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState171
          | VAR _v_0 ->
              _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState171
          | TIMES ->
              _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState171
          | RETURN ->
              _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState171
          | NOT ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState171
          | MINUS ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState171
          | LPAR ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState171
          | INT _v_1 ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState171
          | IF ->
              _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState171
          | CHAR _v_2 ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState171
          | AMPERSAND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState171
          | RBRACE | SEMICOLON ->
              let _v_3 = _menhir_action_52 () in
              _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState171 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_169 : type  ttv_stack. (((((ttv_stack, _menhir_box_prog) _menhir_cell1_IF _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_RPAR, _menhir_box_prog) _menhir_cell1_instruction, _menhir_box_prog) _menhir_cell1_instruction as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _menhir_stack = MenhirCell1_instruction (_menhir_stack, _menhir_s, _v) in
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RBRACE ->
          let MenhirCell1_instruction (_menhir_stack, _, else_) = _menhir_stack in
          let MenhirCell1_instruction (_menhir_stack, _, if_) = _menhir_stack in
          let MenhirCell0_RPAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, e, _, _) = _menhir_stack in
          let MenhirCell0_LPAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
          let next = _v in
          let _v = _menhir_action_42 e else_ if_ next in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_167 : type  ttv_stack. ((((ttv_stack, _menhir_box_prog) _menhir_cell1_IF _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_RPAR, _menhir_box_prog) _menhir_cell1_instruction as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_instruction (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RBRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
          | VAR _v_0 ->
              _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState168
          | TIMES ->
              _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
          | RETURN ->
              _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
          | NOT ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
          | MINUS ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
          | LPAR ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
          | INT _v_1 ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState168
          | IF ->
              _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
          | CHAR _v_2 ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState168
          | AMPERSAND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
          | RBRACE | SEMICOLON ->
              let _v_3 = _menhir_action_52 () in
              _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState168 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_147 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_IF _menhir_cell0_LPAR, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_RPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_instruction (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RBRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ELSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LBRACE ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | WHILE ->
                      _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
                  | VAR _v_0 ->
                      _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState166
                  | TIMES ->
                      _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
                  | RETURN ->
                      _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
                  | NOT ->
                      _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
                  | MINUS ->
                      _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
                  | LPAR ->
                      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
                  | INT _v_1 ->
                      _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState166
                  | IF ->
                      _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
                  | CHAR _v_2 ->
                      _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState166
                  | AMPERSAND ->
                      _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
                  | RBRACE | SEMICOLON ->
                      let _v_3 = _menhir_action_52 () in
                      _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState166 _tok
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_159 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RBRACE | SEMICOLON ->
          let MenhirCell0_VAR (_menhir_stack, _3, _, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _, _) = _menhir_stack in
          let _5 = _v in
          let _v = _menhir_action_49 _1 _3 _5 in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_155 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_RSQR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RBRACE | SEMICOLON ->
          let MenhirCell0_RSQR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, e2, _, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _, _) = _menhir_stack in
          let e3 = _v in
          let _v = _menhir_action_51 e1 e2 e3 in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_152 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RSQR ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ASSIGN ->
              let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_RSQR (_menhir_stack, _endpos_0) in
              let _menhir_s = MenhirState154 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TIMES ->
                  _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NOT ->
                  _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAR ->
                  _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT _v ->
                  _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | CHAR _v ->
                  _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | AMPERSAND ->
                  _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | AND | ARROW | DIV | DOT | EQ | LEQ | LSQR | MINUS | MODULO | OR | PLUS | TIMES ->
              let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
              let (_endpos__4_, e2) = (_endpos_0, _v) in
              let _v = _menhir_action_35 e1 e2 in
              _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _startpos_e1_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_150 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState151 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TIMES ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CHAR _v ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | AMPERSAND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_3 ->
              let _startpos_4 = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos_5 = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | ASSIGN ->
                  let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
                  let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v_3, _startpos_4, _endpos_5) in
                  let _menhir_s = MenhirState158 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | VAR _v ->
                      _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | TIMES ->
                      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | NOT ->
                      _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | MINUS ->
                      _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LPAR ->
                      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INT _v ->
                      _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | CHAR _v ->
                      _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | AMPERSAND ->
                      _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | AND | ARROW | DIV | DOT | EQ | LEQ | LSQR | MINUS | MODULO | OR | PLUS | TIMES ->
                  let (_startpos__1_, _1, _endpos__3_, _3) = (_startpos, _v, _endpos_5, _v_3) in
                  let _v = _menhir_action_32 _1 _3 in
                  _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_9 ->
              let _startpos_10 = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos_11 = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | ASSIGN ->
                  let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
                  let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v_9, _startpos_10, _endpos_11) in
                  let _menhir_s = MenhirState162 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | VAR _v ->
                      _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | TIMES ->
                      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | NOT ->
                      _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | MINUS ->
                      _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LPAR ->
                      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INT _v ->
                      _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | CHAR _v ->
                      _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | AMPERSAND ->
                      _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | AND | ARROW | DIV | DOT | EQ | LEQ | LSQR | MINUS | MODULO | OR | PLUS | TIMES ->
                  let (_startpos__1_, _1, _endpos__3_, _3) = (_startpos, _v, _endpos_11, _v_9) in
                  let _v = _menhir_action_33 _1 _3 in
                  _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_144 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_IF _menhir_cell0_LPAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAR ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_RPAR (_menhir_stack, _endpos_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | WHILE ->
                  _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
              | VAR _v_1 ->
                  _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState146
              | TIMES ->
                  _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
              | RETURN ->
                  _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
              | NOT ->
                  _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
              | MINUS ->
                  _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
              | LPAR ->
                  _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
              | INT _v_2 ->
                  _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState146
              | IF ->
                  _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
              | CHAR _v_3 ->
                  _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState146
              | AMPERSAND ->
                  _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
              | RBRACE | SEMICOLON ->
                  let _v_4 = _menhir_action_52 () in
                  _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState146 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_140 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_RETURN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_RETURN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_45 _2 in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_137 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_TIMES, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RBRACE | SEMICOLON ->
          let MenhirCell1_expr (_menhir_stack, _, _2, _, _) = _menhir_stack in
          let MenhirCell1_TIMES (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_48 _2 _4 in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_135 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_TIMES as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASSIGN ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState136 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TIMES ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CHAR _v ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | AMPERSAND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | DIV | EQ | MINUS | OR | PLUS | TIMES ->
          let MenhirCell1_TIMES (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__2_, _2) = (_endpos, _v) in
          let _v = _menhir_action_28 _2 in
          _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_133 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RBRACE | SEMICOLON ->
          let MenhirCell1_VAR (_menhir_stack, _menhir_s, _1, _, _) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_47 _1 _3 in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_125 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_WHILE _menhir_cell0_LPAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAR ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_RPAR (_menhir_stack, _endpos_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | WHILE ->
                  _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
              | VAR _v_1 ->
                  _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState127
              | TIMES ->
                  _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
              | RETURN ->
                  _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
              | NOT ->
                  _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
              | MINUS ->
                  _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
              | LPAR ->
                  _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
              | INT _v_2 ->
                  _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState127
              | IF ->
                  _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
              | CHAR _v_3 ->
                  _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState127
              | AMPERSAND ->
                  _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
              | RBRACE | SEMICOLON ->
                  let _v_4 = _menhir_action_52 () in
                  _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState127 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_119 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_typed_var as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEVAR _v_0 ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState120
          | TVOID ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
          | TINT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
          | STRUCT ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
          | AMPERSAND | CHAR _ | IF | INT _ | LPAR | MINUS | NOT | RBRACE | RETURN | SEMICOLON | TIMES | VAR _ | WHILE ->
              let _v_1 = _menhir_action_57 () in
              _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 _tok
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_121 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_typed_var, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, e, _, _) = _menhir_stack in
      let MenhirCell1_typed_var (_menhir_stack, _menhir_s, tv) = _menhir_stack in
      let ld = _v in
      let _v = _menhir_action_56 e ld tv in
      _menhir_goto_local_declarations _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_local_declarations : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState114 ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState116 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_117 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_typed_var -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_typed_var (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_55 _1 _3 in
      _menhir_goto_local_declarations _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_101 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_typed_var as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_typed_var (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_40 _1 _3 in
          _menhir_goto_global_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_goto_global_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_13 _1 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_098 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState099 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TIMES ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CHAR _v ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | AMPERSAND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAR ->
          let x = _v in
          let _v = _menhir_action_67 x in
          _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_expr_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState099 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState129 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState058 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_100 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_68 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_095 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let x = _v in
      let _v = _menhir_action_59 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState129 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState058 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_096 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell0_LPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
      let (xs, _endpos__4_) = (_v, _endpos) in
      let _v = _menhir_action_34 _1 xs in
      _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_094 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_TIMES as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ASSIGN | COMMA | DIV | EQ | MINUS | OR | PLUS | RBRACE | RPAR | RSQR | SEMICOLON | TIMES ->
          let MenhirCell1_TIMES (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__2_, _2) = (_endpos, _v) in
          let _v = _menhir_action_28 _2 in
          _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_093 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_NOT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ASSIGN | COMMA | DIV | EQ | MINUS | OR | PLUS | RBRACE | RPAR | RSQR | SEMICOLON | TIMES ->
          let MenhirCell1_NOT (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__2_, _2) = (_endpos, _v) in
          let _v = _menhir_action_29 _2 in
          _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_092 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_MINUS as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASSIGN | COMMA | RBRACE | RPAR | RSQR | SEMICOLON ->
          let MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__2_, _2) = (_endpos, _v) in
          let _v = _menhir_action_30 _2 in
          _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_090 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr _menhir_cell0_binop as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ASSIGN | COMMA | RBRACE | RPAR | RSQR | SEMICOLON ->
          let MenhirCell0_binop (_menhir_stack, _2) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
          let (_endpos__3_, _3) = (_endpos, _v) in
          let _v = _menhir_action_26 _1 _2 _3 in
          _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_077 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RSQR ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos__4_, e2) = (_endpos_0, _v) in
          let _v = _menhir_action_35 e1 e2 in
          _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _startpos_e1_ _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_069 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OR ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MODULO ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSQR ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEQ ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_067 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAR (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_2, _endpos__3_) = (_v, _endpos_0) in
          let _v = _menhir_action_36 _2 in
          _menhir_goto_expr_descr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | AND | ARROW | DIV | DOT | EQ | LEQ | LSQR | MINUS | MODULO | OR | PLUS | TIMES ->
          let (_endpos_descr_, _startpos_descr_, descr) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_22 _endpos_descr_ _startpos_descr_ descr in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_descr_ _startpos_descr_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_direct_declarator : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LSQR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RSQR ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let (_1, _3) = (_v, _v_0) in
                  let _v = _menhir_action_20 _1 _3 in
                  _menhir_goto_direct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | LPAR ->
          let _menhir_stack = MenhirCell1_direct_declarator (_menhir_stack, _menhir_s, _v) in
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LPAR (_menhir_stack, _startpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEVAR _v_2 ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState044
          | TVOID ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
          | TINT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
          | STRUCT ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
          | RPAR ->
              let _v_3 = _menhir_action_60 () in
              _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
          | _ ->
              _eRR ())
      | ASSIGN | COMMA | RPAR | SEMICOLON ->
          let _1 = _v in
          let _v = _menhir_action_17 _1 in
          _menhir_goto_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_045 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_direct_declarator _menhir_cell0_LPAR -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell0_LPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_direct_declarator (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_21 _1 xs in
      _menhir_goto_direct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_declarator : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState104 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState035 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState037 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState039 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_050 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_simple_typ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_simple_typ (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_81 _1 _2 in
      _menhir_goto_typed_var _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_typed_var : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState120 ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState182 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_115 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typed_var (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEVAR _v_0 ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState116
          | TVOID ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
          | TINT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
          | STRUCT ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
          | AMPERSAND | CHAR _ | IF | INT _ | LPAR | MINUS | NOT | RBRACE | RETURN | SEMICOLON | TIMES | VAR _ | WHILE ->
              let _v_1 = _menhir_action_57 () in
              _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 _tok
          | _ ->
              _eRR ())
      | ASSIGN ->
          let _menhir_s = MenhirState118 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TIMES ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CHAR _v ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | AMPERSAND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_107 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_typed_var (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState108 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEVAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVOID ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TINT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRUCT ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | RPAR ->
          let x = _v in
          let _v = _menhir_action_71 x in
          _menhir_goto_separated_nonempty_list_COMMA_typed_var_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_typed_var_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState106 ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState108 ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_110 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_simple_typ, _menhir_box_prog) _menhir_cell1_VAR _menhir_cell0_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let x = _v in
      let _v = _menhir_action_63 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_typed_var__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_109 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_typed_var -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_typed_var (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_72 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_typed_var_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_054 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_39 _1 in
          _menhir_goto_global_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | ASSIGN ->
          let _menhir_stack = MenhirCell1_typed_var (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState056 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TIMES ->
              _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CHAR _v ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | AMPERSAND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_032 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typed_var (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEVAR _v_0 ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState033
          | TVOID ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState033
          | TINT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState033
          | STRUCT ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState033
          | RBRACE ->
              let _v_1 = _menhir_action_78 () in
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_typed_var -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_typed_var (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_77 _1 _3 in
      _menhir_goto_struct_fields _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_struct_fields : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState031 ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState033 ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_049 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_TIMES -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_TIMES (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_16 _2 in
      _menhir_goto_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_047 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_LPAR _menhir_cell0_TIMES -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_TIMES (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_LPAR (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_19 _3 in
          _menhir_goto_direct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_TIMES (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState037 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TIMES ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_036 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_18 _1 in
      _menhir_goto_direct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_038 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAR (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_TIMES (_menhir_stack, _startpos) in
          let _menhir_s = MenhirState039 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TIMES ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_035 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_simple_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState035
      | TIMES ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | LPAR ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | _ ->
          _eRR ()
  
  and _menhir_run_019 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_simple_typ (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState020 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEVAR _v ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TVOID ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TINT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRUCT ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | RPAR ->
          let x = _v in
          let _v = _menhir_action_69 x in
          _menhir_goto_separated_nonempty_list_COMMA_simple_typ_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_simple_typ_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState044 ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState018 ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState020 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_022 : type  ttv_stack. (ttv_stack _menhir_cell0_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let x = _v in
      let _v = _menhir_action_61 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_simple_typ__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_simple_typ__ : type  ttv_stack. (ttv_stack _menhir_cell0_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState044 ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState018 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_023 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_typedef_direct_declarator _menhir_cell0_LPAR -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell0_LPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_typedef_direct_declarator (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_87 _1 xs in
      _menhir_goto_typedef_direct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_typedef_direct_declarator : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LSQR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RSQR ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let (_1, _3) = (_v, _v_0) in
                  let _v = _menhir_action_86 _1 _3 in
                  _menhir_goto_typedef_direct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | LPAR ->
          let _menhir_stack = MenhirCell1_typedef_direct_declarator (_menhir_stack, _menhir_s, _v) in
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LPAR (_menhir_stack, _startpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEVAR _v_2 ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState018
          | TVOID ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState018
          | TINT ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState018
          | STRUCT ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState018
          | RPAR ->
              let _v_3 = _menhir_action_60 () in
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
          | _ ->
              _eRR ())
      | RPAR | SEMICOLON ->
          let _1 = _v in
          let _v = _menhir_action_83 _1 in
          _menhir_goto_typedef_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_typedef_declarator : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState009 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState011 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState013 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_028 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_TYPEDEF, _menhir_box_prog) _menhir_cell1_simple_typ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_simple_typ (_menhir_stack, _, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_88 _1 _2 in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_TYPEDEF (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_80 _2 in
          _menhir_goto_type_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_027 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_TIMES -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_TIMES (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_82 _2 in
      _menhir_goto_typedef_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_025 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_LPAR _menhir_cell0_TIMES -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_TIMES (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_LPAR (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_85 _3 in
          _menhir_goto_typedef_direct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_021 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_simple_typ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_simple_typ (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_70 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_simple_typ_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_009 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_TYPEDEF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_simple_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPEVAR _v_0 ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState009
      | TIMES ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState009
      | LPAR ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState009
      | _ ->
          _eRR ()
  
  and _menhir_run_010 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_84 _1 in
      _menhir_goto_typedef_direct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_011 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_TIMES (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState011 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPEVAR _v ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TIMES ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_012 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAR (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_TIMES (_menhir_stack, _startpos) in
          let _menhir_s = MenhirState013 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEVAR _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TIMES ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPEVAR _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000
      | TYPEDEF ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TVOID ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TINT ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | STRUCT ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | EOF ->
          let _v = _menhir_action_53 () in
          _menhir_run_178 _menhir_stack _v
      | _ ->
          _eRR ()
  
end

let prog =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_prog v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
