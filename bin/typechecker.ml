[@@@warning "-26-27"]
(*used to hide annoying dune warnings, remove this when you have something that works*)

open C_AST

(*all of what's below is just a suggestion on how to proceed, you are completely free to use a different implementation, we just need a function [typecheck: untyped_prog -> typed_prog]*)
(*IMPORTANT: if you use mutable values, such as references or hashtables, reset them in each call of [typecheck]*)

module Env = Map.Make (String)
(**We can use a map from variable names to a type [`a] For instance,
   [Env.find "toto" (Env.singleton "toto" 4) = 4] *)

type env = {
  g_types : typ Env.t;
  typs : typ Env.t;
  locals : typ Env.t;
  funcs : (typ * typ list) Env.t;
  reached_ret : bool;
}
(**throughout our analysis, we will thread an environment that carries all the
   information we need. Most of our functions will look something like `foo env
   x = ... in (env,result)`

   THIS IS ONE WAY TO PROCEED, you are completely free to use something else
   such as a global reference that you change accordingly; the fields of this
   environment may also be extended *)

(**the only function we know is [putchar]*)
let prebuilt_funcs =
  [ ("putchar", (TVoid, [ TInt ])) ]
  |> List.fold_left (fun env (f, t) -> Env.add f t env) Env.empty

let empty_env =
  {
    g_types = Env.empty;
    typs = Env.empty;
    locals = Env.empty;
    funcs = prebuilt_funcs;
    reached_ret = false;
  }

let type_global_decl env g = assert false
let type_typ_decl env typ = assert false

(**compute the size of an inlined type*)
let size_of_type env typ = assert false

let rec type_expr env e =
  let env, typ, descr = type_descr env e.descr in
  let size = size_of_type env typ in
  (* ... *)
  (env, { descr; info = { typ; size; loc = e.info } })

and type_descr env = function
  | Const c -> (env, TInt, Const c)
  | _ -> assert false

let rec type_instruction env = function
  | Skip -> (env, Skip)
  | Seq (i1, i2) ->
      let env, i1 = type_instruction env i1 in
      type_instruction env i2
  | Return (Some e) ->
      let env, e = type_expr env e in
      (* ... *)
      (env, Return (Some e))
  | _ -> assert false

let type_function env ({ f_name; return_type; args; var_decl; body } as func) =
  (* ... *)
  let locals =
    List.fold_left (fun m (s, t) -> Env.add s t m) Env.empty (args @ var_decl)
  in
  (* ... *)
  match body with
  | Skip -> (env, { func with body = Skip })
  | body ->
      let env = { env with locals } in
      let env, body = type_instruction env body in
      (* ... *)
      (env, { f_name; return_type; args; var_decl; body })

let type_decls =
  List.fold_left_map (fun env -> function
    | DFun f ->
        let env, f = type_function env f in
        (env, DFun f)
    | DGlob g ->
        let env, g = type_global_decl env g in
        (env, DGlob g)
    | DTyp t ->
        let env, t = type_typ_decl env t in
        (env, DTyp t))

let main_exists _ = (*...*) ()

let typecheck (p : untyped_prog) : typed_prog =
  main_exists p;
  type_decls empty_env p |> snd
