module Env = struct
  module S = Syntax

  exception ScopeEmpty

  module Local = struct
    type 'a t = (string, 'a) Hashtbl.t list

    let rec lookup k = function
      | [] ->
          None
      | h :: tl -> (
          match Hashtbl.find_opt h k with
          | Some v ->
              Some v
          | None ->
              lookup k tl)

    let add k v = function
      | [] ->
          raise ScopeEmpty
      | h :: _ ->
          Hashtbl.replace h k v

    let rec replace k v = function
      | [] ->
          raise Not_found
      | h :: tl ->
          if Hashtbl.mem h k then Hashtbl.replace h k v else replace k v tl

    let mem k t =
      match lookup k t with
      | Some _ ->
          true
      | None ->
          false

    let rec dup = function
      | [] ->
          []
      | h :: tl ->
          Hashtbl.copy h :: dup tl
  end

  type def_entry = {
    def_params : string list;
    def_body : S.block;
  }

  and 'a class_entry = {
    class_params : string list;
    class_base : 'a t;
  }

  and 'a t = {
    defs : (string, def_entry) Hashtbl.t;
    classes : (string, 'a class_entry) Hashtbl.t;
    mutable locals : 'a Local.t;
  }

  let create () =
    {
      defs = Hashtbl.create 21;
      classes = Hashtbl.create 21;
      locals = [ Hashtbl.create 21 ];
    }

  let lookup k t = Hashtbl.find_opt t k
  let mem k t = Hashtbl.mem t k
  let add k v t = Hashtbl.replace t k v
  let push_scope e = e.locals <- Hashtbl.create 21 :: e.locals

  let pop_scope e =
    match e.locals with
    | [] ->
        raise ScopeEmpty
    | _ :: tl ->
        e.locals <- tl
end

module Value = struct
  type t =
    | VNil
    | VUnit
    | VBool of bool
    | VInt of int
    | VStr of string
    | VArray of t Dynarray.t
    | VObject of (string * t Env.Local.t)

  let rec show = function
    | VNil ->
        "nil"
    | VUnit ->
        "()"
    | VBool b ->
        string_of_bool b
    | VInt i ->
        string_of_int i
    | VStr s ->
        s
    | VArray a ->
        let inner = Dynarray.to_list a |> List.map show |> String.concat ", " in
        "[" ^ inner ^ "]"
    | VObject (name, _) ->
        name
end

module RuntimeError = struct
  type kind =
    | Arguments
    | ArgumentType
    | ArgumentSize
    | NotArray
    | SubscriptInt
    | IndexRange
    | ArrayEmpty
    | NotFound
    | RVal
    | LVal
    | NotObject
    | ClassNotFound
    | Duplicate
    | Return

  type t = Loc.t * kind

  let string_of_kind = function
    | Arguments ->
        "bad arguments"
    | ArgumentType ->
        "wrong argument type"
    | ArgumentSize ->
        "wrong argument size"
    | NotArray ->
        "not an array"
    | SubscriptInt ->
        "array subscript must be an int"
    | IndexRange ->
        "index out of range"
    | ArrayEmpty ->
        "array is empty"
    | NotFound ->
        "symbol not found"
    | RVal ->
        "invalid rval"
    | LVal ->
        "invalid lval"
    | NotObject ->
        "not an object"
    | ClassNotFound ->
        "class not found"
    | Duplicate ->
        "duplicate definition"
    | Return ->
        "return outside of method"
end

module Primitive = struct
  open Either

  module Binary = struct
    let int_math f = function
      | Value.VInt l, Value.VInt r ->
          Some (Value.VInt (f l r))
      | _ ->
          None

    let addition = function
      | Value.VInt l, Value.VInt r ->
          Some (Value.VInt (l + r))
      | Value.VStr l, Value.VStr r ->
          Some (Value.VStr (l ^ r))
      | _ ->
          None

    let subtraction = int_math ( - )
    let multiplication = int_math ( * )
    let division = int_math ( / )

    let modulos =
      let f l r =
        let result = l mod r in
        if result >= 0 then result else result + r
      in
      int_math f

    let logical_and = function
      | Value.VBool l, Value.VBool r ->
          Some (Value.VBool (l && r))
      | _ ->
          None

    let logical_or = function
      | Value.VBool l, Value.VBool r ->
          Some (Value.VBool (l || r))
      | _ ->
          None

    let int_compare f = function
      | Value.VInt l, Value.VInt r ->
          Some (Value.VBool (f l r))
      | _ ->
          None

    let less = int_compare ( < )
    let less_eq = int_compare ( <= )
    let greater = int_compare ( > )
    let greater_eq = int_compare ( >= )
  end

  module Unary = struct
    let negate = function
      | Value.VInt i ->
          Some (Value.VInt (-i))
      | _ ->
          None

    let logical_not = function
      | Value.VBool b ->
          Some (Value.VBool (not b))
      | _ ->
          None
  end

  module Global = struct
    let print = function
      | [ value ] ->
          print_endline (Value.show value);
          Left Value.VUnit
      | _ ->
          Right RuntimeError.Arguments

    let str = function
      | [ value ] ->
          Left (Value.VStr (Value.show value))
      | _ ->
          Right RuntimeError.Arguments

    let primitives = [ ("print", print); ("str", str) ]
  end

  module String = struct
    let len s = function
      | [] ->
          Left (Value.VInt (String.length s))
      | _ ->
          Right RuntimeError.ArgumentSize

    let primitives = [ ("len", len) ]
  end

  module Array = struct
    let len a = function
      | [] ->
          Left (Value.VInt (Dynarray.length a))
      | _ ->
          Right RuntimeError.ArgumentSize

    let push a = function
      | [ v ] ->
          Dynarray.add_last a v;
          Left Value.VUnit
      | _ ->
          Right RuntimeError.Arguments

    let pop a = function
      | [] -> (
          match Dynarray.pop_last_opt a with
          | Some v ->
              Left v
          | None ->
              Right RuntimeError.ArrayEmpty)
      | _ ->
          Right RuntimeError.ArgumentSize

    let append a1 = function
      | [ Value.VArray a2 ] ->
          Dynarray.append a1 a2;
          Left Value.VUnit
      | _ ->
          Right RuntimeError.Arguments

    let primitives =
      [ ("len", len); ("push", push); ("pop", pop); ("append", append) ]
  end
end

module Eval = struct
  module E = Env
  module L = Loc
  module P = Primitive
  module S = Syntax
  module V = Value

  exception RuntimeError of RuntimeError.t

  let error at kind = raise (RuntimeError (at, kind))

  exception Return of L.t * V.t

  let rec eval_expr env expr =
    let eval_lit lit =
      match lit.L.it with
      | S.LBool b ->
          V.VBool b
      | S.LInt i ->
          V.VInt i
      | S.LStr s ->
          V.VStr s
    in
    let eval_unary unary =
      let op, rhs = unary.L.it in
      let rhs_value = eval_expr env rhs in
      match op with
      | S.Neg ->
          P.Unary.negate rhs_value
      | S.Not ->
          P.Unary.logical_not rhs_value
    in
    let eval_binary binary =
      let op, lhs, rhs = binary.L.it in
      let lhs_value = eval_expr env lhs in
      let rhs_value = eval_expr env rhs in
      let arg = (lhs_value, rhs_value) in
      match op with
      | S.Add ->
          P.Binary.addition arg
      | S.Sub ->
          P.Binary.subtraction arg
      | S.Mul ->
          P.Binary.multiplication arg
      | S.Div ->
          P.Binary.division arg
      | S.Mod ->
          P.Binary.modulos arg
      | S.And ->
          P.Binary.logical_and arg
      | S.Or ->
          P.Binary.logical_or arg
      | S.Lt ->
          P.Binary.less arg
      | S.Lte ->
          P.Binary.less_eq arg
      | S.Gt ->
          P.Binary.greater arg
      | S.Gte ->
          P.Binary.greater_eq arg
      | S.Eq ->
          Some (V.VBool (lhs_value = rhs_value))
      | S.Neq ->
          Some (V.VBool (lhs_value <> rhs_value))
    in
    let eval_array array =
      let exprs = array.L.it in
      let values = List.map (eval_expr env) exprs in
      V.VArray (Dynarray.of_list values)
    in
    let eval_new new_c =
      let cname, exprs = new_c.L.it in
      let values = List.map (eval_expr env) exprs in
      match Env.lookup cname.L.it env.Env.classes with
      | Some klass ->
          (* params are base env *)
          let base = [ Hashtbl.create 21 ] in
          List.iter2
            (fun n v -> Env.Local.add n v base)
            klass.class_params values;
          (* klass top_level locals are next scope *)
          let klass_locals = Env.Local.dup klass.class_base.locals in
          let state = klass_locals @ base in
          (* instance the class to make a new object *)
          V.VObject (cname.L.it, state)
      | None ->
          error cname.L.at RuntimeError.NotFound
    in
    match expr.L.it with
    | S.Lit lit ->
        eval_lit lit
    | S.Var var ->
        eval_var env var
    | S.Unary unary -> (
        match eval_unary unary with
        | Some v ->
            v
        | None ->
            error unary.L.at RuntimeError.ArgumentType)
    | S.Binary binary -> (
        match eval_binary binary with
        | Some v ->
            v
        | None ->
            error binary.L.at RuntimeError.ArgumentType)
    | S.ArrayE array ->
        eval_array array
    | S.Call call ->
        eval_call env call
    | S.New new_c ->
        eval_new new_c

  and get_subscript env v e =
    match eval_var env v with
    | V.VArray array -> (
        match eval_expr env e with
        | V.VInt index ->
            (array, index)
        | _ ->
            error e.L.at RuntimeError.SubscriptInt)
    | _ ->
        error v.L.at RuntimeError.NotArray

  and eval_var env var =
    let eval_subscript v e =
      let array, index = get_subscript env v e in
      try Dynarray.get array index with
      | Invalid_argument _ ->
          error e.L.at RuntimeError.IndexRange
    in
    let eval_field v n =
      match eval_var env v with
      | V.VObject (_, state) -> (
          match Env.Local.lookup n.L.it state with
          | Some v ->
              v
          | None ->
              error n.L.at RuntimeError.NotFound)
      | _ ->
          error v.L.at RuntimeError.NotObject
    in
    match var.L.it with
    | S.Name name -> (
        match E.Local.lookup name.it env.E.locals with
        | Some v ->
            v
        | None ->
            error var.L.at RuntimeError.NotFound)
    | S.Subscript (v, e) ->
        eval_subscript v e
    | S.Field (v, n) ->
        eval_field v n

  and eval_call env call =
    let var, exprs = call.L.it in
    let arguments = List.map (eval_expr env) exprs in
    let call_primitive f =
      let open Either in
      match f arguments with
      | Left v ->
          v
      | Right e ->
          error call.L.at e
    in
    let call_primitive_obj f obj =
      let open Either in
      match f obj arguments with
      | Left v ->
          v
      | Right e ->
          error call.L.at e
    in
    let call_def e s d =
      (* TODO: move env manipulation to Env module *)
      (* cache local state *)
      let locals = e.Env.locals in
      (* load context state *)
      e.Env.locals <- s;
      Env.push_scope e;
      List.iter2
        (fun k v -> Env.Local.add k v e.locals)
        d.Env.def_params arguments;
      let value =
        try
          eval_block e d.Env.def_body;
          V.VUnit
        with
        | Return (_, v) ->
            v
      in
      Env.pop_scope e;
      (* restore local state *)
      e.Env.locals <- locals;
      value
    in
    match var.L.it with
    | S.Name name -> (
        match List.assoc_opt name.L.it Primitive.Global.primitives with
        | Some f ->
            call_primitive f
        | None -> (
            match Env.lookup name.L.it env.defs with
            | Some d ->
                call_def env [] d
            | None ->
                error name.L.at RuntimeError.NotFound))
    | S.Field (v, n) -> (
        match eval_var env v with
        | V.VObject (klass, state) -> (
            match Env.lookup klass env.classes with
            | Some c -> (
                match Env.lookup n.L.it c.class_base.defs with
                | Some d ->
                    call_def env state d
                | None ->
                    error n.L.at RuntimeError.NotFound)
            | None ->
                error v.L.at RuntimeError.ClassNotFound)
        | V.VArray array -> (
            match List.assoc_opt n.L.it Primitive.Array.primitives with
            | Some f ->
                call_primitive_obj f array
            | None ->
                error n.L.at RuntimeError.NotFound)
        | V.VStr str -> (
            match List.assoc_opt n.L.it Primitive.String.primitives with
            | Some f ->
                call_primitive_obj f str
            | None ->
                error n.L.at RuntimeError.NotFound)
        | _ ->
            error v.L.at RuntimeError.NotObject)
    | S.Subscript _ ->
        error var.L.at RuntimeError.RVal

  and eval_block env block =
    Env.push_scope env;
    List.iter (eval_command env) block;
    Env.pop_scope env

  and eval_command env command =
    let declare_value name v =
      if Env.Local.mem name.L.it env.locals then
        error name.L.at RuntimeError.Duplicate
      else Env.Local.add name.L.it v env.locals
    in
    let eval_declare declare =
      let name, expr = declare.L.it in
      let value =
        match expr with
        | Some e ->
            eval_expr env e
        | None ->
            V.VNil
      in
      declare_value name value
    in
    let eval_assign assign =
      let var, expr = assign.L.it in
      let value = eval_expr env expr in
      let assign_subscript v e =
        let array, index = get_subscript env v e in
        try Dynarray.set array index value with
        | Invalid_argument _ ->
            error e.L.at RuntimeError.IndexRange
      in
      match var.L.it with
      | S.Name name -> (
          try Env.Local.replace name.L.it value env.locals with
          | Not_found ->
              error assign.L.at RuntimeError.NotFound)
      | S.Subscript (v, e) ->
          assign_subscript v e
      | _ ->
          error var.L.at RuntimeError.LVal
    in
    let eval_while while_c =
      let cond, body = while_c.L.it in
      let rec loop () =
        match eval_expr env cond with
        | V.VBool b ->
            if b then (
              eval_block env body;
              loop ())
        | _ ->
            error cond.L.at RuntimeError.ArgumentType
      in
      loop ()
    in
    let eval_if if_c =
      let if_test, then_block, else_block = if_c.L.it in
      match if_test with
      | S.Expr cond -> (
          match eval_expr env cond with
          | V.VBool b -> (
              if b then eval_block env then_block
              else
                match else_block with
                | Some b ->
                    eval_block env b
                | None ->
                    ())
          | _ ->
              error cond.L.at RuntimeError.ArgumentType)
      | S.Dec (name, expr) -> (
          match eval_expr env expr with
          | V.VNil -> (
              match else_block with
              | Some b ->
                  eval_block env b
              | None ->
                  ())
          | v ->
              Env.push_scope env;
              declare_value name v;
              eval_block env then_block;
              Env.pop_scope env)
    in
    let eval_return return =
      let value = eval_expr env return.L.it in
      raise (Return (return.L.at, value))
    in
    match command with
    | S.CallC call ->
        let _ = eval_call env call in
        ()
    | S.DeclareC declare ->
        eval_declare declare
    | S.AssignC assign ->
        eval_assign assign
    | S.WhileC while_c ->
        eval_while while_c
    | S.IfC if_c ->
        eval_if if_c
    | S.Return return ->
        eval_return return
    | S.Block block ->
        eval_block env block

  and eval_top_level env top_level =
    let eval_def def =
      let name, params, body = def.L.it in
      if Env.mem name.L.it env.Env.defs then
        error def.L.at RuntimeError.Duplicate
      else
        let p = List.map (fun p -> p.L.it) params in
        let def_entry = { Env.def_body = body; Env.def_params = p } in
        Env.add name.L.it def_entry env.defs
    in
    let eval_class class_t =
      let name, params, class_top_levels = class_t.L.it in
      if Env.mem name.L.it env.Env.classes then
        error class_t.L.at RuntimeError.Duplicate
      else
        let p = List.map (fun p -> p.L.it) params in
        let e = Env.create () in
        List.iter (eval_top_level e) class_top_levels;
        let class_entry = { Env.class_params = p; Env.class_base = e } in
        Env.add name.L.it class_entry env.classes
    in
    match top_level with
    | S.Command command ->
        eval_command env command
    | S.Def def ->
        eval_def def
    | S.Class class_t ->
        eval_class class_t

  let run top_levels =
    try
      let env = Env.create () in
      try List.iter (eval_top_level env) top_levels with
      | Return (at, _) ->
          error at RuntimeError.Return
    with
    | RuntimeError (at, error) ->
        Error.runtime at (RuntimeError.string_of_kind error)
end
