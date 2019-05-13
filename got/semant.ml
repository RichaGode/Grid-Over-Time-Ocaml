(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *) 
let check (globals, functions) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	(Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)

  check_binds "global" globals;

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name; 
      formals = [(ty, "x")];
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
			                         ("printb", Bool);
			                         ("printf", Float);
			                         ("printbig", Int);
                               ("print_str", String);]
  in 
  let non_void_decls = 
    let add_bind_nv map (name, ret_type, ty1, ty2) = StringMap.add name {
        typ = ret_type;
        fname = name; 
        formals = [(ty1, "x"); (ty2, "y")];
        locals = []; body = [] } map
      in List.fold_left add_bind_nv StringMap.empty [ ("pow_func", Float, Float, Float); 
                                                      ("set_stealth", Knave, Knave, Int);
                                                      ("move_knave", Knave, Int, Int)
                                                    ]
  (* Add function name to symbol table *)
  in 
  let no_arg_decls = 
    let add_bind_object map (name, ret_type, ty1, ty2) = StringMap.add name {
      typ = ret_type; 
      fname = name;
      formals = [(ty1, "x"); (ty2, "y");];
      locals = []; 
      body = [] } map
    in List.fold_left add_bind_object StringMap.empty [("grid_init", Grid, Int, Int);
                                                       ("new_knight", Knight, Int, Int); 
                                                       ("new_knave", Knave, Int, Int);
                                                       ("attack_knight", Knight, Knave, Knight);]
  in
  let int_decls = 
    let func map (name, ret_type, ty1) = StringMap.add name {
      typ = ret_type; 
      fname = name;
      formals = [(ty1, "x")];
      locals = []; 
      body = [] } map
    in List.fold_left func StringMap.empty [("get_grid_x", Int, Grid);
                                           ("get_stealth", Int, Knave);
                                           ("get_knave_health", Int, Knave); 
                                           ("get_x_pos", Int, Knave);
                                           ("get_y_pos", Int, Knave);
                                           ("get_knight_health", Int, Knight);
                                           ("knight_die", Void, Knight);]
  in 
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in
  (* Collect all function names into one symbol table *)
  let var_arg_function_decls = StringMap.merge (fun k xo yo -> match xo,yo with
    | Some xo, Some yo -> Some xo 
    | None, Some yo -> Some yo
    | Some xo, None -> Some xo
  ) built_in_decls non_void_decls in
  let temp = StringMap.merge (fun k xo yo -> match xo,yo with
    | Some xo, Some yo -> Some xo 
    | None, Some yo -> Some yo
    | Some xo, None -> Some xo
  ) var_arg_function_decls no_arg_decls in
  let master_function_decls = StringMap.merge (fun k xo yo -> match xo,yo with
    | Some xo, Some yo -> Some xo 
    | None, Some yo -> Some yo
    | Some xo, None -> Some xo
  ) temp int_decls in 
  let function_decls = List.fold_left add_func master_function_decls functions
  in 
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Literal  l -> (Int, SLiteral l)
      | Fliteral l -> (Float, SFliteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | Noexpr     -> (Void, SNoexpr)
      | Str_literal l -> (String, SStr_literal l)
      | Id s       -> (type_of_identifier s, SId s)
      (* | VDeclAssign(e1, e2, e3) -> 
        let lt = e1 
        and (rt, e') = expr e3 in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
          string_of_typ rt ^ " in "
        in (check_assign lt rt err, SAssign(e2, (rt, e'))) *)
      (* assign takes in the variable and the value. It does a) checks for the type in the stringmap of the variable,
    and b) gets the type of the value by the recursive function expr *)
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e'))) 
      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop (e1, Exp, e2) as call -> 
        let fd = find_func "pow_func" in
          let param_length = List.length fd.formals in
          if List.length [e1; e2] != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals [e1; e2]
          in (fd.typ, SCall("pow_func", args'))
      | Binop(e1, Access, e2) as call ->
        let function_name = match e2 with
        | health -> "get_knight_health" 
        | _ -> raise ( Failure (" " ^ string_of_expr e2 ^ " no such variable exists for this class"))
        in 
        let fd = find_func function_name in
            let param_length = List.length fd.formals in
            if List.length[e1] != param_length then
              raise (Failure ("expecting " ^ string_of_int param_length ^ 
                                " arguments in " ^ string_of_expr call))
            else let check_call (ft, _) e =
              let (et, e') = expr e in 
              let err = "illegal argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in (check_assign ft et err, e')
            in 
            let args' = List.map2 check_call fd.formals [e1]
          in (fd.typ, SCall(function_name, args'))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div | Mod when same && t1 = Float -> Float
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = fname ^ "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	   SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e') 
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = match check_stmt (Block func.body) with
	SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)
