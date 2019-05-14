  (* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Got" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and str_t      = L.pointer_type (L.i8_type context)
  and grid_ptr_t = L.pointer_type (L.i8_type context)
  and knight_ptr_t = L.pointer_type (L.i8_type context)
  and knave_ptr_t = L.pointer_type (L.i8_type context)
  and void_t     = L.void_type   context 
in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.String -> str_t
    | A.Grid -> grid_ptr_t
    | A.Knight -> knight_ptr_t
    | A.Knave -> knave_ptr_t
  (* TODO Actually add all types *)
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in

  let pow_t : L.lltype = 
      L.var_arg_function_type float_t [| float_t; float_t |] in
  let pow_func : L.llvalue = 
      L.declare_function "pow_func" pow_t the_module in


(*GRID FUNCTIONS *)
  let grid_init_t : L.lltype = 
      L.function_type grid_ptr_t [| i32_t; i32_t |] in
  let grid_init_func : L.llvalue = 
      L.declare_function "grid_init" grid_init_t the_module in

  let grid_end_t : L.lltype = 
    L.function_type void_t [| grid_ptr_t |] in
  let grid_end_func : L.llvalue = 
    L.declare_function "grid_end" grid_end_t the_module in

  let get_grid_x_t : L.lltype = 
      L.function_type i32_t [| grid_ptr_t |] in
  let get_grid_x_func : L.llvalue = 
      L.declare_function "get_grid_x" get_grid_x_t the_module in


(*KNIGHT FUNCTIONS *)
  let new_knight_t : L.lltype =
      L.function_type knight_ptr_t [| i32_t; i32_t|] in
  let new_knight_func : L.llvalue = 
      L.declare_function "new_knight" new_knight_t the_module in

  let get_knight_health_t : L.lltype = 
    L.function_type i32_t [| knight_ptr_t |] in
  let get_knight_health_func : L.llvalue = 
    L.declare_function "get_knight_health" get_knight_health_t the_module in

  let get_knight_x_pos_t : L.lltype = 
    L.function_type i32_t [| knight_ptr_t |] in
  let get_knight_x_pos_func : L.llvalue = 
    L.declare_function "get_knight_x_pos" get_knight_x_pos_t the_module in

  let get_knight_y_pos_t : L.lltype = 
    L.function_type i32_t [| knight_ptr_t |] in
  let get_knight_y_pos_func : L.llvalue = 
    L.declare_function "get_knight_y_pos" get_knight_y_pos_t the_module in

  let knight_die_t : L.lltype = 
    L.function_type void_t [| knight_ptr_t |] in
  let knight_die_func : L.llvalue = 
    L.declare_function "knight_die" knight_die_t the_module in




(*KNAVE FUNCTIONS *)
  let new_knave_t : L.lltype =
      L.function_type knave_ptr_t [| i32_t; i32_t |] in
  let new_knave_func : L.llvalue = 
      L.declare_function "new_knave" new_knave_t the_module in

  let knave_die_t : L.lltype = 
    L.function_type void_t [| knave_ptr_t |] in
  let knave_die_func : L.llvalue = 
    L.declare_function "knave_die" knave_die_t the_module in

  let set_stealth_t : L.lltype =
      L.function_type knave_ptr_t [| knave_ptr_t; i32_t |] in
  let set_stealth_func : L.llvalue =
      L.declare_function "set_stealth" set_stealth_t the_module in

  let get_stealth_t : L.lltype = 
      L.function_type i32_t [| knave_ptr_t |] in
  let get_stealth_func : L.llvalue = 
      L.declare_function "get_stealth" get_stealth_t the_module in

  let get_knave_health_t : L.lltype = 
    L.function_type i32_t [| knave_ptr_t |] in
  let get_knave_health_func : L.llvalue = 
    L.declare_function "get_knight_health" get_knave_health_t the_module in

  let get_knave_x_pos_t : L.lltype = 
    L.function_type i32_t [| knave_ptr_t |] in
  let get_knave_x_pos_func : L.llvalue = 
    L.declare_function "get_knave_x_pos" get_knave_x_pos_t the_module in

  let get_knave_y_pos_t : L.lltype = 
    L.function_type i32_t [| knave_ptr_t |] in
  let get_knave_y_pos_func : L.llvalue = 
    L.declare_function "get_knave_y_pos" get_knave_y_pos_t the_module in

  let move_knave_t : L.lltype = 
      L.function_type knave_ptr_t [| knave_ptr_t; i32_t; i32_t |] in
  let move_knave_func : L.llvalue = 
      L.declare_function "move_knave" move_knave_t the_module in

(* INTERACTION FUNCTIONS *)
  let attack_knight_t : L.lltype = 
      L.function_type knight_ptr_t [| knave_ptr_t; knight_ptr_t|] in
  let attack_knight_func : L.llvalue = 
      L.declare_function "attack_knight" attack_knight_t the_module in


  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%f\n" "fmt" builder
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
	StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals 
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
	      SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral l -> L.const_float_of_string float_t l
      | SStr_literal s -> L.build_global_stringptr s "str" builder
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with 
	    A.Add     -> L.build_fadd
	  | A.Sub     -> L.build_fsub
	  | A.Mult    -> L.build_fmul
	  | A.Div     -> L.build_fdiv
    | A.Mod     -> L.build_frem
(*     | A.Exp     -> L.pow *)
    | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
	  | A.Neq     -> L.build_fcmp L.Fcmp.One
	  | A.Less    -> L.build_fcmp L.Fcmp.Olt
	  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
	  | A.Greater -> L.build_fcmp L.Fcmp.Ogt
	  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
	  | A.And | A.Or ->
	      raise (Failure "internal error: semant should have rejected and/or on float")
	  ) e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	    A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
    | A.Div     -> L.build_sdiv
    | A.Mod     -> L.build_srem
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
	  ) e1' e2' "tmp" builder
      | SUnop(op, ((t, _) as e)) ->
          let e' = expr builder e in
	  (match op with
	    A.Neg when t = A.Float -> L.build_fneg 
	  | A.Neg                  -> L.build_neg
          | A.Not                  -> L.build_not) e' "tmp" builder
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
	      L.build_call printf_func [| int_format_str ; (expr builder e) |]
	      "printf" builder
      | SCall ("pow_func", [e1;e2]) ->  
        L.build_call pow_func [| (expr builder e1); (expr builder e2) |] 
        "pow_func" builder
      | SCall ("printbig", [e]) ->
	      L.build_call printbig_func [| (expr builder e) |] 
        "printbig" builder
      | SCall ("printf", [e]) -> 
	      L.build_call printf_func [| float_format_str ; (expr builder e) |]
	      "printf" builder
      | SCall ("get_grid_x", [e]) -> 
        L.build_call get_grid_x_func [| (expr builder e) |]
        "get_grid_x" builder
      | SCall ("print_str", [e]) ->
        L.build_call printf_func [| string_format_str ; (expr builder e) |]
        "printf" builder
      | SCall ("grid_init", [e1; e2]) -> 
        L.build_call grid_init_func [| (expr builder e1); (expr builder e2) |] 
        "grid_init" builder
      | SCall ("grid_end", [e]) ->
      L.build_call grid_end_func [| (expr builder e) |]
      "" builder
      | SCall ("new_knight", [e1; e2]) -> 
        L.build_call new_knight_func [| (expr builder e1); (expr builder e2) |] 
        "new_knight" builder
      | SCall ("get_knight_health", [e]) -> 
        L.build_call get_knight_health_func [| (expr builder e)|] 
        "get_knight_health" builder
      | SCall ("get_knight_x_pos", [e]) ->
        L.build_call get_knight_x_pos_func[| (expr builder e)|] 
        "get_knight_x_pos" builder
      | SCall ("get_knight_y_pos", [e]) ->
        L.build_call get_knight_y_pos_func [| (expr builder e)|] 
        "get_knight_y_pos" builder



      | SCall ("new_knave", [e1; e2]) -> 
        L.build_call new_knave_func [|  (expr builder e1); (expr builder e2) |] 
        "new_knave" builder
      | SCall ("get_stealth", [e]) -> 
        L.build_call get_stealth_func [| (expr builder e) |] 
        "get_stealth" builder
      | SCall ("set_stealth", [e1;e2]) -> 
        L.build_call set_stealth_func [| (expr builder e1); (expr builder e2) |] 
        "set_stealth" builder
      | SCall ("get_knave_health", [e]) -> 
        L.build_call get_knave_health_func [| (expr builder e)|] 
        "get_knave_health" builder
      | SCall ("get_knave_x_pos", [e]) ->
        L.build_call get_knave_x_pos_func[| (expr builder e)|] 
        "get_knave_x_pos" builder
      | SCall ("get_knave_y_pos", [e]) ->
        L.build_call get_knave_y_pos_func [| (expr builder e)|] 
        "get_knave_y_pos" builder
      | SCall ("move_knave", [e1;e2;e3]) ->
        L.build_call move_knave_func [| (expr builder e1); (expr builder e2); (expr builder e3) |]
        "move_knave" builder
      | SCall ("attack_knight", [e1;e2]) ->
        L.build_call attack_knight_func [| (expr builder e1); (expr builder e2) |]
        "attack_knight" builder
      | SCall ("knight_die", [e]) ->
        L.build_call knight_die_func [| (expr builder e) |]
        "" builder
      | SCall ("knave_die", [e]) ->
        L.build_call knave_die_func [| (expr builder e) |]
        "" builder
      | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	 let result = (match fdecl.styp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
    in
    
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (instr builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt builder = function
	SBlock sl -> List.fold_left stmt builder sl
      | SExpr e -> ignore(expr builder e); builder 
      | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder );
                     builder
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   build_br_merge;

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   build_br_merge;

	 ignore(L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore(L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
	    ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
