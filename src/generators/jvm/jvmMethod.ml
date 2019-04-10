open JvmGlobals
open JvmData
open JvmAttribute
open JvmSignature
open JvmSignature.NativeSignatures
open JvmBuilder

(* High-level method builder. *)

type var_init_state =
	| VarArgument
	| VarWillInit
	| VarNeedDefault

type construction_kind =
	| ConstructInitPlusNew
	| ConstructInit

class builder jc name jsig = object(self)
	inherit base_builder
	val code = new JvmCode.builder jc#get_pool

	val mutable max_num_locals = 0
	val mutable debug_locals = []
	val mutable stack_frames = []
	val mutable exceptions = []
	val mutable argument_locals = []
	val mutable thrown_exceptions = Hashtbl.create 0

	(* per-branch *)
	val mutable terminated = false

	(* per-frame *)
	val mutable locals = []
	val mutable local_offset = 0

	method has_method_flag flag =
		MethodAccessFlags.has_flag access_flags flag

	(** Pushes a new scope onto the stack. Returns a function which when called reverts to the previous state. **)
	method push_scope =
		let old_locals = locals in
		let old_offset = local_offset in
		(fun () ->
			let delta = local_offset - old_offset in
			let fp_end = code#get_fp in
			let rec loop i l =
				if i = 0 then
					()
				else begin match l with
					| (fpo,name,t) :: l ->
						let fp = match !fpo with
							| None -> failwith ("Uninitialized local " ^ name);
							| Some fp -> fp
						in
						let ld = {
							ld_start_pc = fp;
							ld_length = fp_end - fp;
							ld_name_index = jc#get_pool#add_string name;
							ld_descriptor_index = jc#get_pool#add_string (generate_signature false t);
							ld_index = old_offset + i - 1;
						} in
						debug_locals <- ld :: debug_locals;
						loop (i - (signature_size t)) l
					| [] ->
						assert false
				end
			in
			loop delta locals;
			locals <- old_locals;
			local_offset <- old_offset;
		)

	method private get_locals_for_stack_frame locals =
		List.map (fun (init,_,t) ->
			match !init with
			| None -> JvmVerificationTypeInfo.VTop
			| _ -> JvmVerificationTypeInfo.of_signature jc#get_pool t
		) locals

	(** Adds the current state of locals and stack as a stack frame. This has to be called on every branch target. **)
	method add_stack_frame =
		let locals = self#get_locals_for_stack_frame locals in
		let astack = List.map (JvmVerificationTypeInfo.of_signature jc#get_pool) (code#get_stack#get_stack) in
		let r = code#get_fp in
		let ff = (r,locals,astack) in
		(* If we already have a frame at the same position, overwrite it. This can happen in the case of nested branches. *)
		stack_frames <- (match stack_frames with
			| (r',_,_) :: stack_frames when r' = r -> ff :: stack_frames
			| _ -> ff :: stack_frames)

	(** Adds [exc] as an exception. This will be added to the Code attribute. **)
	method add_exception (exc : jvm_exception) =
		exceptions <- exc :: exceptions

	(** Adds [path] as a thrown exception for this method. Deals with duplicates. **)
	method add_thrown_exception (path : jpath) =
		Hashtbl.replace thrown_exceptions (jc#get_pool#add_path path) true

	(* Convenience *)

	method invokevirtual (path : jpath) (name : string) (jsig : jsignature) (jsigm : jsignature) = match jsigm with
		| TMethod(tl,tr) ->
			let offset = code#get_pool#add_field path name jsigm FKMethod in
			code#invokevirtual offset jsig tl (match tr with None -> [] | Some tr -> [tr])
		| _ -> assert false

	method invokespecial (path : jpath) (name : string) (jsig : jsignature) (jsigm : jsignature) = match jsigm with
		| TMethod(tl,tr) ->
			let offset = code#get_pool#add_field path name jsigm FKMethod in
			code#invokespecial offset jsig tl (match tr with None -> [] | Some tr -> [tr])
		| _ -> assert false

	method invokestatic (path : jpath) (name : string) (jsigm : jsignature) = match jsigm with
		| TMethod(tl,tr) ->
			let offset = code#get_pool#add_field path name jsigm FKMethod in
			code#invokestatic offset tl (match tr with None -> [] | Some tr -> [tr])
		| _ -> assert false

	method getfield (path : jpath) (name : string) (jsigf : jsignature) =
		let offset = code#get_pool#add_field path name jsigf FKField in
		code#getfield offset (object_path_sig path) jsigf

	method putfield (path : jpath) (name : string) (jsigf : jsignature) =
		let offset = code#get_pool#add_field path name jsigf FKField in
		code#putfield offset (object_path_sig path) jsigf

	method getstatic (path : jpath) (name : string) (jsigf : jsignature) =
		let offset = code#get_pool#add_field path name jsigf FKField in
		code#getstatic offset jsigf

	method putstatic (path : jpath) (name : string) (jsigf : jsignature) =
		let offset = code#get_pool#add_field path name jsigf FKField in
		code#putstatic offset jsigf

	method load_this =
		assert (not (self#has_method_flag MStatic));
		code#aload jc#get_jsig 0

	method call_super_ctor (kind : construction_kind) (jsig_method : jsignature) =
		assert (not (self#has_method_flag MStatic));
		match kind with
		| ConstructInitPlusNew ->
			self#invokespecial jc#get_super_path "new" jc#get_jsig jsig_method;
		| ConstructInit ->
			self#invokespecial jc#get_super_path "<init>" jc#get_jsig jsig_method;
			self#set_this_initialized

	method add_argument_and_field (name : string) (jsig_field : jsignature) =
		assert (not (self#has_method_flag MStatic));
		let jf = new builder jc name jsig_field in
		jf#add_access_flag 1;
		jc#add_field jf#export_field;
		let _,load,_ = self#add_local name jsig_field VarArgument in
		self#load_this;
		load();
		self#putfield jc#get_this_path name jsig_field;

	method construct ?(no_value=false) (kind : construction_kind) (path : jpath) (f : unit -> jsignature list) =
		let pool = code#get_pool in
		let offset = pool#add_path path in
		code#new_ offset;
		match kind with
		| ConstructInitPlusNew ->
			code#dup;
			if not no_value then code#dup;
			code#aconst_null haxe_empty_constructor_sig;
			self#invokespecial path "<init>" jc#get_jsig (method_sig [haxe_empty_constructor_sig] None);
			if not no_value then self#set_top_initialized (object_path_sig path);
			let jsigs = f () in
			self#invokevirtual path "new" jc#get_jsig (method_sig jsigs None);
		| ConstructInit ->
			if not no_value then code#dup;
			let jsigs = f () in
			self#invokespecial path "<init>" jc#get_jsig (method_sig jsigs None);
			if not no_value then self#set_top_initialized (object_path_sig path)

	method load_default_value = function
		| TByte | TBool | TChar | TShort | TInt ->
			code#iconst Int32.zero;
		| TFloat -> code#fconst 0.
		| TDouble -> code#dconst 0.
		| TLong -> code#lconst Int64.zero
		| jsig -> code#aconst_null jsig

	method return = match jsig with
		| TMethod(_,tr) ->
			begin match tr with
			| None ->
				code#return_void
			| Some jsig ->
				code#return_value jsig
			end
		| _ ->
			assert false

	(* casting *)

	(** Checks if the stack top is a basic type and wraps accordingly. **)
	method expect_reference_type =
		let wrap_null jsig name =
			let path = (["java";"lang"],name) in
			self#invokestatic path "valueOf" (method_sig [jsig] (Some (object_path_sig path)))
		in
		match code#get_stack#top with
		| TByte as t -> wrap_null t "Byte"
		| TChar as t -> wrap_null t "Character"
		| TDouble as t -> wrap_null t "Double"
		| TFloat as t -> wrap_null t "Float"
		| TInt as t -> wrap_null t "Integer"
		| TLong as t -> wrap_null t "Long"
		| TShort as t -> wrap_null t "Short"
		| TBool as t -> wrap_null t "Boolean"
		| _ -> ()

	method private expect_basic_type ?(not_null=false) jsig =
		if not_null then begin
			let unwrap_null tname name =
				let path = (["java";"lang"],tname) in
				let jsig_wrapper = object_path_sig path in
				self#cast (get_boxed_type jsig);
				self#invokevirtual path name jsig_wrapper (method_sig [] (Some jsig))
			in
			match jsig with
			| TByte -> unwrap_null "Number" "byteValue"
			| TChar -> unwrap_null "Character" "charValue"
			| TDouble -> unwrap_null "Number" "doubleValue"
			| TFloat -> unwrap_null "Number" "floatValue"
			| TInt -> unwrap_null "Number" "intValue"
			| TLong -> unwrap_null "Number" "longValue"
			| TShort -> unwrap_null "Number" "shortValue"
			| TBool -> unwrap_null "Boolean" "booleanValue"
			| _ -> ()
		end else begin
			let unwrap_null tname name =
				self#invokestatic (["haxe";"jvm"],"Jvm") name (method_sig [object_sig] (Some jsig))
			in
			match jsig with
			| TByte -> unwrap_null "Byte" "toByte"
			| TChar -> unwrap_null "Character" "toChar"
			| TDouble -> unwrap_null "Double" "toDouble"
			| TFloat -> unwrap_null "Float" "toFloat"
			| TInt -> unwrap_null "Integer" "toInt"
			| TLong -> unwrap_null "Long" "toLong"
			| TShort -> unwrap_null "Short" "toShort"
			| TBool -> unwrap_null "Boolean" "toBoolean"
			| _ -> ()
		end

	(** Casts the top of the stack to [jsig]. If [allow_to_string] is true, Jvm.toString is called. **)
	method cast ?(not_null=false) ?(allow_to_string=false) jsig =
		let jsig' = code#get_stack#top in
		begin match jsig,jsig' with
		| TObject((["java";"lang"],"Double"),_),TInt ->
			code#i2d;
			self#expect_reference_type;
		| TObject((["java";"lang"],"Double"),_),TObject((["java";"lang"],"Integer"),_) ->
			self#invokestatic (["haxe";"jvm"],"Jvm") "nullIntToNullFloat" (method_sig [integer_sig] (Some double_sig))
		| TObject((["java";"lang"],"Double"),_),TObject((["java";"lang"],"Object"),_) ->
			self#invokestatic (["haxe";"jvm"],"Jvm") "dynamicToNullFloat" (method_sig [object_sig] (Some double_sig))
		(* from double *)
		| TFloat,TDouble ->
			code#d2f
		| TInt,TDouble ->
			code#d2i;
		| TLong,TDouble ->
			code#d2l;
		(* from float *)
		| TDouble,TFloat ->
			code#f2d
		| TInt,TFloat ->
			code#f2i;
		| TLong,TFloat ->
			code#f2l;
		(* from int *)
		| TBool,TInt ->
			ignore(code#get_stack#pop);
			code#get_stack#push TBool;
		| TByte,TInt ->
			code#i2b TByte
		| TChar,TInt ->
			code#i2c
		| TDouble,TInt ->
			code#i2d;
		| TFloat,TInt ->
			code#i2f
		| TLong,TInt ->
			code#i2l;
		| TShort,TInt ->
			code#i2s
		(* from long *)
		| TDouble,TLong ->
			code#l2d;
		| TFloat,TLong ->
			code#l2f
		| TInt,TLong ->
			code#l2i;
		(* widening *)
		| TInt,(TByte | TShort | TChar) ->
			(* No cast, but rewrite stack top *)
			ignore(code#get_stack#pop);
			code#get_stack#push jsig;
		| TObject(path1,_),TObject(path2,_) when path1 = path2 ->
			()
		| TObject((["java";"lang"],"String"),_),_ when allow_to_string ->
			self#expect_reference_type;
			self#invokestatic (["haxe";"jvm"],"Jvm") "toString" (method_sig [object_sig] (Some string_sig))
		| TObject(path1,_),TObject(path2,_) ->
			if path1 = object_path then begin
				(* We should never need a checkcast to Object, but we should adjust the stack so stack maps are wide enough *)
				ignore(code#get_stack#pop);
				code#get_stack#push object_sig
			end else
				code#checkcast path1;
		| TObject(path,_),TTypeParameter _ ->
			code#checkcast path
		| TMethod _,TMethod _ ->
			()
		| TMethod _,TObject((["java";"lang";"invoke"],"MethodHandle"),_)
		| TObject((["java";"lang";"invoke"],"MethodHandle"),_),TMethod _ ->
			()
		| TMethod _,_ ->
			code#checkcast (["java";"lang";"invoke"],"MethodHandle")
		| TArray(jsig1,_),TArray(jsig2,_) when jsig1 = jsig2 ->
			()
		| TArray _,_ ->
			code#checkcast_sig jsig
		| t1,t2 ->
			match is_unboxed t1,is_unboxed t2 with
			| true,false -> self#expect_basic_type ~not_null t1
			| false,true -> self#expect_reference_type
			| _ -> ()
		end

	(* branches *)

	(** Starts a branch. Returns a restore function which reverts the stack and termination status back
	    to the previous state. The restore function can be called multiple times for multiple branches.

		This function has no effect on locals. Use [push_scope] for them.
	**)
	method start_branch =
		let save = code#get_stack#save in
		let old_terminated = terminated in
		(fun () ->
			code#get_stack#restore save;
			terminated <- old_terminated;
		)

	(** Generates code which executes [f_if()] and then branches into [f_then()] and [f_else()]. **)
	method if_then_else (f_if : unit -> jbranchoffset ref) (f_then : unit -> unit) (f_else : unit -> unit) =
		let jump_then = f_if () in
		let restore = self#start_branch in
		let pop = self#push_scope in
		f_then();
		pop();
		let r_then = ref code#get_fp in
		let term_then = self#is_terminated in
		if not term_then then code#goto r_then;
		jump_then := code#get_fp - !jump_then;
		restore();
		self#add_stack_frame;
		let pop = self#push_scope in
		f_else();
		pop();
		self#set_terminated (term_then && self#is_terminated);
		r_then := code#get_fp - !r_then;
		if not self#is_terminated then self#add_stack_frame

	(** Generates code which executes [f_if()] and then branches into [f_then()], if the condition holds. **)
	method if_then (f_if : unit -> jbranchoffset ref) (f_then : unit -> unit) =
		let jump_then = f_if () in
		let restore = self#start_branch in
		let pop = self#push_scope in
		f_then();
		pop();
		restore();
		jump_then := code#get_fp - !jump_then;
		self#add_stack_frame

	(** Adds a local with a given [name], signature [jsig] and an [init_state].
	    This function returns a tuple consisting of:

		  * The slot of the local
		  * The function to load the value
		  * The function to store a value

		If [init_state = VarNeedDefault], the local is initialized to a default value matching [jsig].
		If [init_state = VarArgument], the local is considered initialized.
		If [init_state = VarWillInit], this function assumes that the returned [store] function will be called appropriately.
	**)
	method add_local (name : string) (jsig : jsignature) (init_state : var_init_state) =
		let slot = local_offset in
		let load,store,d = match jsig with
			| TInt | TBool | TByte | TShort | TChar ->
				if init_state = VarNeedDefault then begin
					code#iconst Int32.zero;
					code#istore slot
				end;
				(fun () -> code#iload ~jsig slot),(fun () -> code#istore slot),1
			| TLong ->
				if init_state = VarNeedDefault then begin
					code#lconst Int64.zero;
					code#lstore slot
				end;
				(fun () -> code#lload slot),(fun () -> code#lstore slot),2
			| TFloat ->
				if init_state = VarNeedDefault then begin
					code#fconst 0.;
					code#fstore slot
				end;
				(fun () -> code#fload slot),(fun () -> code#fstore slot),1
			| TDouble ->
				if init_state = VarNeedDefault then begin
					code#dconst 0.;
					code#dstore slot
				end;
				(fun () -> code#dload slot),(fun () -> code#dstore slot),2
			| _ ->
				if init_state = VarNeedDefault then begin
					code#aconst_null jsig;
					code#astore jsig slot
				end;
				(fun () -> code#aload jsig slot),(fun () -> code#astore jsig slot),1
		in
		let init = ref None in
		locals <- (init,name,jsig) :: locals;
		local_offset <- local_offset + d;
		if local_offset > max_num_locals then max_num_locals <- local_offset;
		let check_store =
			let did_store = ref false in
			(fun () ->
				if not !did_store then begin
					did_store := true;
					init := Some (code#get_fp)
				end
			)
		in
		begin match init_state with
		| VarArgument | VarNeedDefault -> check_store();
		| _ -> ()
		end;
		slot,
		load,
		(fun () ->
			store();
			check_store();
		)

	method set_this_initialized =
		let rec loop acc locals = match locals with
			| [(init,name,_)] -> List.rev ((init,name,jc#get_jsig) :: acc)
			| [] -> assert false
			| l :: locals -> loop (l :: acc) locals
		in
		locals <- loop [] locals

	method set_top_initialized jsig =
		ignore(code#get_stack#pop);
		code#get_stack#push jsig

	(** This function has to be called once all arguments are declared. *)
	method finalize_arguments =
		argument_locals <- locals

	method private get_stack_map_table =
		let argument_locals = self#get_locals_for_stack_frame argument_locals in
		let stack_map = List.fold_left (fun ((last_offset,last_locals,last_locals_length),acc) (offset,locals,stack) ->
			let cur = offset - last_offset - 1 in
			let a_locals = Array.of_list (List.rev locals) in
			let locals_length = Array.length a_locals in
			let default () =
				StackFull(cur,a_locals,Array.of_list (List.rev stack))
			in
			let entry = match stack,locals_length - last_locals_length with
			| [],0 ->
				if last_locals = locals then begin
					if cur < 64 then StackSame cur
					else StackSameExtended cur
				end else
					default()
			| [vt],0 ->
				if last_locals = locals then begin
					if cur < 64 then Stack1StackItem(cur,vt)
					else Stack1StackItemExtended(cur,vt)
				end else
					default()
			| [],1 ->
				begin match locals with
				| vt1 :: locals when locals = last_locals -> StackAppend1(cur,vt1)
				| _ -> default()
				end
			| [],2 ->
				begin match locals with
				| vt1 :: vt2 :: locals when locals = last_locals -> StackAppend2(cur,vt2,vt1)
				| _ -> default()
				end
			| [],3 ->
				begin match locals with
				| vt1 :: vt2 :: vt3 :: locals when locals = last_locals -> StackAppend3(cur,vt3,vt2,vt1)
				| _ -> default()
				end
			| [],-1 ->
				begin match last_locals with
				| _ :: last_locals when locals = last_locals -> StackChop1 cur
				| _ -> default()
				end
			| [],-2 ->
				begin match last_locals with
				| _ :: _ :: last_locals when locals = last_locals -> StackChop2 cur
				| _ -> default()
				end
			| [],-3 ->
				begin match last_locals with
				| _ :: _ :: _ :: last_locals when locals = last_locals -> StackChop3 cur
				| _ -> default()
				end
			| _ ->
				default()
			in
			((offset,locals,locals_length),entry :: acc)
		) ((-1,argument_locals,List.length argument_locals),[]) (List.rev stack_frames) in
		Array.of_list (List.rev (snd stack_map))

	method get_code = code
	method is_terminated = terminated
	method get_name = name
	method get_jsig = jsig
	method set_terminated b = terminated <- b

	method private get_jcode (config : export_config) =
		let attributes = DynArray.create () in
		let lines = code#get_lines in
		if config.export_debug && DynArray.length lines > 0 then
			DynArray.add attributes (AttributeLineNumberTable (DynArray.to_array lines));
		let stack_map_table = self#get_stack_map_table in
		if Array.length stack_map_table > 0 then
			DynArray.add attributes (AttributeStackMapTable stack_map_table);
		let exceptions = Array.of_list (List.rev exceptions) in
		let attributes = List.map (JvmAttribute.write_attribute jc#get_pool) (DynArray.to_list attributes) in
		{
			code_max_stack = code#get_max_stack_size;
			code_max_locals = max_num_locals;
			code_code = code#export_code;
			code_exceptions = exceptions;
			code_attributes = Array.of_list attributes;
		}

	(** Exports the method as a [jvm_field]. No other functions should be called on this object afterwards. *)
	method export_method (config : export_config) =
		assert (not was_exported);
		was_exported <- true;
		self#commit_annotations jc#get_pool;
		if code#get_fp > 0 then begin
			let code = self#get_jcode config in
			self#add_attribute (AttributeCode code);
		end;
		if Hashtbl.length thrown_exceptions > 0 then
			self#add_attribute (AttributeExceptions (Array.of_list (Hashtbl.fold (fun k _ c -> k :: c) thrown_exceptions [])));
		if config.export_debug then begin match debug_locals with
		| [] ->
			()
		| _ ->
			let a = Array.of_list debug_locals in
			self#add_attribute (AttributeLocalVariableTable a);
		end;
		let attributes = self#export_attributes jc#get_pool in
		let offset_name = jc#get_pool#add_string name in
		let jsig = generate_method_signature false jsig in
		let offset_desc = jc#get_pool#add_string jsig in
		{
			field_access_flags = access_flags;
			field_name_index = offset_name;
			field_descriptor_index = offset_desc;
			field_attributes = attributes;
		}

	(** Exports the method as a [jvm_field]. No other functions should be called on this object afterwards. *)
	method export_field =
		assert (code#get_fp = 0);
		assert (not was_exported);
		was_exported <- true;
		let attributes = self#export_attributes jc#get_pool in
		let offset_name = jc#get_pool#add_string name in
		let jsig = generate_signature false jsig in
		let offset_desc = jc#get_pool#add_string jsig in
		{
			field_access_flags = access_flags;
			field_name_index = offset_name;
			field_descriptor_index = offset_desc;
			field_attributes = attributes;
		}
end