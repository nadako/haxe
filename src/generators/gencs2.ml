open Common
open Type

class source_writer = object(self)
	val buf = Buffer.create 0
	val mutable has_content = false
	val mutable indent = ""
	val mutable indents = []

	method get_contents =
		Buffer.contents buf

	method push_indent =
		indents <- "\t" :: indents;
		indent <- String.concat "" indents

	method pop_indent =
		match indents with
		| _ :: tail ->
			indents <- tail;
			indent <- String.concat "" indents
		| [] ->
			indent <- "/*?*/"

	method write x =
		if not has_content then begin
			has_content <- true;
			Buffer.add_string buf indent;
			Buffer.add_string buf x
		end else
			Buffer.add_string buf x;

		let last = (String.length x) - 1 in
		if last >= 0 && String.get x last = '\n' then
			has_content <- false
		else
			has_content <- true

	method newline =
		self#write "\n"

	method begin_block =
		self#write "{";
		self#push_indent;
		self#newline;
		(fun () -> self#end_block)

	method private end_block =
		self#pop_indent;
		if has_content then self#newline;
		self#write "}";
		self#newline
end;;

let write_file ctx pack name contents =
	mkdir_recursive ctx.file pack;
	let out_file = String.concat "/" (ctx.file :: pack @ [name ^ ".cs"]) in
	let out_chan = open_out_bin out_file in
	output_string out_chan contents;
	close_out out_chan

let emit_namespace w pack =
	match pack with
	| [] ->
		(fun () -> ())
	| _ ->
		w#write ("namespace " ^ (String.concat "." pack) ^ " ");
		w#begin_block

let generate_class_field ctx cl is_static w cf =
	if is_static then w#write "static ";
	w#write "int ";
	w#write cf.cf_name;
	w#write ";";
	w#newline

let generate_class ctx cl =
	let pack,name = cl.cl_path in

	let w = new source_writer in

	let close_ns = emit_namespace w pack in

	let kind = if cl.cl_interface then "interface" else "class" in
	w#write (Printf.sprintf "%s %s " kind name);
	let close_cl = w#begin_block in

	List.iter (generate_class_field ctx cl false w) cl.cl_ordered_fields;
	List.iter (generate_class_field ctx cl true w) cl.cl_ordered_statics;

	close_cl ();
	close_ns ();

	write_file ctx pack name w#get_contents

let generate_type ctx t =
	match t with
	| TClassDecl cl when not cl.cl_extern ->
		generate_class ctx cl
	|_ ->
		() (* TODO *)

(**
	Entry point to Gencs2
*)
let generate com =
	List.iter (generate_type com) com.types;
	print_endline "cs2"
