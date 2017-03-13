open Common
open Type

let write_file ctx pack name contents =
	mkdir_recursive ctx.file pack;
	let out_file = String.concat "/" (ctx.file :: pack @ [name ^ ".cs"]) in
	let out_chan = open_out_bin out_file in
	output_string out_chan contents;
	close_out out_chan

let generate_class ctx cl =
	let pack,name = cl.cl_path in
	let contents = "// TODO" in
	write_file ctx pack name contents

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
