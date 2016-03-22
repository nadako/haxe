(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

module Json = struct
	type json =
		| JString of string
		| JFloat of float
		| JInt of int
		| JObject of (string * json) list
		| JArray of json list
		| JBool of bool
		| JNull

	let write_iter f_el f_sep l =
		let rec rest = function
			| [] -> ()
			| v :: l ->
				f_sep();
				f_el v;
				rest l
		in
		match l with
		| [] -> ()
		| v :: l ->
			f_el v;
			rest l

	let write_sep ch =
		IO.write ch ','

	let rec write_json ch v =
		match v with
		| JNull -> write_null ch
		| JBool b -> write_bool ch b
		| JString s -> write_string ch s
		| JFloat f -> write_float ch f
		| JInt i -> write_int ch i
		| JObject o -> write_object ch o
		| JArray a -> write_array ch a

	and write_null ch =
		IO.nwrite ch "null"

	and write_bool ch b =
		IO.nwrite ch (if b then "true" else "false")

	and write_string ch s =
		IO.write ch '"';
		for i = 0 to String.length s - 1 do
			match String.unsafe_get s i with
			| '"' -> IO.nwrite ch "\\\""
			| '\t' -> IO.nwrite ch "\\t"
			| '\r' -> IO.nwrite ch "\\r"
			| '\b' -> IO.nwrite ch "\\b"
			| '\n' -> IO.nwrite ch "\\n"
			| '\012' -> IO.nwrite ch "\\f"
			| '\\' -> IO.nwrite ch "\\\\"
			| '\x00'..'\x1F' | '\x7F' as c -> IO.nwrite ch (Printf.sprintf "\\u%04X" (int_of_char c))
			| c -> IO.write ch c
		done;
		IO.write ch '"'

	and write_int ch i =
		IO.nwrite ch (string_of_int i)

	and write_float ch f =
		match classify_float f with
		| FP_nan | FP_infinite -> failwith "NaN and infinity floats are unsupported in JSON"
		| _ ->
			let s = Printf.sprintf "%.16g" f in
			let s = if float_of_string s = f then s else Printf.sprintf "%.17g" f in
			IO.nwrite ch s

	and write_array ch a =
		IO.write ch '[';
		write_iter (write_json ch) (fun() -> write_sep ch) a;
		IO.write ch ']'

	and write_object ch o =
		let write_el (k, v) =
			write_string ch k;
			IO.write ch ':';
			write_json ch v
		in
		IO.write ch '{';
		write_iter write_el (fun() -> write_sep ch) o;
		IO.write ch '}'

end

open Ast
open Json
open Common
open Type

let generate_type com t =
	let generate_path_fields path =
		[
			("pack", JArray (List.map (fun s -> JString s) (fst path)));
			("name", JString (snd path));
		]
	in

	let generate_type_fields kind infos =
		let path_fields = generate_path_fields infos.mt_path in
		let path_fields = if infos.mt_module.m_path <> infos.mt_path then
			("module", JObject (generate_path_fields infos.mt_module.m_path)) :: path_fields
		else
			path_fields
		in

		let fields = [
			("kind", JString kind);
			("path", JObject path_fields);
			("file", if infos.mt_pos <> Ast.null_pos then (JString infos.mt_pos.pfile) else JNull);
			("isPrivate", JBool infos.mt_private);
		] in

		let fields = Option.map_default (fun s -> ("doc", JString s) :: fields) fields infos.mt_doc in

		let fields = if infos.mt_params <> [] then
			let generate_type_param_entry tparam =
				let name, t = tparam in
				let fields = [("name", JString name)] in
				let fields = match follow t with
					| TInst ({cl_kind = KTypeParameter constraints},_) when constraints <> [] ->
						("constraints", JArray (List.map (fun t -> JString "#TODO#") constraints)) :: fields
					| _ ->
						fields
				in
				JObject fields
			in
			("params", JArray (List.map generate_type_param_entry infos.mt_params)) :: fields
		else
			fields
		in


		let rec loop acc ml =
			match ml with
			| [] -> acc
			| (m,pl,_) :: ml ->
				let info = MetaInfo.to_string m in
				let flags = snd (snd info) in
				let rest = loop acc ml in
				if List.mem MetaInfo.Internal flags then
					rest
				else
					(fst info, pl) :: rest
		in
		let meta = loop [] infos.mt_meta in
		let fields = if meta <> [] then
			let generate_meta_entry m =
				JObject [
					("name", JString (fst m));
					("params", JArray (List.map (fun e -> JString (Ast.s_expr e)) (snd m)));
				]
			in
			("meta", JArray (List.map generate_meta_entry meta)) :: fields
		else
			fields
		in

		fields
	in

	let generate_class_field_info f =
		let fields = [
			("name", JString f.cf_name);
			("isPublic", JBool f.cf_public);
			("line", JInt (Lexer.get_error_line f.cf_pos));
		] in
		let fields = match f.cf_doc with
			| Some s -> ("doc", JString s) :: fields
			| None -> fields
		in

		let kind,fields = match f.cf_kind with
			| Var v ->
				let accessmod acc rw fields =
					match acc with
					| AccNormal | AccResolve | AccRequire _ -> fields
					| AccNo | AccNever -> (rw, JString "no") :: fields
					| AccCall -> (rw, JString "call") :: fields
					| AccInline -> (rw, JString "inline") :: fields
				in
				let fields = accessmod v.v_read "read" fields in
				let fields = accessmod v.v_write "write" fields in
				"var",fields
			| Method mk ->
				let kind = match mk with
					| MethNormal -> None
					| MethMacro -> Some "macro" (* TODO: these don't show up here wtf *)
					| MethInline -> Some "inline"
					| MethDynamic -> Some "dynamic"
				in
				let fields = Option.map_default (fun v -> ("modifier", JString v) :: fields) fields kind in
				"method",fields
		in
		let fields = ("kind", JString kind) :: fields in

		JObject fields
	in

	let infos = t_infos t in

	let generate_class_fields cl =
		[
			("fields", JArray (List.map generate_class_field_info cl.cl_ordered_fields));
			("statics", JArray (List.map generate_class_field_info cl.cl_ordered_statics));
		]
	in

	let generate_enum_fields en =
		[]
	in

	let generate_typedef_fields td =
		[]
	in

	let generate_abstract_fields ab =
		[]
	in

	match t with
	| TClassDecl c -> JObject ((generate_type_fields "class" infos) @ (generate_class_fields c))
	| TEnumDecl e -> JObject ((generate_type_fields "enum" infos) @ (generate_enum_fields e))
	| TTypeDecl t -> JObject ((generate_type_fields "typedef" infos) @ (generate_typedef_fields t))
	| TAbstractDecl a -> JObject ((generate_type_fields "abstract" infos) @ (generate_abstract_fields a))

let generate com file =
	let t = Common.timer "construct json" in
	let json = JArray (List.map (generate_type com) com.types) in
	t();
	let t = Common.timer "write json" in
	let ch = IO.output_channel (open_out_bin file) in
	write_json ch json;
	IO.close_out ch;
	t()
