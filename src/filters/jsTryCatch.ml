(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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
open Common
open Type
open Texpr.Builder

let rec is_error t =
	match follow t with
	| TInst ({cl_path = (["js"],"Error")},_) -> true
	| TInst ({cl_super = Some (csup,tl)}, _) -> is_error (TInst (csup,tl))
	| _ -> false

let find_class com path = List.find (fun mt -> match mt with TClassDecl {cl_path = p} when p = path -> true | _ -> false) com.types

let get_class mt = match mt with TClassDecl c -> c | _ -> assert false

let transform com e =
	let clboot = get_class (find_class com (["js"],"Boot")) in
	let terr = find_class com (["js";"_Boot"],"HaxeError") in
	let clerr = get_class terr in

	let rec loop rethrow_var e =
		match e.eexpr with
		| TThrow eerr when not (is_error eerr.etype) ->
			(match eerr.etype with
			| TDynamic _ ->
				let eclerr = make_static_this clerr e.epos in
				let ewrap = fcall eclerr "wrap" [eerr] t_dynamic e.epos in
				{ e with eexpr = TThrow ewrap }
			| _ ->
				let ewrap = { eerr with eexpr = TNew (clerr,[],[eerr]); etype = TInst (clerr,[]) } in
				{ e with eexpr = TThrow ewrap })

		| TCall ({ eexpr = TIdent "__rethrow__" },_) ->
			(match rethrow_var with
			| Some e -> { e with eexpr = TThrow e }
			| None -> abort "rethrow outside of catch" e.epos)

		| TTry (etry,catches) ->
			let etry = loop rethrow_var etry in

			let vcatchall = alloc_var "e" t_dynamic e.epos in
			let ecatchall = make_local vcatchall e.epos in

			let vunwrapped = alloc_var "e" t_dynamic e.epos in
			let eunwrap =
				let einstanceof = mk (TIdent "__instanceof__") t_dynamic e.epos in
				let eterr = make_static_this clerr e.epos in
				let echeck = mk (TCall (einstanceof,[ecatchall;eterr])) com.basic.tbool e.epos in
				let efield = field { ecatchall with etype = TInst (clerr,[]) } "val" t_dynamic e.epos in
				mk (TIf (echeck, efield, Some ecatchall)) t_dynamic e.epos
			in
			let eunwrapped = make_local vunwrapped e.epos in

			let erethrow = mk (TThrow ecatchall) t_dynamic e.epos in

			let ebody = List.fold_left (fun eprev (v,e) ->
				let e = loop (Some ecatchall) e in

				let eboot = make_static_this clboot v.v_pos in
				(* None - catch dynamic, Some - catch typed *)
				let mt = try Some (module_type_of_type v.v_type) with Exit -> None in
				match mt with
				| None ->
					mk (TBlock [
						mk (TVar (v, Some eunwrapped)) com.basic.tvoid v.v_pos;
						e
					]) e.etype e.epos
				| Some mt ->
					let etype = make_typeexpr mt v.v_pos in
					let echeck = fcall eboot "__instanceof" [eunwrapped; etype] com.basic.tbool v.v_pos in
					let ecatchbody = mk (TBlock [
						mk (TVar (v, Some eunwrapped)) com.basic.tvoid v.v_pos;
						e
					]) e.etype e.epos in
					mk (TIf (echeck,ecatchbody,Some eprev)) e.etype e.epos
			) erethrow (List.rev catches) in

			let ebody = mk (TBlock [
				mk (TVar (vunwrapped, Some eunwrap)) com.basic.tvoid e.epos;
				ebody;
			]) e.etype e.epos in

			{ e with eexpr = TTry (etry, [(vcatchall,ebody)]) }
		| _ ->
			Type.map_expr (loop rethrow_var) e
	in
	loop None e
