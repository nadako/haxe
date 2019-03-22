open JvmGlobals
open JvmData
open JvmVerificationTypeInfo
open JvmWriter

(* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.4 *)
type j_stack_map_frame =
	| StackSame of int
	| Stack1StackItem of int * JvmVerificationTypeInfo.t
	| Stack1StackItemExtended of int * JvmVerificationTypeInfo.t
	| StackChop1 of int
	| StackChop2 of int
	| StackChop3 of int
	| StackSameExtended of int
	| StackAppend1 of int * JvmVerificationTypeInfo.t
	| StackAppend2 of int * JvmVerificationTypeInfo.t * JvmVerificationTypeInfo.t
	| StackAppend3 of int * JvmVerificationTypeInfo.t * JvmVerificationTypeInfo.t * JvmVerificationTypeInfo.t
	| StackFull of int * JvmVerificationTypeInfo.t array * JvmVerificationTypeInfo.t array

(* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.13 *)
type jvm_local_debug = {
	ld_start_pc : int;
	ld_length : int;
	ld_name_index : jvm_constant_pool_index;
	ld_descriptor_index : jvm_constant_pool_index;
	ld_index : int;
}

(* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7 *)
type j_attribute =
	| AttributeConstantValue of jvm_constant_pool_index
	| AttributeCode of jvm_code
	| AttributeStackMapTable of j_stack_map_frame array
	| AttributeSourceFile of jvm_constant_pool_index
	| AttributeLineNumberTable of (int * int) array
	| AttributeSignature of jvm_constant_pool_index
	| AttributeLocalVariableTable of jvm_local_debug array

let write_verification_type ch = function
	| VTop -> write_byte ch 0
	| VInteger -> write_byte ch 1
	| VFloat -> write_byte ch 2
	| VDouble -> write_byte ch 3
	| VLong -> write_byte ch 4
	| VNull -> write_byte ch 5
	| VUninitializedThis -> write_byte ch 6
	| VObject i ->
		write_byte ch 7;
		write_ui16 ch i;
	| VUninitialized i ->
		write_byte ch 8;
		write_ui16 ch i

let write_stack_map_frame ch = function
	| StackSame i ->
		assert (i <= 64);
		write_byte ch i
	| Stack1StackItem(i,t) ->
		assert (i <= 64);
		write_byte ch (i + 64);
		write_verification_type ch t;
	| Stack1StackItemExtended(i,t) ->
		write_byte ch 247;
		write_ui16 ch i;
		write_verification_type ch t;
	| StackChop1 i1 ->
		write_byte ch 250;
		write_ui16 ch i1;
	| StackChop2 i1 ->
		write_byte ch 249;
		write_ui16 ch i1;
	| StackChop3 i1 ->
		write_byte ch 248;
		write_ui16 ch i1;
	| StackSameExtended i ->
		write_byte ch 251;
		write_ui16 ch i
	| StackAppend1(i1,t1) ->
		write_byte ch 252;
		write_ui16 ch i1;
		write_verification_type ch t1;
	| StackAppend2(i1,t1,t2) ->
		write_byte ch 253;
		write_ui16 ch i1;
		write_verification_type ch t1;
		write_verification_type ch t2;
	| StackAppend3(i1,t1,t2,t3) ->
		write_byte ch 254;
		write_ui16 ch i1;
		write_verification_type ch t1;
		write_verification_type ch t2;
		write_verification_type ch t3;
	| StackFull(i1,tl1,tl2) ->
		write_byte ch 255;
		write_ui16 ch i1;
		write_array16 ch write_verification_type tl1;
		write_array16 ch write_verification_type tl2

let write_constant pool ch const =
	let offset = pool#add const in
	write_ui16 ch offset

let write_attribute pool jvma =
	let ch = IO.output_bytes () in
	let name = match jvma with
	| AttributeConstantValue const ->
		write_ui16 ch const;
		"ConstantValue";
	| AttributeCode code ->
		write_ui16 ch code.code_max_stack;
		write_ui16 ch code.code_max_locals;
		write_ui32 ch (Bytes.length code.code_code);
		write_bytes ch code.code_code;
		write_array16 ch write_exception code.code_exceptions;
		write_jvm_attributes ch code.code_attributes;
		"Code";
	| AttributeStackMapTable stack_map ->
		write_array16 ch write_stack_map_frame stack_map;
		"StackMapTable"
	| AttributeSourceFile offset ->
		write_ui16 ch offset;
		"SourceFile";
	| AttributeLineNumberTable a ->
		write_array16 ch (fun _ (i1,i2) ->
			write_ui16 ch i1;
			write_ui16 ch i2
		) a;
		"LineNumberTable"
	| AttributeSignature offset ->
		write_ui16 ch offset;
		"Signature"
	| AttributeLocalVariableTable table ->
		write_array16 ch (fun _ d ->
			write_ui16 ch d.ld_start_pc;
			write_ui16 ch d.ld_length;
			write_ui16 ch d.ld_name_index;
			write_ui16 ch d.ld_descriptor_index;
			write_ui16 ch d.ld_index;
		) table;
		"LocalVariableTable"
	in
	{
		attr_index = pool#add (ConstUtf8 name);
		attr_data = IO.close_out ch
	}