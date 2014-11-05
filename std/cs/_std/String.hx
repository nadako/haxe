/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
import cs.internal.StringExt;
import cs.StdTypes;

@:coreApi extern class String implements ArrayAccess<Char16> {

	@:overload private static function Compare(s1:String, s2:String):Int;
	@:overload private static function Compare(s1:String, s2:String, kind:cs.system.StringComparison):Int;

	@:native("Length")
	var length(default,null) : Int;

	@:overload(function(c:Char16, count:Int) : Void {})
	function new(string:String) : Void;

	@:native("ToUpperInvariant")
	function toUpperCase() : String;

	@:native("ToLowerInvariant")
	function toLowerCase() : String;

	inline function charAt( index : Int) : String return StringExt.charAt(this, index);

	inline function charCodeAt( index : Int) : Null<Int> return StringExt.charCodeAt(this, index);

	inline function indexOf( str : String, ?startIndex : Int ) : Int return StringExt.indexOf(this, str, startIndex);

	inline function lastIndexOf( str : String, ?startIndex : Int ) : Int return StringExt.lastIndexOf(this, str, startIndex);

	inline function split( delimiter : String ) : Array<String> return StringExt.split(this, delimiter);

	inline function substr( pos : Int, ?len : Int ) : String return StringExt.substr(this, pos, len);

	inline function substring( startIndex : Int, ?endIndex : Int ) : String return StringExt.substring(this, startIndex, endIndex);

	inline function toString() : String return this;

	inline static function fromCharCode( code : Int ) : String return new String(code, 1);

	private function IndexOf(value:String, startIndex:Int, comparisonType:cs.system.StringComparison):Int;
	private function Replace(oldValue:String, newValue:String):String;
	private function StartsWith(value:String):Bool;
	private function EndsWith(value:String):Bool;
	private function TrimStart():String;
	private function TrimEnd():String;
	private function Trim():String;
	private function CompareTo(obj:Dynamic):Int;
	@:overload(function(startIndex:Int):String {})
	private function Substring(startIndex:Int, length:Int):String;

}
