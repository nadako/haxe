package flash;

extern class Syntax {
	// 0x32 HNext (__has_next__)
	static function hasnext2(object:Ref<Dynamic>, index:Ref<Int>):Bool;
	// 0x1E HForIn (__forin__)
	static function nextname(object:Dynamic, index:Int):Dynamic;
	// 0x23 HForEach (__foreach__)
	static function nextvalue(collection:Dynamic, index:Int):Dynamic;
}

@:semantics(reference)
typedef Ref<T> = T;
