package jvm;

import haxe.extern.Rest;
import haxe.Constraints;
import Enum;
import jvm.DynamicObject;
import jvm.Exception;
import jvm.EmptyConstructor;
import jvm.annotation.ClassReflectionInformation;
import jvm.annotation.EnumReflectionInformation;
import jvm.annotation.EnumValueReflectionInformation;
import java.lang.invoke.*;

@:keep
@:native('haxe.jvm.Jvm')
class Jvm {
	extern static public function instanceof<S, T>(obj:S, type:T):Bool;

	extern static public function referenceEquals<T>(v1:T, v2:T):Bool;

	extern static public function invokedynamic<T>(bootstrapMethod:Function, fieldName:String, staticArguments:Array<Dynamic>, rest:Rest<Dynamic>):T;

	static public function stringCompare(v1:String, v2:String):Int {
		if (v1 == null) {
			return v2 == null ? 0 : 1;
		}
		return (cast v1 : java.lang.JavaString.String).compareTo(v2);
	}

	static public function equals<T>(v1:T, v2:T):Bool {
		if (referenceEquals(v1, v2))
			return true;
		if (v1 == null || v2 == null)
			return false;

		if (instanceof(v1, java.lang.Number)) {
			if (!(instanceof(v2, java.lang.Number)))
				return false;

			var v1c = (cast v1 : java.lang.Number);
			var v2c = (cast v2 : java.lang.Number);
			if (instanceof(v1, java.lang.Long) || instanceof(v2, java.lang.Long)) {
				return v1c.longValue() == v2c.longValue();
			}
			return v1c.doubleValue() == v2c.doubleValue();
		} else if (instanceof(v1, java.lang.JavaString.String)) {
			return (cast v1 : java.lang.JavaString.String).equals(v2);
		}

		return false;
	}

	// casts

	static public function dynamicToNullFloat<T>(d:T):Null<Float> {
		if (instanceof(d, java.lang.Integer.IntegerClass)) {
			return nullIntToNullFloat(cast d);
		}
		// TODO: need a better strategy to avoid infinite recursion here
		return cast d;
	}

	static public function nullIntToNullFloat(i:Null<Int>):Null<Float> {
		if (i == null) {
			return null;
		}
		return (cast i : java.lang.Number).intValue();
	}

	static public function toByte(d:Dynamic) {
		return d == null ? 0 : (d : java.lang.Byte).byteValue();
	}

	static public function toChar(d:Dynamic) {
		return d == null ? 0 : (d : java.lang.Character).charValue();
	}

	static public function toDouble(d:Dynamic) {
		return d == null ? 0. : (d : java.lang.Number).doubleValue();
	}

	static public function toFloat(d:Dynamic):Single {
		return d == null ? 0. : (d : java.lang.Number).floatValue();
	}

	static public function toInt(d:Dynamic) {
		return d == null ? 0 : (d : java.lang.Number).intValue();
	}

	static public function toLong(d:Dynamic) {
		return d == null ? 0 : (d : java.lang.Long).longValue();
	}

	static public function toShort(d:Dynamic) {
		return d == null ? 0 : (d : java.lang.Short).shortValue();
	}

	static public function toBoolean(d:Dynamic) {
		return d == null ? false : (d : java.lang.Boolean).booleanValue();
	}

	static public function getWrapperClass<S, T>(c:java.lang.Class<S>):java.lang.Class<S> {
		if (!c.isPrimitive()) {
			return c;
		}
		// TODO: other basic types
		return if (c == cast Int) {
			cast java.lang.Integer.IntegerClass;
		} else if (c == cast Float) {
			cast java.lang.Double.DoubleClass;
		} else if (c == cast Bool) {
			cast java.lang.Boolean.BooleanClass;
		} else {
			c;
		}
	}

	// access

	static public function arrayRead(obj:Dynamic, index:Int) {
		if (instanceof(obj, Array)) {
			return (obj : Array<Dynamic>)[index];
		}
		throw 'Cannot array-read on $obj';
	}

	static public function arrayWrite(obj:Dynamic, index:Int, value:Dynamic):Void {
		if (instanceof(obj, Array)) {
			(obj : Array<Dynamic>)[index] = value;
			return;
		}
		throw 'Cannot array-write on $obj';
	}

	static public function bootstrap(caller:MethodHandles.MethodHandles_Lookup, name:String, type:MethodType):CallSite {
		var handle = caller.findStatic(caller.lookupClass(), name, type);
		return new ConstantCallSite(handle);
	}

	static public function readField(obj:Dynamic, name:String):Dynamic {
		if (obj == null) {
			return null;
		}
		var isStatic = instanceof(obj, java.lang.Class);
		var cl = isStatic ? obj : (obj : java.lang.Object).getClass();
		try {
			var field = cl.getField(name);
			field.setAccessible(true);
			return field.get(obj);
		} catch (_:java.lang.NoSuchFieldException) {
			if (instanceof(obj, java.lang.JavaString.String)) {
				switch (name) {
					case "length": return (obj : String).length;
					case "charAt": return (cast jvm.StringExt.charAt : java.lang.invoke.MethodHandle).bindTo(obj);
					case "charCodeAt": return (cast jvm.StringExt.charCodeAt : java.lang.invoke.MethodHandle).bindTo(obj);
					case "indexOf": return (cast jvm.StringExt.indexOf : java.lang.invoke.MethodHandle).bindTo(obj);
					case "lastIndexOf": return (cast jvm.StringExt.lastIndexOf : java.lang.invoke.MethodHandle).bindTo(obj);
					case "split": return (cast jvm.StringExt.split : java.lang.invoke.MethodHandle).bindTo(obj);
					case "substr": return (cast jvm.StringExt.substr : java.lang.invoke.MethodHandle).bindTo(obj);
					case "substring": return (cast jvm.StringExt.substring : java.lang.invoke.MethodHandle).bindTo(obj);
				}
			}
			while (cl != null) {
				var methods = cl.getMethods();
				for (m in methods) {
					if (m.getName() == name) {
						var method = java.lang.invoke.MethodHandles.lookup().unreflect(m);
						if (!isStatic || cl == cast java.lang.Class) {
							method = method.bindTo(obj);
						}
						return method;
					}
				}
				if (instanceof(obj, DynamicObject)) {
					return (obj : DynamicObject)._hx_getField(name);
				}
				if (isStatic) {
					if (cl == cast java.lang.Class) {
						break;
					}
					cl = cast java.lang.Class;
				} else {
					cl = cl.getSuperclass();
				}
			}
			return null;
		}
	}

	static public function writeFieldNoDyn<T>(obj:Dynamic, name:String, value:T) {
		try {
			var cl = (obj : java.lang.Object).getClass();
			var field = cl.getField(name);
			field.setAccessible(true);
			try {
				field.set(obj, value);
			} catch (_:java.lang.IllegalArgumentException) {
				if (value == null) {
					field.setByte(obj, 0); // rely on widening
				} else if (field.getType() == (cast Int) && instanceof(value, java.lang.Number)) {
					// Can happen with ++ on Dynamic because that defaults to Float
					field.setInt(obj, (cast value : java.lang.Number).intValue());
				}
			}
		} catch (_:java.lang.NoSuchFieldException) {
			return;
		}
	}

	static public function writeField<T>(obj:Dynamic, name:String, value:T) {
		if (obj == null) {
			return;
		}
		if (instanceof(obj, DynamicObject)) {
			return (obj : DynamicObject)._hx_setField(name, value);
		}
		writeFieldNoDyn(obj, name, value);
	}

	// string

	static public function toString<T:java.lang.Object>(obj:T) {
		if (obj == null) {
			return "null";
		} else if (instanceof(obj, java.lang.Double.DoubleClass)) {
			var n:java.lang.Number = cast obj;
			if (n.doubleValue() == n.intValue()) {
				return java.lang.Integer.IntegerClass.valueOf(n.intValue()).toString();
			}
			return obj.toString();
		} else {
			return obj.toString();
		}
	}

	static public function stringConcat<A:java.lang.Object, B:java.lang.Object>(a:A, b:B):String {
		return (cast toString(a) : java.lang.JavaString.String).concat(toString(b));
	}

	// ops

	@:native("+")
	static public function opAdd<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.JavaString.String) || instanceof(b, java.lang.JavaString.String)) {
			return stringConcat(a, b);
		}
		if (instanceof(a, java.lang.Double.DoubleClass) || instanceof(b, java.lang.Double.DoubleClass)) {
			return toDouble(a) + toDouble(b);
		}
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) + toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) + toInt(b);
		}
		throw "Invalid operation";
	}

	@:native("-")
	static public function opSub<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass) || instanceof(b, java.lang.Double.DoubleClass)) {
			return toDouble(a) - toDouble(b);
		}
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) - toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) - toInt(b);
		}
		throw "Invalid operation";
	}

	@:native("*")
	static public function opMul<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass) || instanceof(b, java.lang.Double.DoubleClass)) {
			return toDouble(a) * toDouble(b);
		}
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) * toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) * toInt(b);
		}
		throw "Invalid operation";
	}

	@:native("/")
	static public function opDiv<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass) || instanceof(b, java.lang.Double.DoubleClass)) {
			return toDouble(a) / toDouble(b);
		}
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) / toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) / toInt(b);
		}
		throw "Invalid operation";
	}

	@:native("%")
	static public function opMod<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass) || instanceof(b, java.lang.Double.DoubleClass)) {
			return toDouble(a) % toDouble(b);
		}
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) % toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) % toInt(b);
		}
		throw "Invalid operation";
	}

	@:native("&")
	static public function opAnd<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) & toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) & toInt(b);
		}
		throw "Invalid operation";
	}

	@:native("|")
	static public function opOr<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) | toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) | toInt(b);
		}
		throw "Invalid operation";
	}

	@:native("^")
	static public function opXor<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) ^ toLong(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) ^ toInt(b);
		}
		throw "Invalid operation";
	}

	@:native("shl")
	static public function opShl<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) << toInt(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) << toInt(b);
		}
		throw "Invalid operation";
	}

	@:native(">>")
	static public function opShr<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) >> toInt(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) >> toInt(b);
		}
		throw "Invalid operation";
	}

	@:native(">>>")
	static public function opUshr<T1:java.lang.Object, T2:java.lang.Object>(a:T1, b:T2):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass) || instanceof(b, java.lang.Long.LongClass)) {
			return toLong(a) >>> toInt(b);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass) || instanceof(b, java.lang.Integer.IntegerClass)) {
			return toInt(a) >>> toInt(b);
		}
		throw "Invalid operation";
	}

	@:native("++")
	static public function opIncrement<T1:java.lang.Object>(a:T1):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass)) {
			return toDouble(a) + 1.;
		}
		if (instanceof(a, java.lang.Long.LongClass)) {
			return toLong(a) + 1.;
		}
		if (instanceof(a, java.lang.Integer.IntegerClass)) {
			return toInt(a) + 1;
		}
		throw "Invalid operation";
	}

	@:native("--")
	static public function opDecrement<T1:java.lang.Object>(a:T1):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass)) {
			return toDouble(a) - 1.;
		}
		if (instanceof(a, java.lang.Long.LongClass)) {
			return toLong(a) - 1.;
		}
		if (instanceof(a, java.lang.Integer.IntegerClass)) {
			return toInt(a) - 1;
		}
		throw "Invalid operation";
	}

	@:native("neg")
	static public function opNeg<T1:java.lang.Object>(a:T1):Dynamic {
		if (instanceof(a, java.lang.Double.DoubleClass)) {
			return -toDouble(a);
		}
		if (instanceof(a, java.lang.Long.LongClass)) {
			return -toLong(a);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass)) {
			return -toInt(a);
		}
		throw "Invalid operation";
	}

	@:native("~")
	static public function opNegBits<T1:java.lang.Object>(a:T1):Dynamic {
		if (instanceof(a, java.lang.Long.LongClass)) {
			return ~toLong(a);
		}
		if (instanceof(a, java.lang.Integer.IntegerClass)) {
			return ~toInt(a);
		}
		throw "Invalid operation";
	}
}
