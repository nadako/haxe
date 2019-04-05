import java.lang.invoke.*;
import java.lang.NoSuchMethodException;
import jvm.annotation.*;
import jvm.Jvm;

enum ValueType {
	TNull;
	TInt;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass(c:Class<Dynamic>);
	TEnum(e:Enum<Dynamic>);
	TUnknown;
}

@:coreApi
class Type {
	static function hackGetAnnotation<T:java.lang.annotation.Annotation>(c:java.lang.Class<Dynamic>, cAnnotation:java.lang.Class<T>):T {
		// TODO: We should use e.getAnnotation(clInfo) here, but our type system has some issues with that
		var annotations = c.getAnnotations();
		for (annotation in annotations) {
			if (cAnnotation == cast annotation.annotationType()) {
				return cast annotation;
			}
		}
		return null;
	}

	static function isEnumClass<T>(c:java.lang.Class<T>):Bool {
		// TODO: have to be careful if we ever decide to omit EnumReflectionInformation
		// Maybe a separate HaxeEnum annotation would be better here
		return c.isAnnotationPresent(cast EnumReflectionInformation);
	}

	static function isEnumValueClass<T>(c:java.lang.Class<T>):Bool {
		// TODO: have to be careful if we ever decide to omit EnumValueReflectionInformation
		// Maybe a separate HaxeEnum annotation would be better here
		return c.isAnnotationPresent(cast EnumValueReflectionInformation);
	}

	public static function getClass<T>(o:T):Class<T> {
		if (o == null) {
			return null;
		}
		if (Jvm.instanceof(o, Class)) {
			return null;
		}
		var c = (cast o : java.lang.Object).getClass();
		if (isEnumValueClass(c)) {
			return null;
		}
		return c;
	}

	public static function getEnum(o:EnumValue):Enum<Dynamic> {
		if (o == null) {
			return null;
		}
		var c = (cast o : java.lang.Object).getClass().getSuperclass();
		if (!c.isAnnotationPresent(cast EnumReflectionInformation)) {
			return null;
		}
		return cast c;
	}

	public static function getSuperClass(c:Class<Dynamic>):Class<Dynamic> {
		var c = c.native();
		var cSuper = c.getSuperclass();
		if (cSuper == null) {
			return null;
		}
		var annotation = hackGetAnnotation(c, (cast ClassReflectionInformation : java.lang.Class<ClassReflectionInformation>));
		if (annotation != null && annotation.hasSuperClass() == false) {
			return null;
		}
		return cSuper;
	}

	public static function getClassName(c:Class<Dynamic>):String {
		// TODO: java.lang.String has to become String somehow
		return c.native().getName();
	}

	public static function getEnumName(e:Enum<Dynamic>):String {
		return e.native().getName();
	}

	public static function resolveClass(name:String):Class<Dynamic> {
		return try {
			java.lang.Class.forName(name);
		} catch (e:java.lang.ClassNotFoundException) {
			return null;
		}
	}

	public static function resolveEnum(name:String):Enum<Dynamic> {
		return try {
			var c = java.lang.Class.forName(name);
			if (!isEnumClass(c)) {
				null;
			} else {
				cast c;
			}
		} catch (e:java.lang.ClassNotFoundException) {
			return null;
		}
	}

	static final emptyArg = {
		var a = new java.NativeArray(1);
		a[0] = (null : jvm.EmptyConstructor);
		a;
	}

	static final emptyClass = {
		var a = new java.NativeArray(1);
		a[0] = (cast jvm.EmptyConstructor : java.lang.Class<Dynamic>);
		a;
	}

	public static function createInstance<T>(cl:Class<T>, args:Array<Dynamic>):T {
		var argTypes:java.NativeArray<java.lang.Class<Dynamic>> = new java.NativeArray(args.length);
		var cl = cl.native();
		for (i in 0...args.length) {
			var arg = (cast args[i] : java.lang.Object);
			argTypes[i] = arg.getClass();
			args[i] = arg;
		}
		var methodType = MethodType.methodType(cast Void, argTypes);

		// 1. attempt: direct constructor lookup
		try {
			var ctor = MethodHandles.lookup().findConstructor(cl, methodType);
            return ctor.invokeWithArguments(@:privateAccess args.getNative());
		} catch(_:NoSuchMethodException) { }

		// 2. attempt direct new lookup
		try {
			var ctor = MethodHandles.lookup().findVirtual(cl, "new", methodType);
			var obj = cl.getConstructor(emptyClass).newInstance(emptyArg);
			ctor.bindTo(obj).invokeWithArguments(@:privateAccess args.getNative());
			return obj;
		} catch (_:NoSuchMethodException) { }

		function unify(params:java.NativeArray<java.lang.Class<Dynamic>>) {
			if (params.length != args.length) {
				return false;
			}
			for (i in 0...params.length) {
				if (!Jvm.getWrapperClass(params[i]).isAssignableFrom(argTypes[i])) {
					return false;
				}
			}
			return true;
		}

		// 3. attempt: unify actual constructor
		for (ctor in cl.getDeclaredConstructors()) {
			if (unify(ctor.getParameterTypes())) {
				return MethodHandles.lookup().unreflectConstructor(ctor).invokeWithArguments(@:privateAccess args.getNative());
			}
		}

		// 4. attempt: unify new
		for (ctor in cl.getDeclaredMethods()) {
			if (ctor.getName() != "new") {
				continue;
			}
			if (unify(ctor.getParameterTypes())) {
				return MethodHandles.lookup().unreflect(ctor).invokeWithArguments(@:privateAccess args.getNative());
			}
		}

		return null;
	}

	public static function createEmptyInstance<T>(cl:Class<T>):T {
		var annotation = hackGetAnnotation(cl.native(), (cast ClassReflectionInformation : java.lang.Class<ClassReflectionInformation>));
		if (annotation != null) {
			return cl.native().getConstructor(emptyClass).newInstance(emptyArg);
		} else {
			return cl.native().newInstance();
		}
	}

	public static function createEnum<T>(e:Enum<T>, constr:String, ?params:Array<Dynamic>):T {
		if (params == null || params.length == 0) {
			return Jvm.readField(e, constr);
		} else {
			return Reflect.callMethod(null, Jvm.readField(e, constr), params);
		}
	}

	public static function createEnumIndex<T>(e:Enum<T>, index:Int, ?params:Array<Dynamic>):T {
		var clInfo:java.lang.Class<EnumReflectionInformation> = cast EnumReflectionInformation;
		var annotation = hackGetAnnotation(e.native(), clInfo);
		if (params == null || params.length == 0) {
			return Jvm.readField(e, annotation.constructorNames()[index]);
		} else {
			return Reflect.callMethod(null, Jvm.readField(e, annotation.constructorNames()[index]), params);
		}
	}

	static function getFields<T>(c:java.lang.Class<T>, statics:Bool):Array<String> {
		var ret = [];
		for (f in c.getDeclaredFields()) {
			if (java.lang.reflect.Modifier.isStatic(f.getModifiers()) == statics && !f.isSynthetic()) {
				ret.push(f.getName());
			}
		}
		for (m in c.getDeclaredMethods()) {
			if (java.lang.reflect.Modifier.isStatic(m.getModifiers()) == statics && !m.isSynthetic()) {
				ret.push(m.getName());
			}
		}
		return ret;
	}

	public static function getInstanceFields(c:Class<Dynamic>):Array<String> {
		return getFields(c.native(), false);
	}

	public static function getClassFields(c:Class<Dynamic>):Array<String> {
		return getFields(c.native(), true);
	}

	public static function getEnumConstructs(e:Enum<Dynamic>):Array<String> {
		var clInfo:java.lang.Class<EnumReflectionInformation> = cast EnumReflectionInformation;
		var annotation = hackGetAnnotation(e.native(), clInfo);
		return @:privateAccess Array.ofNative(annotation.constructorNames());
	}

	public static function typeof(v:Dynamic):ValueType {
		// could optimize this with an annotation on Haxe classes
		if (v == null) {
			return TNull;
		}
		if (Jvm.instanceof(v, java.lang.Number)) {
			var v:java.lang.Number = cast v;
			if (v.intValue() == v.doubleValue()) {
				return TInt;
			}
			return TFloat;
		}
		if (Jvm.instanceof(v, java.lang.Boolean.BooleanClass)) {
			return TBool;
		}
		if (Jvm.instanceof(v, jvm.DynamicObject)) {
			return TObject;
		}
		if (Jvm.instanceof(v, java.lang.invoke.MethodHandle)) {
			return TFunction;
		}
		var c = (cast v : java.lang.Object).getClass();
		// TODO: native enums?
		if (isEnumValueClass(c)) {
			return TEnum(cast c.getSuperclass());
		}
		if (Jvm.instanceof(v, java.lang.Class)) {
			return TObject;
		}
		return TClass(c);
	}

	public static function enumEq<T>(a:T, b:T):Bool {
		if (a == null) {
			return b == null;
		}
		if (b == null) {
			return false;
		}
		var a:jvm.Enum = cast a;
		var b:jvm.Enum = cast b;
		if (a._hx_index != b._hx_index) {
			return false;
		}
		var params1 = enumParameters(cast a);
		var params2 = enumParameters(cast b);
		if (params1.length != params2.length) {
			return false;
		}
		for (i in 0...params1.length) {
			if (params1[i] != params2[i]) {
				return false;
			}
		}
		return true;
	}

	public static function enumConstructor(e:EnumValue):String {
		var clInfo:java.lang.Class<EnumReflectionInformation> = cast EnumReflectionInformation;
		var annotation = hackGetAnnotation(getEnum(e).native(), clInfo);
		if (annotation == null) {
			return null;
		}
		return annotation.constructorNames()[(cast e : jvm.Enum)._hx_index];
	}

	public static function enumParameters(e:EnumValue):Array<Dynamic> {
		var clInfo:java.lang.Class<EnumValueReflectionInformation> = cast EnumValueReflectionInformation;
		var annotation = hackGetAnnotation((cast e : java.lang.Object).getClass(), clInfo);
		var ret = [];
		if (annotation == null) {
			return ret;
		}
		for (name in annotation.argumentNames()) {
			ret.push(Jvm.readField(e, name));
		}
		return ret;
	}

	public static function enumIndex(e:EnumValue):Int {
		return (cast e : jvm.Enum)._hx_index;
	}

	public static function allEnums<T>(e:Enum<T>):Array<T> {
		var all = getEnumConstructs(e);
		var ret = [];
		for (name in all) {
			var v = Jvm.readField(e, name);
			if (Jvm.instanceof(v, jvm.Enum)) {
				ret.push(v);
			}
		}
		return ret;
	}
}