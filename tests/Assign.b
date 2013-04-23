using System;

namespace K {
	class Foo {
		public var i:Int32;

		public static new(i: Int32):Foo {
			var ret = ctor();
			ret.i = i;
			return ret;
		}
	}

	class Program {
		public static main() {
			Console.println(Foo.new(10).i);
		}
	}
}

/*<<<
10

>>>*/