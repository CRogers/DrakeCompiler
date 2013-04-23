using System;

namespace G {
	class Foo`[T] {
		public static new():Foo`[T] {
			return ctor();
		}
	}

	class Bar`[T] {
		public static new():Bar`[T] {
			return ctor();
		}
	}

	class Program {
		public static main() {
			Foo`[Bar`[Foo`[Bar`[Int32]]]].new();
            Console.println(10);
		}
	}
}

/*<<<
10

>>>*/