using System;

namespace G {
	class Foo`[T] {
		private var bar:Bar`[T];

		public static new():Foo`[T] {
			return ctor();
		}
	}

	class Bar`[T] {
		private var foo:Foo`[T];

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