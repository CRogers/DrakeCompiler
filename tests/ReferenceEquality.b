using System;

namespace T {
	class Foo() {}

	class Program {
		public static main() {
			val a = Foo.new();
			val b = Foo.new();

			if (a == a)
				Console.println(1);

			if (a == b)
				Console.println(-1);

			if (b == a)
				Console.println(-2);

			if (b == b)
				Console.println(2);
		}
	}
}

/*[[[
1
2

]]]*/