using System;

namespace K {
	class Foo {
		public static **(a: Int32, b: Int32): Int32 {
			return 3232;
		}

		public static **(a: Int8, b: Int32): Int32 {
			return 832;
		}
	}

	class Bar {
		public static **(a: Int8, b: Int8): Int32 {
			return 88;
		}
	}

	class Program {
		public static main() {
			Console.println(1 ** 2);
			Console.println(1B ** 2);
			Console.println(1B ** 2B);
		}
	}
}

/*<<<
3232
832
88

>>>*/