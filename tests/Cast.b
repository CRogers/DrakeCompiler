using System;

namespace K {
	interface X {
		x();
	}

	class ImplX : X {
		public static new():ImplX { return ImplX(); }
		public x() { Console.println(3); }
		public y() {}
	} 

	class Program {
		public static test(x:ImplX) {
			Console.println(999);
		}

		public static test(x:X) {
			Console.println(0);
		}

		public static main() {
			var a = (X)ImplX.new();
			a.x();
			test(a);
		}
	}
}

/*<<<
3
0

>>>*/