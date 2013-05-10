using System;

namespace K {
	class IntPair(a: Int32, b: Int32) {
		public getA():Int32 { return a; }
		public getB():Int32 { return b; }
	}

	class Pair`[A,B](a: A, b: B) {
		public getA():A { return a; }
		public getB():B { return b; }
	}

	class Program {
		static printBool(b: Bool) {
			if (b) Console.println(1);
			else Console.println(0);
		}

		public static main() {
			var ip = IntPair.new(15,16);
			Console.println(ip.getA());
			Console.println(ip.getB());

			var bp = Pair`[Bool,Bool].new(true,false);
			printBool(bp.getA());
			printBool(bp.getB());
		}
	}
}

/*[[[
15
16
1
0

]]]*/