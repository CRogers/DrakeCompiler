using System;

namespace T {
	class Program {
		static fac(n: Int32): Int32 {
			Console.println(n);
			if (n <= 1)
				return 1;
			return n * fac(n - 1);
		}

		public static main() {
			Console.println(fac(6));
		}
	}
}

/*[[[
6
5
4
3
2
1
720

]]]*/