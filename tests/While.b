using System;

namespace T {
	class Program {
		public static main() {
			var n = 1;
			var i = 6;
			while (i > 1) {
				n = n * i;
				i = i - 1;
				Console.println(n);
			}
			Console.println(n);
		}
	}
}

/*[[[
6
30
120
360
720
720

]]]*/