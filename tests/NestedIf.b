using System;

namespace K {
	class T {

		static test1() {
			if (true) {
				if (true) {
					Console.println(0);
				}
			}
			return;
		}

		static test2() {
			if (true)
				if (true)
					Console.println(10);
			return;
		}

		static test3() {
			if (true)
				if (true)
					if (false)
						Console.println(20);
					else
						Console.println(21);
			return;
		}

		public static main() {
			test1();
			test2();
			test3();
			return;
		}
	}
}

/*<<<
0
10
21

>>>*/