using System;

namespace T {
	class IfTest {
		static oneReturnTest(a: Bool):Int32 {
			if (a)
				return 0;
			else
				var c = 0;
			return 1;
		}

		static twoReturnTest(a: Bool):Int32 {
			if (a)
				return 10;
			else {
				return 11;
			}
		}

		static twoReturnVoidTest(a: Bool) {
			if (a)
				return;
			else
				return;
		}

		public static main(){
			Console.println(oneReturnTest(true));
			Console.println(oneReturnTest(false));
			Console.println(twoReturnTest(true));/*FFF
			Console.println(twoReturnTest(false));
			twoReturnVoidTest(true);
			twoReturnVoidTest(false);*/
		}
	}
}

/*<<<
0
1
10
11

>>>*/