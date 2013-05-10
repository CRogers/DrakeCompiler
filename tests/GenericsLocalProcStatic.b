using System;

namespace T {
	class Program {
		static id`[X,Y](x: X, y: Y):X { return x; }

		public static main() {
			if (id`[Bool,Bool](true,false)) Console.println(1);
			else Console.println(2);

			Console.println(id`[Int32,Bool](3,true));
		}
	}
}

/*[[[
1
3

]]]*/