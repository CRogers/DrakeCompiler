using System;

namespace T {
	class Program {
		id`[X,Y](x: X, y: Y):X { return x; }

		public static main() {
			var p = ctor();

			if (p.id`[Bool,Bool](true,false)) Console.println(1);
			else Console.println(2);

			Console.println(p.id`[Int32,Bool](3,true));
		}
	}
}

/*[[[
1
3

]]]*/