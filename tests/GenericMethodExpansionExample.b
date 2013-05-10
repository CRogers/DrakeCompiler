using System;

namespace G {
	class Program {
		public static id`[A](a: A, b: Int32):A { return a; }
		public static id`[A](a: A, b: Bool):A  { return a; }
		public static id`[A,B](a: A, a:B):A    { return a; }

		public static main() {
			var i = Program.id`[Int32](3,true);
			Console.println(i);
		}
	}
}


/*[[[
3

]]]*/