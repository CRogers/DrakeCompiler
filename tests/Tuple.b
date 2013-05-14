using System;

namespace N {
	class Tuple2`[A,B](item1: A, item2: B) {
		public getItem1():A { return item1; }
		public getItem2():B { return item2; }
	}

	class Program {
		public static main() {
			var tIB = Tuple2`[Int32,Bool].new(10,true);
			var t = Tuple2`[Int8,Tuple2`[Int32,Bool]].new(20B, tIB);
			Console.println(4);
		}
	}
}

/*[[[
4

]]]*/