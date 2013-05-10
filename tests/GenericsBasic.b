using System;

namespace G {
	class Box`[T] {
		public var item: T;

		public static new(item: T):Box`[T] {
			var b = ctor();
			b.item = item;
			return b;
		}
	}

	class Program {
		public static main() {
			var b32 = Box`[Int32].new(3);
			Console.println(b32.item);
			var bbool = Box`[Bool].new(true);
			if (bbool.item) {
				Console.println(10);
			}
		}
	}
}


/*[[[
3
10

]]]*/