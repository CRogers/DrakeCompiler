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
			var b = Box`[Int32].new(3);
			var bb = Box`[Box`[Int32]].new(b);
			var bbb = Box`[Box`[Box`[Int32]]].new(bb);
			Console.println(bbb.item.item.item);
		}
	}
}


/*<<<
3

>>>*/