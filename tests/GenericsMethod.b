using System;

namespace G {
	class Box`[T] {
		private var item: T;

		public static new(item: T):Box`[T] {
			var b = ctor();
			b.item = item;
			return b;
		}

		public id`[A](a: A):T {
			return item;
		}
	}

	class Program {
		public static main() {
			var b32 = Box`[Int32].new(3);
			Console.println(b32.get`[Bool](true));

			var bbool = Box`[Bool].new(true);
			if (bbool.get`[Int8](1B)) {
				Console.println(10);
			}
		}
	}
}


/*<<<
3
10

>>>*/