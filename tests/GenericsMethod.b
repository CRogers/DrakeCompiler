using System;

namespace G {
	class Box`[T] {
		private var item: T;

		public static new(item: T):Box`[T] {
			var b = ctor();
			b.item = item;
			return b;
		}

		public id`[A](a: A, i: Int32):T {
			return item;
		}

		public id`[A](a: A, i: Int8):T {
			return item;
		}

		public id`[A,B](a: A, a:B):T {
			return item;
		}
	}

	class Program {
		public static main() {
			var b = Box`[Bool].new(false);
			b.id`[Int32](10);
		}
	}
}


/*<<<
10

>>>*/