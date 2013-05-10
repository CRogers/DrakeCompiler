using System;

namespace K {
	interface Option`[T] {
		has():Bool;
		get():T;
	}

	class None`[T] : Option`[T] {
		public has():Bool { return false; }
		public get():T { return 1/0; }

		public static new():None`[T] { return ctor(); }
	}

	class Some`[T] : Option`[T] {
		private var value:T;

		public has():Bool { return true; }
		public get():T { return value; }

		public static new(value: T):Some`[T] {
			var some = ctor();
			some.value = value;
			return some;
		}
	}

	class Program {
		static tryPrint`[T](o: Option`[T]) {
			if (o.has())
				Console.println(o.get());
		}

		public static main() {
			var some = Some`[Int32].new(3);
			var none = None`[Int32].new();

			tryPrint`[Int32](some);
			tryPrint`[Int32](none);
		}
	}
}

/*[[[
3

]]]*/