using System;

namespace K {
	interface Bar {}
	class BarImpl() : Bar {}

	interface Foo {}
	class FooImpl() : Foo {}

	class Test {
		static test(b: Bar, x: Foo) {} // 1
		static test(b: Bar, x: FooImpl) {} // 2
		static test(b: BarImpl, x: Foo) {} // 3

		public static main() {
			var bi = BarImpl.new();
			var b  = [Bar] bi;
			var fi = FooImpl.new();
			var f  = [Foo] fi;

			test(b, fi);  // 2
			test(b, f);   // 1 
			test(bi, fi); // Cannot choose between 2 and 3
		}
	}
}

/*[[[
]]]*/