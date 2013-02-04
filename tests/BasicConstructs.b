using System;

namespace Foo {
	class Kitten {
		public var age: Int = 3;

		public agePlus(a: Int) {
			return age + 3;
		}
	}

	interface Cat {
		agePlusOne(a: Int);
	}
}