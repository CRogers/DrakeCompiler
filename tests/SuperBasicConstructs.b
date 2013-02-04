using System;

namespace Foo {
	class Kitten {
		var age: Int = 3;

		agePlus(a: Int) {
			return age + a;
		}
	}

	interface Cat {
		agePlusOne(a: Int);
	}
}

namespace System {
	class Int {
	}
}