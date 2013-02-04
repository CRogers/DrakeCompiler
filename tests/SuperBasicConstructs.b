using System;

namespace Foo {
	class Kitten {
		var age: Int32 = 3;

		agePlus(a: Int32) {
			return age + a;
		}
	}

	interface Cat {
		agePlusOne(a: Int32);
	}
}

namespace System {
	class Int32 {
	}
}