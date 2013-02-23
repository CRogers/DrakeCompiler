using System;

namespace Foo {
	class Kitten {
		pub age: Int32;

		pub ctor new() {
			age = 4;
		}

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