using System;

namespace K {
	interface A {
		a();
	}

	interface B {
		b();
	}

	interface X : A, B {
		x();
	}

	interface Y : X {
		y();
	}
}