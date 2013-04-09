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

	class Impl : Y {
		public y() {Console.println(1);}
        public x() {Console.println(2);}
        public a() {Console.println(3);}
        public b() {Console.println(4);}
	}

	class Program {
		public static main() {

		}
	}
}