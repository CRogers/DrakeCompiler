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

        public static new():Impl { return Impl(); }
	}

	class Program {
		public static main() {
			var i = Impl.new();
			i.y();
			i.x();
			i.a();
			i.b();
		}
	}
}

/*<<<
1
2
3
4

>>>*/