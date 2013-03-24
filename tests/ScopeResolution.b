using System;

namespace K {
	class Kitten {
		public static meow() {
			Console.println(0);
		}
	}

	class T {
		public static main() {
			Kitten.meow();
			K::Kitten.meow();
			Animals::Kitten.meow();
			Foo::Bar::Baz::Kitten.meow();
			Animals::Cat.meow();
		}
	}
}

namespace Animals {
	class Kitten {
		public static meow() {
			Console.println(1);
		}
	}

	class Cat {
		public static meow() {
			Console.println(2);
		}
	}
}

namespace Foo::Bar::Baz {
	class Kitten {
		public static meow() {
			Console.println(3);
		}
	}
}

/*<<<
0
0
1
3
2

>>>*/