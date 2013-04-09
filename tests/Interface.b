using System;

namespace I {
	interface Strokable {
		stroke(strength: Int32):Int32;
	}

	class Kitten : Strokable {
		public stroke(strength: Int32):Int32 {
			return strength * 2;
		}

		public static new():Kitten {
			return Kitten();
		}
	}

	class Cat : Strokable {
		public stroke(strength: Int32):Int32 {
			return strength * 3;
		}

		public static new():Cat {
			return Cat();
		}
	}

	class Program {
		static stroke(s: Strokable) {
			Console.println(s.stroke(10));
		}

		static stroke(s: Int32) {
			// This should never be called
			Console.println(99999);
		}

		public static main() {
			stroke(Kitten.new());
			stroke(Cat.new());
		}
	}
}

/*<<<
20
30

>>>*/