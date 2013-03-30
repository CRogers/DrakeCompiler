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

		public static main() {
			stroke(Kitten.new());
			stroke(Cat.new());
		}
	}
/*
	implicit interface Object on () {
		equals(x: Object):Bool default {
			return false;
		}

		hashCode():Int default {
			// Behind the scenes magic - memory address? Hashcodes of things below?
			return 0;
		}

		toString():String default {
			// Behind the scenes magic 
			return "";
		}
	}*/
}