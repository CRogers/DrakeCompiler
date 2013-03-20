using System;

namespace T {
	class Cat {

		meow() {
			return;
		}

        callMeow() {
            meow();
            return;
        }

		static purr() {
			return;
		}

		public static main() {			
			purr();             // purr :: PFunc ([], UserType Unit)
            Cat().callMeow();
			Cat().meow();       // Cat :: PFunc ([], UserType Cat), Cat() :: UserType Cat, Cat().meow :: PFunc ([], UserType Unit)
			Dog.woof();         // Dog :: StaticType Dog, Dog.woof :: PFunc ([], UserType Unit)
			Dog.new().bark();   // Dog :: StaticType Dog, Dog.new :: PFunc ([], UserType Dog)
			                    // Dog.new() :: UserType Dog, Dog.new().bark :: PFunc ([], UserType Unit)
			var d = Dog.new();
			d.bark();           // d :: UserType Dog, d.bark :: PFunc ([], UserType Unit)
            return;
		}
	}

	class Dog {
		public bark() {
			return;
		}

		public static woof() {
			return;
		}

		public static new():Dog {
			return Dog();
		}
	}
}