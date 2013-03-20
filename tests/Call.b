using System;

namespace T {
	class Cat {

		meow() {
			Console.println(0);
			return;
		}

        callMeow() {
        	Console.println(1);
            meow();
            Console.println(2);
            return;
        }

		static purr() {
			Console.println(3);
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
			Console.println(4);
			return;
		}

		public static woof() {
			Console.println(5);
			return;
		}

		public static new():Dog {
			Console.println(6);
			return Dog();
		}
	}
}

/*<<<
3
1
0
2
0
5
6
4
6
4

>>>*/