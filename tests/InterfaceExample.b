using System;

namespace T {
	interface Vehicle {
		drive();
		getNumberOfWheels():Int8;
	}

	interface Flying {
		getAltitude():Int32;
	}

	class Car : Vehicle {
		public drive() { Console.println(1); }
		public getNumberOfWheels():Int8 { return 4B; }
		public static new():Car { return ctor(); }
	}

	class Airplane : Vehicle, Flying {
		public drive() { Console.println(2); }
		public getNumberOfWheels():Int8 { return 2B; }
		public getAltitude():Int32 { return 42000; }
	}

	class Program {
		public static main() {
			var v = [Vehicle] Car.new();
			v.drive();
		}
	}
}

/*<<<
1

>>>*/