using System;

namespace T {
	class Bus {
		private var numPassengers:Int32 = 0;
		private var speed:Int32 = 0;

		public static new():Bus { return ctor(); }

		public getNumPassengers():Int32 { return numPassengers; }
		public getSpeed():Int32 { return speed; }

		public addPassengers(num: Int32) { numPassengers = numPassengers + num; }
		public removePassengers(num: Int32) { numPassengers = numPassengers - num; }
	}

	class Program {
		public static main() {
			var b = Bus.new();
			b.addPassengers(10);
			Console.println(b.getNumPassengers());
		}
	}
}

/*<<<
10

>>>*/