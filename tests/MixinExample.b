using System;

namespace K {
	mixin Vehicle {
		private var speed:Int32 = 0;
		private var mileage:Int32 = 0;
		public static new():Vehicle { return ctor(); }
		public speed():Int32 { return speed; }
		public mileage():Int32 { return mileage; }
		public accelerate(delta: Int32) { speed = speed + delta; }
		public decelerate(delta: Int32) { speed = speed - delta; }
	}

	mixin Asset {
		private var value:Int64;
		public static new(value: Int64):Asset {
			val ret = ctor(); 
			ret.value = value;
			return ret;
		}
		public value():Int64 { return value; }
	}

	class Car : Asset {
		mixin vehicle:Vehicle = Vehicle.new()
			forwarding speed():Int32, accelerate(Int32);
		mixin asset:Asset;

		public static new(value: Int64):Car {
			val ret = ctor();
			ret.asset = Asset.new(value);
			return ret;
		}
	}

	class Program {
		public static main() {}
	}
}

/*[[[
]]]*/