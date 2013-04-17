using System;

namespace T {
	public class ThisTest {
		public var x: Int32 = 3;

		public id():ThisTest {
			return this;
		}

		public static main() {
			var tt = ctor();
			Console.println(tt.id().x);
		}
	}
}

/*<<<
3

>>>*/