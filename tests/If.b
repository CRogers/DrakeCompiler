using System;

namespace T {
	class IfTest {
		static oneReturnTest(a: Bool):Int32 {
			if (a)
				return 0;
			else
				var c = 0;
			return 1;
		}

		static twoReturnTest(a: Bool):Int32 {
			if (a)
				return 0;
			else {
				return 1;
			}
		}

		static twoReturnVoidTest(a: Bool) {
			if (a)
				return;
			else
				return;
		}
	}
}