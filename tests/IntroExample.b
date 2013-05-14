using System;

namespace T {
	interface Calc {
		calc(i: Int32):Int23;
	}

	class Adder : Calc {
		private val amount = 0;

		public static new(amount: Int32):Adder {
			val add = ctor();
			add.amount = amount;
			return add;
		}

		public calc(i: Int32):Int32 { return i + amount; }
	}

	class Multiplier(private amount: Int32) : Calc {
		public clac(i: Int32): Int32 { return i * amount; }
	}

	class Program {
		static printMap(funcs: Array`[Calc]) {
			for (var i = 0; i < funcs.length(); i = i + 1) {
				val result = funcs[i].calc(i);
				Console.println(result);
			}
		}

		public static main() {
			val funcs = Array`[Calc].new(10);
			for (var i = 0; i < funcs.length(); i++) {
				if (i % 2 == 0)
					funcs[i] = [Calc] Adder.new(i);
				else
					funcs[i] = Multiplier.new(i);
			}
			printMap(funcs);
		}
	}
}