using System;

namespace T {
	mixin Position {
		private var x: Int32 = 0;
		private var y: Int32 = 0;

		public getX():Int32 { return x; }
		public getY():Int32 { return y; }

		public static new(x: Int32, y: Int32):Position {
			var pos = ctor();
			pos.x = x;
			pos.y = y;
			return pos;
		}

		public moveForward():Position  { y = y + 1; return this; }
		public moveBackward():Position { y = y - 1; return this; }
		public moveRight():Position { x = x + 1; return this; }
		public moveLeft():Position  { x = x - 1; return this; }
	}

	interface Animal : Position {
		getNumberOfLegs():Int32;
	}

	class Cat : Animal {
		mixin pos:Position = Position.new(0,0);

		public getNumberOfLegs():Int32 { return 4; }

		public static new():Cat {
			return ctor();
		}
	}

	class Program {
		static printPos(p: Position) {
			Console.println(p.getX());
			Console.println(p.getY());
		}

		public static main() {
			var c = Cat.new();

			printPos(c);
			printPos(c.moveForward());
			printPos(c.moveRight());
			printPos(c.moveLeft());
			printPos(c.moveBackward());
		}
	}
}
/*

interface Position {
	moveForward():Position;
	moveBackward():Position;
	moveRight():Position;
	moveLeft():Position;
}

class @Mixin@Position(x: Int32, y: Int32) : Position {
	public getX():Int32 { return x; }
	public getY():Int32 { return y; }

	public moveForward():Position  { y = y + 1; return this; }
	public moveBackward():Position { y = y - 1; return this; }
	public moveRight():Position { x = x + 1; return this; }
	public moveLeft():Position  { x = x - 1; return this; }
}

interface Animal : Position {
	getNumberOfLegs():Int32;
}

class Cat() : Animal {
	private var pos:Position = @Mixin@Position.new(0,0);

	public getX():Int32 { return pos.getX(); }
	public getY():Int32 { return pos.getY(); }

	public moveForward():Position  { return pos.moveForward(); }
	public moveBackward():Position { return pos.moveBackward();	}
	public moveRight():Position { return pos.moveRight(); }
	public moveLeft():Postion   { return pos.moveLeft(); }

	public getNumberOfLegs():Int32 { return 4; }
}
*/

/*[[[
0
0
0
1
1
1
0
1
0
0

]]]*/