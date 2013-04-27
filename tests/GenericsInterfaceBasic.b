using System;

namespace G {

    interface Foo`[T] {
    }

    class Bar`[T] : Foo`[T] {
    	public static new() : Bar`[T] {
    		return ctor();
    	}
    }

	class Program {
		public static main() {
			Bar`[Int32].new();
		}
	}
}


/*<<<
3
10

>>>*/