using System;

namespace G {

    interface Foo`[T] {
        foo();
    }

    class Bar`[T] : Foo`[T] {
        public static new() : Bar`[T] {
            return ctor();
        }

        public foo(){}
    }

    class Program {
        public static main() {
            Bar`[Int32].new();
            Console.println(99);
        }
    }
}


/*<<<
99

>>>*/