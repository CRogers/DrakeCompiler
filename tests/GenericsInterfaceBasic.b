using System;

namespace G {

    interface Foo`[T] {
        id(t: T): T;
    }

    class Bar`[T] : Foo`[T] {
        public static new() : Bar`[T] {
            return ctor();
        }

        public id(t: T):T { return t; }
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