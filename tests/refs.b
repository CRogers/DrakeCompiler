using System.Foo;

namespace System {
	public class Cat {
		public var age;
		public meow();
		public static m();
	}

	public class Kitten {
		public var boo = 3;

		public foo(a: Cat) {
			// ref to local var
			boo;
			// ref to local function
			foo(a);
			// ref to parameter
			a.meow();
			a.age;
			// ref to class in same namespace
			Cat.m();
			// global ref to class/interface in same namespace
			System::Cat.m();
			// global ref to class/interface in other namespace
			Bar::Baz.quux();
			// using ref to interface in other namespace
		}
	}
}

namespace Bar {
	public class Baz {
		public static quux();
	}
}

namespace System.Foo {
	public interface A {
		a();
	}
}