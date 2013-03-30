using System;

namespace K {
	class T {
		public static main() {
			var a = Array.new(20);
			a[0] = Array.new(1);  // a.indexer(0,6)
			a[10] = Array.new(3); // a.indexer(10,Array.new(3))
			a[10][1] = a[0];      // a.indexer(10).indexer(1, a.indexer(0))
		}
	}
}