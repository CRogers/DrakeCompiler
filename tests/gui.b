namespace GUI {
	interface Control {

	}

	class ControlImpl {
		var visible = true;
		var focused = false;

		public static new(size:Vector2):ControlImpl {
			return ControlImpl();
		}

		public isFocused() {
			return focused;
		}

		public focus() {
			focused = true;
		}

		public unfocus() {
			focused = false;
		}
	}

	class ContainerImpl {
		var ci = ControlImpl.new()
			forwarding focus, unfocus, isFocused;

	}

}