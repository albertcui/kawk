int run_gcd (int a, int b) {
	while (a != b) {
		if (a > b) {
			a = a-b;
		}
		else {
			b = b-a;
		}
	}
	return a;
}

  void main() {
 	unit:run_gcd(24, 54):equals(6):accept;
 }
