struct gcd {
  	int a; 
  	int b;
  	int result;
 	@(!(a > b)) { a=a-b; }
 	@(!(a < b)) { b=b-a; }
 	@(!(a = a)) { print("gcd is"); print(a); result=a; }
 }

 int run_gcd (int a, int b) {
 	struct gcd gg = { a, b };
 	return gg.result;
 }

  void main() {
 	unit:run_gcd(24, 54):equals(6):accept;
 }