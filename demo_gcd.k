 void main() {
 	unit:run_gcd(24, 54):equals(6):accept;
 }

 struct gcd {
  	int a; 
  	int b;
  	int result;
 	@(!(a > b)) { a=a-b; }
 	@(!(a < b)) { b=b-a; }
 	@(!(a = a)) { print("gcd is"); print(a); result=a; }
 }

 int run_gcd (a, b) {
 	struct gcd gg = { a, b };
 	//gg.a = a;
 	return gg.result;
 }