 int run_99_bott (int a) {
 	struct bott_beer bb;
 	int i;
 	bb.bottles_of_beer = a;
 	for(i = a; i > 0; (i = (i - 1))) {
 		bb.bottles_of_beer = i;
 	}
 	return 0;
 }

 struct bott_beer {
  	int bottles_of_beer;
 	@(!(bottles_of_beer > 0)) { 
 			print(bottles_of_beer); 
 			print("bottles of beer on the wall. take one down , pass it around");
 			print(bottles_of_beer-1);
 			print("bottles of beer on the wall.");
 	}
 }



 void main() {
 	unit:run_99_bott(99):equals(0):accept;
 }

