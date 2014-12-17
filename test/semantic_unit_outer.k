int foo(int a){
	return a;
}

void main(){
 	int hi = 1;
	unit:foo(hi):equals(1):accept;
	unit:foo(hi):equals(0):reject;
}