REJECT
int foo(){
	return 0;
}

int foo(){
	return 1;
}

int main(){
	int a = foo();
	return a;
}