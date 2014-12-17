int main(){
	return 0;
}

int foo(){
	unit:main():equals(0):accept;
}


int main(x,y{
	return x+y;
	unit(3,5):equals(8):accept;
}


int foo(){
	int x = bar();
	int y = 5;
	unit:main(x,y):equals(8):accept;


}

unit:main(bar(), 5);

foo();