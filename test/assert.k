struct Player {
	int size = 100;
	@(size > 10) { print(“Not big enough”); }
}
