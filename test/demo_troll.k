/* Demo Program trololol */


struct Groups {
	int members = 4;
	struct Student[members] member_name;
	bool finished_PLT;
}

struct Teacher {
	bool cool;
	int stress_level;
	str subject;
}

struct Student {
	int stress_level;
	int grade;
	str nickname;
	@ (grade > 2.0) { print "You shall not pass!" }
}

struct Class() {
	str level;
	str professor;
	int credits;
}

void main() {
	struct Teacher Edwards = {True, -100, "PLT"};
	struct Class PLT = {"hard", Edwards.subject, 4};
	struct Groups kAWK;
	struct Student Albert = {-2, 4.02, "walbert"}
	kAWK.member_name[0] = Albert;
	kAWK.member_name[1] = struct Student Karen = {3, 20.0, "nan bread"};
	kAWK.member_3 = struct Student Michael = {5, 3.8, "mocha"};
	kAWK.member_4 = struct Student Mei-Vern = {10000, 1.0, "mv"};
	kAWK.finished_PLT = false;
} 



