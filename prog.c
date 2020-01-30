#include "cheader.h"

void v0() {
	int v0[] = {
		1,
		2,
		3,
	};
	Array v1 = array(v0, 3, TInt);
	int v2[] = {
		3,
		4,
		5,
	};
	v1 = array(v2, 3, TInt);
	Array v3 = v1;
	printArray(v1);
	fputs(", ", stdout);
	printArray(v3);
	putchar('\n');
	int v4[] = {
		1,
		2,
		3,
	};
	int v5[] = {
		4,
		5,
		5,
	};
	int v6[] = {
		6,
		7,
		8,
	};
	Array v7[] = {
		array(v4, 3, TInt),
		array(v5, 3, TInt),
		array(v6, 3, TInt),
	};
	Array v8 = array(v7, 3, TArrayPtr);
	printArray(v8);
	putchar('\n');
}

int main() { v0(); return 0; }
