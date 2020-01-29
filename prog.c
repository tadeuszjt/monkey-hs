#include "cheader.h"

void v0() {
	int v0 = 12;
	Ord v1[] = {
		boolToOrd(true),
		intToOrd(1),
	};
	Any v2 = arrayToAny(array(v1, 2, TOrd));
	v2 = ordToAny(intToOrd(v0));
	int v3[] = {
		5,
	};
	Any v4[] = {
		ordToAny(intToOrd(3)),
		ordToAny(intToOrd(4)),
		arrayToAny(array(v3, 1, TInt)),
	};
	printAny(v2);
	fputs(", ", stdout);
	printArray(array(v4, 3, TAny));
	putchar('\n');
}

int main() { v0(); return 0; }
