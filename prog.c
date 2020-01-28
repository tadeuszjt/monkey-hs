#include "cheader.h"

void v0() {
	int v2 = 12;
	Ord v4[] = {
		boolToOrd(true),
		intToOrd(1),
	};
	int v5[] = {
		5,
	};
	Any v6[] = {
		ordToAny(intToOrd(4)),
		arrayToAny(array(v5, 1, TInt)),
		ordToAny(intToOrd(6)),
	};
	printf("%d", v2);
	fputs(", ", stdout);
	printArray(array(v4, 2, TOrd));
	fputs(", ", stdout);
	printArray(array(v6, 3, TAny));
	putchar('\n');
}

int main() { v0(); return 0; }
