#include "cheader.h"

void v0() {
	Ord v0[] = {
		intToOrd(23),
		intToOrd(3),
		boolToOrd(true),
	};
	bool v1[] = {
		true,
		false,
	};
	Any v2[] = {
		ordToAny(intToOrd(12)),
		arrayToAny(array(v0, 3, TOrd)),
		arrayToAny(array(v1, 2, TBool)),
	};
	printArray(array(v2, 3, TAny));
	putchar('\n');
}

int main() { v0(); return 0; }
