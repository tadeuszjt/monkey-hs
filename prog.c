#include "cheader.h"

void v0() {
	Ord v0 = intToOrd(0);
	for (;;) {
		if (!(ordToInt(v0) < 10)) {
			break;
		}
		printOrd(v0);
		putchar('\n');
		char v1[] = {
			'b', 
			'e', 
			'n', 
			'i', 
			's', 
		};
		printArray(array(v1, 5, TChar));
		putchar('\n');
		v0 = intToOrd(ordToInt(v0) + 1);
	}
	v0 = boolToOrd(true);
}

int main() { v0(); return 0; }
