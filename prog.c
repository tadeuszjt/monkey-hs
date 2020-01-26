#include "cheader.h"

void v0() {
	Ord v2 = intToOrd(12);
	bool v3 = false;
	v2 = boolToOrd(true);
	if (ordToBool(v2)) {
	}
	for (;;) {
		if (ordToBool(v2)) {
			break;
		}
		v2 = boolToOrd(false);
	}
}

int main() { v0(); return 0; }
