
#include <cheader.h>

int v0() {
	int v1[] = {
		1,
		2,
		3,
	};
	int v2[] = {
		1,
		2,
		3,
	};
	int v3[] = {
		1,
		2,
		3,
	};
	Any v4[] = {
		arrayToAny(array(v1, 3, TInt)),
		arrayToAny(array(v2, 3, TInt)),
		ordToAny(boolToOrd(true)),
	};
	putchar('[');
	for (int i = 0; i < 3; i++) {
		printAny(v4[i]);
		if (i < 2) fputs(", ", stdout);
	}
	putchar(']');
	putchar('\n');
}

int main() {
	v0();
	return 0;
}
