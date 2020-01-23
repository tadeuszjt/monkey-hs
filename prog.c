
#include <cheader.h>

int v0() {
	int v1[] = {
		1,
		2,
		3,
	};
	bool v2[] = {
		true,
		false,
	};
	int v3[] = {
		2,
		3,
		4,
	};
	memcpy(v1, v3, sizeof(v1));
	Any v4[] = {
		arrayToAny(array(v1, 3, TInt)),
		arrayToAny(array(v2, 2, TBool)),
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
