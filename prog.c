
#include <cheader.h>

int v0() {
	int v1[] = {
		1,
		2,
		3,
	};
	char* v2[] = {
		"benis",
		"bongis",
	};
	bool v3[] = {
		true,
		false,
	};
	Any v4[] = {
		arrayToAny(array(v1, 3, TInt)),
		arrayToAny(array(v2, 2, TString)),
	};
	int v5[] = {
		67,
		68,
	};
	Any v6[] = {
		arrayToAny(array(v3, 2, TBool)),
		arrayToAny(array(v5, 2, TInt)),
		arrayToAny(array(v4, 2, TAny)),
	};
	Any v7[] = {
		arrayToAny(array(v1, 3, TInt)),
		arrayToAny(array(v2, 2, TString)),
		arrayToAny(array(v3, 2, TBool)),
		arrayToAny(array(v4, 2, TAny)),
		arrayToAny(array(v6, 3, TAny)),
	};
	putchar('[');
	for (int i = 0; i < 5; i++) {
		printAny(v7[i]);
		if (i < 4) fputs(", ", stdout);
	}
	putchar(']');
	putchar('\n');
}

int main() {
	v0();
	return 0;
}
