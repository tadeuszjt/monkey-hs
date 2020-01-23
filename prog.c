
#include <cheader.h>

int v0() {
	int v1[] = {
		1,
		2,
		3,
	};
	Ord v2[] = {
		{5, TInt},
		{true, TBool},
	};
	Any v3[] = {
		ordToAny(intToOrd(4)),
		arrayToAny(array(v2, 2, TOrd)),
		ordToAny(intToOrd(6)),
	};
	Any v4[] = {
		arrayToAny(array(v1, 3, TInt)),
		arrayToAny(array(v3, 3, TAny)),
	};
	printAny(accessAny(accessAny(v4[1], 1), 0));
	putchar('\n');
}

int main() {
	v0();
	return 0;
}
