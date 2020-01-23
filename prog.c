
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
	Array v4[] = {
		{v1, 3, TInt},
		{v2, 3, TInt},
		{v3, 3, TInt},
	};
	putchar('[');
	for (int i = 0; i < 3; i++) {
		printf("%d", access(v4[1], int)[i]);
		if (i < 2) fputs(", ", stdout);
	}
	putchar(']');
	putchar('\n');
}

int main() {
	v0();
	return 0;
}
