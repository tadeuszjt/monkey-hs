#include "cheader.h"

void v0() {
	int v0 = 0;
	Any v1 = ordToAny(boolToOrd(true));
	label2:;
	if (v0 < 10) goto label4;
	goto label3;
	label4:;
	v0 = v0 + 1;
	int v5[] = {
		v0, 
		v0, 
	};
	v1 = arrayToAny(array(v5, 2, TInt));
	printf("%d", v0);
	fputs(", ", stdout);
	printAny(v1);
	putchar('\n');
	goto label2;
	label3:;
}

int main() { v0(); return 0; }
