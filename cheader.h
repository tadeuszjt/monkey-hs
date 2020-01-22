#include <stdio.h>
#include <stdbool.h>
#include <assert.h>


typedef enum {
	TInt,
	TBool,
	TString,
	TOrd,
	TArray,
	TAny,
} Type;


typedef struct {
	union {
		int asInt;
		bool asBool;
		char* asString;
	};
	Type type;
} Ord;


typedef struct {
	void* ptr;
	int   len;
} Array;


typedef struct {
	void* ptr;
	int   len;
	Type  type;
} TypedArray;


typedef struct {
	union {
		Ord        asOrd;
		TypedArray asArray;
	};
	Type type;
} Any;


Array arr(void *ptr, int len) {
	Array a = {ptr, len};
	return a;
}

TypedArray tarr(void *ptr, int len, Type type) {
	TypedArray a = {ptr, len, type};
	return a;
}

int ordToInt(Ord ord) {
	assert(ord.type == TInt);
	return ord.asInt;
}

bool ordToBool(Ord ord) {
	assert(ord.type == TBool);
	return ord.asBool;
}

char* ordToString(Ord ord) {
	assert(ord.type == TString);
	return ord.asString;
}

Ord intToOrd(int i) {
	Ord o = {i, TInt};
	return o;
}

Ord boolToOrd(bool b) {
	Ord o = {b, TBool};
	return o;
}

Ord stringToOrd(char *s) {
	Ord o;
	o.asString = s;
	o.type = TString;
	return o;
}

Any ordToAny(Ord o) {
	Any a;
	a.asOrd = o;
	a.type = TOrd;
	return a;
}

Any tarrToAny(TypedArray a) {
	Any any;
	any.asArray = a;
	any.type = TArray;
	return any;
}


void printOrd(Ord ord);
void printTypedArray(TypedArray array);
void printAny(Any any);


void printOrd(Ord ord) {
	switch (ord.type) {
	case TInt:
		printf("%d", ord.asInt);
		break;
	case TBool:
		fputs(ord.asBool ? "true" : "false", stdout);
		break;
	case TString:
		fputs(ord.asString, stdout);
		break;
	}
}


void printTypedArray(TypedArray array) {
	putchar('[');
	switch (array.type) {

	case TInt:
		for (int i = 0; i < array.len; i++)
			printf("%d%s", ((int*)array.ptr)[i], i < array.len - 1 ? ", " : "");
		break;

	case TBool:
		for (int i = 0; i < array.len; i++)
			printf(
				"%s%s",
				((bool*)array.ptr)[i] ? "true" : "false",
				i < array.len - 1 ? ", " : ""
			);
		break;

	case TString:
		for (int i = 0; i < array.len; i++)
			printf("%s%s", ((char*)array.ptr)[i], i < array.len - 1 ? ", " : "");
		break;

	case TOrd:
		for (int i = 0; i < array.len; i++) {
			printOrd(((Ord*)array.ptr)[i]);
			if (i < array.len - 1) fputs(", ", stdout);
		}
		break;

	case TArray:
		for (int i = 0; i < array.len; i++) {
			printTypedArray(((TypedArray*)array.ptr)[i]);
			if (i < array.len - 1) fputs(", ", stdout);
		}
		break;

	case TAny:
		for (int i = 0; i < array.len; i++) {
			printAny(((Any*)array.ptr)[i]);
			if (i < array.len - 1) fputs(", ", stdout);
		}
		break;

	default:
		assert(false);
	}
	putchar(']');
}


void printAny(Any any) {
	switch (any.type) {
	case TOrd:
		printOrd(any.asOrd);
		break;
	case TArray:
		printTypedArray(any.asArray);
		break;
	default:
		assert(false);
	}
}
