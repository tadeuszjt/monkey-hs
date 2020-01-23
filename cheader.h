#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

#define access(a, t) ((t*)(a).ptr)

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
	Type  type;
} Array;


typedef struct {
	union {
		Ord   asOrd;
		Array asArray;
	};
	Type type;
} Any;


Array array(void *ptr, int len, Type type) {
	Array a = {ptr, len, type};
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

Any arrayToAny(Array a) {
	Any any;
	any.asArray = a;
	any.type = TArray;
	return any;
}

Array anyToArray(Any a) {
	assert(a.type == TArray);
	return a.asArray;
}

Any accessAny(Any a, int i) {
	assert(a.type == TArray);
	Array array = a.asArray;
	assert(i >= 0 && i < array.len);
	switch (array.type) {
	case TInt:
		return ordToAny(intToOrd(((int*)array.ptr)[i]));
		break;
	case TBool:
		return ordToAny(boolToOrd(((bool*)array.ptr)[i]));
		break;
	case TString:
		return ordToAny(stringToOrd(((char**)array.ptr)[i]));
		break;
	case TOrd:
		return ordToAny(((Ord*)array.ptr)[i]);
		break;
	case TArray:
		return arrayToAny(((Array*)array.ptr)[i]);
		break;
	case TAny:
		return ((Any*)array.ptr)[i];
		break;
	default:
		assert(false);
		break;
	}
}


void printOrd(Ord ord);
void printArray(Array array);
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


void printArray(Array array) {
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
			printf("%s%s", ((char**)array.ptr)[i], i < array.len - 1 ? ", " : "");
		break;

	case TOrd:
		for (int i = 0; i < array.len; i++) {
			printOrd(((Ord*)array.ptr)[i]);
			if (i < array.len - 1) fputs(", ", stdout);
		}
		break;

	case TArray:
		for (int i = 0; i < array.len; i++) {
			printArray(((Array*)array.ptr)[i]);
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
		printArray(any.asArray);
		break;
	default:
		assert(false);
	}
}
