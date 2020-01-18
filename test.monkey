let x = 2;

let f = fn(x) {
	if (x > 10) {
		return x * x;
	} else {
		return 1;
	}
};

let y = 2;
print(f(x));

let z = fn(switch) {
	if switch {
		return fn(x) { return x + 1; };
	} else {
		return fn(x) { return x - 1; };
	}
};

print(z(true)(2));
