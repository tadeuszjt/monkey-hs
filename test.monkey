let f = fn() {
	let a = [1, 2, true, 4, false, 7];
	let i = 0;

	while i < 6 {
		print(i, a[i]);
		i = i + 1;
	}

	return 0;
};

f();

