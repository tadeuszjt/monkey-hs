i := 0;
while i < 10 {
	if i % 2 == 0 {
		print(i, 2);
	} else if i % 3 == 0 {
		print(i, 3);
	} else if i % 4 == 0 {
		print(i, 4);
	} else {
		print("benis");
	}
	i = i + 1;
}

f := fn() {
	print("fn");
};

f();
