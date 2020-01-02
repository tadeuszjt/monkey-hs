i := 0;
while (i < 20) {
	if (i % 2 == 0) {
		print(i, 2);
	} else if (i % 3 == 0) {
		print(i, 3);
	} else if (i % 4 == 0) {
		print(i, 4);
	} else {
		print("Nothing", i / 2 * 4 + 3);
	}

	i = i + 1;
}


