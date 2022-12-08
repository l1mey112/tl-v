proc fib_recursive {
	if a0 <= 1 {
		r = a0;
		return;
	}

	s0 = a0;

	a0 = s0 - 1;
	call fib_recursive;
	s1 = r;
	a0 = s0 - 2;
	call fib_recursive;
	r = r + s1;
}

proc fib_iterative {
	if a0 == 0 {
		r = a0;
		return;
	}

	s0 = 2; // index

	s1 = 0; // a
	s2 = 1; // b
	
	while s0 <= a0 {
		s3 = s1 + s2;
		s1 = s2;
		s2 = s3;		

		s0 = s0 + 1;
	}

	r = s2;
}

proc main {
	a0 = 9;
	call fib_recursive;
	a0 = r;
	call print;

	a0 = 9;
	call fib_iterative;
	a0 = r;
	call print;

	r = 0;
}