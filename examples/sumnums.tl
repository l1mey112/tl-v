proc sumnums {
	s0 = 0;
	s1 = 1;
	while s1 <= a0 {
		s0 = s0 + s1;
		s1 = s1 + 1;
	}
	r = s0;
}

proc main {
	a0 = 10;
	call sumnums;
	a0 = r;
	call print;

	r = 0;
}