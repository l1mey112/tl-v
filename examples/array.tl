data my_data {
	1; 2; 3; 4;
}

proc main {
	
	s0 = 0;
	s1 = 0;
	while s0 < 4 {
		s1 = s1 + my_data[s0];
		s0 = s0 + 1;
	}

	a0 = s1;
	call print;
	
	r = 0;
}