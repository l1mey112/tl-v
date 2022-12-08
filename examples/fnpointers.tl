proc give1 { r = 1; }
proc give2 { r = 2; }
proc give3 { r = 3; }
proc give4 { r = 4; }

data fn_pointers {
	give1;
	give2;
	give3;
	give4;
	0;
}

proc main {
	s0 = 0;
	while fn_pointers[s0] != 0 {
		call fn_pointers[s0];
		a0 = r;
		call print;
		s0 = s0 + 1;
	}

	r = 0;
}