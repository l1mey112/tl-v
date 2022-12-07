proc addup {
	r = a0 + a1;
}

proc main {
	a0 = 22;
	a1 = 22;

	call addup;

	r = 0;
}