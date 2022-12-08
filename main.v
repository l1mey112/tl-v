import os

fn (p Parser) walk_impl(n &AstNode, _dep int) {
	dep := if n.kind != .stmtseq {
		print(` `.repeat(_dep))
		println(n.kind)
		_dep + 1
	} else {
		_dep
	}

	if unsafe { n.n1 != nil } {
		p.walk_impl(n.n1, dep)
	}
	if unsafe { n.n2 != nil } {
		p.walk_impl(n.n2, dep)
	}
	if unsafe { n.n3 != nil } {
		p.walk_impl(n.n3, dep)
	}
}

fn (p Parser) walk(n &AstNode) {
	p.walk_impl(n, 0)
}

fn main() {
	mut l := Lexer{text: os.get_raw_lines_joined()}
	/* for {
		e := l.next()
		println(e)
		if e == .eof {
			break
		}
	} */
	mut p := Parser{l: l}
	p.procs << 'print'
	a := p.parse()
	/* p.walk(a)
	println(p.procs) */

	mut g := Gen{symtable: p.procs}

	g.generate_all(a)
}