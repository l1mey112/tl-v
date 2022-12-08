enum AstKind {
	s_proc
	stmtseq
	s_if s_while empty proc_call
	expr
		add sub mul div ident dec assign
}

type AstValue = u64 | string

struct AstNode {
mut:
	kind AstKind
	value AstValue
	n1 &AstNode = unsafe { nil }
	n2 &AstNode = unsafe { nil }
	n3 &AstNode = unsafe { nil }
}

struct Parser {
mut:
	l Lexer
	procs []string
}

fn (mut p Parser) expr_paren() &AstNode {
	if p.l.curr() != .opar || p.l.curr() == .eof {
		panic("syntax error in expression")
	}
	p.l.next()
	mut n := p.expr()
	if p.l.curr() != .cpar || p.l.curr() == .eof {
		panic("syntax error in expression")
	}
	p.l.next()
	return n
}

// PRECEDENCE ident
fn (mut p Parser) term() &AstNode {
	match p.l.curr() {
		.ident {
			p.l.next()
			return &AstNode{kind: .ident, value: p.l.str_data}
		}
		.dec {
			p.l.next()
			return &AstNode{kind: .dec, value: p.l.int_data}
		}
		else {
			return p.expr_paren()
		}
	}
}

// PRECEDENCE '*' '/'
fn (mut p Parser) expr2() &AstNode {
	mut n := p.term()
	for p.l.curr() in [.mul, .div] {
		mut x := &AstNode{}
		x.kind = if p.l.curr() == .mul { .mul } else { .div }
		x.n1 = n
		p.l.next()
		x.n2 = p.term()
		n = x
	}
	return n
}

// PRECEDENCE '+' '-'
fn (mut p Parser) expr1() &AstNode {
	mut n := p.expr2()
	for p.l.curr() in [.add, .sub] {
		mut x := &AstNode{}
		x.kind = if p.l.curr() == .add { .add } else { .sub }
		x.n1 = n
		p.l.next()
		x.n2 = p.expr2()
		n = x
	}
	return n
}

// PRECEDENCE '='
fn (mut p Parser) expr() &AstNode {
	if p.l.curr() != .ident {
		return p.expr1()
	}
	mut n := p.expr1()
	if n.kind == .ident && p.l.curr() == .eq {
		mut x := &AstNode{kind: .assign}
		x.n1 = n
		p.l.next()
		x.n2 = p.expr()
		n = x
	}
	return n
}

fn (mut p Parser) stmt() &AstNode {
	mut n := &AstNode(0)

	tok := p.l.curr()
	match tok {
		.s_if {
			n = &AstNode{kind: .s_if}
			p.l.next()
			n.n1 = p.expr()
			if p.l.curr() == .obr {
				n.n2 = p.stmt()
				if p.l.curr() == .s_else {
					if p.l.next() == .obr {
						n.n3 = p.stmt()
					} else {
						panic("expected opening brace to begin else case")
					}
				}
				return n
			}
			panic("expected opening brace to begin if statement")
		}
		.s_while {
			n = &AstNode{kind: .s_while}
			p.l.next()
			n.n1 = p.expr()
			if p.l.curr() == .obr {
				n.n2 = p.stmt()
				return n
			}
			panic("expected opening brace to begin while loop")
		}
		.s_call {
			n = &AstNode{kind: .proc_call}
			if !p.l.expect(.ident) {
				panic("expected name of procedure to call")
			}
			n.n1 = &AstNode{kind: .ident, value: p.l.str_data}
			p.l.next()
			if p.l.curr() == .semi {
				p.l.next()
			} else {
				panic("expected semicolon to complete procedure call")
			}
		}
		.obr {
			if p.l.next() != .cbr {
				n = &AstNode{kind: .stmtseq}
				for {
					mut x := &AstNode{kind: .stmtseq}
					x.n1 = n
					x.n2 = p.stmt()
					n = x
					if p.l.curr() == .cbr {
						break
					}
				}
				p.l.next()
				return n
			} else {
				p.l.next()
				return &AstNode{kind: .empty}
			}
		}
		.semi {
			p.l.next()
			n = &AstNode{kind: .empty}
		}
		.eof {
			panic("unexpected eof")
		}
		else {
			n = &AstNode{kind: .expr}
			n.n1 = p.expr()
			if p.l.curr() == .semi {
				p.l.next()
			} else {
				panic("expected semicolon to complete expression")
			}
		}
	}
	return n
}

fn (mut p Parser) proc() &AstNode {
	if p.l.curr() != .s_proc || p.l.curr() == .eof {
		panic("procedures may only be expressed at the top level")
	}
	if !p.l.expect(.ident) {
		panic("expected name of procedure")
	}
	if p.l.str_data in p.procs {
		panic("duplicate function name ${p.l.str_data}")
	}
	mut n := &AstNode{kind: .s_proc, value: u64(p.procs.len)}
	p.procs << p.l.str_data
	if !p.l.expect(.obr) {
		panic("expected open bracket to begin procedure")
	}
	n.n1 = p.stmt()
	return n
}

fn (mut p Parser) parse() &AstNode {
	p.l.next()
	mut n := &AstNode{kind: .stmtseq}
	if p.l.curr() != .eof {
		for {
			mut x := &AstNode{kind: .stmtseq}
			x.n1 = n
			x.n2 = p.proc()
			n = x
			if p.l.curr() == .eof {
				break
			}
		}
	}
	return n
}