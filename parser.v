enum AstKind {
	s_proc
	s_data
	stmtseq
	s_if s_while s_return empty proc_call
	expr
		add sub mul div ident dec assign index
		gt gte lt lte eq neq
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

// LEFT PRECEDENCE '[]'
fn (mut p Parser) expr4() &AstNode {
	mut n := p.term()
	for p.l.curr() in [.osb] {
		mut x := &AstNode{kind: .index}
		x.n1 = n
		p.l.next()
		x.n2 = p.expr()
		n = x
		if p.l.curr() != .csb {
			panic("use ']' to close index expression")
		}
		p.l.next()
	}
	return n
}

// LEFT PRECEDENCE '*' '/'
fn (mut p Parser) expr3() &AstNode {
	mut n := p.expr4()
	for p.l.curr() in [.mul, .div] {
		mut x := &AstNode{}
		x.kind = if p.l.curr() == .mul { .mul } else { .div }
		x.n1 = n
		p.l.next()
		x.n2 = p.expr4()
		n = x
	}
	return n
}

// LEFT PRECEDENCE '+' '-'
fn (mut p Parser) expr2() &AstNode {
	mut n := p.expr3()
	for p.l.curr() in [.add, .sub] {
		mut x := &AstNode{}
		x.kind = if p.l.curr() == .add { .add } else { .sub }
		x.n1 = n
		p.l.next()
		x.n2 = p.expr3()
		n = x
	}
	return n
}

// LEFT PRECEDENCE '>' '>=' '<' '<=' '==' '!='
fn (mut p Parser) expr1() &AstNode {
	mut n := p.expr2()
	for p.l.curr() in [.gt, .gte, .lt, .lte, .eq, .neq] {
		mut x := &AstNode{}
		x.kind = match p.l.curr() {
			.gt  { .gt  }
			.gte { .gte }
			.lt  { .lt  }
			.lte { .lte }
			.eq  { .eq  }
			.neq { .neq }
			else {
				panic("unreachable")
			}
		}
		x.n1 = n
		p.l.next()
		x.n2 = p.expr2()
		n = x
	}
	return n
}

// RIGHT PRECEDENCE '='
fn (mut p Parser) expr() &AstNode {
	if p.l.curr() != .ident {
		return p.expr1()
	}
	mut n := p.expr1()
	if n.kind == .ident && p.l.curr() == .assign {
		mut x := &AstNode{kind: .assign}
		x.n1 = n
		p.l.next()
		x.n2 = p.expr()
		n = x
	}
	return n
}

fn (mut p Parser) stmtseq(is_expr bool) &AstNode {
	mut n := &AstNode(0)
	
	if p.l.next() != .cbr {
		n = &AstNode{kind: .stmtseq}
		for {
			mut x := &AstNode{kind: .stmtseq}
			x.n1 = n
			if is_expr {
				x.n2 = p.stmtexpr()
			} else {
				x.n2 = p.stmt()
			}
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

fn (mut p Parser) stmtexpr() &AstNode {
	mut n := &AstNode{kind: .expr}
	n.n1 = p.expr()
	if p.l.curr() == .semi {
		p.l.next()
	} else {
		panic("expected semicolon to complete expression")
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
			p.l.next()
			n.n1 = p.expr()
			if p.l.curr() == .semi {
				p.l.next()
			} else {
				panic("expected semicolon to complete expression")
			}
			return n
		}
		.s_return {
			p.l.next()
			n = &AstNode{kind: .s_return}
			if p.l.curr() == .semi {
				p.l.next()
			} else {
				panic("expected semicolon to complete return statement")
			}
			return n
		}
		.obr {
			return p.stmtseq(false)
		}
		.semi {
			p.l.next()
			n = &AstNode{kind: .empty}
			return n
		}
		.eof {
			panic("unexpected eof")
			return n
		}
		else {
			return p.stmtexpr()
		}
	}
}

fn (mut p Parser) toplevel() &AstNode {
	if !(p.l.curr() == .s_proc || p.l.curr() == .s_data) || p.l.curr() == .eof {
		panic("procedures and data may only be expressed at the top level")
	}
	is_proc := p.l.curr() == .s_proc
	if !p.l.expect(.ident) {
		panic("expected name")
	}
	if p.l.str_data in p.procs {
		panic("duplicate symbol name ${p.l.str_data}")
	}
	mut n := &AstNode(0)
	if is_proc {
		n = &AstNode{kind: .s_proc, value: u64(p.procs.len)}
	} else {
		n = &AstNode{kind: .s_data, value: u64(p.procs.len)}
	}
	p.procs << p.l.str_data
	
	if !p.l.expect(.obr) {
		panic("expected open bracket")
	}
	if is_proc {
		n.n1 = p.stmt()
	} else {
		n.n1 = p.stmtseq(true)
	}
	return n
}

fn (mut p Parser) parse() &AstNode {
	p.l.next()
	mut n := &AstNode{kind: .stmtseq}
	if p.l.curr() != .eof {
		for {
			mut x := &AstNode{kind: .stmtseq}
			x.n1 = n
			x.n2 = p.toplevel()
			n = x
			if p.l.curr() == .eof {
				break
			}
		}
	}
	return n
}