import os

enum Tok {
	s_if s_else s_while s_proc // keywords
	opar cpar obr cbr semi     // punc
	add sub mul div eq dec     // ops
	ident                      // vars
	eof
}

struct Lexer {
mut:
	pos  int
	text string
	int_data u64
	str_data string
	curr Tok
}

fn is_wp(ch u8) bool {
	return ch in [` `, `\t`, `\r`, `\n`]
}

fn (mut l Lexer) get() Tok {
	for {
		if l.pos >= l.text.len {
			return .eof
		}

		mut ch := l.text[l.pos] l.pos++

		// lookahead := l.text[l.pos] or { 0 } // use for `==`

		if is_wp(ch) { continue }
		
		match ch {
			`{` { return .obr  }
			`}` { return .cbr  }
			`(` { return .opar }
			`)` { return .cpar }
			`+` { return .add  }
			`-` { return .sub  }
			`*` { return .mul  }
			`/` { return .div  }
			`=` { return .eq   }
			`;` { return .semi }
			else {}
		}

		mut is_dec := true
		start := l.pos - 1

		for l.pos < l.text.len && !(is_wp(ch) || ch in [`{`, `}`, `(`, `)`, `+`, `-`, `=`, `;`]) {
			if is_dec && !(ch >= `0` && ch <= `9`) {
				is_dec = false
			}
			ch = l.text[l.pos] l.pos++
		}
		word := l.text[start..l.pos - 1]

		if is_dec {
			l.pos--
			l.int_data = word.u64()
			return .dec
		}

		match word {
			'if'    { return .s_if    }
			'else'  { return .s_else  }
			'while' { return .s_while }
			'proc'  { return .s_proc  }
			else {
				l.str_data = word
				return .ident
			}
		}
		break
	}
	panic("syntax error")
}

fn (mut l Lexer) expect(i Tok) bool {
	t := l.next()
	return t == i
}

fn (mut l Lexer) next() Tok {
	t := l.get()
	l.curr = t
	return t
}

fn (mut l Lexer) curr() Tok {
	return l.curr
}

enum AstKind {
	s_proc s_if s_while
	stmt stmtseq expr empty assign
	add sub mul div eq
	ident dec
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

// PRECEDENCE 0
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
				p.l.next()
				return n
			}
			panic("expected opening brace to begin while loop")
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
	p.procs << p.l.str_data
	if !p.l.expect(.obr) {
		panic("expected open bracket to begin procedure")
	}
	mut n := &AstNode{kind: .s_proc}
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
	mut p := Parser{l: l}

	a := p.parse()
	p.walk(a)
}