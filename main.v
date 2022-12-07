import os

enum Tok {
	s_if s_else s_while s_do // keywords
	opar cpar obr cbr semi   // punc
	add sub eq dec           // ops
	svar                     // stack
	avar                     // args
	eof
}

struct Lexer {
mut:
	pos  int
	text string
	int_data u64
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
			'do'    { return .s_do    }
			's0'    { l.int_data = 0 return .svar }
			's1'    { l.int_data = 1 return .svar }
			's2'    { l.int_data = 2 return .svar }
			's3'    { l.int_data = 3 return .svar }
			's4'    { l.int_data = 4 return .svar }
			's5'    { l.int_data = 5 return .svar }
			'a0'    { l.int_data = 0 return .avar }
			'a1'    { l.int_data = 1 return .avar }
			'a2'    { l.int_data = 2 return .avar }
			'a3'    { l.int_data = 3 return .avar }
			'a4'    { l.int_data = 4 return .avar }
			'a5'    { l.int_data = 5 return .avar }
			else {}
		}
		break
	}
	panic("syntax error")
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
	s_if s_block
	program stmt stmtseq expr empty assign
	add sub eq
	svar avar dec
}

struct AstNode {
mut:
	kind AstKind
	value u64
	n1 &AstNode = unsafe { nil }
	n2 &AstNode = unsafe { nil }
	n3 &AstNode = unsafe { nil }
}

struct Parser {
mut:
	l Lexer
}

fn (mut p Parser) expr_paren() &AstNode {
	if p.l.curr() != .opar {
		panic("syntax error")
	}
	mut n := p.expr()
	if p.l.next() != .cpar {
		panic("syntax error")
	}
	return n
}

fn (mut p Parser) term() &AstNode {
	match p.l.curr() {
		.svar {
			p.l.next()
			return &AstNode{kind: .svar, value: p.l.int_data}
		}
		.avar {
			p.l.next()
			return &AstNode{kind: .avar, value: p.l.int_data}
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

fn (mut p Parser) expr1() &AstNode {
	mut n := p.term()
	for {
		t := p.l.curr()
		if t !in [.add, .sub] {
			break
		}
		mut x := &AstNode{kind: if t == .add { .add } else { .sub }}
		x.n1 = n
		x.n2 = p.term()
		n = x
	}
	return n
}

fn (mut p Parser) expr() &AstNode {
	if p.l.curr() !in [.svar, .avar] {
		return p.expr1()
	}
	mut n := p.expr1()
	if (n.kind == .svar || n.kind == .avar) && p.l.curr() == .eq {
		mut x := &AstNode{kind: .assign}
		x.n1 = n
		x.n2 = p.expr()
		p.l.next()
		return x
	}
	return n
}

fn (mut p Parser) stmt() &AstNode {
	tok := p.l.next()
	match tok {
		/* .s_if {
			mut n := &AstNode{kind: .s_if}
			n.n1 = p.expr()
			n.n2 = p.stmt()
			if p.l.next() == .s_else {
				n.n3 = p.stmt()
			}
			return n
		}
		.obr {
			mut n := &AstNode{kind: .empty}
			for p.l.next() != .cbr {
				mut x := AstNode{kind: .stmtseq}
				x.n1 = n
				x.n2 = p.stmt()
			}
			return n
		} */
		.semi {
			return &AstNode{kind: .empty}
		}
		else {
			mut n := &AstNode{kind: .expr}
			n.n1 = p.expr()
			if p.l.next() != .semi {
				panic("syntax error ${p.l.curr()}")
			}
			return n
		}
	}
	panic("unreachable")
}

fn (mut p Parser) program() &AstNode {
	mut n := &AstNode{kind: .program}
	n.n1 = p.stmt()
	if p.l.next() != .eof {
		panic("syntax error")
	}
	return n
}

fn (p Parser) walk_impl(n &AstNode, _dep int) {
	dep := _dep + 1

	print(` `.repeat(_dep))
	println(n.kind)

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

	a := p.program()
	p.walk(a)
}