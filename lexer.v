
enum Tok {
	s_if s_else s_while s_data     // keywords
	s_proc s_call s_return         // 
	opar cpar obr cbr semi osb csb // punc
	add sub mul div assign dec     // ops
	gt gte lt lte eq neq           // 
	ident                          // vars
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

fn is_id(ch u8) bool { return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || ch == `_` }

fn (mut l Lexer) get() Tok {
	outer: for {
		if l.pos >= l.text.len {
			return .eof
		}

		mut ch := l.text[l.pos] l.pos++
		
		if ch.is_space() { continue }
		
		lookahead := l.text[l.pos] or { 0 } // use for `==`

		if is_id(ch) {
			start := l.pos - 1
			for l.pos < l.text.len {
				c := l.text[l.pos]
				if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_` {
					l.pos++
					continue
				}
				break
			}
			word := l.text[start..l.pos]

			match word {
				'if'     { return .s_if     }
				'else'   { return .s_else   }
				'while'  { return .s_while  }
				'proc'   { return .s_proc   }
				'call'   { return .s_call   }
				'return' { return .s_return }
				'data'   { return .s_data   }
				else {
					l.str_data = word
					return .ident
				}
			}
		} else if ch.is_digit() {
			start := l.pos - 1
			for l.pos < l.text.len {
				c := l.text[l.pos]
				if !c.is_digit() {
					if is_id(c) {
						panic("unsuitable character in integer")
					}
					break
				}
				l.pos++
			}
			word := l.text[start..l.pos]

			l.int_data = word.u64()
			return .dec
		} else {
			match ch {
				`{` { return .obr  }
				`}` { return .cbr  }
				`(` { return .opar }
				`)` { return .cpar }
				`+` { return .add  }
				`-` { return .sub  }
				`*` { return .mul  }
				`/` {
					if lookahead == `/` {
						l.pos++
						for l.pos < l.text.len {
							if l.text[l.pos] == `\n` {
								continue outer
							}
							l.pos++
						}
					} else {
						return .div
					}
				}
				`>` {
					if lookahead == `=` {
						l.pos++
						return .gte
					}
					return .gt
				}
				`<` {
					if lookahead == `=` {
						l.pos++
						return .lte
					}
					return .lt
				}
				`!` {
					if lookahead == `=` {
						l.pos++
						return .neq
					}
					// implement unary operators
				}
				`=` {
					if lookahead == `=` {
						l.pos++
						return .eq
					}
					return .assign
				}
				`[` {
					return .osb
				}
				`]` {
					return .csb
				}
				`;` { return .semi }
				else {}
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