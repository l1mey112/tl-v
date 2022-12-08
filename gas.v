struct Gen {
	symtable []string
mut:
	label_count int
}

fn (g Gen) writeln(str string) {
	println(str)
}

fn (mut g Gen) lbl() int {
	lbl := g.label_count
	g.label_count++
	return lbl
}

fn (g Gen) get_identifier(s string) {
	if s == 'r' {
		g.writeln("\tmov rax, r14")
		return
	}
	if s.len == 2 && s[1].is_digit() && s[1] - `0` <= 5 {
		v := int(s[1] - `0`)
		match s[0] {
			`a` {
				g.writeln("\tmov rax, r${v + 8}")
				return
			}
			`s` {
				g.writeln("\tmov rax, [rbp - ${v * 8}]")
				return
			}
			else {}
		}
	}
	if s !in g.symtable {
		panic("identifier '${s}' is not a function or a variable")
	}
	g.writeln("\tmov rax, ${s}")
}

fn (g Gen) mov_to_identifier(s string) {
	if s == 'r' {
		g.writeln("\tmov r14, rax")
		return
	}
	if s.len == 2 && s[1].is_digit() && s[1] - `0` <= 5 {
		v := int(s[1] - `0`)
		match s[0] {
			`a` {
				g.writeln("\tmov r${v + 8}, rax")
				return
			}
			`s` {
				g.writeln("\tmov [rbp - ${v * 8}], rax")
				return
			}
			else {}
		}
	}
	panic("identifier '${s}' is not a variable")
}

fn (g Gen) expr(root &AstNode) {
	match root.kind {
		.dec {
			g.writeln("\tmov rax, ${root.value as u64}")
			return
		}
		.assign {
			g.expr(root.n2)
			g.mov_to_identifier(root.n1.value as string)
			// value stays in `rax`
			return
		}
		.ident {
			g.get_identifier(root.value as string)
			return
		}
		else {}
	}
	if unsafe { root.n2 != nil } {
		g.expr(root.n2)
		match root.n1.kind {
			.ident {
				g.writeln("\tmov rcx, rax")
				g.get_identifier(root.n1.value as string)
			}
			else {
				g.writeln("\tpush rax")
				g.expr(root.n1)
				g.writeln("\tpop rcx")
			}
		}
		match root.kind {
			.add {
				g.writeln("\tadd rax, rcx")
			}
			.sub {
				g.writeln("\tsub rax, rcx")
			}
			.mul {
				g.writeln("\timul rax, rcx")
			}
			.div {
				g.writeln("\txor rdx, rdx")
				g.writeln("\tdiv rcx")
			}
			else {
				panic("unreachable")
			}
		}
	}
}

fn (mut g Gen) gen(root &AstNode) {
	match root.kind {
		.s_proc {
			g.label_count = 0
			name := g.symtable[root.value as u64]
			if name == 'main' {
				g.writeln("tlmain:")
			} else {
				g.writeln("${name}:")
			}
			g.writeln("\tpush rbp")
			g.writeln("\tmov rbp, rsp")
			g.writeln("\tsub rsp, 48")
			g.gen(root.n1)
			g.writeln("\tleave")
			g.writeln("\tret")
		}
		.stmtseq {
			if unsafe { root.n1 != nil } {
				g.gen(root.n1)
			}
			if unsafe { root.n2 != nil } {
				g.gen(root.n2)
			}
		}
		.expr {
			g.expr(root.n1)
		}
		.s_if {
			lbl := g.lbl()
			mut lbl2 := 0
			
			g.expr(root.n1)
			g.writeln("\ttest rax, rax")
			g.writeln("\tjz .${lbl}")
			g.gen(root.n2)
			if unsafe { root.n3 != nil } {
				lbl2 = g.lbl()
				g.writeln("\tjmp .${lbl2}")
			}
			g.writeln(".${lbl}:")
			if unsafe { root.n3 != nil } {
				g.gen(root.n3)
				g.writeln(".${lbl2}:")
			}
		}
		.s_while {
			lbl := g.lbl()
			lbl2 := g.lbl()
			g.writeln(".${lbl}")
			g.expr(root.n1)
			g.writeln("\ttest rax, rax")
			g.writeln("\tjz .${lbl2}")
			g.gen(root.n2)
			g.writeln("\tjmp .${lbl}")
			g.writeln(".${lbl2}:")
		}
		.proc_call {
			g.writeln("\tcall ${root.n1.value as string}")
		}
		.empty {}
		else {
			panic("unreachable")
		}
	}
}

fn (mut g Gen) generate_all(root &AstNode) {
	g.writeln(
".intel_syntax noprefix
.extern printf
.globl main
.data
message: .asciz \"%llu\\n\"
.text
main:
	call tlmain
	mov rax, r14
	ret
print:
	lea rdi, [rip + message]
	mov rsi, r8
	xor eax, eax
	call printf@plt
	ret") // r8
	g.gen(root)
}