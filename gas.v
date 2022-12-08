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
				g.writeln("\tmov rax, [rbp - ${v * 8 + 8}]")
				return
			}
			else {}
		}
	}
	if s !in g.symtable {
		panic("identifier '${s}' is not a function or a variable")
	}
	g.writeln("\tlea rax, ${s}[rip]")
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
				g.writeln("\tmov [rbp - ${v * 8 + 8}], rax")
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
			.gt {
				g.writeln("\tcmp rax, rcx")
				g.writeln("\tmov rax, 0")
				g.writeln("\tseta al")
			}
			.gte {
				g.writeln("\tcmp rax, rcx")
				g.writeln("\tmov rax, 0")
				g.writeln("\tsetae al")
			}
			.lt {
				g.writeln("\tcmp rax, rcx")
				g.writeln("\tmov rax, 0")
				g.writeln("\tsetb al")
			}
			.lte {
				g.writeln("\tcmp rax, rcx")
				g.writeln("\tmov rax, 0")
				g.writeln("\tsetbe al")
			}
			.eq {
				g.writeln("\tcmp rax, rcx")
				g.writeln("\tmov rax, 0")
				g.writeln("\tsete al")
			}
			.neq {
				g.writeln("\tcmp rax, rcx")
				g.writeln("\tmov rax, 0")
				g.writeln("\tsetne al")
			}
			.index {
				g.writeln("\tmov rax, [rax + rcx * 8]")
			}
			else {
				panic("unreachable")
			}
		}
	}
}

fn (mut g Gen) const_data(root &AstNode) {
	if unsafe { root.n1 != nil } {
		g.const_data(root.n1)
	}
	if unsafe { root.n2 != nil } {
		if root.n2.n1.kind == .dec {
			g.writeln("\t.quad ${root.n2.n1.value as u64}")
		} else if root.n2.n1.kind == .ident {
			v := root.n2.n1.value as string
			if v !in g.symtable {
				panic("pointer '${v}' not in symbol table")
			}
			g.writeln("\t.quad ${v}")
		} else {
			panic('unreachable')
		}
	}	
}


fn (mut g Gen) gen(root &AstNode) {
	match root.kind {
		.s_proc {
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
		.s_data {
			g.writeln(".data")
			name := g.symtable[root.value as u64]
			if name == 'main' {
				panic("cannot use symbol 'main' as data")
			}
			g.writeln("${name}:")
			g.const_data(root)
			g.writeln(".text")
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
			g.writeln("\tjz .L${lbl}")
			g.gen(root.n2)
			if unsafe { root.n3 != nil } {
				lbl2 = g.lbl()
				g.writeln("\tjmp .${lbl2}")
			}
			g.writeln(".L${lbl}:")
			if unsafe { root.n3 != nil } {
				g.gen(root.n3)
				g.writeln(".${lbl2}:")
			}
		}
		.s_while {
			lbl := g.lbl()
			lbl2 := g.lbl()
			g.writeln(".L${lbl}:")
			g.expr(root.n1)
			g.writeln("\ttest rax, rax")
			g.writeln("\tjz .L${lbl2}")
			g.gen(root.n2)
			g.writeln("\tjmp .L${lbl}")
			g.writeln(".L${lbl2}:")
		}
		.s_return {
			g.writeln("\tleave")
			g.writeln("\tret")
		}
		.proc_call {
			g.expr(root.n1)
			g.writeln("\tcall rax")
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
__print_msg: .asciz \"%llu\\n\"
.text
main:
	call tlmain
	mov rax, r14
	ret
print: // builtin function
	mov rsi, r8
	lea rdi, __print_msg[rip]
	xor eax, eax
	jmp printf@plt") // r8
	g.gen(root)
}