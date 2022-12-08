import os
import flag

fn (p Parser) walk_impl(n &AstNode, _dep int) {
	mut dep := if n.kind != .stmtseq {
		print(` `.repeat(_dep))
		if n.kind == .ident {
			println('${n.kind} `${n.value as string}`')
		} else if n.kind == .dec {
			println('${n.kind} `${n.value as u64}`')
		} else {
			println(n.kind)
		}
		_dep + 1
	} else {
		_dep
	}

	if unsafe { n.n1 != nil } {
		if n.kind != .stmtseq && n.n1.kind == .stmtseq {
			dep++
		}
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
	mut fp := flag.new_flag_parser(os.args)
    fp.application('tl')
    fp.version('v0.0.1')
    fp.limit_free_args_to_exactly(1)! // comment this, if you expect arbitrary texts after the options
    fp.description('The `tl` programming language compiler.')
    fp.skip_executable()

    dump_tok := fp.bool('dump-tok', 0, false, 'dump tokens from the lexer.')
    dump_ast := fp.bool('dump-ast', 0, false, 'dump AST representation.')

    args := fp.finalize() or {
        eprintln(fp.usage())
		exit(1)
        return
    }
	
	mut l := Lexer{text: os.read_file(args[0])!}
	if dump_tok {
		for {
			e := l.next()
			if e == .ident {
				println('${e} `${l.str_data}`')
			} if e == .dec {
				println('${e} `${l.int_data}`')
			} else {
				println(e)
			}
			if e == .eof {
				break
			}
		}
		exit(0)
	}

	mut p := Parser{l: l}
	p.procs << ['print', '__print_msg']
	a := p.parse()

	if dump_ast {
		p.walk(a)
		println(p.procs)
		exit(0)
	}

	mut g := Gen{symtable: p.procs}
	g.generate_all(a)
}