use super::*;

// TODO: refactor this so no depth is required for single node printing
pub fn print_ast(ast: &[Node]) {
    fn print(depth: usize, node: &Node) {
        macro_rules! w {
            ($depth:expr, $($arg:tt)*) => {{
                //COLORS[($depth+COLORS.len() - 1) % COLORS.len()],
                let pad = ::std::iter::repeat(" ").take($depth*4).collect::<String>();
                eprint!("{}{}", pad, format!($($arg)*));
            }};
        }

        macro_rules! kind {
            ($e:expr) => {
                $e.as_ref()
            };
        }

        let newline = || eprintln!();

        use Node::*;
        match node {
            Func {
                name,
                args,
                body,
                stacksize,
            } => {
                w!(depth, "Func {} (", name);
                if *stacksize != 0 {
                    w!(depth, " -- size: {}", stacksize);
                }
                newline();
                for (i, arg) in args.iter().enumerate() {
                    print(depth + 1, arg.as_ref());
                    if i < args.len() - 1 {
                        newline();
                    }
                }
                if !args.is_empty() {
                    newline();
                }
                if body.has_val() {
                    print(depth + 1, kind!(body));
                }
                newline();
                w!(depth, ")");
                newline();
            }

            Vardef {
                name,
                init,
                offset,
                ty,
            } => {
                if !init.has_val() {
                    w!(depth, "Var {} {}", name, ty);
                    if *offset != 0 {
                        w!(0, " -- offset: {}", offset);
                    }
                } else {
                    w!(depth, "Var {} {} (", name, ty);
                    if *offset != 0 {
                        w!(0, " -- offset: {}", offset);
                    }
                    newline();
                    print(depth + 1, kind!(init));
                    newline();
                    w!(depth, ")");
                }
            }

            Addr { expr, ty} => {
                w!(depth, "Addr {} (\n", ty);                
                print(depth + 1, kind!(expr));                
                newline();
                w!(depth, ")");
            },

            Compound { stmts } => {
                w!(depth, "Compound (");
                newline();
                for (i, stmt) in stmts.iter().enumerate() {
                    print(depth + 1, stmt.as_ref());
                    if i < stmts.len() - 1 {
                        newline();
                    }
                }
                newline();
                w!(depth, ")");
            }

            Return { expr } => {
                w!(depth, "Return (");
                newline();
                print(depth + 1, kind!(expr));
                newline();
                w!(depth, ")");
            }

            Sizeof { expr} => {
                w!(depth, "Sizeof (");
                newline();
                print(depth + 1, kind!(expr));
                newline();
                w!(depth, ")");
            }

            Call { name, args } => {
                w!(depth, "Call {}(", name);
                if !args.is_empty() {
                    newline();
                }

                for (i, a) in args.iter().enumerate() {
                    print(depth + 1, &a.as_ref());
                    if i < args.len() - 1 {
                        w!(0, ",\n")
                    } else {
                        w!(0, "\n")
                    }
                }
                w!(if args.is_empty() { 0 } else { depth }, ")");
            }

            Constant { val, ty } => {
                w!(depth, "Constant {} (\n", ty);
                w!(depth + 1, "{}", val);
                newline();
                w!(depth, ")");
            }

            Ident { name } => w!(depth, "Ident {}", name),

            If { cond, body, else_ } => {
                w!(depth, "If ");
                if cond.has_val() {
                    w!(0, "Cond (");
                    newline();
                    print(depth + 1, kind!(cond));
                }
                newline();
                w!(depth, ")");
                if body.has_val() {
                    newline();
                    w!(depth, "Body (");
                    newline();
                    print(depth + 1, kind!(body));
                    newline();
                    w!(depth, ")");
                }
                if else_.has_val() {
                    newline();
                    w!(depth, "Else (");
                    newline();
                    print(depth + 1, kind!(else_));
                    newline();
                    w!(depth, ")");
                }
            }

            Else { body } => {
                w!(depth, "Else ");
                newline();
                if body.has_val() {
                    w!(depth, "Body\n");
                    print(depth + 1, kind!(body));
                }
            }

            For {
                init,
                cond,
                step,
                body,
            } => {
                w!(depth, "For (");
                newline();
                if init.has_val() {
                    w!(depth + 1, "Init (");
                    newline();
                    print(depth + 2, kind!(init));
                    newline();
                    w!(depth + 1, ")");
                }
                if cond.has_val() {
                    newline();
                    w!(depth + 1, "Cond (");
                    newline();
                    print(depth + 2, kind!(cond));
                    newline();
                    w!(depth + 1, ")");
                }
                if step.has_val() {
                    newline();
                    w!(depth + 1, "Step (");
                    newline();
                    print(depth + 2, kind!(step));
                    newline();
                    w!(depth + 1, ")");
                }
                if body.has_val() {
                    newline();
                    w!(depth + 1, "Body (");
                    newline();
                    print(depth + 2, kind!(body));
                    newline();
                    w!(depth + 1, ")");
                }
                newline();
                w!(depth, ")");
            }

            Statement { expr } => {
                w!(depth, "Statement (");
                newline();
                print(depth + 1, kind!(expr));
                newline();
                w!(depth, ")");
            }

            LVal { offset, ty } => {
                w!(depth, "LVal {} -- offset: {}", ty,  offset);
            }

            Add { lhs, rhs, .. } => {
                w!(depth, "Add (\n");
                print(depth + 1, kind!(lhs));
                w!(0, ",\n");
                print(depth + 1, kind!(rhs));
                newline();
                w!(depth, ")");
            }

            Sub { lhs, rhs, .. } => {
                w!(depth, "Sub (\n");
                print(depth + 1, kind!(lhs));
                w!(0, ",\n");
                print(depth + 1, kind!(rhs));
                newline();
                w!(depth, ")");
            }

            Mul { lhs, rhs, .. } => {
                w!(depth, "Mul (\n");
                print(depth + 1, kind!(lhs));
                w!(0, ",\n");
                print(depth + 1, kind!(rhs));
                newline();
                w!(depth, ")");
            }

            Div { lhs, rhs, .. } => {
                w!(depth, "Div (\n");
                print(depth + 1, kind!(lhs));
                w!(0, ",\n");
                print(depth + 1, kind!(rhs));
                newline();
                w!(depth, ")");
            }

            Comparison { lhs, rhs, comp } => {
                w!(depth, "Cmp {:?} (\n", comp);
                print(depth + 1, kind!(lhs));
                w!(0, ",\n");
                print(depth + 1, kind!(rhs));
                newline();
                w!(depth, ")");
            }

            LogAnd { lhs, rhs } => {
                w!(depth, "And (\n");
                print(depth + 1, kind!(lhs));
                w!(0, ",\n");
                print(depth + 1, kind!(rhs));
                newline();
                w!(depth, ")");
            }

            LogOr { lhs, rhs } => {
                w!(depth, "Or (\n");
                print(depth + 1, kind!(lhs));
                w!(0, ",\n");
                print(depth + 1, kind!(rhs));
                newline();
                w!(depth, ")");
            }

            Assign { lhs, rhs } => {
                w!(depth, "Assign (\n");
                print(depth + 1, kind!(lhs));
                w!(0, ",\n");
                print(depth + 1, kind!(rhs));
                newline();
                w!(depth, ")");
            }

            Deref { expr } => {
                w!(depth, "Deref (\n");
                print(depth + 1, kind!(expr));
                newline();
                w!(depth, ")");
            }
            //tok => w!(depth, "?? {:?}\n", tok),
        }
    }

    for node in ast {
        print(0, node);
    }
}

pub fn join_with<S, I, T>(mut iter: I, sep: S) -> String
where
    S: AsRef<str>,
    T: AsRef<str>,
    I: Iterator<Item = T>,
{
    let mut buf = String::new();
    if let Some(s) = iter.next() {
        buf.push_str(s.as_ref());
    }
    for i in iter {
        buf.push_str(sep.as_ref());
        buf.push_str(i.as_ref());
    }
    buf
}
