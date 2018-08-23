use super::*;

type NodeKind = Option<Box<Node>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Constant {
        val: u32,
    },

    Ident {
        name: String,
    },

    Add {
        lhs: NodeKind,
        rhs: NodeKind,
    },

    Sub {
        lhs: NodeKind,
        rhs: NodeKind,
    },

    Mul {
        lhs: NodeKind,
        rhs: NodeKind,
    },

    Div {
        lhs: NodeKind,
        rhs: NodeKind,
    },

    LVal {
        offset: i32,
    },

    Vardef {
        name: String,
        init: NodeKind,
        offset: i32,
    },

    Return {
        expr: NodeKind,
    },

    Assign {
        lhs: NodeKind,
        rhs: NodeKind,
    },

    LogAnd {
        lhs: NodeKind,
        rhs: NodeKind,
    },

    LogOr {
        lhs: NodeKind,
        rhs: NodeKind,
    },

    Comparison {
        lhs: NodeKind,
        rhs: NodeKind,
    },

    If {
        cond: NodeKind,
        body: NodeKind,
        else_: NodeKind,
    },

    Else {
        body: NodeKind,
    },

    For {
        init: NodeKind,
        cond: NodeKind,
        step: NodeKind,
        body: NodeKind,
    },

    Call {
        name: String,
        args: Vec<Node>,
    },

    Func {
        name: String,
        args: Vec<Node>,
        body: NodeKind,
        stacksize: i32,
    },

    Statement {
        expr: NodeKind,
    },

    Compound {
        stmts: Vec<Node>,
    },
}

impl Node {
    pub fn parse(tokens: &mut Tokens) -> Vec<Self> {
        let mut nodes = vec![];
        while let Some((_, tok)) = tokens.peek() {
            if *tok == Token::EOF {
                break;
            }
            nodes.push(Self::function(tokens))
        }
        nodes
    }

    fn function(tokens: &mut Tokens) -> Self {
        let (_, _ty) = expect(tokens, Token::Int, "function return type");
        let (_, name) = expect_ident(tokens, "function name");

        expect_token(tokens, '(');
        let mut args = vec![];
        if !consume(tokens, ')') {
            args.push(Self::param(tokens));
            while consume(tokens, ',') {
                args.push(Self::param(tokens));
            }
            expect_token(tokens, ')');
        }

        expect_token(tokens, '{');
        Node::Func {
            name,
            args,
            body: make(Self::compound_stmt(tokens)),
            stacksize: 0,
        }
    }

    fn compound_stmt(tokens: &mut Tokens) -> Self {
        let mut stmts = vec![];
        while !consume(tokens, '}') {
            stmts.push(Self::stmt(tokens))
        }
        Node::Compound { stmts }
    }

    fn stmt(tokens: &mut Tokens) -> Self {
        let (pos, next) = tokens.peek().expect("token for statement");

        match next {
            Token::Int => Self::decl(tokens),
            Token::If => {
                tokens.advance();
                expect_token(tokens, '(');
                let cond = make(Self::assign(tokens));

                expect_token(tokens, ')');
                let body = make(Self::stmt(tokens));

                let else_ = if consume(tokens, "else") {
                    make(Self::stmt(tokens))
                } else {
                    None
                };

                Node::If { cond, body, else_ }
            }
            Token::For => {
                tokens.advance();
                expect_token(tokens, '(');
                let init = if is_typename(tokens) {
                    Self::decl(tokens)
                } else {
                    Self::expr_stmt(tokens)
                };

                let cond = Self::assign(tokens);
                expect_token(tokens, ';');

                let step = Self::assign(tokens);
                expect_token(tokens, ')');

                let body = Self::stmt(tokens);
                Node::For {
                    init: make(init),
                    cond: make(cond),
                    step: make(step),
                    body: make(body),
                }
            }
            Token::Return => {
                tokens.advance();
                let node = Node::Return {
                    expr: make(Node::assign(tokens)),
                };
                expect_token(tokens, ';');
                node
            }
            tok if *tok == '{' => {
                tokens.advance();
                let mut stmts = vec![];
                while !consume(tokens, '}') {
                    stmts.push(Self::stmt(tokens));
                }
                Node::Compound { stmts }
            }
            tok if *tok != Token::EOF => Self::expr_stmt(tokens),
            _ => expect_fail(tokens.input_at(0), *pos, "token wasn't"), // expected
        }
    }

    fn expr_stmt(tokens: &mut Tokens) -> Self {
        let node = Node::Statement {
            expr: make(Self::assign(tokens)),
        };
        expect_token(tokens, ';');
        node
    }

    fn decl(tokens: &mut Tokens) -> Self {
        tokens.advance();
        let (_, name) = expect_ident(tokens, "variable name expected");
        let name = name.to_string();

        let init = if consume(tokens, '=') {
            make(Self::assign(tokens))
        } else {
            None
        };

        expect_token(tokens, ';');
        Node::Vardef {
            name,
            init,
            offset: 0,
        }
    }

    fn assign(tokens: &mut Tokens) -> Self {
        let lhs = Self::logor(tokens);
        if consume(tokens, '=') {
            return Node::Assign {
                lhs: make(lhs),
                rhs: make(Self::logor(tokens)),
            };
        }
        lhs
    }

    fn logor(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::logand(tokens);
        'expr: loop {
            if let Some((_, Token::LogOr)) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogOr {
                    lhs: make(lhs),
                    rhs: make(Self::logand(tokens)),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn logand(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::rel(tokens);
        'expr: loop {
            if let Some((_, Token::LogAnd)) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogAnd {
                    lhs: make(lhs),
                    rhs: make(Self::rel(tokens)),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn rel(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::add(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for rel");
            match next {
                tok if *tok == '<' => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: make(lhs),
                        rhs: make(Self::add(tokens)),
                    };
                }
                tok if *tok == '>' => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: make(Self::add(tokens)),
                        rhs: make(lhs),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn add(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::mul(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for add");
            match next {
                tok if *tok == '+' => {
                    tokens.advance();
                    lhs = Node::Add {
                        lhs: make(lhs),
                        rhs: make(Self::mul(tokens)),
                    };
                }
                tok if *tok == '-' => {
                    tokens.advance();
                    lhs = Node::Sub {
                        lhs: make(lhs),
                        rhs: make(Self::mul(tokens)),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn mul(tokens: &mut Tokens) -> Self {
        let mut lhs = Self::term(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for mul");
            match next {
                tok if *tok == '*' => {
                    tokens.advance();
                    lhs = Node::Mul {
                        lhs: make(lhs),
                        rhs: make(Self::term(tokens)),
                    };
                }
                tok if *tok == '/' => {
                    tokens.advance();
                    lhs = Node::Div {
                        lhs: make(lhs),
                        rhs: make(Self::term(tokens)),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn param(tokens: &mut Tokens) -> Self {
        tokens.advance();

        let (_, name) = expect_ident(tokens, "variable name expected");
        let name = name.to_string();

        let init = if consume(tokens, '=') {
            make(Self::assign(tokens))
        } else {
            None
        };

        Node::Vardef {
            name,
            init,
            offset: 0,
        }
    }

    fn term(tokens: &mut Tokens) -> Self {
        let (pos, next) = tokens.next_token().expect("token for mul");
        match next {
            Token::Num(n) => Node::Constant { val: *n },
            Token::Ident(ref name) => {
                let n = name.clone();
                if !consume(tokens, '(') {
                    return Node::Ident { name: n };
                }

                if consume(tokens, ')') {
                    return Node::Call {
                        name: n,
                        args: vec![],
                    };
                }

                let mut args = vec![];
                args.push(Self::assign(tokens));
                while consume(tokens, ',') {
                    args.push(Self::assign(tokens));
                }
                expect_token(tokens, ')');
                Node::Call { name: n, args }
            }
            tok if *tok == '(' => {
                let node = Node::assign(tokens);
                expect_token(tokens, ')');
                node
            }
            _ => {
                let pos = *pos;
                expect_fail(tokens.input_at(0), pos, "number or ident")
            }
        }
    }
}

#[inline]
fn make(node: Node) -> Option<Box<Node>> {
    Some(Box::new(node))
}

#[inline]
fn is_typename(tokens: &mut Tokens) -> bool {
    match tokens.peek() {
        Some((_, Token::Int)) => true,
        // TODO add more types
        _ => false,
    }
}

#[inline]
fn consume(tokens: &mut Tokens, tok: impl Into<Token>) -> bool {
    let tok = tok.into();
    match tokens.peek() {
        Some((_, t)) if *t == tok => {
            tokens.advance();
            true
        }
        _ => false,
    }
}

// TODO: this really needs a better name
#[inline]
fn expect_token(tokens: &mut Tokens, tok: impl Into<Token>) {
    let tok = tok.into();

    let (pos, next) = tokens.next_token().expect("get next token");
    if *next == tok {
        return;
    }

    const SOURCE_LINE: &str = "source=> ";
    let (pos, next) = (*pos, next.clone());
    let (input, adjusted) = midpoint(tokens.input_at(0), pos, 80 - SOURCE_LINE.len());

    eprintln!("{}{}", wrap_color!(Color::Yellow {}, SOURCE_LINE), input);
    draw_caret(SOURCE_LINE.len() + adjusted, Color::Red {});

    fail!(
        "{} {} was expected. found {} at position: {}.\n",
        wrap_color!(Color::Red {}, "ERROR:"),
        wrap_color!(Color::Green {}, tok.get_char()),
        wrap_color!(Color::Cyan {}, "{:?}", next),
        wrap_color!(Color::Blue {}, pos),
    );
}

/// this uses a discriminant comparison
#[inline]
fn expect(tokens: &mut Tokens, tok: impl Into<Token>, msg: impl AsRef<str>) -> (usize, Token) {
    use std::mem::discriminant;
    let (pos, next) = tokens.next_token().expect("get next token");
    if discriminant(next) == discriminant(&tok.into()) {
        return (*pos, next.clone());
    }
    let pos = *pos;
    expect_fail(tokens.input_at(0), pos, msg.as_ref());
}

#[inline]
fn expect_ident(tokens: &mut Tokens, msg: impl AsRef<str>) -> (usize, String) {
    let (pos, next) = tokens.next_token().expect("get next token");
    if let Token::Ident(name) = next {
        return (*pos, name.to_string());
    }
    let pos = *pos;
    expect_fail(tokens.input_at(0), pos, msg.as_ref());
}

fn expect_fail(input: &str, pos: usize, msg: &str) -> ! {
    const SOURCE_LINE: &str = "source=> ";
    let (input, adjusted) = midpoint(&input, pos, 80 - SOURCE_LINE.len());

    eprintln!("{}{}", wrap_color!(Color::Yellow {}, SOURCE_LINE), input);
    draw_caret(SOURCE_LINE.len() + adjusted, Color::Red {});

    fail!(
        "{} {} was expected. at position: {}.\n",
        wrap_color!(Color::Red {}, "ERROR:"),
        wrap_color!(Color::Cyan {}, msg),
        wrap_color!(Color::Blue {}, pos),
    );
}

/// gets +/- `width` around the `cursor`
fn midpoint(input: &str, cursor: usize, width: usize) -> (&str, usize) {
    let half = width / 2;
    if input.len() > width {
        if cursor < half {
            (&input[..half], cursor)
        } else {
            (&input[cursor - half..], half)
        }
    } else {
        (input, cursor)
    }
}

fn draw_caret(width: usize, color: Color) {
    let s = ::std::iter::repeat(" ").take(width).collect::<String>();
    eprintln!("{}{}", s, wrap_color!(color, "^"));
}

pub fn dump_ast(ast: &[Node]) {
    macro_rules! kind {
        ($e:expr) => {
            $e.as_ref().unwrap()
        };
    }

    fn dump(depth: usize, node: &Node) {
        // const COLORS: [Color; 7] = [
        //     Color::White,
        //     Color::Red,
        //     Color::Green,
        //     Color::Yellow,
        //     Color::Cyan,
        //     Color::Magenta,
        //     Color::Blue,
        // ];

        macro_rules! w {
            ($depth:expr, $($arg:tt)*) => {{
                // let pad = wrap_color!(
                //     COLORS[($depth+COLORS.len() - 1) % COLORS.len()],
                //     "{}",
                //     ::std::iter::repeat("·").take($depth * 2).collect::<String>()
                // );
                let pad = ::std::iter::repeat(" ").take($depth*2).collect::<String>();
                eprint!("{}{}", pad, format!($($arg)*));
            }};
        }

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
                w!(0, "\n");
                for (i, arg) in args.iter().enumerate() {
                    dump(depth + 1, arg);
                    if i < args.len() - 1 {
                        w!(0, "\n");
                    }
                }
                if !args.is_empty() {
                    w!(0, "\n");
                }
                if body.is_some() {
                    dump(depth + 1, kind!(body));
                }
                w!(0, "\n");
                w!(depth, ")");
                w!(0, "\n");
            }

            Vardef { name, init, offset } => {
                if init.is_none() {
                    w!(depth, "Var {}", name);
                    if *offset != 0 {
                        w!(0, " -- offset: {}", offset);
                    }
                } else {
                    w!(depth, "Var {} (", name);
                    if *offset != 0 {
                        w!(0, " -- offset: {}", offset);
                    }
                    w!(0, "\n");
                    dump(depth + 1, kind!(init));
                    w!(0, "\n");
                    w!(depth, ")");
                }
            }

            Compound { stmts } => {
                w!(depth, "Compound (");
                w!(0, "\n");
                for (i, stmt) in stmts.iter().enumerate() {
                    dump(depth + 1, stmt);
                    if i < stmts.len() - 1 {
                        w!(0, "\n");
                    }
                }
                w!(0, "\n");
                w!(depth, ")");
            }

            Return { expr } => {
                w!(depth, "Return (");
                w!(0, "\n");
                dump(depth + 1, kind!(expr));
                w!(0, "\n");
                w!(depth, ")");
            }

            Call { name, args } => {
                w!(depth, "Call {}(", name);
                if !args.is_empty() {
                    w!(0, "\n");
                }

                for (i, a) in args.iter().enumerate() {
                    dump(depth + 1, &a);
                    if i < args.len() - 1 {
                        w!(0, ",\n")
                    } else {
                        w!(0, "\n")
                    }
                }
                w!(if args.is_empty() { 0 } else { depth }, ")");
            }

            Constant { val } => w!(depth, "Constant {}", val),

            Ident { name } => w!(depth, "Ident {}", name),

            If { cond, body, else_ } => {
                w!(depth, "If ");
                if cond.is_some() {
                    w!(0, "Cond (");
                    w!(0, "\n");
                    dump(depth + 1, kind!(cond));
                }
                w!(0, "\n");
                w!(depth, ")");
                if body.is_some() {
                    w!(0, "\n");
                    w!(depth, "Body (");
                    w!(0, "\n");
                    dump(depth + 1, kind!(body));
                    w!(0, "\n");
                    w!(depth, ")");
                }
                if else_.is_some() {
                    w!(0, "\n");
                    w!(depth, "Else (");
                    w!(0, "\n");
                    dump(depth + 1, kind!(else_));
                    w!(0, "\n");
                    w!(depth, ")");
                }
            }

            Else { body } => {
                w!(depth, "Else ");
                w!(0, "\n");
                if body.is_some() {
                    w!(depth, "Body\n");
                    dump(depth + 1, kind!(body));
                }
            }

            For {
                init,
                cond,
                step,
                body,
            } => {
                w!(depth, "For (");
                w!(0, "\n");
                if init.is_some() {
                    w!(depth + 1, "Init (");
                    w!(0, "\n");
                    dump(depth + 2, kind!(init));
                    w!(0, "\n");
                    w!(depth + 1, ")");
                }
                if cond.is_some() {
                    w!(0, "\n");
                    w!(depth + 1, "Cond (");
                    w!(0, "\n");
                    dump(depth + 2, kind!(cond));
                    w!(0, "\n");
                    w!(depth + 1, ")");
                }
                if step.is_some() {
                    w!(0, "\n");
                    w!(depth + 1, "Step (");
                    w!(0, "\n");
                    dump(depth + 2, kind!(step));
                    w!(0, "\n");
                    w!(depth + 1, ")");
                }
                if body.is_some() {
                    w!(0, "\n");
                    w!(depth + 1, "Body (");
                    w!(0, "\n");
                    dump(depth + 2, kind!(body));
                    w!(0, "\n");
                    w!(depth + 1, ")");
                }
                w!(0, "\n");
                w!(depth, ")");
            }

            Statement { expr } => {
                w!(depth, "Statement (");
                w!(0, "\n");
                dump(depth + 1, kind!(expr));
                w!(0, "\n");
                w!(depth, ")");
            }

            LVal { offset } => {
                w!(depth, "LVal -- offset: {}", offset);
            }

            Add { lhs, rhs } => {
                w!(depth, "Add (\n");
                dump(depth + 1, kind!(lhs));
                w!(0, ",\n");
                dump(depth + 1, kind!(rhs));
                w!(0, "\n");
                w!(depth, ")");
            }

            Sub { lhs, rhs } => {
                w!(depth, "Sub (\n");
                dump(depth + 1, kind!(lhs));
                w!(0, ",\n");
                dump(depth + 1, kind!(rhs));
                w!(0, "\n");
                w!(depth, ")");
            }

            Mul { lhs, rhs } => {
                w!(depth, "Mul (\n");
                dump(depth + 1, kind!(lhs));
                w!(0, ",\n");
                dump(depth + 1, kind!(rhs));
                w!(0, "\n");
                w!(depth, ")");
            }

            Div { lhs, rhs } => {
                w!(depth, "Div (\n");
                dump(depth + 1, kind!(lhs));
                w!(0, ",\n");
                dump(depth + 1, kind!(rhs));
                w!(0, "\n");
                w!(depth, ")");
            }

            Comparison { lhs, rhs } => {
                w!(depth, "Cmp (\n");
                dump(depth + 1, kind!(lhs));
                w!(0, ",\n");
                dump(depth + 1, kind!(rhs));
                w!(0, "\n");
                w!(depth, ")");
            }

            LogAnd { lhs, rhs } => {
                w!(depth, "And (\n");
                dump(depth + 1, kind!(lhs));
                w!(0, ",\n");
                dump(depth + 1, kind!(rhs));
                w!(0, "\n");
                w!(depth, ")");
            }

            LogOr { lhs, rhs } => {
                w!(depth, "Or (\n");
                dump(depth + 1, kind!(lhs));
                w!(0, ",\n");
                dump(depth + 1, kind!(rhs));
                w!(0, "\n");
                w!(depth, ")");
            }

            Assign { lhs, rhs } => {
                w!(depth, "Assign (\n");
                dump(depth + 1, kind!(lhs));
                w!(0, ",\n");
                dump(depth + 1, kind!(rhs));
                w!(0, "\n");
                w!(depth, ")");
            }
        }
    }

    for node in ast {
        // eprintln!("{:#?}", node);
        dump(0, node);
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
