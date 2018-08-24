use super::*;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Kind {
    val: Option<Box<Node>>,
    ty: Option<Type>,
}

impl Kind {
    pub fn make(node: Node) -> Self {
        Kind {
            val: Some(Box::new(node)),
            ty: None,
        }
    }

    pub fn empty() -> Self {
        Kind {
            val: None,
            ty: None,
        }
    }

    pub fn has_val(&self) -> bool {
        self.val.is_some()
    }

    pub fn get_type(&self) -> &Type {
        self.ty.as_ref().unwrap()
    }

    pub fn get_type_mut(&mut self) -> &mut Type {
        self.ty.as_mut().unwrap()
    }
}

impl AsRef<Node> for Kind {
    fn as_ref(&self) -> &Node {
        self.val.as_ref().unwrap()
    }
}

impl AsMut<Node> for Kind {
    fn as_mut(&mut self) -> &mut Node {
        self.val.as_mut().unwrap()
    }
}

impl AsMut<Node> for Node {
    fn as_mut(&mut self) -> &mut Node {
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Constant {
        val: u32,
        ty: Type,
    },

    Ident {
        name: String,
    },

    Add {
        lhs: Kind,
        rhs: Kind,
    },

    Sub {
        lhs: Kind,
        rhs: Kind,
    },

    Mul {
        lhs: Kind,
        rhs: Kind,
    },

    Div {
        lhs: Kind,
        rhs: Kind,
    },

    LVal {
        offset: i32,
    },

    Deref {
        expr: Kind,
    },

    Vardef {
        name: String,
        init: Kind,
        offset: i32,
        ty: Type,
    },

    Return {
        expr: Kind,
    },

    Assign {
        lhs: Kind,
        rhs: Kind,
    },

    LogAnd {
        lhs: Kind,
        rhs: Kind,
    },

    LogOr {
        lhs: Kind,
        rhs: Kind,
    },

    Comparison {
        lhs: Kind,
        rhs: Kind,
        comp: Comp,
    },

    If {
        cond: Kind,
        body: Kind,
        else_: Kind,
    },

    Else {
        body: Kind,
    },

    For {
        init: Kind,
        cond: Kind,
        step: Kind,
        body: Kind,
    },

    Call {
        name: String,
        args: Vec<Kind>,
    },

    Func {
        name: String,
        args: Vec<Kind>,
        body: Kind,
        stacksize: i32,
    },

    Statement {
        expr: Kind,
    },

    Compound {
        stmts: Vec<Kind>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Comp {
    Lt,
    Gt,
}

impl fmt::Display for Comp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let c = match self {
            Comp::Lt => '<',
            Comp::Gt => '>',
        };
        write!(f, "{}", c)
    }
}

// TODO: need to denote the type that the pointer points too
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Ptr { ptr_of: Box<Type> },
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Ptr { ptr_of } => write!(f, "Ptr: {}", ptr_of),
        }
    }
}

impl Node {
    pub fn parse(mut tokens: Lexer) -> Vec<Self> {
        let tokens = &mut tokens;
        let mut nodes = vec![];
        while let Some((_, tok)) = tokens.peek() {
            if *tok == Token::EOF {
                break;
            }
            nodes.push(Self::function(tokens))
        }
        nodes
    }

    fn function(tokens: &mut Lexer) -> Self {
        let (_, _ty) = expect_type(tokens, "function return type");
        let (_, name) = expect_ident(tokens, "function name");

        expect_token(tokens, '(');
        let mut args = vec![];
        if !consume(tokens, ')') {
            args.push(Kind::make(Self::param(tokens)));
            while consume(tokens, ',') {
                args.push(Kind::make(Self::param(tokens)));
            }
            expect_token(tokens, ')');
        }

        expect_token(tokens, '{');
        Node::Func {
            name,
            args,
            body: Kind::make(Self::compound_stmt(tokens)),
            stacksize: 0,
        }
    }

    fn param(tokens: &mut Lexer) -> Self {
        let ty = Self::ty(tokens);

        let (_, name) = expect_ident(tokens, "variable name as param");
        let name = name.to_string();

        let init = if consume(tokens, '=') {
            Kind::make(Self::assign(tokens))
        } else {
            Kind::empty()
        };

        Node::Vardef {
            name,
            init,
            offset: 0,
            ty,
        }
    }

    fn compound_stmt(tokens: &mut Lexer) -> Self {
        let mut stmts = vec![];
        while !consume(tokens, '}') {
            stmts.push(Kind::make(Self::stmt(tokens)))
        }
        Node::Compound { stmts }
    }

    fn stmt(tokens: &mut Lexer) -> Self {
        let (pos, next) = tokens.peek().expect("token for statement");

        match next {
            Token::Type(_) => Self::decl(tokens),
            Token::If => {
                tokens.advance();
                expect_token(tokens, '(');
                let cond = Kind::make(Self::assign(tokens));

                expect_token(tokens, ')');
                let body = Kind::make(Self::stmt(tokens));

                let else_ = if consume(tokens, "else") {
                    Kind::make(Self::stmt(tokens))
                } else {
                    Kind::empty()
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
                    init: Kind::make(init),
                    cond: Kind::make(cond),
                    step: Kind::make(step),
                    body: Kind::make(body),
                }
            }
            Token::Return => {
                tokens.advance();
                let node = Node::Return {
                    expr: Kind::make(Node::assign(tokens)),
                };
                expect_token(tokens, ';');
                node
            }
            tok if *tok == '{' => {
                tokens.advance();
                let mut stmts = vec![];
                while !consume(tokens, '}') {
                    stmts.push(Kind::make(Self::stmt(tokens)));
                }
                Node::Compound { stmts }
            }
            tok if *tok != Token::EOF => Self::expr_stmt(tokens),
            _ => expect_fail(tokens.input_at(0), *pos, "token wasn't"), // expected
        }
    }

    fn expr_stmt(tokens: &mut Lexer) -> Self {
        let node = Node::Statement {
            expr: Kind::make(Self::assign(tokens)),
        };
        expect_token(tokens, ';');
        node
    }

    fn decl(tokens: &mut Lexer) -> Self {
        let ty = Self::ty(tokens);

        let (_, name) = expect_ident(tokens, "variable name as decl");
        let name = name.to_string();

        let init = if consume(tokens, '=') {
            Kind::make(Self::assign(tokens))
        } else {
            Kind::empty()
        };

        expect_token(tokens, ';');
        Node::Vardef {
            name,
            init,
            offset: 0,
            ty,
        }
    }

    fn assign(tokens: &mut Lexer) -> Self {
        let lhs = Self::logor(tokens);
        if consume(tokens, '=') {
            return Node::Assign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(Self::logor(tokens)),
            };
        }
        lhs
    }

    fn logor(tokens: &mut Lexer) -> Self {
        let mut lhs = Self::logand(tokens);
        'expr: loop {
            if let Some((_, Token::LogOr)) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogOr {
                    lhs: Kind::make(lhs),
                    rhs: Kind::make(Self::logand(tokens)),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn logand(tokens: &mut Lexer) -> Self {
        let mut lhs = Self::rel(tokens);
        'expr: loop {
            if let Some((_, Token::LogAnd)) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogAnd {
                    lhs: Kind::make(lhs),
                    rhs: Kind::make(Self::rel(tokens)),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn rel(tokens: &mut Lexer) -> Self {
        let mut lhs = Self::add(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for rel");
            match next {
                tok if *tok == '<' => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(Self::add(tokens)),
                        comp: Comp::Lt,
                    };
                }
                tok if *tok == '>' => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: Kind::make(Self::add(tokens)),
                        rhs: Kind::make(lhs),
                        comp: Comp::Gt,
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn add(tokens: &mut Lexer) -> Self {
        let mut lhs = Self::mul(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for add");
            match next {
                tok if *tok == '+' => {
                    tokens.advance();
                    lhs = Node::Add {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(Self::mul(tokens)),
                    };
                }
                tok if *tok == '-' => {
                    tokens.advance();
                    lhs = Node::Sub {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(Self::mul(tokens)),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn mul(tokens: &mut Lexer) -> Self {
        let mut lhs = Self::unary(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for mul");
            match next {
                tok if *tok == '*' => {
                    tokens.advance();
                    lhs = Node::Mul {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(Self::unary(tokens)),
                    };
                }
                tok if *tok == '/' => {
                    tokens.advance();
                    lhs = Node::Div {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(Self::unary(tokens)),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn unary(tokens: &mut Lexer) -> Self {
        if consume(tokens, '*') {
            return Node::Deref {
                expr: Kind::make(Self::mul(tokens)),
            };
        }
        Self::term(tokens)
    }

    fn term(tokens: &mut Lexer) -> Self {
        let (pos, next) = tokens.next_token().expect("token for term");
        match next {
            Token::Num(n) => Node::Constant {
                val: *n,
                ty: Type::Int,
            },
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
                args.push(Kind::make(Self::assign(tokens)));
                while consume(tokens, ',') {
                    args.push(Kind::make(Self::assign(tokens)));
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

    fn ty(tokens: &mut Lexer) -> Type {
        let (_pos, ty) = expect_type(tokens, "typename");

        match ty {
            lexer::Type::Int => {
                let mut ty = Type::Int;
                while consume(tokens, '*') {
                    ty = ptr_of(&ty);
                }
                ty
            }
        }
    }
}

#[inline]
fn ptr_of(base: &Type) -> Type {
    Type::Ptr {
        // TODO: will this be a problem with indirection?
        ptr_of: Box::new(base.clone()),
    }
}

#[inline]
fn is_typename(tokens: &mut Lexer) -> bool {
    match tokens.peek() {
        Some((_, Token::Type(_))) => true,
        _ => false,
    }
}

#[inline]
fn consume(tokens: &mut Lexer, tok: impl Into<Token>) -> bool {
    let tok = tok.into();
    match tokens.peek() {
        Some((_, t)) if *t == tok => {
            tokens.advance();
            true
        }
        _ => false,
    }
}

// TODO track col:row + filename
#[inline]
fn expect_token(tokens: &mut Lexer, tok: impl Into<Token>) {
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
#[allow(dead_code)]
fn expect(tokens: &mut Lexer, tok: impl Into<Token>, msg: impl AsRef<str>) -> (usize, Token) {
    use std::mem::discriminant;
    let (pos, next) = tokens.next_token().expect("get next token");
    if discriminant(next) == discriminant(&tok.into()) {
        return (*pos, next.clone());
    }
    let pos = *pos;
    expect_fail(tokens.input_at(0), pos, msg.as_ref());
}

#[inline]
fn expect_type(tokens: &mut Lexer, msg: impl AsRef<str>) -> (usize, lexer::Type) {
    let (pos, next) = tokens.next_token().expect("get next token");
    if let Token::Type(ty) = next {
        return (*pos, ty.clone());
    }
    let pos = *pos;
    expect_fail(tokens.input_at(0), pos, msg.as_ref());
}

#[inline]
fn expect_ident(tokens: &mut Lexer, msg: impl AsRef<str>) -> (usize, String) {
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

pub fn print_ast(ast: &[Node]) {
    fn print(depth: usize, node: &Node) {
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

            LVal { offset } => {
                w!(depth, "LVal -- offset: {}", offset);
            }

            Add { lhs, rhs } => {
                w!(depth, "Add (\n");
                print(depth + 1, kind!(lhs));
                w!(0, ",\n");
                print(depth + 1, kind!(rhs));
                newline();
                w!(depth, ")");
            }

            Sub { lhs, rhs } => {
                w!(depth, "Sub (\n");
                print(depth + 1, kind!(lhs));
                w!(0, ",\n");
                print(depth + 1, kind!(rhs));
                newline();
                w!(depth, ")");
            }

            Mul { lhs, rhs } => {
                w!(depth, "Mul (\n");
                print(depth + 1, kind!(lhs));
                w!(0, ",\n");
                print(depth + 1, kind!(rhs));
                newline();
                w!(depth, ")");
            }

            Div { lhs, rhs } => {
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
