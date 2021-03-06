use super::*;
use kind::Kind;
use node::{Comp, Node};
use span::Span;
use tokens::{Token, Tokens, Type as TokType};
use types::Type;

use std::collections::{HashMap, VecDeque};

pub struct Parser {
    env: VecDeque<Environment>,
}

impl Parser {
    pub fn parse(mut tokens: Tokens) -> Vec<Node> {
        let tokens = &mut tokens;

        let mut env = VecDeque::new();
        env.push_front(Environment::new());

        let mut this = Parser { env };

        let mut nodes = vec![];
        while let Some((_, tok)) = tokens.peek() {
            if *tok == Token::EOF {
                break;
            }
            nodes.push(this.root(tokens))
        }
        nodes
    }

    fn root(&mut self, tokens: &mut Tokens) -> Node {
        let is_typedef = consume(tokens, Token::Typedef);
        let is_extern = consume(tokens, Token::Extern);

        let mut ty = self.decl_specifiers(tokens);
        while consume(tokens, '*') {
            ty = types::ptr_of(&Rc::new(RefCell::new(ty)));
        }

        let name = self.ident(tokens);

        // functions
        if consume(tokens, '(') {
            let mut args = vec![];
            if !consume(tokens, ')') {
                args.push(Kind::make(self.param_decl(tokens)));
                while consume(tokens, ',') {
                    args.push(Kind::make(self.param_decl(tokens)));
                }
                expect(tokens, ')', None);
            }

            expect(tokens, '{', None);
            if is_typedef {
                fail!("typedef {} has function definition", name);
            }

            return Node::Func {
                ty: Rc::new(RefCell::new(ty)),
                body: Kind::make(self.compound(tokens)),
                name,
                args,
                stacksize: 0,
                globals: vec![],
            };
        }

        let ty = self.read_array(tokens, ty);
        expect(tokens, ';', None);
        if is_typedef {
            self.env
                .front_mut()
                .expect("root environment")
                .typedefs
                .insert(name.clone(), ty);

            return Node::Noop {}; // TODO: this should have a better name
        }

        // let data = if is_extern { 0 } else { types::size_of(&ty) };
        Node::Vardef {
            ty: Rc::new(RefCell::new(ty)),
            name,
            init: Kind::empty(),
            offset: 0,
            is_extern,
        }
    }

    fn compound(&mut self, tokens: &mut Tokens) -> Node {
        let env = Environment::new();
        self.env.push_front(env);

        let mut stmts = vec![];
        while !consume(tokens, '}') {
            stmts.push(Kind::make(self.statement(tokens)))
        }

        let old = self.env.pop_front().expect("get first environment");
        self.env.push_back(old);

        Node::Compound { stmts }
    }

    fn statement(&mut self, tokens: &mut Tokens) -> Node {
        let (_pos, next) = tokens.peek().expect("token for statement");

        match next {
            Token::Typedef => {
                tokens.advance();
                let node = self.declaration(tokens);
                if let Node::Vardef { name, ty, .. } = &node {
                    if name.is_empty() {
                        fail!("typename name is empty");
                    }

                    self.env
                        .front_mut()
                        .expect("get first environment")
                        .typedefs
                        .insert(name.clone(), ty.borrow().clone());
                    return node;
                }
                unreachable!();
            }

            Token::Type(TokType::Int) | Token::Type(TokType::Char) | Token::Struct => {
                self.declaration(tokens)
            }

            Token::If => {
                tokens.advance();
                expect(tokens, '(', None);
                let cond = Kind::make(self.expression(tokens));

                expect(tokens, ')', None);
                let body = Kind::make(self.statement(tokens));

                let else_ = if consume(tokens, Token::Else) {
                    Kind::make(self.statement(tokens))
                } else {
                    Kind::empty()
                };

                Node::If { cond, body, else_ }
            }

            Token::For => {
                tokens.advance();
                expect(tokens, '(', None);
                let init = if self.is_typename(tokens) {
                    self.declaration(tokens)
                } else if consume(tokens, ';') {
                    Node::Noop {}
                } else {
                    self.expression_statement(tokens)
                };

                let cond = if !consume(tokens, ';') {
                    let cond = self.expression(tokens);
                    expect(tokens, ';', None);
                    cond
                } else {
                    Node::Noop {}
                };

                let step = if !consume(tokens, ')') {
                    let step = Node::Expression {
                        expr: Kind::make(self.expression(tokens)),
                    };
                    expect(tokens, ')', None);
                    step
                } else {
                    Node::Noop {}
                };

                let body = self.statement(tokens);
                Node::For {
                    init: Kind::make(init),
                    cond: Kind::make(cond),
                    step: Kind::make(step),
                    body: Kind::make(body),
                }
            }

            Token::While => {
                tokens.advance();

                expect(tokens, '(', None);
                let cond = self.expression(tokens);

                expect(tokens, ')', None);
                let body = self.statement(tokens);

                Node::For {
                    init: Kind::empty(),
                    cond: Kind::make(cond),
                    step: Kind::empty(),
                    body: Kind::make(body),
                }
            }

            Token::Do => {
                tokens.advance();
                let body = self.statement(tokens);

                expect(tokens, Token::While, None); // TODO maybe put a message here
                expect(tokens, '(', None);
                let cond = self.expression(tokens);
                expect(tokens, ')', None);
                expect(tokens, ';', None);

                Node::DoWhile {
                    body: Kind::make(body),
                    cond: Kind::make(cond),
                }
            }

            Token::Break => {
                tokens.advance();
                Node::Break {}
            }

            Token::Return => {
                tokens.advance();
                let node = Node::Return {
                    expr: Kind::make(self.expression(tokens)),
                };
                expect(tokens, ';', None);
                node
            }

            tok if *tok == '{' => {
                tokens.advance();
                let mut stmts = vec![];
                while !consume(tokens, '}') {
                    stmts.push(Kind::make(self.statement(tokens)));
                }
                Node::Compound { stmts }
            }
            tok if *tok == ';' => {
                tokens.advance();
                Node::Noop {}
            }
            tok if *tok != Token::EOF => {
                if self.is_typename(tokens) {
                    self.declaration(tokens)
                } else {
                    self.expression_statement(tokens)
                }
            }
            _ => {
                // TODO better msg
                expect_fail(&tokens, &MessageType::User("no token"))
            }
        }
    }

    fn expression_statement(&mut self, tokens: &mut Tokens) -> Node {
        let node = Node::Expression {
            expr: Kind::make(self.expression(tokens)),
        };
        expect(tokens, ';', None);
        node
    }

    fn declaration(&mut self, tokens: &mut Tokens) -> Node {
        let ty = self.decl_specifiers(tokens);
        let node = self.declarator(ty, tokens);
        expect(tokens, ';', None);
        node
    }

    fn param_decl(&mut self, tokens: &mut Tokens) -> Node {
        let ty = self.decl_specifiers(tokens);
        self.declarator(ty, tokens)
    }

    fn declarator(&mut self, mut ty: types::Type, tokens: &mut Tokens) -> Node {
        while consume(tokens, '*') {
            ty = types::ptr_of(&Rc::new(RefCell::new(ty)));
        }
        let direct = self.direct_decl(ty, tokens);
        direct
    }

    // this is a real mess
    fn direct_decl(&mut self, ty: types::Type, tokens: &mut Tokens) -> Node {
        let (name, mut node) = if let Some((_, Token::Ident(name))) = tokens.current() {
            let name = name.clone();
            tokens.advance();
            (Some(name), None)
        } else if consume(tokens, '(') {
            // TODO what type should be here?
            let node = self.declarator(Type::Int {}, tokens);
            expect(tokens, ')', None);
            (None, Some(node))
        } else {
            expect_fail(&tokens, &MessageType::User("bad direct-declarator"))
        };

        let name = match (&node, &name) {
            (_, Some(name)) => name,
            (Some(Node::Vardef { name, .. }), _) => name,
            _ => unreachable!(), // inb4
        };

        let ty = Rc::new(RefCell::new(self.read_array(tokens, ty)));

        // TODO maybe check that the array base type isn't void

        let init = if consume(tokens, '=') {
            Kind::make(self.assign(tokens))
        } else {
            Kind::empty()
        };

        if node.is_none() {
            node = Some(Node::Vardef {
                name: name.clone(),
                init,
                offset: 0,
                ty,
                is_extern: false,
            })
        } else if let Some(Node::Vardef {
            init: ref mut vinit,
            ty: ref mut vty,
            ..
        }) = node
        {
            *vinit = init;
            *vty = ty;
        }

        node.unwrap()
    }

    fn conditional(&mut self, tokens: &mut Tokens) -> Node {
        let cond = self.logor(tokens);
        if !consume(tokens, '?') {
            return cond;
        }
        let then = self.expression(tokens);
        expect(tokens, ':', None);

        let else_ = self.conditional(tokens);
        Node::Conditional {
            cond: Kind::make(cond),
            then: Kind::make(then),
            else_: Kind::make(else_),
        }
    }

    fn expression(&mut self, tokens: &mut Tokens) -> Node {
        let lhs = self.assign(tokens);
        if !consume(tokens, ',') {
            return lhs;
        }

        Node::Comma {
            lhs: Kind::make(lhs),
            rhs: Kind::make(self.expression(tokens)),
        }
    }

    fn assign(&mut self, tokens: &mut Tokens) -> Node {
        let lhs = self.conditional(tokens);

        if consume(tokens, '=') {
            return Node::Assign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.assign(tokens)),
            };
        }

        if consume(tokens, "*=") {
            return Node::MulAssign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.assign(tokens)),
            };
        }

        if consume(tokens, "/=") {
            return Node::DivAssign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.assign(tokens)),
            };
        }

        if consume(tokens, "%=") {
            return Node::ModAssign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.assign(tokens)),
            };
        }

        if consume(tokens, "+=") {
            return Node::AddAssign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.assign(tokens)),
            };
        }

        if consume(tokens, "-=") {
            return Node::SubAssign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.assign(tokens)),
            };
        }

        if consume(tokens, "&=") {
            return Node::AndAssign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.assign(tokens)),
            };
        }

        if consume(tokens, "^=") {
            return Node::XorAssign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.assign(tokens)),
            };
        }

        if consume(tokens, "|=") {
            return Node::OrAssign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.assign(tokens)),
            };
        }

        if consume(tokens, "<<=") {
            return Node::ShlAssign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.assign(tokens)),
            };
        }

        if consume(tokens, ">>=") {
            return Node::ShrAssign {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.assign(tokens)),
            };
        }

        lhs
    }

    fn logor(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.logand(tokens);
        'expr: loop {
            if let Some((_, Token::MChar('|', '|', None))) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogOr {
                    lhs: Kind::make(lhs),
                    rhs: Kind::make(self.logand(tokens)),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn bit_and(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.equality(tokens);
        while let Some((_, t)) = tokens.peek() {
            if *t != '&' {
                return lhs;
            }

            let _ = tokens.next_token();
            lhs = Node::And {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.equality(tokens)),
            };
        }

        unreachable!()
    }

    fn bit_xor(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.bit_and(tokens);
        while let Some((_, t)) = tokens.peek() {
            if *t != '^' {
                return lhs;
            }

            let _ = tokens.next_token();
            lhs = Node::Xor {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.bit_and(tokens)),
            };
        }

        unreachable!()
    }

    fn bit_or(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.bit_xor(tokens);
        while let Some((_, t)) = tokens.peek() {
            if *t != '|' {
                return lhs;
            }

            let _ = tokens.next_token();
            lhs = Node::Or {
                lhs: Kind::make(lhs),
                rhs: Kind::make(self.bit_xor(tokens)),
            };
        }

        unreachable!()
    }

    fn logand(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.bit_or(tokens);
        'expr: loop {
            if let Some((_, Token::MChar('&', '&', None))) = tokens.peek() {
                tokens.advance();
                lhs = Node::LogAnd {
                    lhs: Kind::make(lhs),
                    rhs: Kind::make(self.bit_or(tokens)),
                };
            } else {
                break 'expr;
            }
        }
        lhs
    }

    fn equality(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.relational(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for equality");
            match next {
                Token::MChar('=', '=', None) => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.relational(tokens)),
                        comp: Comp::Equal,
                    };
                }
                Token::MChar('!', '=', None) => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.relational(tokens)),
                        comp: Comp::NotEqual,
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn shift(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.add(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for shift");
            match next {
                tok if *tok == Token::MChar('<', '<', None) => {
                    tokens.advance();
                    lhs = Node::Shl {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.add(tokens)),
                    };
                }
                tok if *tok == Token::MChar('>', '>', None) => {
                    tokens.advance();
                    lhs = Node::Shr {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.add(tokens)),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn relational(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.shift(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for relational");
            match next {
                tok if *tok == '<' => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.shift(tokens)),
                        comp: Comp::LessThan,
                    };
                }
                tok if *tok == '>' => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: Kind::make(self.shift(tokens)),
                        rhs: Kind::make(lhs),
                        comp: Comp::GreaterThan,
                    };
                }
                tok if *tok == Token::MChar('<', '=', None) => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.shift(tokens)),
                        comp: Comp::LessThanEq,
                    };
                }
                tok if *tok == Token::MChar('>', '=', None) => {
                    tokens.advance();
                    lhs = Node::Comparison {
                        lhs: Kind::make(self.shift(tokens)),
                        rhs: Kind::make(lhs),
                        comp: Comp::GreaterThanEq,
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn add(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.multiply(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for add");
            match next {
                tok if *tok == '+' => {
                    tokens.advance();
                    lhs = Node::Add {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.multiply(tokens)),
                    };
                }
                tok if *tok == '-' => {
                    tokens.advance();
                    lhs = Node::Sub {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.multiply(tokens)),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn multiply(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.unary(tokens);
        'expr: loop {
            let (_, next) = tokens.peek().expect("token for mul");
            match next {
                tok if *tok == '*' => {
                    tokens.advance();
                    lhs = Node::Mul {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.unary(tokens)),
                    };
                }
                tok if *tok == '/' => {
                    tokens.advance();
                    lhs = Node::Div {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.unary(tokens)),
                    };
                }
                tok if *tok == '%' => {
                    tokens.advance();
                    lhs = Node::Mod {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.unary(tokens)),
                    };
                }
                _ => break 'expr,
            }
        }
        lhs
    }

    fn unary(&mut self, tokens: &mut Tokens) -> Node {
        if consume(tokens, '-') {
            return Node::Neg {
                expr: Kind::make(self.unary(tokens)),
            };
        }
        if consume(tokens, '*') {
            return Node::Deref {
                expr: Kind::make(self.unary(tokens)),
            };
        }
        if consume(tokens, '&') {
            return Node::Addr {
                expr: Kind::make(self.unary(tokens)),
                ty: Rc::new(RefCell::new(Type::Int)), // TODO: ?? what to do here
            };
        }
        if consume(tokens, '!') {
            return Node::Not {
                expr: Kind::make(self.unary(tokens)),
            };
        }
        if consume(tokens, '~') {
            return Node::BNot {
                expr: Kind::make(self.unary(tokens)),
            };
        }
        if consume(tokens, Token::Sizeof) {
            return Node::Sizeof {
                expr: Kind::make(self.unary(tokens)),
            };
        }
        if consume(tokens, "--") {
            return Node::PreDec {
                expr: Kind::make(self.unary(tokens)),
            };
        }

        if consume(tokens, "++") {
            return Node::PreInc {
                expr: Kind::make(self.unary(tokens)),
            };
        }
        if consume(tokens, Token::Alignof) {
            return Node::Alignof {
                expr: Kind::make(self.unary(tokens)),
            };
        }
        self.postfix(tokens)
    }

    fn primary(&mut self, tokens: &mut Tokens) -> Node {
        let (_pos, next) = tokens.next_token().expect("token for term");
        match next {
            tok if *tok == '(' => {
                if consume(tokens, '{') {
                    let stmt = self.compound(tokens);
                    expect(tokens, ')', None);
                    return Node::Statement {
                        stmt: Kind::make(stmt),
                        ty: Rc::new(RefCell::new(Type::Int)),
                    };
                }
                let node = self.expression(tokens);
                expect(tokens, ')', None);
                node
            }

            Token::Num(n) => Node::Constant {
                val: *n,
                ty: Rc::new(RefCell::new(Type::Int)),
            },

            Token::Str(s) => Node::Str {
                str: s.to_string(),
                ty: Rc::new(RefCell::new(types::array_of(
                    &Rc::new(RefCell::new(Type::Char)),
                    // TODO: handle this in Types
                    s.len() + 1, // count the \0
                ))),
            },

            Token::Ident(ref name) => {
                let name = name.to_string();
                if !consume(tokens, '(') {
                    return Node::Ident { name };
                }

                if consume(tokens, ')') {
                    return Node::Call { name, args: vec![] };
                }

                let mut args = vec![];
                args.push(Kind::make(self.assign(tokens)));
                while consume(tokens, ',') {
                    args.push(Kind::make(self.assign(tokens)));
                }
                expect(tokens, ')', None);
                Node::Call { name, args }
            }
            // TODO: better message
            _ => expect_fail(&tokens, &MessageType::User("number or ident")),
        }
    }

    fn postfix(&mut self, tokens: &mut Tokens) -> Node {
        let mut lhs = self.primary(tokens);
        loop {
            if consume(tokens, "++") {
                lhs = Node::PostInc {
                    expr: Kind::make(lhs),
                };
                continue;
            }

            if consume(tokens, "--") {
                lhs = Node::PostDec {
                    expr: Kind::make(lhs),
                };
                continue;
            }

            if consume(tokens, '.') {
                lhs = Node::Dot {
                    offset: 0,
                    expr: Kind::make(lhs),
                    name: self.ident(tokens),
                };
                continue;
            }

            if consume(tokens, "->") {
                lhs = Node::Dot {
                    offset: 0,
                    expr: Kind::make(Node::Deref {
                        expr: Kind::make(lhs),
                    }),
                    name: self.ident(tokens),
                };
                continue;
            }

            if consume(tokens, '[') {
                lhs = Node::Deref {
                    expr: Kind::make(Node::Add {
                        lhs: Kind::make(lhs),
                        rhs: Kind::make(self.assign(tokens)),
                    }),
                };
                expect(tokens, ']', None);
                continue;
            }
            return lhs;
        }
    }

    fn read_array(&mut self, tokens: &mut Tokens, ty: Type) -> Type {
        let mut param = vec![];
        while consume(tokens, '[') {
            match self.expression(tokens) {
                Node::Constant { val, .. } => param.push(val),
                _ => {
                    // TODO better msg
                    expect_fail(&tokens, &MessageType::User("number"));
                }
            };
            expect(tokens, ']', None);
        }

        let mut ty = ty;
        for el in param.iter().rev() {
            ty = types::array_of(&Rc::new(RefCell::new(ty)), *el as usize);
        }
        ty
    }

    fn ident(&mut self, tokens: &mut Tokens) -> String {
        if let (_, Token::Ident(name)) =
            expect_discriminant(tokens, Token::Ident("".into()), Some("identifier"))
        {
            return name;
        }
        unreachable!()
    }

    // TODO split this up
    fn decl_specifiers(&mut self, tokens: &mut Tokens) -> Type {
        // TODO get rid of this
        let (_pos, token) = expect_tokens(
            tokens,
            &[
                Token::Type(TokType::Char),
                Token::Type(TokType::Int),
                Token::Type(TokType::Void),
                Token::Ident("".into()),
                Token::Struct,
            ],
        );

        match token {
            Token::Type(ty) => match ty {
                TokType::Char => Type::Char,
                TokType::Int => Type::Int,
                TokType::Void => Type::Void,
            },
            Token::Ident(s) => match self.find(&s, &FieldType::Typedef) {
                Some(ty) => ty,
                None => {
                    expect_fail(tokens, &MessageType::User(&format!("{}: unknown type", s)));
                }
            },
            Token::Struct => {
                let tag = if let Some((_, Token::Ident(s))) = tokens.peek() {
                    let s = Some(s.clone());
                    tokens.advance();
                    s
                } else {
                    None
                };

                let members = if consume(tokens, '{') {
                    let mut v = vec![];
                    while !consume(tokens, '}') {
                        v.push(self.declaration(tokens));
                    }
                    Some(v)
                } else {
                    None
                };

                if tag.is_none() && members.is_none() {
                    fail!("bad struct definition");
                }

                let mut ty = if tag.is_some() && members.is_none() {
                    self.find(tag.as_ref().unwrap().as_str(), &FieldType::Tag)
                } else {
                    None
                };

                if ty.is_none() {
                    ty = Some(Type::Struct {
                        size: 0,
                        align: 0,
                        members: vec![],
                    })
                }

                if members.is_some() {
                    // add member
                    ty.as_mut().unwrap().add_members(&members.as_ref().unwrap());
                    if tag.is_some() {
                        self.env
                            .front_mut()
                            .expect("to get first environment")
                            .tags
                            .insert(tag.clone().unwrap(), ty.clone().unwrap());
                    }
                }

                ty.unwrap()
            }
            _ => unreachable!(),
        }
    }

    fn is_typename(&self, tokens: &mut Tokens) -> bool {
        match tokens.peek() {
            Some((_, Token::Type(_))) | Some((_, Token::Struct)) => true,
            Some((_, Token::Ident(n))) => self.find(n, &FieldType::Typedef).is_some(),
            _ => false,
        }
    }

    fn find(&self, name: &str, ty: &FieldType) -> Option<Type> {
        for env in &self.env {
            match ty {
                FieldType::Typedef => match env.typedefs.get(name) {
                    Some(td) => return Some(td.clone()),
                    None => continue,
                },
                FieldType::Tag => match env.tags.get(name) {
                    Some(tag) => return Some(tag.clone()),
                    None => continue,
                },
            }
        }

        None
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

#[inline]
fn expect_tokens(tokens: &mut Tokens, toks: &[Token]) -> (Span, Token) {
    use std::mem::discriminant;
    let (pos, next) = tokens.next_token().expect("get next token");
    for tok in toks {
        if discriminant(next) == discriminant(tok) {
            return (pos.clone(), next.clone());
        }
    }

    const SOURCE_LINE: &str = "source=> ";
    let (pos, next) = (pos, next.clone());
    // TODO fix this
    let (input, adjusted) = midpoint("", 0, 80 - SOURCE_LINE.len());

    eprintln!("{}{}", wrap_color!(Color::Yellow {}, SOURCE_LINE), input);
    draw_caret(SOURCE_LINE.len() + adjusted, Color::Red {});

    let mut out = String::new();
    for (i, s) in toks
        .iter()
        .map(|t| match t {
            Token::Char(c) => c.to_string(),
            tok => format!("{}", tok),
        }).enumerate()
    {
        out.push_str(&s);
        if i < toks.len() {
            out.push_str(", ");
        }
    }

    fail!(
        "{} one of {} was expected. found {} at: {}.\n",
        wrap_color!(Color::Red {}, "ERROR:"),
        wrap_color!(Color::Green {}, out),
        wrap_color!(Color::Cyan {}, "{:?}", next),
        wrap_color!(Color::Blue {}, pos),
    );
}

enum MessageType<'a> {
    User(&'a str),
    Token(Token),
    None,
}

#[inline]
fn expect(tokens: &mut Tokens, tok: impl Into<Token>, msg: Option<&str>) -> (Span, Token) {
    let tok = tok.into();

    let (pos, next) = tokens.next_token().expect("get next token");
    if *next == tok {
        return (pos.clone(), next.clone());
    }

    match msg {
        Some(msg) => expect_fail(&tokens, &MessageType::User(msg)),
        None => expect_fail(&tokens, &MessageType::Token(tok)),
    }
}

#[inline]
fn expect_discriminant(
    tokens: &mut Tokens,
    tok: impl Into<Token>,
    msg: Option<&str>,
) -> (Span, Token) {
    use std::mem::discriminant;

    let tok = tok.into();
    if let Some((pos, next)) = tokens.next_token() {
        if discriminant(next) == discriminant(&tok) {
            return (pos.clone(), next.clone());
        }
    }

    match msg {
        Some(msg) => expect_fail(&tokens, &MessageType::User(msg)),
        None => expect_fail(&tokens, &MessageType::Token(tok)),
    }
}

fn expect_fail(tokens: &Tokens, msg: &MessageType) -> ! {
    const SOURCE_LINE: &str = "source=> ";

    let span = tokens.current_span();
    let prev = tokens.previous_span();

    let prev = match prev {
        Some(prev) => prev,
        None => span,
    };

    let input = tokens.input_at(prev);
    let (input, adjusted) = midpoint(&input, prev.column() - 1, 80 - SOURCE_LINE.len());

    eprintln!("{}{}", wrap_color!(Color::Yellow {}, SOURCE_LINE), input);
    draw_caret(SOURCE_LINE.len() + adjusted, Color::Red {});

    match msg {
        MessageType::User(msg) => {
            fail!(
                "{} {} at: {}.\n",
                wrap_color!(Color::Red {}, "ERROR:"),
                wrap_color!(Color::Cyan {}, msg),
                wrap_color!(Color::Blue {}, prev),
            );
        }
        MessageType::Token(tok) => {
            fail!(
                "{} {} was expected at: {}.\n",
                wrap_color!(Color::Red {}, "ERROR:"),
                wrap_color!(Color::Cyan {}, tok.as_string()),
                wrap_color!(Color::Blue {}, prev),
            );
        }
        MessageType::None => {
            fail!(
                "{} unknown error at: {}.\n",
                wrap_color!(Color::Red {}, "ERROR:"),
                wrap_color!(Color::Blue {}, prev),
            );
        }
    }
}

// TODO refactor this and semantics environment to be a generic environment type
#[derive(Debug, Clone)]
struct Environment {
    tags: HashMap<String, Type>,
    typedefs: HashMap<String, Type>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            tags: HashMap::new(),
            typedefs: HashMap::new(),
        }
    }
}

enum FieldType {
    Typedef,
    Tag,
}
