use std::fmt;

/// The AST node for expressions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Number(String),
    String(usize),
    Identifier(String),
    Assign(String, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    IfElse(Box<Expr>, Vec<Expr>, Vec<Expr>),
    WhileLoop(Box<Expr>, Vec<Expr>),
    Call(String, Vec<Expr>),
    GlobalDataAddr(String),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub the_return: String,
    pub stmts: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub functions: Vec<Function>,
    pub strings: Vec<String>,
}

#[derive(Debug, Clone)]
struct Token {
    type_: TokenType,
    lexeme: String,
    line: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokenType {
    Newline,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    Comma,
    Arrow,
    Equal,
    EqualEqual,
    BangEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Plus,
    Minus,
    Star,
    Slash,
    Amp,
    Fn,
    If,
    Else,
    While,
    Number,
    String,
    Ident,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            TokenType::Newline => "newline",
            TokenType::LeftBrace => "'{'",
            TokenType::RightBrace => "'}'",
            TokenType::LeftParen => "'('",
            TokenType::RightParen => "')'",
            TokenType::Comma => "','",
            TokenType::Arrow => "'->'",
            TokenType::Equal => "'='",
            TokenType::EqualEqual => "'=='",
            TokenType::BangEqual => "'!='",
            TokenType::LessThan => "'<'",
            TokenType::LessThanEqual => "'<='",
            TokenType::GreaterThan => "'>'",
            TokenType::GreaterThanEqual => "'>='",
            TokenType::Plus => "'+'",
            TokenType::Minus => "'-'",
            TokenType::Star => "'*'",
            TokenType::Slash => "'/'",
            TokenType::Amp => "'&'",
            TokenType::Fn => "'fn'",
            TokenType::If => "'if'",
            TokenType::Else => "'else'",
            TokenType::While => "'while'",
            TokenType::Number => "number",
            TokenType::String => "string",
            TokenType::Ident => "identifier",
        };
        write!(f, "{}", s)
    }
}

impl TokenType {
    fn precedence(&self) -> Option<usize> {
        match self {
            TokenType::EqualEqual
            | TokenType::BangEqual
            | TokenType::LessThan
            | TokenType::LessThanEqual
            | TokenType::GreaterThan
            | TokenType::GreaterThanEqual => Some(1),

            TokenType::Plus | TokenType::Minus => Some(2),

            TokenType::Star | TokenType::Slash => Some(3),

            _ => None,
        }
    }

    fn binary_expr(&self, lhs: Expr, rhs: Expr) -> Expr {
        let lhs = Box::new(lhs);
        let rhs = Box::new(rhs);
        match self {
            TokenType::EqualEqual => Expr::Eq(lhs, rhs),
            TokenType::BangEqual => Expr::Ne(lhs, rhs),
            TokenType::LessThan => Expr::Lt(lhs, rhs),
            TokenType::LessThanEqual => Expr::Le(lhs, rhs),
            TokenType::GreaterThan => Expr::Gt(lhs, rhs),
            TokenType::GreaterThanEqual => Expr::Ge(lhs, rhs),
            TokenType::Plus => Expr::Add(lhs, rhs),
            TokenType::Minus => Expr::Sub(lhs, rhs),
            TokenType::Star => Expr::Mul(lhs, rhs),
            TokenType::Slash => Expr::Div(lhs, rhs),
            _ => unreachable!(),
        }
    }
}

struct Parser {
    source: Vec<char>,
    current_char: usize,
    line: usize,
    lexeme: String,
    current: Option<Token>,
    strings: Vec<String>,
}

impl Parser {
    fn new(s: &str) -> Result<Self, String> {
        let mut parser = Parser {
            source: s.chars().collect(),
            current_char: 0,
            line: 1,
            lexeme: String::new(),
            current: None,
            strings: Vec::new(),
        };
        parser.next_token()?;
        Ok(parser)
    }

    fn module(&mut self) -> Result<Module, String> {
        let mut functions = Vec::new();

        while self.current.is_some() {
            while self.is_type(TokenType::Newline).unwrap_or(false) {
                self.next_token()?;
            }
            match self.is_type(TokenType::Fn) {
                Ok(true) => functions.push(self.function()?),
                Ok(false) => {
                    return Err(format!(
                        "unexpected token {}",
                        self.current.as_ref().unwrap().lexeme
                    ))
                }
                Err(_) => (),
            }
        }

        Ok(Module {
            functions,
            strings: self.strings.drain(..).collect(),
        })
    }

    fn function(&mut self) -> Result<Function, String> {
        self.expect(TokenType::Fn)?;
        let name = self.expect(TokenType::Ident)?.lexeme;
        self.expect(TokenType::LeftParen)?;

        let mut params = Vec::new();
        loop {
            if self.is_type(TokenType::RightParen)? {
                self.next_token()?;
                break;
            } else {
                let param = self.expect(TokenType::Ident)?.lexeme;
                params.push(param);
                if self.is_type(TokenType::RightParen)? {
                    self.next_token()?;
                    break;
                } else {
                    self.expect(TokenType::Comma)?;
                }
            }
        }

        self.expect(TokenType::Arrow)?;
        self.expect(TokenType::LeftParen)?;
        let the_return = self.expect(TokenType::Ident)?.lexeme;
        self.expect(TokenType::RightParen)?;

        self.expect(TokenType::LeftBrace)?;
        self.expect(TokenType::Newline)?;
        let stmts = self.statements()?;
        self.expect(TokenType::RightBrace)?;
        self.expect(TokenType::Newline)?;

        Ok(Function {
            name,
            params,
            the_return,
            stmts,
        })
    }

    fn statements(&mut self) -> Result<Vec<Expr>, String> {
        let mut exprs = Vec::new();
        while !self.is_type(TokenType::RightBrace)? {
            exprs.push(self.statement()?);
        }
        Ok(exprs)
    }

    fn statement(&mut self) -> Result<Expr, String> {
        let expr = self.expression()?;
        self.expect(TokenType::Newline)?;
        Ok(expr)
    }

    fn expression(&mut self) -> Result<Expr, String> {
        if self.is_type(TokenType::If)? {
            self.next_token()?;
            self.if_else()
        } else if self.is_type(TokenType::While)? {
            self.next_token()?;
            self.while_loop()
        } else {
            self.binary_op(1)
        }
    }

    fn if_else(&mut self) -> Result<Expr, String> {
        let cond = self.expression()?;
        self.expect(TokenType::LeftBrace)?;
        self.expect(TokenType::Newline)?;
        let then_body = self.statements()?;
        self.expect(TokenType::RightBrace)?;
        self.expect(TokenType::Else)?;
        self.expect(TokenType::LeftBrace)?;
        self.expect(TokenType::Newline)?;
        let else_body = self.statements()?;
        self.expect(TokenType::RightBrace)?;
        Ok(Expr::IfElse(Box::new(cond), then_body, else_body))
    }

    fn while_loop(&mut self) -> Result<Expr, String> {
        let cond = self.expression()?;
        self.expect(TokenType::LeftBrace)?;
        self.expect(TokenType::Newline)?;
        let loop_body = self.statements()?;
        self.expect(TokenType::RightBrace)?;
        Ok(Expr::WhileLoop(Box::new(cond), loop_body))
    }

    fn atom(&mut self) -> Result<Expr, String> {
        let curr = self
            .current
            .take()
            .ok_or_else(|| "unexpected EOF".to_owned())?;
        self.next_token()?;
        match curr.type_ {
            TokenType::String => {
                let s = curr.lexeme.trim_matches('"').to_owned();
                let idx = self.strings.len();
                self.strings.push(s);
                Ok(Expr::String(idx))
            }
            TokenType::Number => Ok(Expr::Number(curr.lexeme)),
            TokenType::Amp => {
                let name = self.expect(TokenType::Ident)?.lexeme;
                Ok(Expr::GlobalDataAddr(name))
            }
            TokenType::Ident => {
                if self.is_type(TokenType::LeftParen)? {
                    self.next_token()?;

                    let mut args = Vec::new();
                    loop {
                        if self.is_type(TokenType::RightParen)? {
                            self.next_token()?;
                            break;
                        } else {
                            let arg = self.expression()?;
                            args.push(arg);
                            if self.is_type(TokenType::RightParen)? {
                                self.next_token()?;
                                break;
                            } else {
                                self.expect(TokenType::Comma)?;
                            }
                        }
                    }

                    Ok(Expr::Call(curr.lexeme, args))
                // NOTE: this is kinda brittle, it only works because
                // we can only assign to bare identifiers
                } else if self.is_type(TokenType::Equal)? {
                    self.next_token()?;
                    let rhs = self.expression()?;
                    Ok(Expr::Assign(curr.lexeme, Box::new(rhs)))
                } else {
                    Ok(Expr::Identifier(curr.lexeme))
                }
            }
            _ => Err(format!("unexpected token {}", curr.lexeme)),
        }
    }

    fn binary_op(&mut self, min_prec: usize) -> Result<Expr, String> {
        let mut lhs = self.atom()?;
        loop {
            let maybe_prec = self.current.as_ref().and_then(|t| t.type_.precedence());
            match maybe_prec {
                Some(prec) if prec >= min_prec => {
                    let op = self.current.as_ref().map(|t| t.type_).unwrap();
                    self.next_token()?;
                    let rhs = self.binary_op(prec)?;
                    lhs = op.binary_expr(lhs, rhs);
                }
                Some(_) | None => break,
            }
        }
        Ok(lhs)
    }

    fn expect(&mut self, token_type: TokenType) -> Result<Token, String> {
        if self.is_type(token_type)? {
            let token = self.current.take().unwrap();
            self.next_token()?;
            Ok(token)
        } else {
            Err(format!("expected {}", token_type))
        }
    }

    fn is_type(&self, token_type: TokenType) -> Result<bool, String> {
        match &self.current {
            Some(t) if t.type_ == token_type => Ok(true),
            Some(_) => Ok(false),
            None => Err("unexpected EOF".to_owned()),
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.peek_char();
        if let Some(c2) = c {
            self.lexeme.push(c2);
        }
        self.current_char += 1;
        c
    }

    fn peek_char(&mut self) -> Option<char> {
        self.source.get(self.current_char).copied()
    }

    fn match_or(&mut self, c: char, t1: TokenType, t2: TokenType) -> Result<Option<Token>, String> {
        match self.peek_char() {
            Some(next_char) if next_char == c => {
                self.next_char();
                self.make_token(t1)
            }
            _ => self.make_token(t2),
        }
    }

    fn make_token(&mut self, type_: TokenType) -> Result<Option<Token>, String> {
        Ok(Some(Token {
            type_,
            line: self.line,
            lexeme: self.lexeme.clone(),
        }))
    }

    fn next_token(&mut self) -> Result<(), String> {
        let token = self.lex()?;
        self.current = token;
        Ok(())
    }

    fn lex(&mut self) -> Result<Option<Token>, String> {
        self.lexeme.clear();

        match self.next_char() {
            Some('(') => self.make_token(TokenType::LeftParen),
            Some(')') => self.make_token(TokenType::RightParen),
            Some('{') => self.make_token(TokenType::LeftBrace),
            Some('}') => self.make_token(TokenType::RightBrace),
            Some(',') => self.make_token(TokenType::Comma),
            Some('+') => self.make_token(TokenType::Plus),
            Some('*') => self.make_token(TokenType::Star),
            Some('&') => self.make_token(TokenType::Amp),
            Some('-') => self.match_or('>', TokenType::Arrow, TokenType::Minus),
            Some('=') => self.match_or('=', TokenType::EqualEqual, TokenType::Equal),
            Some('<') => self.match_or('=', TokenType::LessThanEqual, TokenType::LessThan),
            Some('>') => self.match_or('=', TokenType::GreaterThanEqual, TokenType::GreaterThan),

            Some('!') => {
                if let Some('=') = self.peek_char() {
                    self.next_char();
                    self.make_token(TokenType::BangEqual)
                } else {
                    Err("Unexpected character '!'".to_owned())
                }
            }

            Some('/') => {
                if let Some('/') = self.peek_char() {
                    while let Some(c) = self.next_char() {
                        if c == '\n' {
                            break;
                        }
                    }
                    self.lex()
                } else {
                    self.make_token(TokenType::Slash)
                }
            }

            Some('"') => {
                while let Some(c) = self.next_char() {
                    if c == '"' {
                        break;
                    }
                }
                self.make_token(TokenType::String)
            }

            Some(c) if c.is_alphabetic() => {
                while let Some(c) = self.peek_char() {
                    if c.is_alphanumeric() || c == '_' {
                        self.next_char();
                    } else {
                        break;
                    }
                }

                let tt = match &*self.lexeme {
                    "fn" => TokenType::Fn,
                    "if" => TokenType::If,
                    "else" => TokenType::Else,
                    "while" => TokenType::While,
                    _ => TokenType::Ident,
                };
                self.make_token(tt)
            }

            Some(c) if c.is_numeric() => {
                while let Some(c) = self.peek_char() {
                    if c.is_numeric() {
                        self.next_char();
                    } else {
                        break;
                    }
                }
                self.make_token(TokenType::Number)
            }

            Some('\n') => {
                self.line += 1;
                self.make_token(TokenType::Newline)
            }
            Some(c) if c.is_whitespace() => self.lex(),
            Some(c) => Err(format!("Unexpected character '{}'", c)),
            None => Ok(None),
        }
    }
}

pub fn parse(source: &str) -> Result<Module, String> {
    let mut parser = Parser::new(source)?;
    parser.module()
}
