use std::{
    array,
    collections::HashMap,
    iter::Peekable,
    mem,
    str::{CharIndices, FromStr},
};

use crate::{
    chunk::{Chunk, OpCode, Operand},
    string_pool::{InternedString, StringPool},
    value::Value,
};

const MAX_LOCAL_SIZE: usize = u8::MAX as usize + 1;

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    previous: Token<'a>,
    current: Token<'a>,
    had_error: bool,
    // 恐慌模式,避免级联错误
    panic_mode: bool,
    compiling_chunk: &'a mut Chunk,
    rules: HashMap<TokenType, ParseRule<'a>>,
    strings: StringPool,
    compiler: Compiler<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, chunk: &'a mut Chunk) -> Self {
        Parser {
            scanner: Scanner::init_scanner(source),
            previous: Default::default(),
            current: Default::default(),
            had_error: false,
            panic_mode: false,
            compiling_chunk: chunk,
            rules: ParseRule::init_rules(),
            strings: StringPool::default(),
            compiler: Compiler::new(),
        }
    }

    fn intern_interned(&mut self, s: &str) -> InternedString {
        self.strings.intern(s)
    }

    pub fn compile(&mut self) -> bool {
        self.advance();
        while !self.match_token(TokenType::Eof) {
            self.declaration();
        }
        self.end_compiler();
        !self.had_error
    }

    /// 解析声明
    fn declaration(&mut self) {
        if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }
        if self.panic_mode {
            self.synchronize();
        }
    }

    /// 解析变量声明
    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.match_token(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_op_code(OpCode::Nil);
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    /// 定义变量
    fn define_variable(&mut self, variable: Operand) {
        // 此刻,虚拟机已经执行了变量初始化表达式的代码(如果用户省略了初始化则默认为`nil`),
        // 并且该值作为唯一保留的临时变量位于栈顶. 另外我们还知道新的局部变量会被分配到栈顶,
        // 因此,没什么可做的.临时变量直接成为局部变量.
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_op_code_and_operand(OpCode::DefineGlobal, variable);
    }

    fn mark_initialized(&mut self) {
        self.compiler.locals[self.compiler.local_count - 1].depth = self.compiler.scope_depth
    }

    /// 解析变量名
    fn parse_variable(&mut self, error_message: &str) -> Operand {
        self.consume(TokenType::Identifier, error_message);
        self.declare_variable();
        // 如果当前在局部作用域中就退出函数。
        // 在运行时，不会通过名称查询局部变量。
        // 不需要将变量的名称放入常量表中，所以如果声明在局部作用域内，则返回一个假的表索引
        if self.compiler.scope_depth > 0 {
            return Operand::U8(0);
        }
        let previous = &self.previous.clone();
        self.identifier_constant(previous)
    }

    fn identifier_constant(&mut self, identifier: &Token) -> Operand {
        let s = self.intern_interned(identifier.lexeme);
        self.make_constant(Value::String(s))
    }

    /// 声明局部变量(记录变量的存在)
    fn declare_variable(&mut self) {
        // 编译器只记录局部变量.因此如果在顶层全局作用域中就直接退出
        // 因为全局变量是后期绑定的, 所以编译器不会跟踪它所看到的关于全局变量的声明
        if self.compiler.scope_depth == 0 {
            return;
        }

        let name = &self.previous.clone();

        // 检测同一作用域两个同名变量的情况
        if self.compiler.local_count > 0 {
            for index in (0..self.compiler.local_count).rev() {
                let local = &self.compiler.locals[index];
                if local.depth != -1 && local.depth < self.compiler.scope_depth {
                    break;
                }
                if Self::identifiers_equal(name, &local.name) {
                    self.error_at(
                        name.clone(),
                        "Already a variable with this name in this scope.",
                    );
                }
            }
        }

        self.add_local(name.clone());
    }

    fn identifiers_equal(a: &Token, b: &Token) -> bool {
        a.lexeme == b.lexeme
    }

    fn add_local(&mut self, name: Token<'a>) {
        if self.compiler.local_count >= MAX_LOCAL_SIZE {
            self.error_at(name, "Too many local variables in function.");
            return;
        }
        self.compiler.locals[self.compiler.local_count] = Local { name, depth: -1 };
        self.compiler.local_count += 1;
    }

    /// 跳过标识,直到遇到语句边界
    /// 语句边界包括可以结束一条语句的前驱标识,如分号;
    /// 或者是能开始一条语句的后续标识,例如控制流或声明语句的关键字之一
    fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.current.token_type != TokenType::Eof {
            if self.previous.token_type == TokenType::Semicolon {
                return;
            }
            match self.current.token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => {
                    return;
                }
                _ => {}
            }
            self.advance();
        }
    }

    /// 解析语句
    fn statement(&mut self) {
        if self.match_token(TokenType::Print) {
            self.print_statement();
        } else if self.match_token(TokenType::If) {
            self.if_statement();
        } else if self.match_token(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    /// 解析if语句
    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_op_code(OpCode::Pop);
        self.statement();
        let else_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(then_jump);
        self.emit_op_code(OpCode::Pop);
        if self.match_token(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn patch_jump(&mut self, offset: usize) {
        // 用`current_chunk().code_len()`拿到"跳转目标的位置"(then分支之后的指令)
        // 用`offset+2`拿到"跳转指令的下一条指令的位置"
        // 两者的差值就是"ip需要跳过的字节数",也就是最终要回填的偏移量
        let jump = self.current_chunk().code_len() - offset - 2;

        if jump > u16::MAX as usize {
            self.error_at_current("Too much code to jump over.");
        }

        self.current_chunk()
            .replace_operand_by_index(offset, Operand::U16(jump as u16));
    }

    fn emit_jump(&mut self, op: OpCode) -> usize {
        self.emit_op_code(op);
        self.emit_placeholder();
        self.emit_placeholder();

        self.current_chunk().code_len() - 2
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;
        let mut popn = 0;
        while self.compiler.local_count > 0
            && self.compiler.locals[self.compiler.local_count - 1].depth > self.compiler.scope_depth
        {
            // 局部变量存储在栈上,因此在退出作用域时需要丢弃它
            popn += 1;
            // 通过简单的递减数组长度来丢弃刚刚离开的作用域上的变量
            self.compiler.local_count -= 1;
        }
        if popn > 0 {
            self.emit_op_code_and_operand(OpCode::Popn, Operand::U8(popn));
        }
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    /// 解析表达式语句(一个表达式后面跟着一个分号)
    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_op_code(OpCode::Pop);
    }

    /// 如果当前标识是指定类型,就消耗该标识并返回true,否则就不处理并返回false
    fn match_token(&mut self, token_type: TokenType) -> bool {
        if !self.check(token_type) {
            return false;
        }
        self.advance();
        true
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.current.token_type == token_type
    }

    /// 解析打印语句
    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_op_code(OpCode::Print);
    }

    /// 解析表达式
    fn expression(&mut self) {
        // 解析最低优先级及更高优先级的表达式
        self.parse_precedence(Precedence::Assignment);
    }

    /// 推进解析器到下一个token，并处理可能的扫描错误
    ///
    /// 该方法会将当前token移动到previous中，然后从扫描器获取下一个有效token。
    /// 如果扫描到错误token(TokenType::Error)，会持续获取下一个token直到得到有效token，
    /// 并在过程中记录每个错误token的错误信息。
    fn advance(&mut self) {
        self.previous = mem::take(&mut self.current);

        loop {
            self.current = self.scanner.scan_token();
            if self.current.token_type != TokenType::Error {
                break;
            }
            self.error_at_current(self.current.lexeme);
        }
    }

    /// 如果current具有预期的token type则读取下一个标识,否则报告错误
    fn consume(&mut self, token_type: TokenType, message: &str) {
        if self.current.token_type == token_type {
            self.advance();
            return;
        }

        self.error_at_current(message);
    }

    fn emit_op_code(&mut self, op_code: OpCode) {
        let line = self.previous.line;
        self.current_chunk().write_chunk_op_code(op_code, line);
    }

    fn emit_placeholder(&mut self) {
        let line = self.previous.line;
        self.current_chunk().write_chunk_placeholder(line);
    }

    fn emit_op_codes(&mut self, op_code_a: OpCode, op_code_b: OpCode) {
        self.emit_op_code(op_code_a);
        self.emit_op_code(op_code_b);
    }

    fn emit_op_code_and_operand(&mut self, op_code: OpCode, operand: Operand) {
        self.emit_op_code(op_code);
        self.emit_operand(operand);
    }

    fn emit_operand(&mut self, operand: Operand) {
        let line = self.previous.line;
        self.current_chunk().write_chunk_operand(operand, line);
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        self.compiling_chunk
    }

    fn error_at_previous(&mut self, message: &str) {
        self.error_at(self.previous.clone(), message);
    }

    /// 在当前位置报告错误
    fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current.clone(), message);
    }

    /// 在token处报告错误
    fn error_at(&mut self, token: Token, message: &str) {
        if self.panic_mode {
            return;
        }

        self.panic_mode = true;

        eprint!("[line {}] Error", token.line);

        if token.token_type == TokenType::Eof {
            eprint!(" at end");
        } else if token.token_type == TokenType::Error {
            // Nothing
        } else {
            eprint!(" at '{}'", &token.lexeme);
        }

        eprintln!(": {message}");
        self.had_error = true;
    }

    fn end_compiler(&mut self) {
        self.emit_return();
        #[cfg(feature = "debug_trace_execution")]
        {
            if !self.had_error {
                use crate::debug::disassemble_chunk;
                disassemble_chunk(self.current_chunk(), "code");
            }
        }
    }

    fn emit_return(&mut self) {
        self.emit_op_code(OpCode::Return);
    }

    fn emit_constant<T: Into<Value>>(&mut self, value: T) {
        let operand = self.make_constant(value);
        self.emit_op_code_and_operand(OpCode::Constant, operand);
    }

    fn make_constant<T: Into<Value>>(&mut self, value: T) -> Operand {
        let constant = self.current_chunk().add_constant(value);
        if constant > u8::MAX as usize {
            self.error_at_previous("Too many constants in one chunk.");
            return Operand::None;
        }

        Operand::U8(constant as u8)
    }

    /// 解析数值
    fn number(&mut self, _can_assign: bool) {
        let value = self.previous.lexeme.parse::<f64>();
        match value {
            Ok(v) => self.emit_constant(v),
            Err(_) => {
                self.error_at_previous("Invalid number literal.");
            }
        }
    }

    /// 解析小括号
    /// 括号表达式的唯一作用是语法上的: 允许在需要高优先级的地方插入一个低优先级的表达式.
    /// 因此,它本身没有运行时语法,也就不需要发出如何字节码.
    /// 对expression()的内部调用负责为括号内的表达式生成字节码
    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    /// 解析一元表达式
    fn unary(&mut self, _can_assign: bool) {
        let operator_type = self.previous.token_type.clone();

        // 解析优先级大于等于unary的表达式
        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => self.emit_op_code(OpCode::Negate),
            TokenType::Bang => self.emit_op_code(OpCode::Not),
            _ => {}
        }
    }

    /// 解析二元操作符
    fn binary(&mut self, _can_assign: bool) {
        let operator_type = self.previous.token_type.clone();
        let parse_rule = match self.get_rule(&operator_type) {
            Some(parse_rule) => parse_rule,
            None => {
                self.error_at_previous(&format!("Cannot find {operator_type:?} parse rule"));
                return;
            }
        };

        // 因为我们的二元操作符是左结合的,所以使用比当前操作符高一优先级的操作数
        // 如果操作符是右结合的,应该使用与当前操作符相同的优先级来调用parse_precedence
        let higher_precedence = match parse_rule.precedence.higher() {
            Some(hp) => hp,
            None => {
                self.error_at_previous("No higher precedence");
                return;
            }
        };
        self.parse_precedence(higher_precedence);

        match operator_type {
            TokenType::Plus => self.emit_op_code(OpCode::Add),
            TokenType::Minus => self.emit_op_code(OpCode::Subtract),
            TokenType::Star => self.emit_op_code(OpCode::Multiply),
            TokenType::Slash => self.emit_op_code(OpCode::Divide),
            TokenType::BangEqual => self.emit_op_codes(OpCode::Equal, OpCode::Not),
            TokenType::EqualEqual => self.emit_op_code(OpCode::Equal),
            TokenType::Greater => self.emit_op_code(OpCode::Gerater),
            TokenType::GreaterEqual => self.emit_op_codes(OpCode::Less, OpCode::Not),
            TokenType::Less => self.emit_op_code(OpCode::Less),
            TokenType::LessEqual => self.emit_op_codes(OpCode::Gerater, OpCode::Not),
            _ => {}
        }
    }

    /// 从当前开始解析给定优先级或更高优先级的任何表达式
    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let can_assign = precedence <= Precedence::Assignment;

        // 为当前标识查找对应的前缀解析器
        // 根据定义，第一个标识总是属于某种前缀表达式。它可能作为一个操作数嵌套在一个或多个中缀表达式中，
        // 但是当我们从左到右阅读代码时，碰到的第一个标识总是属于一个前缀表达式。
        match self
            .get_rule(&self.previous.token_type)
            .and_then(|rule| rule.prefix)
        {
            Some(prefix_fn) => {
                prefix_fn(self, can_assign);
            }
            None => {
                self.error_at_previous("Expect expression.");
                return;
            }
        };

        // 为前缀表达式的下一个标识寻找中缀解析器
        while self
            .get_rule(&self.current.token_type)
            // 该中缀解析器的优先级应当大于等于指定的precedence
            .filter(|rule| rule.precedence >= precedence)
            .is_some()
        {
            // 如果找得到,我们推进到下一个token,新得到的token是中缀表达式的右操作数
            self.advance();
            match self
                // 获取前一个token(由于使用advance推进了,所有起始就是刚刚while判断中使用的current)的中缀解析函数并执行
                .get_rule(&self.previous.token_type)
                .and_then(|rule| rule.infix)
            {
                Some(infix_fn) => {
                    infix_fn(self, can_assign);
                    if can_assign && self.match_token(TokenType::Equal) {
                        self.error_at_current("Invalid assignment target.");
                    }
                }
                None => {
                    self.error_at_previous("Expect expression.");
                    return;
                }
            }
        }
    }

    fn string(&mut self, _can_assign: bool) {
        let value = self
            .previous
            .lexeme
            .parse::<String>()
            .and_then(|s| String::from_str(s.trim_start_matches('"').trim_end_matches('"')));
        match value {
            Ok(v) => {
                let s = self.intern_interned(&v);
                self.emit_constant(Value::String(s));
            }
            Err(_) => {
                self.error_at_previous("Invalid string literal.");
            }
        }
    }

    /// 解析字面量
    fn literal(&mut self, _can_assign: bool) {
        match self.previous.token_type {
            TokenType::True => self.emit_op_code(OpCode::True),
            TokenType::False => self.emit_op_code(OpCode::False),
            TokenType::Nil => self.emit_op_code(OpCode::Nil),
            _ => {}
        }
    }

    fn get_rule(&self, operator_type: &TokenType) -> Option<&ParseRule<'a>> {
        self.rules.get(operator_type)
    }

    fn variable(&mut self, can_assign: bool) {
        let name = &self.previous.clone();
        self.named_variable(name, can_assign);
    }

    fn named_variable(&mut self, name: &Token, can_assign: bool) {
        let get_op;
        let set_op;

        let mut arg = self.resolve_local(name);
        if let Operand::None = arg {
            arg = self.identifier_constant(name);
            get_op = OpCode::GetGlobal;
            set_op = OpCode::SetGlobal;
        } else {
            get_op = OpCode::GetLocal;
            set_op = OpCode::SetLocal;
        }

        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            self.emit_op_code_and_operand(set_op, arg);
        } else {
            self.emit_op_code_and_operand(get_op, arg);
        }
    }

    fn resolve_local(&mut self, name: &Token) -> Operand {
        for index in (0..self.compiler.local_count).rev() {
            let local = &self.compiler.locals[index];
            if Self::identifiers_equal(name, &local.name) {
                if local.depth == -1 {
                    self.error_at(
                        name.to_owned(),
                        "Can't read local variable in its own initializer.",
                    );
                }
                return Operand::U8(index as u8);
            }
        }
        Operand::None
    }
}

/// 解析规则,其被映射到一个类型标识
/// ParseRule 仅为需要参与表达式解析的 TokenType 定义，
/// 其他 TokenType 因不涉及表达式的递归解析而无需具体规则
struct ParseRule<'a> {
    // 编译以该类型标识为起点的前缀表达式的函数
    prefix: Option<ParseFn<'a>>,
    // 编译一个左操作数后跟该类型标识的中缀表达式的函数
    infix: Option<ParseFn<'a>>,
    // 使用该标识作为操作符的中缀表达式的优先级
    precedence: Precedence,
}

type ParseFn<'a> = fn(&mut Parser<'a>, can_assgin: bool);

impl<'a> Default for ParseRule<'a> {
    fn default() -> Self {
        Self {
            prefix: Default::default(),
            infix: Default::default(),
            precedence: Precedence::None,
        }
    }
}

impl<'a> ParseRule<'a> {
    fn new(
        prefix: Option<ParseFn<'a>>,
        infix: Option<ParseFn<'a>>,
        precedence: Precedence,
    ) -> Self {
        ParseRule {
            prefix,
            infix,
            precedence,
        }
    }

    fn init_rules() -> HashMap<TokenType, ParseRule<'a>> {
        let mut rules = HashMap::new();
        rules.insert(
            TokenType::LeftParen,
            Self::new(Some(Parser::grouping), None, Precedence::None),
        );
        rules.insert(
            TokenType::RightParen,
            Self::new(None, None, Precedence::None),
        );
        rules.insert(
            TokenType::LeftBrace,
            Self::new(None, None, Precedence::None),
        );
        rules.insert(
            TokenType::RightBrace,
            Self::new(None, None, Precedence::None),
        );
        rules.insert(TokenType::Comma, Self::new(None, None, Precedence::None));
        rules.insert(TokenType::Dot, Self::new(None, None, Precedence::None));
        rules.insert(
            TokenType::Minus,
            Self::new(Some(Parser::unary), Some(Parser::binary), Precedence::Term),
        );
        rules.insert(
            TokenType::Plus,
            Self::new(None, Some(Parser::binary), Precedence::Term),
        );
        rules.insert(
            TokenType::Semicolon,
            Self::new(None, None, Precedence::None),
        );
        rules.insert(
            TokenType::Slash,
            Self::new(None, Some(Parser::binary), Precedence::Factor),
        );
        rules.insert(
            TokenType::Star,
            Self::new(None, Some(Parser::binary), Precedence::Factor),
        );
        rules.insert(
            TokenType::Bang,
            Self::new(Some(Parser::unary), None, Precedence::None),
        );
        rules.insert(
            TokenType::BangEqual,
            Self::new(None, Some(Parser::binary), Precedence::Equality),
        );
        rules.insert(TokenType::Equal, ParseRule::default());
        rules.insert(
            TokenType::EqualEqual,
            Self::new(None, Some(Parser::binary), Precedence::Equality),
        );
        rules.insert(
            TokenType::Greater,
            Self::new(None, Some(Parser::binary), Precedence::Comparison),
        );
        rules.insert(
            TokenType::GreaterEqual,
            Self::new(None, Some(Parser::binary), Precedence::Comparison),
        );
        rules.insert(
            TokenType::Less,
            Self::new(None, Some(Parser::binary), Precedence::Comparison),
        );
        rules.insert(
            TokenType::LessEqual,
            Self::new(None, Some(Parser::binary), Precedence::Comparison),
        );
        rules.insert(
            TokenType::Identifier,
            Self::new(Some(Parser::variable), None, Precedence::None),
        );
        rules.insert(
            TokenType::String,
            Self::new(Some(Parser::string), None, Precedence::None),
        );
        rules.insert(
            TokenType::Number,
            Self::new(Some(Parser::number), None, Precedence::None),
        );
        rules.insert(TokenType::And, ParseRule::default());
        rules.insert(TokenType::Class, ParseRule::default());
        rules.insert(TokenType::Else, ParseRule::default());
        rules.insert(
            TokenType::False,
            Self::new(Some(Parser::literal), None, Precedence::None),
        );
        rules.insert(TokenType::For, ParseRule::default());
        rules.insert(TokenType::Fun, ParseRule::default());
        rules.insert(TokenType::If, ParseRule::default());
        rules.insert(
            TokenType::Nil,
            Self::new(Some(Parser::literal), None, Precedence::None),
        );
        rules.insert(TokenType::Or, ParseRule::default());
        rules.insert(TokenType::Print, ParseRule::default());
        rules.insert(TokenType::Return, ParseRule::default());
        rules.insert(TokenType::Super, ParseRule::default());
        rules.insert(TokenType::This, ParseRule::default());
        rules.insert(
            TokenType::True,
            Self::new(Some(Parser::literal), None, Precedence::None),
        );
        rules.insert(TokenType::Var, ParseRule::default());
        rules.insert(TokenType::While, ParseRule::default());
        rules.insert(TokenType::Error, ParseRule::default());
        rules.insert(TokenType::Eof, ParseRule::default());
        rules
    }
}

macro_rules! precedence {
    ($($name:ident = $val:expr),*) => {
        /// 操作符优先级,数值越小优先级越小
        #[derive(Clone, Copy, Eq)]
        #[repr(u8)]
        pub enum Precedence {
            $($name = $val),*
        }

        impl TryFrom<u8> for Precedence {
            type Error = u8;

            fn try_from(v: u8) -> Result<Self, Self::Error> {
                match v {
                    $($val => Ok(Self::$name)),*,
                    _ => Err(v)
                }
            }
        }

        impl std::fmt::Debug for Precedence {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$name => write!(f, "{}({})", stringify!($name), $val)),*,
                }
            }
        }
    };
}

precedence! {
    None=1,
    Assignment=2, // =
    Or=3,         // or
    And=4,        // and
    Equality=5,   // == !=
    Comparison=6, // < > <= >=
    Term=7,       // + -
    Factor=8,     // * /
    Unary=9,      // ! -
    Call=10,      // . ()
    Primary=11
}

impl Precedence {
    fn higher(&self) -> Option<Precedence> {
        Precedence::try_from(*self as u8 + 1).ok()
    }
}

impl PartialEq for Precedence {
    fn eq(&self, other: &Self) -> bool {
        *self as u8 == *other as u8
    }
}

impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Precedence {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (*self as u8).cmp(&(*other as u8))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenType {
    // Single-character tokens. 单字符词法
    // (
    LeftParen,
    // )
    RightParen,
    // {
    LeftBrace,
    // }
    RightBrace,
    // ,
    Comma,
    // .
    Dot,
    // -
    Minus,
    // +
    Plus,
    // ;
    Semicolon,
    // /
    Slash,
    // *
    Star,
    // One or two character tokens. 一或两字符词法
    // !
    Bang,
    // !=
    BangEqual,
    // =
    Equal,
    // ==
    EqualEqual,
    // >
    Greater,
    // >=
    GreaterEqual,
    // <
    Less,
    // <=
    LessEqual,
    // Literals. 字面量
    Identifier,
    String,
    Number,
    // Keywords. 关键字
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    token_type: TokenType,
    lexeme: &'a str,
    line: u32,
}

impl<'a> Default for Token<'a> {
    fn default() -> Self {
        Self {
            token_type: TokenType::Eof,
            lexeme: Default::default(),
            line: Default::default(),
        }
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    iter: Peekable<CharIndices<'a>>,
    // 当前扫描的词素的首位置
    start: usize,
    // 当前扫描的词素的尾位置
    current: usize,
    line: u32,
}

impl<'a> Scanner<'a> {
    pub fn init_scanner(source: &str) -> Scanner {
        Scanner {
            source,
            iter: source.char_indices().peekable(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn scan_token(&mut self) -> Token<'a> {
        // 跳过空白字符, 下一次advance得到的就是非空白字符
        self.skip_whitespace();

        self.start = self.current;
        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();

        match c {
            Some('(') => self.make_token(TokenType::LeftParen),
            Some(')') => self.make_token(TokenType::RightParen),
            Some('{') => self.make_token(TokenType::LeftBrace),
            Some('}') => self.make_token(TokenType::RightBrace),
            Some(';') => self.make_token(TokenType::Semicolon),
            Some(',') => self.make_token(TokenType::Comma),
            Some('.') => self.make_token(TokenType::Dot),
            Some('-') => self.make_token(TokenType::Minus),
            Some('+') => self.make_token(TokenType::Plus),
            Some('/') => self.make_token(TokenType::Slash),
            Some('*') => self.make_token(TokenType::Star),
            Some('!') => {
                if self.match_token('=') {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            Some('=') => {
                if self.match_token('=') {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            Some('<') => {
                if self.match_token('=') {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            Some('>') => {
                if self.match_token('=') {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }
            Some('"') => self.string(),
            Some(digit) if digit.is_ascii_digit() => self.number(),
            Some(alpha) if Self::is_alpha(alpha) => self.identifier(),
            _ => self.error_token("Unexpected character."),
        }
    }

    /// 消费标识符并返回标识符类型的token
    fn identifier(&mut self) -> Token<'a> {
        while matches!(self.peek(),Some(&c) if Self::is_alpha(c))
            || matches!(self.peek(),Some(c) if c.is_ascii_digit())
        {
            self.advance();
        }
        self.make_token(self.identifier_type())
    }

    /// 根据当前词素获得对应的token类型
    fn identifier_type(&self) -> TokenType {
        let s = &self.source[self.start..self.current];
        match s.len() {
            2 => match s {
                "or" => TokenType::Or,
                "if" => TokenType::If,
                _ => TokenType::Identifier,
            },
            3 => match s {
                "and" => TokenType::And,
                "for" => TokenType::For,
                "fun" => TokenType::Fun,
                "nil" => TokenType::Nil,
                "var" => TokenType::Var,
                _ => TokenType::Identifier,
            },
            4 => match s {
                "else" => TokenType::Else,
                "true" => TokenType::True,
                "this" => TokenType::This,
                _ => TokenType::Identifier,
            },
            5 => match s {
                "class" => TokenType::Class,
                "false" => TokenType::False,
                "print" => TokenType::Print,
                "super" => TokenType::Super,
                "while" => TokenType::While,
                _ => TokenType::Identifier,
            },
            6 => match s {
                "return" => TokenType::Return,
                _ => TokenType::Identifier,
            },
            _ => TokenType::Identifier,
        }
    }

    fn is_alpha(c: char) -> bool {
        c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_'
    }

    /// 消费数值并返回数值类型的token
    fn number(&mut self) -> Token<'a> {
        while matches!(self.peek(),Some(c) if c.is_ascii_digit()) {
            self.advance();
        }

        if matches!(self.peek(),Some(d) if *d == '.')
            && matches!(self.peek_next(),Some(d) if d.is_ascii_digit())
        {
            self.advance();
            while matches!(self.peek(),Some(c) if c.is_ascii_digit()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    /// 消费字符串并返回字符串类型的token
    fn string(&mut self) -> Token<'a> {
        while let Some(&c) = self.peek() {
            if c == '"' {
                break;
            }
            if c == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.peek().is_none() {
            return self.error_token("Unterminated string.");
        }

        // 消费末尾的双引号
        self.advance();

        self.make_token(TokenType::String)
    }

    /// 消费所有前置空白字符
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if matches!(self.peek_next(),Some(c) if c =='/') {
                        // 消费注释(//后的内容)直到遇到换行符,但不消费换行符
                        while matches!(self.peek(),Some(pn) if *pn != '\n') && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => {
                    return;
                }
            }
        }
    }

    /// 如果下一个字符和expected匹配,则消费并返回true,否则返回false
    fn match_token(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if matches!(self.peek(),Some(p) if *p == expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// 预览下一个字符
    #[inline]
    fn peek(&mut self) -> Option<&char> {
        self.iter.peek().map(|(_, c)| c)
    }

    #[inline]
    fn peek_next(&mut self) -> Option<char> {
        self.iter
            .peek()
            .and_then(|&(pos, _)| self.source[pos..].chars().nth(1))
    }

    /// 消费下一个字符,并返回被消费的字符
    fn advance(&mut self) -> Option<char> {
        let (_, c) = self.iter.next()?;
        self.current += c.len_utf8();
        Some(c)
    }

    /// 判断是否到达源代码末尾
    fn is_at_end(&self) -> bool {
        self.current == self.source.len()
    }

    fn make_token(&self, token_type: TokenType) -> Token<'a> {
        Token {
            token_type,
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn error_token(&self, message: &'static str) -> Token<'a> {
        Token {
            token_type: TokenType::Error,
            lexeme: message,
            line: self.line,
        }
    }
}

pub struct Compiler<'a> {
    locals: [Local<'a>; MAX_LOCAL_SIZE],
    // 记录作用域中有多少局部变量(有多少个数组槽在使用)
    local_count: usize,
    // 作用域深度: 正在编译的当前代码外围的代码块数量
    scope_depth: isize,
}

pub struct Local<'a> {
    // 变量的名称
    name: Token<'a>,
    // 声明局部变量的代码块的作用域深度.
    // 0是全局作用域, 1是第一个顶层块, 2是它内部的块, 以此类推
    // -1表示未初始化
    depth: isize,
}

impl<'a> Default for Local<'a> {
    fn default() -> Self {
        Self {
            name: Default::default(),
            depth: -1,
        }
    }
}

impl<'a> Default for Compiler<'a> {
    fn default() -> Self {
        Self {
            locals: array::from_fn(|_i| Local::default()),
            local_count: Default::default(),
            scope_depth: Default::default(),
        }
    }
}

impl<'a> Compiler<'a> {
    fn new() -> Self {
        Default::default()
    }
}
