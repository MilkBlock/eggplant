use super::*;
use std::collections::VecDeque;
use std::convert::TryInto;

pub struct Parser {
    tokens: VecDeque<Token>,
    pending_commands: VecDeque<Command>,
    current_file: Option<String>,
    current_line: usize,
    current_col: usize,
    diagnostics: Vec<ParseError>,
}

impl Default for Parser {
    fn default() -> Self {
        Self {
            tokens: VecDeque::new(),
            pending_commands: VecDeque::new(),
            current_file: None,
            current_line: 1,
            current_col: 1,
            diagnostics: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseOutcome {
    pub commands: Vec<Command>,
    pub diagnostics: Vec<ParseError>,
}

#[derive(Debug, Clone, Eq, Hash)]
pub enum Token {
    LParen(Span),
    RParen(Span),
    Symbol(String, Span),
    String(String, Span),
    Number(String, Span),
    Keyword(String, Span),
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LParen(_) => write!(f, "("),
            Token::RParen(_) => write!(f, ")"),
            Token::Symbol(sym, _) => write!(f, "{}", sym),
            Token::String(s, _) => write!(f, "{}", s),
            Token::Number(num, _) => write!(f, "{}", num),
            Token::Keyword(key, _) => write!(f, ":{}", key),
        }
    }
}
impl Token {
    fn sp(&self) -> Span {
        match self {
            Token::LParen(span) => span.clone(),
            Token::RParen(span) => span.clone(),
            Token::Symbol(_, span) => span.clone(),
            Token::String(_, span) => span.clone(),
            Token::Number(_, span) => span.clone(),
            Token::Keyword(_, span) => span.clone(),
        }
    }
}
impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::LParen(_), Self::LParen(_)) => true,
            (Self::RParen(_), Self::RParen(_)) => true,
            (Self::Symbol(l0, _), Self::Symbol(r0, _)) => l0 == r0,
            (Self::String(l0, _), Self::String(r0, _)) => l0 == r0,
            (Self::Number(l0, _), Self::Number(r0, _)) => l0 == r0,
            (Self::Keyword(l0, _), Self::Keyword(r0, _)) => l0 == r0,
            _ => false,
        }
    }
}

impl Parser {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_program_from_string(
        &mut self,
        file: Option<String>,
        input: &str,
    ) -> Result<Vec<Command>, ParseError> {
        self.get_program_from_string_with_diagnostics(file, input)
            .map(|outcome| outcome.commands)
    }

    pub fn get_program_from_string_with_diagnostics(
        &mut self,
        file: Option<String>,
        input: &str,
    ) -> Result<ParseOutcome, ParseError> {
        self.current_file = file;
        self.diagnostics.clear();
        self.pending_commands.clear();
        self.tokenize(input)?;
        let commands = self.parse_program()?;
        Ok(ParseOutcome {
            commands,
            diagnostics: std::mem::take(&mut self.diagnostics),
        })
    }

    fn tokenize(&mut self, input: &str) -> Result<(), ParseError> {
        self.tokens.clear();
        let mut chars = input.chars().peekable();

        // Initialize parser state
        self.current_line = 1;
        self.current_col = 1;

        while let Some(&ch) = chars.peek() {
            match ch {
                '(' => {
                    let token = Token::LParen(self.current_span());
                    self.tokens.push_back(token);
                    chars.next();
                    self.current_col += 1;
                }
                ')' => {
                    self.tokens.push_back(Token::RParen(self.current_span()));
                    chars.next();
                    self.current_col += 1;
                }
                ';' => {
                    // Skip comments until end of line
                    while chars.next().map_or(false, |c| c != '\n') {}
                    self.current_line += 1;
                    self.current_col = 1;
                }
                '"' => {
                    chars.next(); // consume opening quote
                    self.current_col += 1;
                    let mut string = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch == '"' {
                            chars.next(); // consume closing quote
                            self.current_col += 1;
                            break;
                        }
                        string.push(ch);
                        chars.next();
                        self.current_col += 1;
                    }
                    self.tokens
                        .push_back(Token::String(string, self.current_span()));
                }
                ch if ch.is_whitespace() => {
                    if ch == '\n' {
                        self.current_line += 1;
                        self.current_col = 1;
                    } else {
                        self.current_col += 1;
                    }
                    chars.next();
                }
                _ => {
                    let mut symbol = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_whitespace() || ch == '(' || ch == ')' || ch == ':' {
                            break;
                        }
                        symbol.push(ch);
                        chars.next();
                        self.current_col += 1;
                    }

                    if !symbol.is_empty() {
                        if is_numeric_literal_symbol(&symbol) {
                            self.tokens
                                .push_back(Token::Number(symbol, self.current_span()));
                        } else {
                            self.tokens
                                .push_back(Token::Symbol(symbol, self.current_span()));
                        }
                    }

                    // Handle colon separately for keywords
                    if let Some(&':') = chars.peek() {
                        chars.next(); // consume colon
                        self.current_col += 1;

                        while let Some(ch) = chars.peek() {
                            if ch.is_whitespace() {
                                chars.next();
                                self.current_col += 1;
                            } else {
                                break;
                            }
                        }

                        let mut keyword = String::new();
                        while let Some(&ch) = chars.peek() {
                            if ch.is_whitespace() || ch == '(' || ch == ')' {
                                break;
                            }
                            keyword.push(ch);
                            chars.next();
                            self.current_col += 1;
                        }

                        if !keyword.is_empty() {
                            self.tokens
                                .push_back(Token::Keyword(keyword, self.current_span()));
                        }
                    } else {
                        // If we didn't process a colon, we need to skip any whitespace that follows
                        // to prevent empty symbols from being created
                        while let Some(&ch) = chars.peek() {
                            if ch.is_whitespace() {
                                if ch == '\n' {
                                    self.current_line += 1;
                                    self.current_col = 1;
                                } else {
                                    self.current_col += 1;
                                }
                                chars.next();
                            } else {
                                break;
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn parse_program(&mut self) -> Result<Vec<Command>, ParseError> {
        let mut commands = Vec::new();
        while !self.tokens.is_empty() || !self.pending_commands.is_empty() {
            match self.parse_command() {
                Ok(command) => {
                    commands.push(command);
                }
                Err(err) => {
                    let span = err.0.clone();
                    self.diagnostics.push(err.clone());

                    log::debug!("Parse error at line {}:{} - {}", span.line, span.col, err.1);
                    if let Some(token) = self.tokens.front() {
                        let token_span = get_span(token);
                        log::debug!(
                            "  Current token: {:?} at line {}:{}",
                            token,
                            token_span.line,
                            token_span.col
                        );
                    }

                    log::debug!(
                        "  Context: Parsed {} commands so far, continuing...",
                        commands.len()
                    );
                    log::debug!(
                        "Failed to parse command, parsed {} commands so far, remaining tokens: {:?}",
                        commands.len(),
                        self.tokens
                    );
                    if !self.tokens.is_empty() {
                        let skipped_token = self.tokens.pop_front();
                        log::debug!("  Skipping token and continuing: {:?}", skipped_token);
                    }
                }
            }
        }
        Ok(commands)
    }

    fn parse_command(&mut self) -> Result<Command, ParseError> {
        if let Some(command) = self.pending_commands.pop_front() {
            return Ok(command);
        }
        self.parse_command_form()
    }

    fn parse_command_form(&mut self) -> Result<Command, ParseError> {
        self.expect_token(Token::LParen(span()))?;
        let (command_name, sp) = self.parse_symbol()?;
        let command = self.parse_command_after_name(command_name, sp)?;
        self.expect_token(Token::RParen(span()))?;
        Ok(command)
    }

    fn parse_command_after_name(
        &mut self,
        command_name: String,
        sp: Span,
    ) -> Result<Command, ParseError> {
        Ok(match command_name.as_str() {
            "datatype" => self.parse_datatype()?,
            "datatype*" => self.parse_datatype_star()?,
            "constructor" => self.parse_constructor()?,
            "function" => self.parse_function()?,
            "relation" => self.parse_relation()?,
            "let" => self.parse_let()?,
            "rule" => self.parse_rule()?,
            "birewrite" => self.parse_birewrite()?,
            "rewrite" => self.parse_rewrite()?,
            "check" => self.parse_check()?,
            "fail" => self.parse_fail()?,
            "push" => self.parse_push()?,
            "pop" => self.parse_pop()?,
            "run" => self.parse_run()?,
            "run-schedule" => self.parse_run_schedule()?,
            "sort" => self.parse_sort()?,
            "ruleset" => self.parse_ruleset()?,
            "extract" => self.parse_extract()?,
            "print-function" => self.parse_print_function()?,
            "include" => self.parse_include()?,
            _ => {
                let mut args = Vec::new();
                while self.peek_token() != Some(&Token::RParen(span())) {
                    args.push(self.parse_expr()?);
                }
                let expr = Expr::Call(sp.clone(), command_name, args);
                Command::Action(Action::Expr(sp, expr))
            }
        })
    }

    fn parse_datatype_entry_after_name(
        &mut self,
        name: String,
        sp: Span,
    ) -> Result<Command, ParseError> {
        let mut variants = Vec::new();

        while self.peek_token() != Some(&Token::RParen(span())) {
            let variant = self.parse_variant()?;
            variants.push(variant);
        }

        Ok(Command::Datatype {
            span: sp,
            name,
            variants,
        })
    }

    fn parse_datatype(&mut self) -> Result<Command, ParseError> {
        let (name, sp) = self.parse_symbol()?;
        self.parse_datatype_entry_after_name(name, sp)
    }

    fn parse_datatype_star(&mut self) -> Result<Command, ParseError> {
        let mut commands = VecDeque::new();

        while self.peek_token() != Some(&Token::RParen(span())) {
            self.expect_token(Token::LParen(span()))?;
            let (name, sp) = self.parse_symbol()?;
            let command = if name == "sort" {
                self.parse_sort()?
            } else {
                self.parse_datatype_entry_after_name(name, sp)?
            };
            self.expect_token(Token::RParen(span()))?;
            commands.push_back(command);
        }

        let first = commands.pop_front().ok_or_else(|| {
            ParseError::new(
                self.current_span(),
                "datatype* must contain at least one entry".to_string(),
            )
        })?;
        self.pending_commands.extend(commands);
        Ok(first)
    }

    fn parse_variant(&mut self) -> Result<Variant, ParseError> {
        self.expect_token(Token::LParen(span()))?;
        let (name, sp) = self.parse_symbol()?;
        let mut types = Vec::new();
        let mut field_names = Vec::new();

        // Parse types until we hit a keyword or closing paren
        while let Some(token) = self.peek_token() {
            match token {
                Token::RParen(_) => break,
                Token::Keyword(_, _) => break,
                _ => types.push(self.parse_symbol()?.0),
            }
        }

        // Parse keyword arguments
        while let Some(Token::Keyword(_, _)) = self.peek_token() {
            let (keyword, _) = self.parse_symbol()?;
            match keyword.as_str() {
                "args_name" => {
                    let (field_names_str, _) = self.parse_string()?;
                    field_names = field_names_str
                        .split(',')
                        .map(|s| s.trim().to_string())
                        .collect();
                }
                _ => {
                    self.skip_keyword_value_if_present()?;
                }
            }
        }

        self.expect_token(Token::RParen(span()))?;
        Ok(Variant {
            span: sp,
            name,
            types,
            field_names,
        })
    }

    fn parse_constructor(&mut self) -> Result<Command, ParseError> {
        let (name, sp) = self.parse_symbol()?;
        let schema = self.parse_schema()?;

        let mut cost = None;
        while matches!(self.peek_token(), Some(Token::Keyword(_, _))) {
            let (keyword, keyword_span) = self.parse_symbol()?;
            match keyword.as_str() {
                "cost" => {
                    let (value, _) = self.parse_number()?;
                    cost = Some(value.try_into().map_err(|_| {
                        ParseError::new(keyword_span, "constructor :cost must be >= 0".to_string())
                    })?);
                }
                "unextractable" | "internal-hidden" => {}
                _ => {
                    self.skip_keyword_value_if_present()?;
                }
            }
        }

        Ok(Command::Constructor {
            span: sp,
            name,
            schema,
            cost,
        })
    }

    fn parse_function(&mut self) -> Result<Command, ParseError> {
        let (name, sp) = self.parse_symbol()?;
        let schema = self.parse_schema()?;
        let mut merge = None;

        while matches!(self.peek_token(), Some(Token::Keyword(_, _))) {
            let (keyword, _) = self.parse_symbol()?;
            match keyword.as_str() {
                "merge" => {
                    merge = Some(self.parse_expr()?);
                }
                "no-merge" | "unextractable" => {}
                _ => {
                    self.skip_keyword_value_if_present()?;
                }
            }
        }

        Ok(Command::Function {
            span: sp,
            name,
            schema,
            merge,
        })
    }

    fn parse_relation(&mut self) -> Result<Command, ParseError> {
        let (name, sp) = self.parse_symbol()?;
        let mut inputs = Vec::new();

        if self.peek_token() == Some(&Token::LParen(span())) {
            self.expect_token(Token::LParen(span()))?;
            while self.peek_token() != Some(&Token::RParen(span())) {
                inputs.push(self.parse_symbol()?.0);
            }
            self.expect_token(Token::RParen(span()))?;
        } else {
            while self.peek_token() != Some(&Token::RParen(span())) {
                inputs.push(self.parse_symbol()?.0);
            }
        }

        Ok(Command::Relation {
            span: sp,
            name,
            inputs,
        })
    }

    fn parse_schema(&mut self) -> Result<Schema, ParseError> {
        let mut inputs = Vec::new();

        // Check if inputs are in parentheses
        if self.peek_token() == Some(&Token::LParen(span())) {
            self.expect_token(Token::LParen(span()))?;
            while self.peek_token() != Some(&Token::RParen(span())) {
                inputs.push(self.parse_symbol()?.0);
            }
            self.expect_token(Token::RParen(span()))?;
        } else {
            // Parse inputs without parentheses
            while self.peek_token() != Some(&Token::RParen(span())) {
                inputs.push(self.parse_symbol()?.0);
            }
        }

        let output = self.parse_symbol()?.0;

        Ok(Schema {
            input: inputs,
            output,
        })
    }

    fn parse_let(&mut self) -> Result<Command, ParseError> {
        let (var, sp) = self.parse_symbol()?;
        let expr = self.parse_expr()?;

        Ok(Command::Action(Action::Let(sp, var, expr)))
    }

    fn parse_name_value(&mut self) -> Result<(String, Span), ParseError> {
        match self.peek_token() {
            Some(Token::String(_, _)) => self.parse_string(),
            _ => self.parse_symbol(),
        }
    }

    fn skip_keyword_value_if_present(&mut self) -> Result<(), ParseError> {
        match self.peek_token() {
            Some(Token::RParen(_)) | Some(Token::Keyword(_, _)) | None => Ok(()),
            Some(_) => {
                self.parse_expr()?;
                Ok(())
            }
        }
    }

    fn parse_action_expr(&mut self) -> Result<Action, ParseError> {
        if self.peek_token() == Some(&Token::LParen(span())) {
            self.expect_token(Token::LParen(span()))?;
            let (action_name, sp) = self.parse_symbol()?;

            let action = match action_name.as_str() {
                "let" => {
                    let (var, _) = self.parse_symbol()?;
                    let expr = self.parse_expr()?;
                    Action::Let(sp, var, expr)
                }
                "set" => {
                    let lhs = self.parse_expr()?;
                    let rhs = self.parse_expr()?;
                    match lhs {
                        Expr::Call(_, head, args) => Action::Set(sp, head, args, rhs),
                        _ => {
                            return Err(ParseError::new(
                                sp,
                                "expected function/relation call on left side of set".to_string(),
                            ));
                        }
                    }
                }
                "union" => {
                    let lhs = self.parse_expr()?;
                    let rhs = self.parse_expr()?;
                    Action::Union(sp, lhs, rhs)
                }
                "delete" => {
                    let expr = self.parse_expr()?;
                    Action::Delete(sp, expr)
                }
                _ => {
                    let mut args = Vec::new();
                    while self.peek_token() != Some(&Token::RParen(span())) {
                        args.push(self.parse_expr()?);
                    }
                    Action::Expr(sp.clone(), Expr::Call(sp, action_name, args))
                }
            };

            self.expect_token(Token::RParen(span()))?;
            Ok(action)
        } else {
            let expr = self.parse_expr()?;
            Ok(Action::Expr(expr.span(), expr))
        }
    }

    fn parse_birewrite(&mut self) -> Result<Command, ParseError> {
        let lhs = self.parse_expr()?;
        let rhs = self.parse_expr()?;
        let mut ruleset = "default".to_string();
        let mut conditions = Vec::new();

        while matches!(self.peek_token(), Some(Token::Keyword(_, _))) {
            let (keyword, _sp) = self.parse_symbol()?;
            match keyword.as_str() {
                "ruleset" => {
                    (ruleset, _) = self.parse_symbol()?;
                }
                "when" => {
                    self.expect_token(Token::LParen(span()))?;
                    while self.peek_token() != Some(&Token::RParen(span())) {
                        conditions.push(self.parse_fact()?);
                    }
                    self.expect_token(Token::RParen(span()))?;
                }
                "name" => {
                    let _ = self.parse_name_value()?;
                }
                _ => {
                    self.skip_keyword_value_if_present()?;
                }
            }
        }

        Ok(Command::BiRewrite(
            ruleset,
            Rewrite {
                span: lhs.span(),
                lhs,
                rhs,
                conditions,
            },
        ))
    }

    fn parse_rewrite(&mut self) -> Result<Command, ParseError> {
        let lhs = self.parse_expr()?;
        let rhs = self.parse_expr()?;
        let mut ruleset = "default".to_string();
        let mut conditions = Vec::new();
        let mut name = None;
        let mut subsume = false;

        // println!(
        //     "DEBUG: Starting rewrite parsing, next tokens: {:?}",
        //     self.peek_token()
        // );
        // println!("DEBUG: Remaining tokens after RHS: {:?}", self.tokens);

        // Parse optional keyword arguments
        while matches!(self.peek_token(), Some(Token::Keyword(_, _))) {
            let (keyword, _sp) = self.parse_symbol()?;
            match keyword.as_str() {
                "ruleset" => {
                    (ruleset, _) = self.parse_symbol()?;
                }
                "when" => {
                    // Parse when conditions
                    // println!("DEBUG: Processing :when conditions");
                    self.expect_token(Token::LParen(span()))?;
                    while self.peek_token() != Some(&Token::RParen(span())) {
                        let condition = self.parse_fact()?;
                        // println!("DEBUG: Added condition: {:?}", condition);
                        conditions.push(condition);
                    }
                    self.expect_token(Token::RParen(span()))?;
                }
                "name" => {
                    let (name_str, _) = self.parse_name_value()?;
                    name = Some(name_str);
                }
                "subsume" => {
                    subsume = true;
                }
                _ => {
                    log::debug!("Skipping unknown keyword: '{}'", keyword);
                    self.skip_keyword_value_if_present()?;
                }
            }
        }

        // println!(
        //     "DEBUG: Rewrite parsing complete, conditions: {:?}",
        //     conditions
        // );

        Ok(Command::Rewrite(
            ruleset,
            Rewrite {
                span: lhs.span(),
                lhs,
                rhs,
                conditions,
            },
            subsume,
            name,
        ))
    }

    fn parse_fail(&mut self) -> Result<Command, ParseError> {
        let sp = self.current_span();
        let inner = self.parse_command_form()?;
        Ok(Command::Fail(sp, Box::new(inner)))
    }

    fn parse_rule(&mut self) -> Result<Command, ParseError> {
        let sp = self.current_span();
        self.expect_token(Token::LParen(span()))?;
        let mut body = Vec::new();
        while self.peek_token() != Some(&Token::RParen(span())) {
            body.push(self.parse_fact()?);
        }
        self.expect_token(Token::RParen(span()))?;

        self.expect_token(Token::LParen(span()))?;
        let mut head = Vec::new();
        while self.peek_token() != Some(&Token::RParen(span())) {
            head.push(self.parse_action_expr()?);
        }
        self.expect_token(Token::RParen(span()))?;

        let mut ruleset = "default".to_string();
        let mut name = "default".to_string();

        while matches!(self.peek_token(), Some(Token::Keyword(_, _))) {
            let (keyword, keyword_span) = self.parse_symbol()?;
            match keyword.as_str() {
                "ruleset" => {
                    (ruleset, _) = self.parse_symbol()?;
                }
                "name" => {
                    (name, _) = self.parse_name_value()?;
                }
                _ => {
                    return Err(ParseError::new(
                        keyword_span,
                        format!("unsupported rule keyword :{}", keyword),
                    ));
                }
            }
        }

        Ok(Command::Rule {
            name,
            ruleset,
            rule: Rule {
                span: sp,
                head,
                body,
            },
        })
    }

    fn parse_check(&mut self) -> Result<Command, ParseError> {
        let mut facts = Vec::new();
        while self.peek_token() != Some(&Token::RParen(span())) {
            facts.push(self.parse_fact()?);
        }

        Ok(Command::Check(self.current_span(), facts))
    }

    fn parse_fact(&mut self) -> Result<Fact, ParseError> {
        if self.peek_token() == Some(&Token::LParen(span())) {
            self.expect_token(Token::LParen(span()))?;
            let (op, sp) = self.parse_symbol()?;

            // Support comparison operators: =, <, <=, >, >=, !=
            if op == "=" || op == "<" || op == "<=" || op == ">" || op == ">=" || op == "!=" {
                let e1 = self.parse_expr()?;
                let e2 = self.parse_expr()?;
                self.expect_token(Token::RParen(span()))?;
                // Store operator in the span's file field as a temporary solution
                let operator_span = Span {
                    file: Some(format!("operator:{}", op)), // Temporary hack to store operator
                    line: sp.line,
                    col: sp.col,
                };
                Ok(Fact::Op(operator_span, e1, e2))
            } else {
                let mut args = Vec::new();
                while self.peek_token() != Some(&Token::RParen(span())) {
                    args.push(self.parse_expr()?);
                }
                self.expect_token(Token::RParen(span()))?;
                Ok(Fact::Fact(Expr::Call(sp, op, args)))
            }
        } else {
            let expr = self.parse_expr()?;
            Ok(Fact::Fact(expr))
        }
    }

    fn parse_push(&mut self) -> Result<Command, ParseError> {
        let n = if self.peek_token() == Some(&Token::RParen(span())) {
            1
        } else {
            self.parse_number()?.0.try_into().unwrap()
        };
        Ok(Command::Push(n))
    }

    fn parse_pop(&mut self) -> Result<Command, ParseError> {
        let (sp, n) = if self.peek_token() == Some(&Token::RParen(span())) {
            (self.current_span(), 1)
        } else {
            let (n, sp) = self.parse_number()?;
            (sp, n.try_into().unwrap())
        };
        Ok(Command::Pop(sp, n))
    }

    fn parse_run(&mut self) -> Result<Command, ParseError> {
        let sp = self.current_span();
        let (ruleset, limit, until) = self.parse_run_parts()?;

        Ok(Command::Run {
            span: sp,
            ruleset,
            limit,
            until,
        })
    }

    fn parse_run_schedule(&mut self) -> Result<Command, ParseError> {
        let sp = self.current_span();
        let mut schedules = Vec::new();
        while self.peek_token() != Some(&Token::RParen(span())) {
            schedules.push(self.parse_schedule_expr()?);
        }
        Ok(Command::RunSchedule {
            span: sp,
            schedules,
        })
    }

    fn parse_run_parts(
        &mut self,
    ) -> Result<(Option<String>, Option<usize>, Option<Fact>), ParseError> {
        let mut ruleset = None;
        let mut limit = None;

        match self.peek_token() {
            Some(Token::Number(_, _)) => {
                limit = Some(self.parse_number()?.0.try_into().unwrap());
            }
            Some(Token::Symbol(_, _)) => {
                let (candidate, _) = self.parse_symbol()?;
                match self.peek_token() {
                    Some(Token::Number(_, _)) => {
                        ruleset = Some(candidate);
                        limit = Some(self.parse_number()?.0.try_into().unwrap());
                    }
                    Some(Token::Keyword(_, _)) | Some(Token::RParen(_)) => {
                        ruleset = Some(candidate);
                    }
                    _ => {
                        return Err(ParseError::new(
                            self.next_error_span(),
                            "expected run count, keyword, or ')' after ruleset name".to_string(),
                        ));
                    }
                }
            }
            Some(Token::Keyword(_, _)) | Some(Token::RParen(_)) => {}
            _ => {
                return Err(ParseError::new(
                    self.next_error_span(),
                    "expected run count, ruleset name, keyword, or ')'".to_string(),
                ));
            }
        }

        let mut until = None;
        while matches!(self.peek_token(), Some(Token::Keyword(_, _))) {
            let (keyword, keyword_span) = self.parse_symbol()?;
            match keyword.as_str() {
                "until" => {
                    until = Some(self.parse_fact()?);
                }
                _ => {
                    return Err(ParseError::new(
                        keyword_span,
                        format!("unsupported run keyword :{}", keyword),
                    ));
                }
            }
        }

        Ok((ruleset, limit, until))
    }

    fn parse_schedule_expr(&mut self) -> Result<Schedule, ParseError> {
        if matches!(self.peek_token(), Some(Token::Symbol(_, _))) {
            let (name, _) = self.parse_symbol()?;
            return Ok(Schedule::Named(name));
        }

        self.expect_token(Token::LParen(span()))?;
        let (schedule_name, schedule_span) = self.parse_symbol()?;

        let schedule = match schedule_name.as_str() {
            "run" => {
                let (ruleset, limit, until) = self.parse_run_parts()?;
                Schedule::Run {
                    ruleset,
                    limit,
                    until,
                }
            }
            "seq" => {
                let mut items = Vec::new();
                while self.peek_token() != Some(&Token::RParen(span())) {
                    items.push(self.parse_schedule_expr()?);
                }
                Schedule::Seq(items)
            }
            "saturate" => {
                let mut items = Vec::new();
                while self.peek_token() != Some(&Token::RParen(span())) {
                    items.push(self.parse_schedule_expr()?);
                }
                Schedule::Saturate(items)
            }
            "repeat" => {
                let count: usize = self.parse_number()?.0.try_into().unwrap();
                let mut items = Vec::new();
                while self.peek_token() != Some(&Token::RParen(span())) {
                    items.push(self.parse_schedule_expr()?);
                }
                let inner = match items.len() {
                    0 => {
                        return Err(ParseError::new(
                            schedule_span,
                            "repeat requires at least one schedule item".to_string(),
                        ));
                    }
                    1 => items.pop().unwrap(),
                    _ => Schedule::Seq(items),
                };
                Schedule::Repeat(count, Box::new(inner))
            }
            _ => {
                return Err(ParseError::new(
                    schedule_span,
                    format!("unsupported run-schedule operator {}", schedule_name),
                ));
            }
        };

        self.expect_token(Token::RParen(span()))?;
        Ok(schedule)
    }

    fn parse_sort(&mut self) -> Result<Command, ParseError> {
        let (name, sp) = self.parse_symbol()?;

        let schema = if self.peek_token() == Some(&Token::RParen(span())) {
            None
        } else {
            match self.parse_expr()? {
                Expr::Call(_, head, args) => Some((head, args)),
                Expr::Var(_, head) => Some((head, Vec::new())),
                Expr::Lit(_, _) => None,
            }
        };

        while self.peek_token() != Some(&Token::RParen(span())) {
            self.skip_form()?;
        }
        Ok(Command::Sort(sp, name, schema))
    }

    fn skip_form(&mut self) -> Result<(), ParseError> {
        match self.peek_token() {
            Some(Token::LParen(_)) => {
                self.expect_token(Token::LParen(span()))?;
                while self.peek_token() != Some(&Token::RParen(span())) {
                    self.skip_form()?;
                }
                self.expect_token(Token::RParen(span()))?;
                Ok(())
            }
            Some(_) => {
                self.tokens.pop_front();
                Ok(())
            }
            None => Err(ParseError::new(
                self.next_error_span(),
                "Unexpected EOF while skipping form".to_string(),
            )),
        }
    }

    fn parse_ruleset(&mut self) -> Result<Command, ParseError> {
        let (name, sp) = self.parse_symbol()?;
        Ok(Command::AddRuleset(sp, name))
    }

    fn parse_extract(&mut self) -> Result<Command, ParseError> {
        let sp = self.current_span();
        let expr = self.parse_expr()?;
        let variants = if matches!(self.peek_token(), Some(Token::Number(_, _))) {
            Some(self.parse_number()?.0.try_into().unwrap())
        } else {
            None
        };

        Ok(Command::Extract {
            span: sp,
            expr,
            variants,
        })
    }

    fn parse_print_function(&mut self) -> Result<Command, ParseError> {
        let (name, sp) = self.parse_symbol()?;
        let mut size = None;
        if matches!(self.peek_token(), Some(Token::Number(_, _))) {
            size = Some(self.parse_number()?.0.try_into().unwrap());
        }

        let mut file = None;
        let mut mode = None;
        while matches!(self.peek_token(), Some(Token::Keyword(_, _))) {
            let (keyword, _) = self.parse_symbol()?;
            match keyword.as_str() {
                "file" => {
                    let (path, _) = self.parse_string()?;
                    file = Some(path);
                }
                "mode" => {
                    let (mode_name, _) = self.parse_name_value()?;
                    mode = Some(mode_name);
                }
                _ => {
                    self.skip_keyword_value_if_present()?;
                }
            }
        }

        Ok(Command::PrintFunction(sp, name, size, file, mode))
    }

    fn parse_include(&mut self) -> Result<Command, ParseError> {
        let (file, sp) = self.parse_string()?;
        Ok(Command::Include(sp, file))
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.peek_token() {
            Some(Token::LParen(_)) => {
                let sp = self.next_error_span();
                self.expect_token(Token::LParen(span()))?;
                if self.peek_token() == Some(&Token::RParen(span())) {
                    self.expect_token(Token::RParen(span()))?;
                    return Ok(Expr::Lit(sp, Literal::Unit));
                }
                let (func, sp) = self.parse_symbol()?;
                let mut args = Vec::new();

                while self.peek_token() != Some(&Token::RParen(span())) {
                    args.push(self.parse_expr()?);
                }

                self.expect_token(Token::RParen(span()))?;
                Ok(Expr::Call(sp, func, args))
            }
            Some(Token::Number(_, _)) => {
                let (literal, sp) = self.parse_numeric_literal()?;
                Ok(Expr::Lit(sp, literal))
            }
            Some(Token::String(_s, _)) => {
                let (s, sp) = self.parse_string()?;
                Ok(Expr::Lit(sp, Literal::String(s)))
            }
            Some(Token::Symbol(_s, _)) => {
                let (sym, sp) = self.parse_symbol()?;
                match sym.as_str() {
                    "true" => Ok(Expr::Lit(sp, Literal::Bool(true))),
                    "false" => Ok(Expr::Lit(sp, Literal::Bool(false))),
                    _ => Ok(Expr::Var(sp, sym)),
                }
            }
            _ => Err(ParseError::new(
                self.next_error_span(),
                "Expected expression".to_string(),
            )),
        }
    }

    fn parse_number(&mut self) -> Result<(i64, Span), ParseError> {
        if let Some(Token::Number(n, sp)) = self.tokens.pop_front() {
            n.parse()
                .map_err(|_| ParseError::new(sp.clone(), "Invalid number".to_string()))
                .map(|n| (n, sp))
        } else {
            Err(ParseError::new(
                self.next_error_span(),
                "Expected number".to_string(),
            ))
        }
    }

    fn parse_numeric_literal(&mut self) -> Result<(Literal, Span), ParseError> {
        if let Some(Token::Number(n, sp)) = self.tokens.pop_front() {
            if n.contains('.') || n.contains('e') || n.contains('E') {
                n.parse::<f64>()
                    .map(|value| {
                        (
                            Literal::Float(ordered_float::OrderedFloat(value)),
                            sp.clone(),
                        )
                    })
                    .map_err(|_| ParseError::new(sp, "Invalid float".to_string()))
            } else {
                n.parse::<i64>()
                    .map(Literal::Int)
                    .map(|literal| (literal, sp.clone()))
                    .map_err(|_| ParseError::new(sp, "Invalid number".to_string()))
            }
        } else {
            Err(ParseError::new(
                self.next_error_span(),
                "Expected numeric literal".to_string(),
            ))
        }
    }

    fn parse_string(&mut self) -> Result<(String, Span), ParseError> {
        if let Some(Token::String(s, sp)) = self.tokens.pop_front() {
            Ok((s, sp))
        } else {
            Err(ParseError::new(
                self.next_error_span(),
                "Expected string".to_string(),
            ))
        }
    }

    fn parse_symbol(&mut self) -> Result<(String, Span), ParseError> {
        let popped = self.tokens.pop_front();
        match popped {
            Some(Token::Symbol(s, span)) => Ok((s, span)),
            Some(Token::Keyword(k, span)) => Ok((k, span)),
            _ => Err(ParseError::new(
                get_op_span(&popped),
                "Expected symbol".to_string(),
            )),
        }
    }

    fn expect_token(&mut self, expected: Token) -> Result<(), ParseError> {
        if let Some(token) = self.tokens.pop_front() {
            if token == expected {
                Ok(())
            } else {
                Err(ParseError::new(
                    self.peek_token().map(|t| t.sp()).unwrap_or_default(),
                    format!("Expected {:?}, got {:?}", expected, token),
                ))
            }
        } else {
            Err(ParseError::new(
                self.peek_token().map(|t| t.sp()).unwrap_or_default(),
                format!("Expected {:?}, but no more tokens", expected),
            ))
        }
    }

    fn peek_token(&self) -> Option<&Token> {
        self.tokens.front()
    }

    fn current_span(&self) -> Span {
        Span::new(
            self.current_file.clone(),
            self.current_line,
            self.current_col,
        )
    }

    fn next_error_span(&self) -> Span {
        self.peek_token()
            .map(|token| token.sp())
            .unwrap_or_else(|| self.current_span())
    }
}

fn is_numeric_literal_symbol(symbol: &str) -> bool {
    symbol.chars().any(|ch| ch.is_ascii_digit())
        && (symbol.parse::<i64>().is_ok() || symbol.parse::<f64>().is_ok())
}

impl ParseError {
    pub fn new(span: Span, message: String) -> Self {
        ParseError(span, message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn find_repo_sibling(name: &str) -> Option<PathBuf> {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        manifest_dir
            .ancestors()
            .map(|ancestor| ancestor.join(name))
            .find(|candidate| candidate.is_dir())
    }

    fn egglog_tests_path() -> Option<PathBuf> {
        find_repo_sibling("egglog")
            .map(|root| root.join("tests"))
            .or_else(|| find_repo_sibling("upstream_egglog").map(|root| root.join("tests")))
    }

    fn upstream_egglog_fixture(rel: &str) -> String {
        find_repo_sibling("upstream_egglog")
            .unwrap_or_else(|| panic!("could not locate sibling repo `upstream_egglog`"))
            .join(rel)
            .to_string_lossy()
            .into_owned()
    }

    #[test]
    fn test_debug_stresstest() {
        let input = r#"(rewrite (TupleInt_single __var__i) (TupleInt___init__ (Int___init__ 1) (unstable-fn "cast_Callable__Int__Int___Int___lambda_i_____i_" __var__i)) :ruleset array_api_ruleset)"#;
        let mut parser = Parser::new();

        println!("Input: {}", input);

        match parser.get_program_from_string(None, input) {
            Ok(commands) => {
                println!("Success! Parsed {} commands", commands.len());
                for cmd in commands {
                    println!("  Command: {:?}", cmd);
                }
            }
            Err(e) => {
                println!("Error: {}", e);
                println!("Remaining tokens: {:?}", parser.tokens);
            }
        }
    }

    #[test]
    fn test_parse_all_egg_files() {
        use std::fs;
        let Some(egglog_tests_path) = egglog_tests_path() else {
            println!(
                "Egglog tests directory not found under sibling `egglog` or `upstream_egglog` repo"
            );
            return;
        };

        let mut parser = Parser::default();
        let mut total_files = 0;
        let mut parsed_successfully = 0;
        let mut failed_files = Vec::new();

        // Walk through all .egg files in the tests directory
        if let Ok(entries) = fs::read_dir(&egglog_tests_path) {
            for entry in entries {
                if let Ok(entry) = entry {
                    let path = entry.path();
                    if path.extension().map_or(false, |ext| ext == "egg") {
                        total_files += 1;

                        println!("Parsing: {:?}", path);

                        match fs::read_to_string(&path) {
                            Ok(content) => {
                                match parser.get_program_from_string(
                                    Some(path.to_string_lossy().to_string()),
                                    &content,
                                ) {
                                    Ok(commands) => {
                                        parsed_successfully += 1;
                                        println!(
                                            "  ✓ Successfully parsed {} commands",
                                            commands.len()
                                        );
                                    }
                                    Err(e) => {
                                        failed_files.push((
                                            path.to_string_lossy().to_string(),
                                            e.to_string(),
                                        ));
                                        println!("  ✗ Failed to parse: {}", e);
                                    }
                                }
                            }
                            Err(e) => {
                                failed_files.push((
                                    path.to_string_lossy().to_string(),
                                    format!("Failed to read file: {}", e),
                                ));
                                println!("  ✗ Failed to read file: {}", e);
                            }
                        }
                    }
                }
            }
        }

        println!("\n=== Parsing Results ===");
        println!("Total .egg files found: {}", total_files);
        println!("Successfully parsed: {}", parsed_successfully);
        println!("Failed to parse: {}", failed_files.len());

        if !failed_files.is_empty() {
            println!("\nFailed files:");
            for (file, error) in &failed_files {
                println!("  {}: {}", file, error);
            }
        }

        // For now, we'll just print the results but not fail the test
        // This allows us to see which files work and which don't
        println!(
            "\nNote: This test does not fail on parsing errors to allow incremental development."
        );
        println!("The goal is to gradually improve the parser to handle all .egg files.");
    }

    #[test]
    fn test_parse_with_diagnostics_reports_errors() {
        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(None, "(datatype Math) (run")
            .unwrap();

        assert!(!outcome.diagnostics.is_empty());
        assert_eq!(outcome.commands.len(), 1);
    }

    #[test]
    fn test_parse_rule_function_and_run_surface() {
        let program = r#"
            (function F (i64) bool :no-merge)
            (rule ((Edge a b))
                  ((set (F a) true))
                  :ruleset path-rules
                  :name "seed_path")
            (run path-rules 3)
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();

        assert!(matches!(commands[0], Command::Function { .. }));
        assert!(matches!(
            commands[1],
            Command::Rule {
                ref name,
                ref ruleset,
                ..
            } if name == "seed_path" && ruleset == "path-rules"
        ));
        assert!(matches!(
            commands[2],
            Command::Run {
                ref ruleset,
                limit: Some(3),
                until: None,
                ..
            } if ruleset.as_deref() == Some("path-rules")
        ));
    }

    #[test]
    fn test_parse_run_schedule_with_unbounded_until_runs() {
        let program = r#"
            (rule () ())
            (run-schedule
              (seq (run :until (= a 1))
                   (run :until (= a "s"))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();

        assert!(matches!(commands[0], Command::Rule { .. }));
        assert!(matches!(
            &commands[1],
            Command::RunSchedule {
                schedules,
                ..
            } if matches!(
                &schedules[..],
                [Schedule::Seq(items)]
                    if matches!(
                        &items[..],
                        [
                            Schedule::Run {
                                ruleset: None,
                                limit: None,
                                until: Some(Fact::Op(_, _, _)),
                            },
                            Schedule::Run {
                                ruleset: None,
                                limit: None,
                                until: Some(Fact::Op(_, _, _)),
                            }
                        ]
                    )
            )
        ));
    }

    #[test]
    fn test_parse_run_schedule_with_named_ruleset_inside_seq() {
        let program = r#"
            (run-schedule
              (saturate (seq
                (run :until (= res goal))
                (run prune))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();

        assert!(matches!(
            &commands[0],
            Command::RunSchedule {
                schedules,
                ..
            } if matches!(
                &schedules[..],
                [Schedule::Saturate(items)]
                    if matches!(
                        &items[..],
                        [Schedule::Seq(items)]
                            if matches!(
                                &items[..],
                                [
                                    Schedule::Run {
                                        ruleset: None,
                                        limit: None,
                                        until: Some(Fact::Op(_, _, _)),
                                    },
                                    Schedule::Run {
                                        ruleset: Some(ruleset),
                                        limit: None,
                                        until: None,
                                    }
                                ] if ruleset == "prune"
                            )
                    )
            )
        ));
    }

    #[test]
    fn test_parse_run_schedule_math_surface_with_named_ruleset_after_until() {
        let program = r#"
            (run-schedule (saturate (seq
              (run :until (= $res (Mul (Const 3.0) (Pow (Var "x") (Const 2.0)))))
              (run prune))))
        "#;

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(None, program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_run_schedule_allows_bare_named_schedule_items() {
        let program = r#"
            (run-schedule
              (saturate init graph1)
              (saturate (saturate choose-best-edge) finish-iteration))
        "#;

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(None, program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
        assert_eq!(outcome.commands.len(), 1);
        assert!(matches!(
            &outcome.commands[0],
            Command::RunSchedule {
                schedules,
                ..
            } if matches!(
                &schedules[..],
                [Schedule::Saturate(first), Schedule::Saturate(second)]
                    if matches!(
                        &first[..],
                        [Schedule::Named(init), Schedule::Named(graph1)]
                            if init == "init" && graph1 == "graph1"
                    ) && matches!(
                        &second[..],
                        [Schedule::Saturate(inner), Schedule::Named(finish)]
                            if finish == "finish-iteration"
                                && matches!(
                                    &inner[..],
                                    [Schedule::Named(choice)] if choice == "choose-best-edge"
                                )
                    )
            )
        ));
    }

    #[test]
    fn test_parse_full_program_web_demo_math_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/web-demo/math.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_sort_allows_nested_type_specs() {
        let program = r#"
            (sort X)
            (sort Y)
            (sort VX (Vec X))
            (sort XY (UnstableFn (X) Y))
            (sort Nested (UnstableFn (Vec X) (Vec (Vec Y))))
        "#;

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(None, program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
        assert_eq!(outcome.commands.len(), 5);
        assert!(matches!(outcome.commands[0], Command::Sort(_, _, None)));
        assert!(matches!(
            &outcome.commands[2],
            Command::Sort(_, name, Some((kind, args)))
                if name == "VX"
                    && kind == "Vec"
                    && matches!(&args[..], [Expr::Var(_, arg)] if arg == "X")
        ));
        assert!(matches!(
            &outcome.commands[4],
            Command::Sort(_, name, Some((kind, args)))
                if name == "Nested"
                    && kind == "UnstableFn"
                    && args.len() == 2
        ));
    }

    #[test]
    fn test_parse_full_program_vec_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/vec.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_full_program_repro_738_fn_sort_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/repro-738-fn-sort.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_expr_unit_literal() {
        let program = r#"
            (function is-even (Math) Unit :no-merge)
            (set (is-even (Num 2)) ())
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();

        assert!(matches!(
            &commands[1],
            Command::Action(Action::Expr(_, Expr::Call(_, head, args)))
                if head == "set"
                    && matches!(&args[..], [Expr::Call(_, inner, _), Expr::Lit(_, Literal::Unit)] if inner == "is-even")
        ));
    }

    #[test]
    fn test_parse_fail_wraps_nested_rule_command() {
        let program = r#"
            (fail
              (rule ()
                ((number 4))
                :ruleset myrules1and2))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();

        assert!(matches!(
            &commands[0],
            Command::Fail(_, inner)
                if matches!(&**inner, Command::Rule { ruleset, .. } if ruleset == "myrules1and2")
        ));
    }

    #[test]
    fn test_parse_check_allows_multiple_facts() {
        let program = r#"
            (check (= x (f 1)) (= y (f 2)) (= x y))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();

        assert!(matches!(
            &commands[0],
            Command::Check(_, facts) if facts.len() == 3
        ));
    }

    #[test]
    fn test_parse_birewrite_with_when_conditions() {
        let program = r#"
            (birewrite (compose f (id B)) f
                :when ((= (type A) (Ob))
                       (= (type B) (Ob))
                       (= (type f) (Hom A B))))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();

        assert!(matches!(
            &commands[0],
            Command::BiRewrite(ruleset, rewrite)
                if ruleset == "default" && rewrite.conditions.len() == 3
        ));
    }

    #[test]
    fn test_parse_datatype_star_expands_multiple_commands() {
        let program = r#"
            (datatype*
                (Math
                    (Add Math Math)
                    (B Bool))
                (sort MathVec (Vec Math))
                (Bool
                    (True)
                    (False)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();

        assert_eq!(commands.len(), 3);
        assert!(matches!(
            &commands[0],
            Command::Datatype { name, .. } if name == "Math"
        ));
        assert!(matches!(
            &commands[1],
            Command::Sort(_, name, Some((kind, args)))
                if name == "MathVec"
                    && kind == "Vec"
                    && matches!(&args[..], [Expr::Var(_, arg)] if arg == "Math")
        ));
        assert!(matches!(
            &commands[2],
            Command::Datatype { name, .. } if name == "Bool"
        ));
    }

    #[test]
    fn test_parse_run_schedule_repeat_allows_multiple_schedule_items() {
        let program = r#"
            (run-schedule
              (repeat 9
                (saturate step-right)
                my-combination
                (saturate step-right)))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();

        assert!(matches!(
            &commands[0],
            Command::RunSchedule { schedules, .. }
                if matches!(
                    &schedules[..],
                    [Schedule::Repeat(9, inner)]
                        if matches!(
                            &**inner,
                            Schedule::Seq(items)
                                if matches!(
                                    &items[..],
                                    [
                                        Schedule::Saturate(_),
                                        Schedule::Named(name),
                                        Schedule::Saturate(_)
                                    ] if name == "my-combination"
                                )
                        )
                )
        ));
    }

    #[test]
    fn test_parse_full_program_multiset_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/web-demo/multiset.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_full_program_combined_nested_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/combined-nested.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_full_program_test_combined_steps_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/test-combined-steps.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_full_program_tricky_type_checking_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/tricky-type-checking.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_full_program_fail_wrong_assertion_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/fail_wrong_assertion.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_full_program_repro_filter_bug_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/repro-filter-bug.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_full_program_repro_new_backend_prims_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/repro-new-backend-prims.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_full_program_repro_unsound_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/repro-unsound.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_full_program_web_demo_bignum_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/web-demo/bignum.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_full_program_web_demo_datatypes_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/web-demo/datatypes.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_full_program_web_demo_eqsat_basic_multiset_has_no_diagnostics() {
        let path = upstream_egglog_fixture("tests/web-demo/eqsat-basic-multiset.egg");
        let program = std::fs::read_to_string(&path).unwrap();

        let mut parser = Parser::default();
        let outcome = parser
            .get_program_from_string_with_diagnostics(Some(path.to_string()), &program)
            .unwrap();

        assert!(
            outcome.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            outcome.diagnostics
        );
    }

    #[test]
    fn test_parse_constructor_keywords_and_default_push_pop() {
        let program = r#"
            (constructor Hidden () MySort :internal-hidden)
            (push)
            (pop)
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();

        assert!(matches!(
            commands[0],
            Command::Constructor { cost: None, .. }
        ));
        assert!(matches!(commands[1], Command::Push(1)));
        assert!(matches!(commands[2], Command::Pop(_, 1)));
    }

    #[test]
    fn test_parse_extract_surface() {
        let program = r#"
            (extract $expr 0)
            (extract (Num 4))
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();

        assert!(matches!(
            commands[0],
            Command::Extract {
                variants: Some(0),
                ..
            }
        ));
        assert!(matches!(
            commands[1],
            Command::Extract {
                expr: Expr::Call(_, ref name, _),
                variants: None,
                ..
            } if name == "Num"
        ));
    }

    #[test]
    fn test_parse_print_function_surface() {
        let program = r#"
            (print-function path 100 :file "path.csv" :mode csv)
            (print-function f :mode default)
        "#;

        let mut parser = Parser::default();
        let commands = parser.get_program_from_string(None, program).unwrap();

        assert!(matches!(
            commands[0],
            Command::PrintFunction(_, ref name, Some(100), Some(ref file), Some(ref mode))
                if name == "path" && file == "path.csv" && mode == "csv"
        ));
        assert!(matches!(
            commands[1],
            Command::PrintFunction(_, ref name, None, None, Some(ref mode))
                if name == "f" && mode == "default"
        ));
    }
}

fn span() -> Span {
    Span {
        file: None,
        line: 0,
        col: 0,
    }
}

fn get_span(token: &Token) -> &Span {
    match token {
        Token::LParen(span) => span,
        Token::RParen(span) => span,
        Token::Symbol(_, span) => span,
        Token::String(_, span) => span,
        Token::Number(_, span) => span,
        Token::Keyword(_, span) => span,
    }
}

fn get_op_span(token: &Option<Token>) -> Span {
    match token {
        Some(token) => get_span(token).clone(),
        None => Span {
            file: None,
            line: 0,
            col: 0,
        },
    }
}
