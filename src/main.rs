use std::{
    collections::HashMap,
    fmt,
    io::{self, Write},
};

const QUOTATION_START: char = '[';
const QUOTATION_END: char = ']';

const COMMENT_START: char = '(';
const COMMENT_END: char = ')';

#[derive(Debug, Clone)]
enum ItemKind {
    Symbol(String),
    Bool(bool),
    Integer(isize),
    Float(f64),
    String(String),
    Quotation(Vec<Item>),
}

#[derive(Debug, Clone)]
struct Item {
    loc: Option<usize>,
    kind: ItemKind,
}

impl From<ItemKind> for Item {
    fn from(kind: ItemKind) -> Self {
        Self { loc: None, kind }
    }
}

impl Item {
    fn new(loc: usize, kind: ItemKind) -> Self {
        Self {
            loc: Some(loc),
            kind,
        }
    }
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ItemKind::Symbol(w) => f.write_str(w),
            ItemKind::Bool(b) => write!(f, "{b}"),
            ItemKind::Integer(n) => write!(f, "{n}"),
            ItemKind::Float(n) => write!(f, "{n:?}"),
            ItemKind::String(s) => write!(f, "{s:?}"),
            ItemKind::Quotation(q) => write!(
                f,
                "{QUOTATION_START}{}{QUOTATION_END}",
                q.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
        }
    }
}

fn parse(source: &str) -> Vec<Item> {
    let mut tokens = Vec::new();
    let mut chars = source.char_indices().peekable();

    while let Some((i, c)) = chars.next() {
        if c.is_whitespace() {
            continue;
        }

        match c {
            QUOTATION_START => {
                let mut bracket_count = 1;
                let mut last_i = i;

                while chars.peek().is_some() && bracket_count > 0 {
                    let (last, c) = chars.next().unwrap();

                    match c {
                        QUOTATION_START => bracket_count += 1,
                        QUOTATION_END => bracket_count -= 1,
                        _ => (),
                    }

                    last_i = last;
                }

                let mut quoted = parse(&source[i + 1..last_i]);
                for item in &mut quoted {
                    if let Some(v) = item.loc.as_mut() {
                        *v = *v + i + 1;
                    }
                }
                tokens.push(Item::new(i, ItemKind::Quotation(quoted)));
            }
            COMMENT_START => {
                let mut comment_count = 1;

                while chars.peek().is_some() && comment_count > 0 {
                    let (_, c) = chars.next().unwrap();

                    match c {
                        COMMENT_START => comment_count += 1,
                        COMMENT_END => comment_count -= 1,
                        _ => (),
                    }
                }
            }
            '"' => {
                let mut last_i = i;

                while chars.peek().is_some_and(|(_, c)| *c != '"') {
                    let (last, _) = chars.next().unwrap();
                    last_i = last;
                }

                chars.next();

                tokens.push(Item::new(
                    i,
                    ItemKind::String(source[(i + 1)..=last_i].to_string()),
                ));
            }
            _ => {
                let mut last_i = i;

                while chars.peek().is_some_and(|(_, c)| !c.is_whitespace()) {
                    let (last, _) = chars.next().unwrap();
                    last_i = last;
                }

                let symbol = &source[i..=last_i];

                tokens.push(Item::new(
                    i,
                    match symbol {
                        _ => {
                            if symbol.chars().all(|c| c.is_ascii_digit()) {
                                ItemKind::Integer(symbol.parse().unwrap())
                            } else if let Ok(f) = symbol.parse() {
                                ItemKind::Float(f)
                            } else {
                                ItemKind::Symbol(symbol.to_string())
                            }
                        }
                    },
                ));
            }
        }
    }

    tokens
}

#[derive(Debug)]
enum Type {
    Symbol,
    Bool,
    Integer,
    Float,
    String,
    Quotation,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Bool => "bool",
            Self::Symbol => "symbol",
            Self::Integer => "integer",
            Self::Float => "float",
            Self::String => "string",
            Self::Quotation => "quotation",
        })
    }
}

#[derive(Debug)]
enum RuntimeError {
    PopEmpty {
        instr: Item,
    },
    PopEmptyExec {
        instr: Item,
    },
    Expected {
        type_: Type,
        found: Option<Item>,
        instr: Item,
    },
    ExpectedExec {
        type_: Type,
        found: Option<Item>,
        instr: Item,
    },
    UndefinedSymbol {
        symbol: String,
        instr: Item,
    },
    Explicit {
        desc: String,
        instr: Item,
    },
}

#[derive(Default)]
struct Interpreter {
    data_stack: Vec<Item>,
    execution_stack: Vec<Item>,
    bindings: HashMap<String, Vec<Item>>,
}

macro_rules! pop_fn {
    ($type_name:ident, $name:ident, $typ:ty, $stack:ident, $err:ident) => {
        fn $name(&mut self, instr: &Item) -> Result<(Option<usize>, $typ), RuntimeError> {
            let Some(v) = self.$stack.pop() else {
                return Err(RuntimeError::$err {
                    type_: Type::$type_name,
                    found: None,
                    instr: instr.clone(),
                });
            };

            let Item {
                loc: loc_v,
                kind: ItemKind::$type_name(n),
            } = v
            else {
                return Err(RuntimeError::$err {
                    type_: Type::$type_name,
                    found: Some(v),
                    instr: instr.clone(),
                });
            };

            Ok((loc_v, n))
        }
    };
}

impl Interpreter {
    fn load(&mut self, items: Vec<Item>) {
        self.execution_stack
            .append(&mut items.into_iter().rev().collect());
    }

    fn pop(&mut self, instr: &Item) -> Result<Item, RuntimeError> {
        let Some(v) = self.data_stack.pop() else {
            return Err(RuntimeError::PopEmpty {
                instr: instr.clone(),
            });
        };

        Ok(v)
    }

    fn pop_exec(&mut self, instr: &Item) -> Result<Item, RuntimeError> {
        let Some(v) = self.execution_stack.pop() else {
            return Err(RuntimeError::PopEmptyExec {
                instr: instr.clone(),
            });
        };

        Ok(v)
    }

    pop_fn!(Integer, pop_integer, isize, data_stack, Expected);
    pop_fn!(Float, pop_float, f64, data_stack, Expected);
    pop_fn!(Quotation, pop_quotation, Vec<Item>, data_stack, Expected);
    pop_fn!(Bool, pop_bool, bool, data_stack, Expected);
    pop_fn!(String, pop_string, String, data_stack, Expected);
    pop_fn!(
        Symbol,
        pop_symbol_exec,
        String,
        execution_stack,
        ExpectedExec
    );

    #[allow(clippy::too_many_lines)]
    fn run(&mut self) -> Result<(), RuntimeError> {
        while let Some(item) = self.execution_stack.pop() {
            macro_rules! binop_n {
                ($op:tt) => {{
                    let (_, n2) = self.pop_integer(&item)?;
                    let (_, n1) = self.pop_integer(&item)?;

                    self.data_stack.push(ItemKind::Integer(n1 $op n2).into());
                }};
            }

            macro_rules! binop_f {
                ($op:tt) => {{
                    let (_, n2) = self.pop_float(&item)?;
                    let (_, n1) = self.pop_float(&item)?;

                    self.data_stack.push(ItemKind::Float(n1 $op n2).into());
                }};
            }

            macro_rules! binop_b {
                ($op:tt) => {{
                    let (_, n2) = self.pop_integer(&item)?;
                    let (_, n1) = self.pop_integer(&item)?;

                    self.data_stack.push(ItemKind::Bool(n1 $op n2).into());
                }};
            }

            macro_rules! binop_bf {
                ($op:tt) => {{
                    let (_, n2) = self.pop_float(&item)?;
                    let (_, n1) = self.pop_float(&item)?;

                    self.data_stack.push(ItemKind::Bool(n1 $op n2).into());
                }};
            }

            match item.kind {
                ItemKind::Integer(_)
                | ItemKind::Float(_)
                | ItemKind::Quotation(_)
                | ItemKind::Bool(_)
                | ItemKind::String(_) => self.data_stack.push(item),
                ItemKind::Symbol(ref name) => match name.as_str() {
                    "+" => binop_n!(+),
                    "+." => binop_f!(+),
                    "-" => binop_n!(-),
                    "-." => binop_f!(-),
                    "*" => binop_n!(*),
                    "*." => binop_f!(*),
                    "%" => binop_n!(%),
                    "%." => binop_f!(%),
                    "/" => binop_n!(/),
                    "/." => binop_f!(/),
                    ">" => binop_b!(>),
                    ">." => binop_bf!(>),
                    ">=" => binop_b!(>=),
                    ">=." => binop_bf!(>=),
                    "<" => binop_b!(<),
                    "<." => binop_bf!(<),
                    "<=" => binop_b!(<=),
                    "<=." => binop_bf!(<=),
                    "=" => binop_b!(==),
                    "true" => self.data_stack.push(ItemKind::Bool(true).into()),
                    "false" => self.data_stack.push(ItemKind::Bool(false).into()),
                    "call" => {
                        let (_, q) = self.pop_quotation(&item)?;

                        self.execution_stack
                            .append(&mut q.into_iter().rev().collect());
                    }
                    "dup" => {
                        let v = self.pop(&item)?;

                        self.data_stack.push(v.clone());
                        self.data_stack.push(v);
                    }
                    "swap" => {
                        let v2 = self.pop(&item)?;
                        let v1 = self.pop(&item)?;

                        self.data_stack.push(v2);
                        self.data_stack.push(v1);
                    }
                    "drop" => _ = self.pop(&item)?,
                    "over" => {
                        let v2 = self.pop(&item)?;
                        let v1 = self.pop(&item)?;

                        self.data_stack.push(v1.clone());
                        self.data_stack.push(v2);
                        self.data_stack.push(v1);
                    }
                    "if" => {
                        let (_, false_branch) = self.pop_quotation(&item)?;
                        let (_, true_branch) = self.pop_quotation(&item)?;
                        let (_, cond) = self.pop_bool(&item)?;

                        if cond {
                            self.execution_stack
                                .append(&mut true_branch.into_iter().rev().collect());
                        } else {
                            self.execution_stack
                                .append(&mut false_branch.into_iter().rev().collect());
                        }
                    }
                    ":" => {
                        let mut quotation = Vec::new();

                        let (_, name) = self.pop_symbol_exec(&item)?;

                        while let Some(w) = self.execution_stack.pop() {
                            if let ItemKind::Symbol(ref s) = w.kind {
                                if s.as_str() == ";" {
                                    break;
                                }
                            }

                            quotation.push(w);
                        }

                        self.bindings.insert(name, quotation);
                    }
                    "quote" => {
                        let v = self.pop_exec(&item)?;

                        self.data_stack.push(v);
                    }
                    "wrap" => {
                        let v = self.pop(&item)?;

                        if let ItemKind::Symbol(_) = v.kind {
                            self.data_stack.push(
                                ItemKind::Quotation(vec![
                                    ItemKind::Symbol("quote".to_string()).into(),
                                    v,
                                ])
                                .into(),
                            );
                        } else {
                            self.data_stack.push(ItemKind::Quotation(vec![v]).into());
                        }
                    }
                    "harsh-wrap" => {
                        let v = self.pop(&item)?;

                        self.data_stack.push(ItemKind::Quotation(vec![v]).into());
                    }
                    "compose" => {
                        let (_, mut q2) = self.pop_quotation(&item)?;
                        let (_, mut q1) = self.pop_quotation(&item)?;

                        q1.append(&mut q2);

                        self.data_stack.push(ItemKind::Quotation(q1).into());
                    }
                    "empty" => {
                        let (_, q) = self.pop_quotation(&item)?;

                        self.data_stack.push(ItemKind::Bool(q.is_empty()).into());
                    }
                    "split" => {
                        let (_, q) = self.pop_quotation(&item)?;

                        if q.len() <= 1 {
                            self.data_stack.push(ItemKind::Quotation(Vec::new()).into());
                            self.data_stack.push(ItemKind::Quotation(q).into());
                        } else {
                            let mut q = q;
                            let v = q.pop().unwrap();
                            self.data_stack.push(ItemKind::Quotation(q).into());
                            self.data_stack.push(ItemKind::Quotation(vec![v]).into());
                        }
                    }
                    "length" => {
                        let (_, q) = self.pop_quotation(&item)?;

                        self.data_stack
                            .push(ItemKind::Integer(q.len() as isize).into());
                    }
                    "exit" => {
                        let (_, n) = self.pop_integer(&item)?;

                        std::process::exit(i32::try_from(n).map_err(|e| {
                            RuntimeError::Explicit {
                                desc: format!("an exit code must be convertible to `i32`: {e}"),
                                instr: item,
                            }
                        })?);
                    }
                    "load" => {
                        let (_, s) = self.pop_string(&item)?;

                        let src = match std::fs::read_to_string(&s) {
                            Ok(src) => src,
                            Err(e) => {
                                return Err(RuntimeError::Explicit {
                                    desc: format!("couldn't read file `{s}`: {e}"),
                                    instr: item,
                                });
                            }
                        };

                        self.load(parse(&src));
                    }
                    "." => {
                        let v = self.pop(&item)?;

                        println!("{v}");
                    }
                    "puts" => {
                        let (_, s) = self.pop_string(&item)?;

                        println!("{s}");
                    }
                    "int" => {
                        let (_, f) = self.pop_float(&item)?;

                        self.data_stack
                            .push(ItemKind::Integer(f.trunc() as isize).into());
                    }
                    "float" => {
                        let (_, i) = self.pop_integer(&item)?;

                        self.data_stack.push(ItemKind::Float(i as f64).into());
                    }
                    _ => {
                        let Some(binding) = self.bindings.get(name) else {
                            return Err(RuntimeError::UndefinedSymbol {
                                symbol: name.clone(),
                                instr: item,
                            });
                        };

                        self.execution_stack
                            .append(&mut binding.iter().cloned().rev().collect());
                    }
                },
            }
        }

        Ok(())
    }
}

fn write_error(
    error: &RuntimeError,
    path: &str,
    source: &str,
    mut to: impl Write,
) -> std::io::Result<()> {
    write!(to, "error: ")?;
    match error {
        RuntimeError::PopEmpty { instr } => {
            writeln!(to, "tried to pop from an empty stack at `{instr}`")
        }
        RuntimeError::PopEmptyExec { instr } => {
            writeln!(to, "execution stack is empty at `{instr}`")
        }
        RuntimeError::Expected {
            type_,
            found,
            instr,
        } => {
            write!(to, "expected `{type_}`, but found ")?;
            match found {
                Some(item) => write!(to, "`{item}`")?,
                None => write!(to, "nothing")?,
            }
            writeln!(to, " at `{instr}`")
        }
        RuntimeError::ExpectedExec {
            type_,
            found,
            instr,
        } => {
            write!(to, "expected `{type_}` on execution stack, but found ")?;
            match found {
                Some(item) => write!(to, "`{item}`")?,
                None => write!(to, "nothing")?,
            }
            writeln!(to, " at `{instr}`")
        }
        RuntimeError::UndefinedSymbol { symbol, .. } => {
            writeln!(to, "undefined symbol `{symbol}`")
        }
        RuntimeError::Explicit { desc, instr } => {
            writeln!(to, "{desc} at `{instr}`")
        }
    }?;

    if let Some((line, col)) = error.location_in_source(source) {
        let source_line = source.lines().nth(line).unwrap_or("");

        writeln!(to, " --> [{path}:{}:{}]", line + 1, col + 1)?;
        writeln!(to, "    {source_line}")?;
        writeln!(to, "    {}^", " ".repeat(col))?;
    }

    Ok(())
}

impl RuntimeError {
    fn location(&self) -> Option<usize> {
        match self {
            RuntimeError::PopEmpty { instr }
            | RuntimeError::PopEmptyExec { instr }
            | RuntimeError::Expected { instr, .. }
            | RuntimeError::ExpectedExec { instr, .. }
            | RuntimeError::UndefinedSymbol { instr, .. }
            | RuntimeError::Explicit { instr, .. } => instr.loc,
        }
    }

    fn location_in_source(&self, source: &str) -> Option<(usize, usize)> {
        let offset = self.location()?;
        let mut line = 0;
        let mut col = 0;
        // let mut current_offset = 0;

        for (i, c) in source.chars().enumerate() {
            if i == offset {
                return Some((line, col));
            }

            if c == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }

            // current_offset += 1;
        }

        None
    }
}

fn main() -> Result<(), std::io::Error> {
    let mut args = std::env::args();
    let Some(_program_name) = args.next() else {
        unreachable!("expected program name as first command line argument")
    };

    if let Some(file_name) = args.next() {
        let src = match std::fs::read_to_string(&file_name) {
            Ok(src) => src,
            Err(error) => {
                eprintln!("couldn't read file `{file_name}`");
                return Err(error);
            }
        };

        let mut interpreter = Interpreter::default();
        interpreter.load(parse(&src));
        if let Err(error) = interpreter.run() {
            write_error(&error, &file_name, &src, io::stderr())?;
        }
    } else {
        eprintln!("Flora REPL v0.1");

        let mut interpreter = Interpreter::default();

        loop {
            let previous_stack = interpreter.data_stack.clone();

            eprint!("> ");
            io::stdout().flush()?;

            let mut input = String::new();
            io::stdin().read_line(&mut input)?;

            interpreter.load(parse(&input));
            if let Err(error) = interpreter.run() {
                write_error(&error, "<stdin>", &input, io::stderr())?;
                interpreter.data_stack = previous_stack;
                continue;
            }

            eprintln!(
                "[{}]",
                interpreter
                    .data_stack
                    .iter()
                    .map(|value: &Item| value.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            );
        }
    }

    Ok(())
}
