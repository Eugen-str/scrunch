#[derive (Debug, PartialEq)]
pub enum LispVal {
    Int (i32),
    Char (char),
    String (String),
    Bool (bool),
    Ident (IdentType),
    List (Vec<LispVal>)
}

impl LispVal {
    fn type_of(&self) -> String{
        match self {
            LispVal::Int(_) => "int",
            LispVal::Char(_) => "char",
            LispVal::String(_) => "string",
            LispVal::Bool(_) => "bool",
            LispVal::Ident(_) => "ident",
            LispVal::List(_) => "list",
        }.to_string()
    }
}

pub enum LispErr {
    ErrType(String, String),
    ErrNumArgs(u32, u32),
    ErrNotFunc(String),
    ErrOther
}

impl std::fmt::Display for LispErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrType(exp, got)   => write!(f, "Type error: expected {}, but got {}", exp, got),
            ErrNumArgs(exp, got)=> write!(f, "Wrong number of arguments: expected {}, but got {}", exp, got),
            ErrNotFunc(func)    => write!(f, "`{}` is not a function", func),
            ErrOther            => write!(f, "Error"),
        }
    }
}

use LispErr::*;

pub enum Either<A, B>{
    Left (A),
    Right (B)
}

use Either::*;

#[derive (Debug, PartialEq)]
pub enum IdentType {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    List,
    Eq,
}

impl std::fmt::Display for IdentType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            IdentType::Add  => "+",
            IdentType::Sub  => "-",
            IdentType::Mult => "*",
            IdentType::Div  => "/",
            IdentType::Mod  => "%",
            IdentType::List => "list",
            IdentType::Eq   => "eq?",
        };
        _ = write!(f, "{}", symbol);
        Ok(())
    }
}

impl std::fmt::Display for LispVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LispVal::Int(x) => write!(f, "{}", *x),
            LispVal::Bool(b) => {
                if *b {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            }
            LispVal::Char(c) => {
                match *c {
                    '\n' => write!(f, "#\\newline"),
                    ' ' => write!(f, "#\\space"),
                    _ => write!(f, "#\\{}", *c)
                }
            },
            LispVal::String(s) => write!(f, "\"{}\"", *s),
            LispVal::List(xs) => {
                _ = write!(f, "'(");
                for x in xs.get(0..xs.len()-1).unwrap(){
                    let res = write!(f, "{} ", x);
                    match res {
                        Ok(_) => (),
                        Err(err) => return Err(err),
                    }
                }
                let res = write!(f, "{})", xs.last().unwrap());
                match res {
                    Ok(_) => (),
                    Err(err) => return Err(err),
                }
                Ok(())
            }
            Self::Ident(x) => write!(f, "{}", x)
        }
    }
}

fn get_int(val: &LispVal) -> Either<LispErr, i32>{
    match val {
        LispVal::Int(x) => Right(*x),
        x => Left(ErrType("int".to_string(), x.type_of())),
    }
}

fn apply<F>(args: Vec<LispVal>, func: F) -> Either<LispErr, LispVal>
    where F: Fn(i32, i32) -> i32{
    let mut unwrapped_args = vec![];
    for e in args.into_iter() {
        match get_int(&e){
            Right(val) => unwrapped_args.push(val),
            Left(err) => return Left(err),
        }
    }
    let fst = unwrapped_args.get(0).unwrap().to_owned();

    let res = unwrapped_args.into_iter().skip(1).fold(fst, func);
    Right(LispVal::Int(res))
}

fn eval_list(lst: &Vec<LispVal>) -> Either<LispErr, LispVal>{
    let func;
    match lst.get(0).unwrap() {
        LispVal::Ident(x) => func = x,
        x => {
            return Left(ErrNotFunc(format!("{}", x)));
        },
    };

    let mut args = vec![];
    for e in lst.get(1..).unwrap() {
        match eval(e) {
            Left(eval_err) => {
                return Left(eval_err);
            },
            Right(val) => args.push(val),
        }
    };

    match func {
        IdentType::Add  => return apply(args, |acc, e| acc + e),
        IdentType::Sub  => return apply(args, |acc, e| acc - e),
        IdentType::Mult => return apply(args, |acc, e| acc * e),
        IdentType::Div  => return apply(args, |acc, e| acc / e),
        IdentType::Mod  => return apply(args, |acc, e| acc % e),
        IdentType::Eq   => {
            let fst = args.get(0).unwrap();
            let res = args.get(1..).unwrap().into_iter().map(|e| e == fst).fold(true, |acc, e| acc && e);
            Right(LispVal::Bool(res))
        },
        IdentType::List => return Right(LispVal::List(args)),
    }
}

pub fn eval(expr: &LispVal) -> Either<LispErr, LispVal>{
    match expr {
        LispVal::List(xs)  => eval_list(xs),
        LispVal::Int(x)    => Right(LispVal::Int(*x)),
        LispVal::Char(x)   => Right(LispVal::Char(*x)),
        LispVal::String(x) => Right(LispVal::String(x.clone())),
        LispVal::Bool(x)   => Right(LispVal::Bool(*x)),
        LispVal::Ident(_)  => Left(ErrOther),
    }
}
