#[derive (Debug, PartialEq, Clone)]
pub enum LispVal {
    Int(i32),
    Char(char),
    String(String),
    Bool(bool),
    Ident(IdentType),
    List(Vec<LispVal>),
    Lambda(Box<LispVal>, Box<LispVal>),
    Nil
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
            LispVal::Lambda(_, _) => "lambda",
            LispVal::Nil => "nil",
        }.to_string()
    }
}

#[allow (dead_code)]
pub enum LispErr {
    ErrType(String, String),
    ErrNumArgs(u32, u32),
    ErrNotFunc(String),
    ErrUnknownIdent(String),
    ErrSyntax(String),
    ErrNotImplemented(String),
    ErrOther
}

impl std::fmt::Display for LispErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrType(exp, got)      => write!(f, "ERROR: Type error: expected {}, but got {}", exp, got),
            ErrNumArgs(exp, got)   => write!(f, "ERROR: Wrong number of arguments: expected {}, but got {}", exp, got),
            ErrNotFunc(func)       => write!(f, "ERROR: `{}` is not a function", func),
            ErrUnknownIdent(ident) => write!(f, "ERROR: Unknown identifier `{}`", ident),
            ErrSyntax(msg)         => write!(f, "ERROR: Wrong syntax in: `{}`", msg),
            ErrNotImplemented(x)   => write!(f, "ERROR: Not implemented:`{}`", x),
            ErrOther => write!(f, "ERROR"),
        }
    }
}

use LispErr::*;

pub enum Either<A, B>{
    Left (A),
    Right (B)
}

use Either::*;

use super::env::Env;

#[derive (Debug, PartialEq, Clone)]
pub enum IdentType {
    Name(String),
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    List,
    Eq,
    Println,
    Lambda,
    Define,
    If,
    Lt,
    Gt,
    LtEq,
    GtEq,
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
                match write!(f, "{})", xs.last().unwrap()) {
                    Ok(_) => (),
                    Err(err) => return Err(err),
                }
                Ok(())
            }
            Self::Nil => write!(f, "nil"),
            Self::Lambda(_, _) => write!(f, "proc"),
            Self::Ident(_) => Ok(()),
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

fn eval_define(args: &Vec<LispVal>, env: &mut Env) -> Either<LispErr, LispVal>{
    let args = args.get(1..).unwrap();
    if args.len() != 2 {
        return Left(ErrNumArgs(2, args.len() as u32));
    }

    let name;
    if let LispVal::Ident(ident) = args.get(0).unwrap(){
        if let IdentType::Name(str) = ident{
            name = str.to_string();
        } else {
            return Left(ErrOther);
        }
    } else {
        return Left(ErrOther);
    }
    let var;
    match eval(args.get(1).unwrap(), env){
        Right(x) => var = x,
        Left(err) => return Left(err),
    };
    env.insert(name.clone(), var);

    return Right(LispVal::Nil);
}

fn eval_lambda(lambda: &Vec<LispVal>) -> Either<LispErr, LispVal>{
    let args;
    match lambda.get(1){
        Some(x) => args = x,
        None => return Left(ErrSyntax("lambda arguments".to_string()))
    }
    let def;
    match lambda.get(2){
        Some(x) => def = x,
        None => return Left(ErrSyntax("lambda definition".to_string()))
    }

    Right(LispVal::Lambda(Box::new(args.clone()), Box::new(def.clone())))
}

fn eval_if(args: &Vec<LispVal>, env: &mut Env) -> Either<LispErr, LispVal>{
    let args = args.get(1..).unwrap();

    if args.len() < 3 {
        return Left(ErrNumArgs(3, args.len() as u32));
    }

    let fst;
    match eval(args.get(0).unwrap(), env){
        Right(val) => fst = val,
        Left(err) => return Left(err),
    }
    if fst.type_of() != "bool" {
        return Left(ErrType("bool".to_string(), fst.type_of().to_string()));
    }

    let fst_value;
    if let LispVal::Bool(val) = fst{
        fst_value = val;
    } else {
        return Left(ErrOther);
    }

    if fst_value {
        return eval(args.get(1).unwrap(), env);
    } else {
        return eval(args.get(2).unwrap(), env);
    }
}

fn apply_eq<F>(fst: i32, snd: i32, func: F) -> Either<LispErr, LispVal>
    where F: Fn(i32, i32) -> bool{
    if func(fst, snd){
        return Right(LispVal::Bool(true));
    }
    return Right(LispVal::Bool(false));
}

fn eval_list(lst: &Vec<LispVal>, env: &mut Env) -> Either<LispErr, LispVal>{
    let func;
    match lst.get(0).unwrap() {
        LispVal::Ident(IdentType::Define) => return eval_define(lst, env),
        LispVal::Ident(IdentType::Lambda) => return eval_lambda(lst),
        LispVal::Ident(IdentType::If) => return eval_if(lst, env),
        LispVal::Ident(x) => func = x,
        x => {
            return Left(ErrNotFunc(format!("{}", x)));
        },
    };

    let mut args = vec![];
    for e in lst.get(1..).unwrap() {
        match eval(e, env) {
            Right(val) => args.push(val),
            Left(eval_err) => {
                return Left(eval_err);
            },
        }
    };

    match func {
        IdentType::Add  => apply(args, |acc, e| acc + e),
        IdentType::Sub  => apply(args, |acc, e| acc - e),
        IdentType::Mult => apply(args, |acc, e| acc * e),
        IdentType::Div  => apply(args, |acc, e| acc / e),
        IdentType::Mod  => apply(args, |acc, e| acc % e),
        IdentType::Eq   => {
            let fst = args.get(0).unwrap();
            let res = args.get(1..).unwrap().into_iter().map(|e| e == fst).fold(true, |acc, e| acc && e);
            Right(LispVal::Bool(res))
        },
        IdentType::Lt | IdentType::Gt | IdentType::LtEq | IdentType::GtEq => {
            if args.len() != 2 {
                return Left(ErrNumArgs(2, args.len() as u32));
            }
            let fst;
            match get_int(args.get(0).unwrap()){
                Right(x) => fst = x,
                Left(err) => return Left(err),
            }
            let snd;
            match get_int(args.get(1).unwrap()){
                Right(x) => snd = x,
                Left(err) => return Left(err),
            }

            match func{
                IdentType::Lt => apply_eq(fst, snd, |a, b| a < b),
                IdentType::Gt => apply_eq(fst, snd, |a, b| a > b),
                IdentType::LtEq => apply_eq(fst, snd, |a, b| a <= b),
                IdentType::GtEq => apply_eq(fst, snd, |a, b| a >= b),
                _ => panic!("Unreachable code"),
            }
        }
        IdentType::List => return Right(LispVal::List(args)),
        IdentType::Println => {
            if args.len() != 1 {
                return Left(ErrNumArgs(1, args.len() as u32));
            }
            println!("{}", args.get(0).unwrap());
            Right(LispVal::Nil)
        },
        IdentType::Name(name) => {
            let lambda_def;
            match env.get_def(name.clone()){
                Right(f) => lambda_def = f,
                Left(err) => return Left(err),
            }

            let (lambda_params, lambda);
            // All of the errors here should be unreachable
            if let LispVal::Lambda(list_args, list_def) = lambda_def {
                if let LispVal::List(vec_args) = *list_args{
                    lambda_params = vec_args.clone();
                } else {
                    return Left(ErrOther);
                };

                if let LispVal::List(vec_def) = *list_def{
                    lambda = vec_def.clone();
                } else {
                    return Left(ErrOther);
                };
            } else {
                return Left(ErrOther);
            };

            if lambda_params.len() != args.len(){
                return Left(ErrNumArgs(lambda_params.len() as u32, args.len() as u32));
            }

            let old_env = env.get_state();

            for (i, param) in lambda_params.into_iter().enumerate() {
                let var_name;
                if let LispVal::Ident(temp) = param {
                    if let IdentType::Name(param_name) = temp {
                        var_name = param_name;
                    } else {
                        return Left(ErrSyntax("lambda paramater specifier".to_string()));
                    }
                } else {
                    return Left(ErrSyntax("lambda paramater specifier".to_string()));
                }

                let var_val = args.get(i).unwrap().clone();

                env.insert(var_name, var_val);
            }

            let lambda_res = eval(&LispVal::List(lambda), env);

            env.set_state(old_env);

            return lambda_res;
        },
        IdentType::Lambda => Left(ErrOther),
        IdentType::Define => Left(ErrOther),
        IdentType::If => Left(ErrOther),
    }
}

pub fn eval(expr: &LispVal, env: &mut Env) -> Either<LispErr, LispVal>{
    match expr {
        LispVal::List(xs)  => eval_list(xs, env),
        LispVal::Int(x)    => Right(LispVal::Int(*x)),
        LispVal::Char(x)   => Right(LispVal::Char(*x)),
        LispVal::String(x) => Right(LispVal::String(x.clone())),
        LispVal::Bool(x)   => Right(LispVal::Bool(*x)),
        LispVal::Nil       => Right(LispVal::Nil),
        //LispVal::Lambda(a,d) => eval_lambda(*a, *d),
        LispVal::Ident(x)  => match x {
            IdentType::Name(x) => env.get_def(x.to_string()),
            _ => Left(ErrOther),
        },
        LispVal::Lambda(_, _) => Left(ErrOther),
    }
}
