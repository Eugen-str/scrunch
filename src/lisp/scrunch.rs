#[derive (Debug, PartialEq, Eq, Clone, Hash)]
pub enum LispVal {
    Int(i32),
    Char(char),
    String(String),
    Bool(bool),
    Ident(IdentType),
    List(Vec<LispVal>),
    Lambda(Box<LispVal>, Box<LispVal>),
    Macro(Box<LispVal>, Box<LispVal>),
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
            LispVal::Macro(_, _) => "macro",
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
    ErrOther(String)
}

impl std::fmt::Display for LispErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrType(exp, got)      => write!(f, "ERROR: Type error: expected {}, but got {}", exp, got),
            ErrNumArgs(exp, got)   => write!(f, "ERROR: Wrong number of arguments: expected {}, but got {}", exp, got),
            ErrNotFunc(func)       => write!(f, "ERROR: `{}` is not a function", func),
            ErrUnknownIdent(ident) => write!(f, "ERROR: Unknown identifier `{}`", ident),
            ErrSyntax(msg)         => write!(f, "ERROR: Wrong syntax in {}", msg),
            ErrNotImplemented(x)   => write!(f, "ERROR: Not implemented: {}", x),
            ErrOther(msg) => write!(f, "ERROR: {}", msg),
        }
    }
}

use std::fs;

use LispErr::*;

use super::env::Env;

#[derive (Debug, PartialEq, Eq, Clone, Hash)]
pub enum IdentType {
    Name(String),
    Add, Sub, Mult, Div, Mod,
    List, Append,
    Do,
    Println, Write, Writeln, Display,
    Lambda, Define, Macro,
    If, Cond,
    Eq, Lt, Gt, LtEq, GtEq,
    Car, Cdr, Cons,
    Import, Export,
    ThrowError
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
                if xs.len() != 0 {
                    for x in xs.get(0..xs.len()-1).unwrap(){
                        let res = write!(f, "{} ", x);
                        match res {
                            Ok(_) => (),
                            Err(err) => return Err(err),
                        }
                    }
                    match write!(f, "{}", xs.last().unwrap()) {
                        Ok(_) => (),
                        Err(err) => return Err(err),
                    }
                }
                _ = write!(f, ")");
                Ok(())
            }
            Self::Nil => write!(f, "nil"),
            Self::Lambda(_, _) => write!(f, "proc"),
            Self::Macro(_, _) => write!(f, "macro"),
            Self::Ident(_) => write!(f, "ident"),
        }
    }
}

fn get_int(val: &LispVal) -> Result<i32, LispErr>{
    match val {
        LispVal::Int(x) => Ok(*x),
        x => Err(ErrType("int".to_string(), x.type_of())),
    }
}

fn apply<F>(args: Vec<LispVal>, func: F) -> Result<LispVal, LispErr>
    where F: Fn(i32, i32) -> i32{
    let mut unwrapped_args = vec![];
    for e in args.into_iter() {
        match get_int(&e){
            Ok(val) => unwrapped_args.push(val),
            Err(err) => return Err(err),
        }
    }
    let fst = unwrapped_args.get(0).unwrap().to_owned();

    let res = unwrapped_args.into_iter().skip(1).fold(fst, func);
    Ok(LispVal::Int(res))
}

fn eval_define(args: &Vec<LispVal>, env: &mut Env) -> Result<LispVal, LispErr>{
    let args = args.get(1..).unwrap();
    if args.len() != 2 {
        return Err(ErrNumArgs(2, args.len() as u32));
    }

    let name;
    if let LispVal::Ident(ident) = args.get(0).unwrap(){
        if let IdentType::Name(str) = ident{
            name = str.to_string();
        } else {
            return Err(ErrOther("no description yet".to_string()));
        }
    } else {
        return Err(ErrOther("no description yet".to_string()));
    }
    let var;
    match eval(args.get(1).unwrap(), env){
        Ok(x) => var = x,
        Err(err) => return Err(err),
    };
    env.insert(name.clone(), var);

    return Ok(LispVal::Nil);
}

fn make_lambda(lambda: &Vec<LispVal>) -> Result<LispVal, LispErr>{
    let args;
    match lambda.get(1){
        Some(x) => args = x,
        None => return Err(ErrSyntax("lambda arguments".to_string()))
    }
    let def;
    match lambda.get(2){
        Some(x) => def = x,
        None => return Err(ErrSyntax("lambda definition".to_string()))
    }

    Ok(LispVal::Lambda(Box::new(args.clone()), Box::new(def.clone())))
}

fn make_macro(sc_macro: &Vec<LispVal>, env: &mut Env) -> Result<LispVal, LispErr>{
    let macro_args = match sc_macro.get(1..){
        Some(x) => x.to_vec(),
        None => return Err(ErrNumArgs(3, (sc_macro.len() - 1) as u32)),
    };

    let ident  = match macro_args.get(0).unwrap() {
        LispVal::Ident(IdentType::Name(name)) => name,
        _ => return Err(ErrSyntax("macro name".to_string()))
    };
    let params = match macro_args.get(1).unwrap() {
        LispVal::List(x) => LispVal::List(x.clone()),
        _ => return Err(ErrSyntax(format!("{} macro parameters", ident)))
    };
    let def = match macro_args.get(2).unwrap() {
        LispVal::List(x) => LispVal::List(x.clone()),
        _ => return Err(ErrSyntax(format!("{} macro definition", ident)))
    };

    //println!("macro: \n\tident = {:?}\n\tparams = {:?}\n\tdef = {:?}", ident, params, def);
    env.insert(ident.clone(), LispVal::Macro(Box::new(params.clone()), Box::new(def.clone())));

    //println!("env = {:?}", env);

    Ok(LispVal::Nil)
}

fn eval_if(args: &Vec<LispVal>, env: &mut Env) -> Result<LispVal, LispErr>{
    let args = args.get(1..).unwrap();

    if args.len() < 3 {
        return Err(ErrNumArgs(3, args.len() as u32));
    }

    let fst;
    match eval(args.get(0).unwrap(), env){
        Ok(val) => fst = val,
        Err(err) => return Err(err),
    }
    if fst.type_of() != "bool" {
        return Err(ErrType("bool".to_string(), fst.type_of().to_string()));
    }

    let fst_value;
    if let LispVal::Bool(val) = fst{
        fst_value = val;
    } else {
        return Err(ErrOther("no description yet".to_string()));
    }

    if fst_value {
        return eval(args.get(1).unwrap(), env);
    } else {
        return eval(args.get(2).unwrap(), env);
    }
}

fn eval_cond(args: &Vec<LispVal>, env: &mut Env) -> Result<LispVal, LispErr>{
    let args = args.get(1..).unwrap();

    if args.len() == 0 {
        return Err(ErrNumArgs(1, args.len() as u32));
    }

    for arg in args{
        if let LispVal::List(lst) = arg {
            if lst.len() != 2 {
                return Err(ErrNumArgs(2, lst.len() as u32));
            }
            let cond = match eval(lst.get(0).unwrap(), env){
                Ok(x) => x,
                Err(err) => return Err(err)
            };
            if cond.type_of() != "bool" {
                return Err(ErrType("bool".to_string(), cond.type_of()));
            }

            if let LispVal::Bool(true) = cond {
                return eval(lst.get(1).unwrap(), env);
            }
        } else {
            return Err(ErrType("cond statement".to_string(), arg.type_of()));
        }
    }

    return Err(ErrOther("non exhaustive patterns in cond".to_string()));
}

fn apply_eq<F>(fst: i32, snd: i32, func: F) -> Result<LispVal, LispErr>
    where F: Fn(i32, i32) -> bool{
    if func(fst, snd){
        return Ok(LispVal::Bool(true));
    }
    return Ok(LispVal::Bool(false));
}

fn display(x: LispVal) -> String{
    match x{
        LispVal::Int(x) => format!("{}", x),
        LispVal::Bool(b) => {
            if b {
                format!("#t")
            } else {
                format!("#f")
            }
        }
        LispVal::Char(c) => {
            format!("{}", c)
        },
        LispVal::String(s) => format!("{}", s),
        LispVal::List(xs) => {
            let mut list = vec![];
            list.push(format!("("));
            if xs.len() != 0 {
                for x in xs.get(0..xs.len()-1).unwrap(){
                    list.push(format!("{} ", x));
                }
                list.push(format!("{}", xs.last().unwrap()));
            }
            list.push(format!(")"));
            list.concat()
        }
        LispVal::Nil => format!("()"),
        LispVal::Lambda(_, _) => format!("#<lambda>"),
        LispVal::Macro(_, _) => format!("#<macro>"),
        LispVal::Ident(_) => format!("#<ident>"),
    }
}

fn get_args(lst: &Vec<LispVal>, env: &mut Env) -> Result<Vec<LispVal>, LispErr>{
    let mut args = vec![];
    for e in lst.get(1..).unwrap() {
        match eval(e, env) {
            Ok(val) => args.push(val),
            Err(eval_err) => {
                return Err(eval_err);
            },
        }
    };
    return Ok(args);
}

fn eval_lambda(lst: &Vec<LispVal>, env: &mut Env, p: Box<LispVal>, d: Box<LispVal>) -> Result<LispVal, LispErr>{
    let args = match get_args(lst, env) {
        Ok(args_) => args_,
        Err(err) => return Err(err),
    };

    let lambda_params = match *p{
        LispVal::List(lambda_p) => lambda_p,
        _ => return Err(ErrSyntax("idk what to put here yet".to_string())),
    };

    if lambda_params.len() != args.len(){
        return Err(ErrNumArgs(lambda_params.len() as u32, args.len() as u32));
    }

    let old_env = env.get_state();

    for (i, param) in lambda_params.into_iter().enumerate() {
        let var_name;
        if let LispVal::Ident(temp) = param {
            if let IdentType::Name(param_name) = temp {
                var_name = param_name;
            } else {
                return Err(ErrSyntax("lambda paramater specifier".to_string()));
            }
        } else {
            return Err(ErrSyntax("lambda paramater specifier".to_string()));
        }

        let var_val = args.get(i).unwrap().clone();

        env.insert(var_name, var_val);
    }

    let lambda = match *d{
        LispVal::List(lambda_d) => lambda_d,
        LispVal::Ident(IdentType::Name(name)) => {
            return env.get_def(name);
        },
        x => return Ok(x),
    };
    let lambda_res = eval(&LispVal::List(lambda), env);

    env.set_state(old_env);

    return lambda_res;
}

fn replace_each(expr: &mut LispVal, target: LispVal, replacement: LispVal){
    if *expr == target {
        *expr = replacement;
        return;
    }

    match expr{
        LispVal::List(xs) => {
            for x in xs.into_iter() {
                replace_each(x, target.clone(), replacement.clone());
            }
        },
        _ => {},
    };
}

fn expand_macro(lst: &Vec<LispVal>, p: Box<LispVal>, d: Box<LispVal>) -> Result<LispVal, LispErr>{
    let macro_params = match *p{
        LispVal::List(macro_p) => macro_p,
        _ => return Err(ErrSyntax("idk what to put here yet".to_string())),
    };
    let mut macro_def = match *d{
        LispVal::List(macro_d) => LispVal::List(macro_d),
        _ => return Err(ErrSyntax("idk what to put here yet".to_string())),
    };

    let macro_args = match lst.get(1..){
        Some(x) => x,
        None => return Err(ErrNumArgs(macro_params.len() as u32, 0))
    };

    if macro_params.len() != macro_args.len(){
        return Err(ErrNumArgs(macro_params.len() as u32, macro_args.len() as u32));
    }

    for (i, param) in macro_params.into_iter().enumerate(){
        let corresponding_arg = macro_args.get(i).unwrap();

        replace_each(&mut macro_def, param, corresponding_arg.clone());
    }

    return Ok(macro_def);
}

pub fn get_exprs(contents: String) -> Result<Vec<String>, LispErr>{
    let mut str_acc: Vec<char> = vec![];
    let mut res: Vec<String> = vec![];
    let mut paren_count = 0;
    let mut in_comment = false;

    for c in contents.chars() {
        if c == ';' {
            in_comment = true;
        }
        if c == '\n' {
            in_comment = false;
            continue;
        }
        if in_comment { continue; }

        str_acc.push(c);
        if c == '(' {
            paren_count += 1;
        } else if c == ')' {
            paren_count -= 1;

            if paren_count == 0 {
                res.push(str_acc.clone().into_iter().collect());
                str_acc = vec![];
            }
        }
    }

    return Result::Ok(res);
}

fn eval_list(lst: &Vec<LispVal>, env: &mut Env) -> Result<LispVal, LispErr>{
    if lst.is_empty() {
        return Err(ErrOther("illegal expression".to_string()));
    }
    // evaluating anonymous lambda functions
    if let Ok(LispVal::Lambda(p, d)) = eval(lst.get(0).unwrap(), env) {
        return eval_lambda(lst, env, p, d);
    }
    // expanding + evalutaing macros
    if let Ok(LispVal::Macro(p, d)) = eval(lst.get(0).unwrap(), env) {
        let macro_def = match expand_macro(lst, p, d){
            Ok(x) => x,
            Err(err) => return Err(err),
        };
        return eval(&macro_def, env);
    }

    let func;
    match lst.get(0).unwrap() {
        LispVal::Ident(IdentType::Define) => return eval_define(lst, env),
        LispVal::Ident(IdentType::Macro) => return make_macro(lst, env),
        LispVal::Ident(IdentType::Lambda) =>return make_lambda(lst),
        LispVal::Ident(IdentType::If) => return eval_if(lst, env),
        LispVal::Ident(IdentType::Cond) => return eval_cond(lst, env),
        LispVal::Ident(x) => func = x,
        x => {
            return Err(ErrNotFunc(format!("{}", x)));
        },
    };

    // TODO: Fix misleading error message when trying to run a nonexistant function.
    // Executing `(xyz a)` gives ERROR: Unknown identifier `a`, but should give
    // ERROR: Unknown identifier `xyz`
    let args = match get_args(lst, env) {
        Ok(args_) => args_,
        Err(err) => return Err(err),
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
            Ok(LispVal::Bool(res))
        },
        IdentType::Lt | IdentType::Gt | IdentType::LtEq | IdentType::GtEq => {
            if args.len() != 2 {
                return Err(ErrNumArgs(2, args.len() as u32));
            }
            let fst;
            match get_int(args.get(0).unwrap()){
                Ok(x) => fst = x,
                Err(err) => return Err(err),
            }
            let snd;
            match get_int(args.get(1).unwrap()){
                Ok(x) => snd = x,
                Err(err) => return Err(err),
            }

            match func{
                IdentType::Lt => apply_eq(fst, snd, |a, b| a < b),
                IdentType::Gt => apply_eq(fst, snd, |a, b| a > b),
                IdentType::LtEq => apply_eq(fst, snd, |a, b| a <= b),
                IdentType::GtEq => apply_eq(fst, snd, |a, b| a >= b),
                _ => panic!("Unreachable code"),
            }
        }
        IdentType::List => return Ok(LispVal::List(args)),
        IdentType::Append => {
            if args.len() == 0 {
                return Err(ErrNumArgs(1, 0));
            }

            let mut acc = vec![];
            for e in args.into_iter() {
                match e {
                    LispVal::List(xs) => acc.append(&mut xs.clone()),
                    _ => return Err(ErrType("list".to_string(), e.type_of())),
                }
            };
            return Ok(LispVal::List(acc));
        },
        IdentType::Println => {
            if args.len() != 1 {
                return Err(ErrNumArgs(1, args.len() as u32));
            }
            println!("{}", args.get(0).unwrap());
            Ok(LispVal::Nil)
        },
        IdentType::Writeln | IdentType::Write => {
            if args.len() != 1 {
                return Err(ErrNumArgs(1, args.len() as u32));
            }

            match func {
                IdentType::Write => {
                    print!("{}", display(args.get(0).unwrap().clone()));
                },
                IdentType::Writeln => {
                    println!("{}", display(args.get(0).unwrap().clone()));
                },
                _ => panic!("Unreachable code"),
            }
            Ok(LispVal::Nil)
        },
        IdentType::Display => {
            if args.len() != 1 {
                return Err(ErrNumArgs(1, args.len() as u32));
            }
            Ok(LispVal::String(display(args.get(0).unwrap().clone())))
        }
        IdentType::Name(name) => {
            let def;
            match env.get_def(name.clone()){
                Ok(f) => def = f,
                Err(err) => return Err(err),
            }

            if let LispVal::Lambda(p, d) = def {
                return eval_lambda(lst, env, p, d);
            }

            if let LispVal::Macro(p, d) = def {
                return expand_macro(lst, p, d);
            }
            return Err(ErrOther("apart from builtins, only macros and lambda functions can be evaluated".to_string()));
        },
        IdentType::Car => {
            if args.len() != 1 {
                return Err(ErrNumArgs(1, args.len() as u32));
            }
            let lst = args.get(0).unwrap().clone();

            let lst_vals = if let LispVal::List(unwrapped_list) = lst {
                unwrapped_list
            } else {
                return Err(ErrType("list".to_string(), lst.type_of()));
            };

            if lst_vals.len() >= 1 {
                return Ok(lst_vals.get(0).unwrap().clone());
            } else {
                return Err(ErrOther("no description yet".to_string()));
            }
        },
        IdentType::Cdr => {
            if args.len() != 1 {
                return Err(ErrNumArgs(1, args.len() as u32));
            }
            let lst = args.get(0).unwrap().clone();

            let lst_vals = if let LispVal::List(unwrapped_list) = lst {
                unwrapped_list
            } else {
                return Err(ErrType("list".to_string(), lst.type_of()));
            };

            if lst_vals.len() == 1 {
                return Ok(LispVal::List(vec![]));
            }

            if lst_vals.len() >= 2 {
                return Ok(LispVal::List(lst_vals.get(1..).unwrap().to_vec().clone()));
            } else {
                return Err(ErrOther("no description yet".to_string()));
            }
        },
        IdentType::Cons => {
            if args.len() != 2 {
                return Err(ErrNumArgs(2, args.len() as u32));
            }
            let arg1 = args.get(0).unwrap().clone();
            if arg1.type_of() == "list" {
                return Err(ErrType("any except list".to_string(), "list".to_string()));
            }
            let arg2 = args.get(1).unwrap().clone();
            if arg2.type_of() != "list" {
                return Err(ErrType("list".to_string(), arg2.type_of()));
            }

            let mut res = if let LispVal::List(contents) = arg2 {
                contents
            } else {
                vec![]
            };

            res.insert(0, arg1);

            return Ok(LispVal::List(res));
        },
        IdentType::Import => {
            let filenames = match args.get(0..) {
                Some(x) => x.to_vec(),
                None => return Err(ErrNumArgs(1, 0)),
            };

            for filename in filenames.into_iter() {
                if let LispVal::String(path) = filename{
                    let contents = match fs::read_to_string(path.clone()) {
                        Ok(x) => x,
                        Err(_) => {
                            return Err(ErrOther(format!("File {path} not found")));
                        },
                    };

                    let parsed_exprs = match get_exprs(contents){
                        Result::Ok(exprs_) => exprs_,
                        Result::Err(err) => return Err(err)
                    };

                    let mut exprs: Vec<LispVal> = vec![];
                    let mut exports: Vec<LispVal> = vec![];
                    for expr in parsed_exprs{
                        match crate::parse_expr(expr){
                            Ok(e) => match e.clone() {
                                LispVal::List(xs) => {
                                    match xs.clone().get(0){
                                        Some(LispVal::Ident(IdentType::Export)) => exports = xs.get(1..).unwrap().to_vec(),
                                        Some(_) => exprs.push(e),
                                        _ => {},
                                    }
                                }
                                _ => exprs.push(e)
                            },
                            Err(err) => return Err(err),
                        }
                    }

                    for expr in exprs {
                        if let LispVal::List(lst) = expr.clone() {
                            if lst.len() == 0{
                                continue;
                            }

                            if lst.len() > 2 {
                                if let Ok(LispVal::Macro(p, d)) = eval(lst.get(0).unwrap(), env) {
                                    let macro_def = match expand_macro(&lst, p, d){
                                        Ok(x) => x,
                                        Err(err) => return Err(err),
                                    };
                                    if let LispVal::List(lst) = macro_def{
                                        if lst.len() > 2 {
                                            if exports.contains(lst.get(1).unwrap()){
                                                let res = eval(&expr, env);
                                                if let Result::Err(err) = res {
                                                    println!("{}", err);
                                                }
                                            }
                                        }
                                    }
                                } else if (lst.get(0).unwrap().clone() == LispVal::Ident(IdentType::Define) ||
                                    lst.get(0).unwrap().clone() == LispVal::Ident(IdentType::Macro)) &&
                                    exports.contains(lst.get(1).unwrap()){
                                        let res = eval(&expr, env);
                                        if let Result::Err(err) = res {
                                            println!("{}", err);
                                        }
                                    }
                            }
                        }
                    }
                } else {
                    return Err(ErrType("string".to_string(), filename.type_of()));
                }
            }

            return Ok(LispVal::Nil);
        },
        IdentType::Do => {
            if args.len() == 0 {
                return Err(ErrNumArgs(1, 0));
            }
            for arg in args {
                if let Err(e) = eval(&arg, env){
                    return Err(e);
                }
            }
            return Ok(LispVal::Nil);
        },
        IdentType::ThrowError => {
            if args.len() != 1 {
                return Err(ErrNumArgs(1, args.len() as u32));
            }

            match args.get(0).unwrap(){
                LispVal::String(err) => return Err(ErrOther(err.clone())),
                _ => return Err(ErrType("string".to_string(), args.get(0).unwrap().type_of())),
            }
        }
        IdentType::Macro  |
        IdentType::Lambda |
        IdentType::Define |
        IdentType::Export |
        IdentType::Cond   |
        IdentType::If     => return Err(ErrOther("unreachable code".to_string())),
    }
}

pub fn eval(expr: &LispVal, env: &mut Env) -> Result<LispVal, LispErr>{
    match expr {
        LispVal::List(xs)  => eval_list(xs, env),
        LispVal::Int(x)    => Ok(LispVal::Int(*x)),
        LispVal::Char(x)   => Ok(LispVal::Char(*x)),
        LispVal::String(x) => Ok(LispVal::String(x.clone())),
        LispVal::Bool(x)   => Ok(LispVal::Bool(*x)),
        LispVal::Nil       => Ok(LispVal::Nil),
        LispVal::Ident(x)  => match x {
            IdentType::Name(x) => env.get_def(x.to_string()),
            _ => Err(ErrOther("no description yet".to_string())),
        },
        LispVal::Lambda(_, _) => Err(ErrOther("unreachable code".to_string())),
        LispVal::Macro(_, _) => Err(ErrOther("unreachable code".to_string())),
    }
}
