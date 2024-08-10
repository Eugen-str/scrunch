#[derive (Debug, PartialEq)]
pub enum LispVal {
    Int (i32),
    Ident (String),
    List (Vec<LispVal>)
}

impl std::fmt::Display for LispVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LispVal::Int(x) => write!(f, "{}", *x),
            LispVal::List(xs) => {
                _ = write!(f, "(");
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

fn get_int(val: &LispVal) -> i32{
    match val {
        LispVal::Int(x) => *x,
        _ => panic!("z"),
    }
}

fn apply<F>(args: Vec<LispVal>, func: F) -> LispVal
    where F: Fn(i32, LispVal) -> i32{
    let fst = get_int(args.get(0).unwrap());
    LispVal::Int(args.into_iter().skip(1).fold(fst, func))
}

fn eval_list(lst: &Vec<LispVal>) -> LispVal{
    if lst.len() < 2 {
        return LispVal::Int(0);
    }

    let func = match lst.get(0).unwrap().to_owned() {
        LispVal::Ident(x) => x,
        _ => panic!("x"),
    };
    let args: Vec<LispVal> = lst.get(1..).unwrap().into_iter().map(|e| eval(e)).collect();

    match func.as_str() {
        "+" => return apply(args, |acc, e| acc + get_int(&e)),
        "-" => return apply(args, |acc, e| acc - get_int(&e)),
        "*" => return apply(args, |acc, e| acc * get_int(&e)),
        "/" => return apply(args, |acc, e| acc / get_int(&e)),
        "list" => return LispVal::List(args),
        x => panic!("Unknown operator: {x}"),
    }
}

pub fn eval(expr: &LispVal) -> LispVal{
    match expr {
        LispVal::List(xs) => eval_list(xs),
        LispVal::Int(x) => LispVal::Int(*x),
        LispVal::Ident(_) => panic!("Unreachable code"),
    }
}
