mod lisp;

use std::{env, fs};

use lisp::scrunch::{eval, get_exprs, Either, LispErr, LispVal};
use lisp::parse::parse_expr;
use lisp::env::Env;

use linefeed::{Interface, ReadResult};

fn repl(){
    let reader = Interface::new("scrunch").unwrap();
    reader.set_prompt("scrunch> ").unwrap();

    let mut env = Env::new();

    while let ReadResult::Input(input) = reader.read_line().unwrap() {
        if input == "quit" || input == "exit" {
            break;
        }

        reader.add_history(input.clone());
        let expr;
        match parse_expr(input){
            lisp::scrunch::Either::Right(e) => expr = e,
            lisp::scrunch::Either::Left(err) => {
                println!("{}", err);
                continue;
            },
        }

        match eval(&expr, &mut env) {
            lisp::scrunch::Either::Left(err) => {
                println!("{}", err);
            }
            lisp::scrunch::Either::Right(val) => {
                println!("{}", val);
            }
        };
    }
}

fn eval_file(path: String) -> Option<LispErr>{
    let contents = match fs::read_to_string(path.clone()) {
        Ok(x) => x,
        Err(_) => {
            return Some(LispErr::ErrOther(format!("File {path} not found")));
        },
    };

    let mut env = Env::new();


    let parsed_exprs = match get_exprs(contents){
        Either::Right(exprs_) => exprs_,
        Either::Left(err) => return Some(err)
    };

    let mut exprs: Vec<LispVal> = vec![];
    for expr in parsed_exprs{
        match parse_expr(expr){
            lisp::scrunch::Either::Right(e) => exprs.push(e),
            lisp::scrunch::Either::Left(err) => return Some(err),
        }
    }

    for expr in exprs {
        if let LispVal::List(lst) = expr.clone() {
            if lst.len() == 0{
                continue;
            }
        }
        let res = eval(&expr, &mut env);
        if let Either::Left(err) = res {
            println!("{}", err);
        }
    }

    None
}

fn main() {
    let cmd_args: Vec<String> = env::args().skip(1).collect();
    if cmd_args.len() == 0 {
        repl();
    } else if cmd_args.len() == 1 {
        let filename;
        let arg0 = cmd_args.get(0).unwrap();
        if arg0.ends_with(".lisp") {
            filename = arg0.clone();
        } else {
            println!("ERROR: Filename must end in .lisp, provided `{}`", arg0);
            return;
        }

        let err = eval_file(filename);

        if let Some(e) = err {
            println!("{}", e);
        }
    } else {
        println!("ERROR: Incorrect usage");
    }
}
