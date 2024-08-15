mod lisp;

use std::{env, fs};

use lisp::scrunch::{eval, get_exprs, Either, IdentType, LispErr, LispVal};
use lisp::parse::parse_expr;
use lisp::env::Env;

use linefeed::{Interface, ReadResult};
use colored::Colorize;

fn repl(){
    let reader = Interface::new("scrunch").unwrap();
    reader.set_prompt("scrunch> ").unwrap();

    let mut env = Env::new();

    // include standard.scr by default
    println!("Importing `include/standard.scr...`");
    match eval(&LispVal::List(vec![
            LispVal::Ident(IdentType::Import),
            LispVal::String("include/standard.scr".to_string())
    ]), &mut env){
        Either::Right(_) => println!("{}", "Successfully imported `include/standard.scr`!".bright_green()),
        Either::Left(err) => println!("{}", format!("{}", err).bright_red()),
    }
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
            lisp::scrunch::Either::Right(val) => {
                println!("{}", val);
            }
            lisp::scrunch::Either::Left(err) => {
                println!("{}", format!("{}", err).bright_red());
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
            println!("{}", format!("{}", err).bright_red());
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
        if arg0.ends_with(".scr") {
            filename = arg0.clone();
        } else {
            println!("{}", format!("ERROR: Filename must end in .scr, provided `{}`", arg0).bright_red());
            return;
        }

        let err = eval_file(filename);

        if let Some(e) = err {
            println!("{}", e);
        }
    } else {
        println!("{}", "ERROR: Incorrect usage".bright_red());
    }
}
