mod lisp;

use lisp::scrunch::eval;
use lisp::parse::parse_expr;
use lisp::env::Env;

use linefeed::{Interface, ReadResult};

fn main() {
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

        println!("{:?}", expr);

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
