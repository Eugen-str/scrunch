pub mod parse;
pub mod scrunch;

#[cfg(test)]
mod tests{
    use crate::lisp::scrunch::LispVal;

    use super::parse::parse_expr;

    use super::scrunch::eval;

    #[test]
    fn test_basic() {
        assert_eq!(LispVal::Int(4), eval(&parse_expr("(+ 2 2)".to_string())));
    }

    #[test]
    fn test_rec(){
        assert_eq!(LispVal::Int(7), eval(&parse_expr("(+ 3 (+ 1 1 1 1))".to_string())));
    }
}
