use std::collections::HashMap;

use super::scrunch::{LispVal, Either::{self, *}, LispErr::{self, *}};

pub struct Env {
    definitions: HashMap<String, LispVal>
}

impl Env {
    pub fn new() -> Env{
        Env {
            definitions: HashMap::new()
        }
    }

    pub fn get_var(&mut self, name: String) -> Either<LispErr, LispVal>{
        match self.definitions.get(&name){
            Some(x) => Right(x.clone()),
            None => Left(ErrUnknownIdent(name)),
        }
    }

    pub fn insert(&mut self, name: String, val: LispVal){
        self.definitions.insert(name, val);
    }
}
