use std::collections::HashMap;

use super::scrunch::{LispVal, Either::{self, *}, LispErr::{self, *}};

pub struct Env {
    pub definitions: HashMap<String, LispVal>,
}

impl Env {
    pub fn new() -> Env{
        Env {
            definitions: HashMap::new(),
        }
    }

    pub fn get_def(&mut self, name: String) -> Either<LispErr, LispVal>{
        match self.definitions.get(&name){
            Some(x) => Right(x.clone()),
            None => Left(ErrUnknownIdent(name)),
        }
    }

    pub fn insert(&mut self, name: String, val: LispVal){
        self.definitions.insert(name, val);
    }

    pub fn get_state(&mut self) -> HashMap<String, LispVal>{
        self.definitions.clone()
    }

    pub fn set_state(&mut self, map: HashMap<String, LispVal,>){
        self.definitions = map;
    }
}
