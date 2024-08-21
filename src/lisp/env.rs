use std::collections::HashMap;

use super::scrunch::{LispVal, LispErr::{self, *}};

#[derive(Debug)]
pub struct Env {
    pub definitions: HashMap<String, LispVal>,
}

impl Env {
    pub fn new() -> Env{
        Env {
            definitions: HashMap::new(),
        }
    }

    pub fn get_def(&mut self, name: String) -> Result<LispVal, LispErr>{
        match self.definitions.get(&name){
            Some(x) => Ok(x.clone()),
            None => Err(ErrUnknownIdent(name)),
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
