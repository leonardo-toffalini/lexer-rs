use crate::object::Object;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    store: HashMap<String, Object>,
    outer: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Env) -> Env {
        Env {
            store: HashMap::new(),
            outer: Some(Box::new(outer)),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        match self.store.get(name) {
            Some(val) => Some(val),
            None => match &self.outer {
                Some(outer) => outer.get(name),
                None => None,
            },
        }
    }

    pub fn set(self: &mut Self, name: &str, val: Object) -> () {
        self.store.insert(name.to_string(), val);
    }
}
