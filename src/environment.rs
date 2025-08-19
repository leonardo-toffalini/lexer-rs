use crate::object::Object;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    store: HashMap<String, Object>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            store: HashMap::new(),
        }
    }

    pub fn get(self: &Self, name: String) -> Option<&Object> {
        self.store.get(&name)
    }

    pub fn set(self: &mut Self, name: String, val: Object) -> () {
        self.store.insert(name, val);
    }
}
