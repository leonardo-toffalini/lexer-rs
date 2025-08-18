use crate::object::Object;
use std::collections::HashMap;

pub struct Env {
    store: HashMap<String, Object>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            store: HashMap::new(),
        }
    }

    pub fn get(env: &Env, name: String) -> Option<&Object> {
        env.store.get(&name)
    }

    pub fn set(env: &mut Env, name: String, val: Object) -> Option<Object> {
        env.store.insert(name, val)
    }
}
