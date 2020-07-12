use crate::object::Object;
use std::collections::HashMap;

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        return Environment {
            store: HashMap::new(),
        };
    }

    pub fn get(&self, name: &String) -> Option<&Object> {
        self.store.get(name)
    }

    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name, value);
        value
    }
}
