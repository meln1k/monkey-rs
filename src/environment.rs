use crate::ast::Identifier;
use crate::object::Object;
use std::collections::HashMap;

pub struct Environment {
    store: HashMap<Identifier, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        return Environment {
            store: HashMap::new(),
        };
    }

    pub fn get(&self, name: &Identifier) -> Option<&Object> {
        self.store.get(name)
    }

    pub fn set(&mut self, name: Identifier, value: Object) -> Object {
        self.store.insert(name, value);
        value
    }
}
