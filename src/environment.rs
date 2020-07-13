use crate::ast::Identifier;
use crate::object::Value;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Environment {
    store: HashMap<Identifier, Rc<Value>>,
}

impl Environment {
    pub fn new() -> Environment {
        return Environment {
            store: HashMap::new(),
        };
    }

    pub fn get(&self, name: &Identifier) -> Option<Rc<Value>> {
        self.store.get(name).map(|v| Rc::clone(v))
    }

    pub fn set(&mut self, name: Identifier, value: Rc<Value>) -> Rc<Value> {
        let clone        = Rc::clone(&value);
        self.store.insert(name, value);
        clone
    }
}
