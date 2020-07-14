use crate::ast::Identifier;
use crate::object::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Environment {
    bindings: HashMap<Identifier, Rc<Value>>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Environment>> {
        return Rc::new(RefCell::new(Environment {
            bindings: HashMap::new(),
            outer: None,
        }));
    }

    pub fn new_enclosed(env: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        return Rc::new(RefCell::new(Environment {
            bindings: HashMap::new(),
            outer: Some(env),
        }));
    }

    pub fn get(&self, name: &Identifier) -> Option<Rc<Value>> {
        match self.bindings.get(name) {
            Some(rc) => Some(Rc::clone(rc)),
            None => match &self.outer {
                Some(e) => e.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: Identifier, value: Rc<Value>) -> Rc<Value> {
        self.bindings.insert(name, Rc::clone(&value));
        value
    }
}
