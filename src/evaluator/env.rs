use super::object::Object;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(PartialEq, Debug, Clone)]
pub struct Env {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Env>>>, // Without RefCell we can't borrow Env as mutable
}

impl Env {
    pub fn new() -> Self {
        Env {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        match self.store.get(key) {
            Some(obj) => Some(obj.clone()),
            None => match self.outer {
                Some(ref outer) => outer.borrow_mut().get(key),
                None => None,
            },
        }
    }

    pub fn set(&mut self, key: String, val: &Object) {
        self.store.insert(key, val.clone());
    }

    pub fn new_with_outer(outer: &Rc<RefCell<Env>>) -> Self {
        Env {
            store: HashMap::new(),
            outer: Some(outer.clone()),
        }
    }
}