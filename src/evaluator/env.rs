use super::object::Object;
use std::collections::HashMap;

pub struct Env {
    store: HashMap<String, Object>
}

impl Env {
    pub fn new() -> Env{
        Env{
            store: HashMap::new()
        }
    }
    pub fn get(&self, key: &String) -> Option<Object> {
        match self.store.get(key) {
            Some(obj) => Some(obj.clone()),
            None => None
        }
    }

}