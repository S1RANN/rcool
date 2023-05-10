use std::{borrow::Borrow, collections::HashSet, hash::Hash, ops::Deref, rc::Rc, fmt::Display};

#[derive(Debug)]
pub struct SharedString(Rc<String>);
impl Eq for SharedString {}
impl PartialEq for SharedString {
    fn eq(&self, other: &Self) -> bool {
        *self.0 == *other.0
    }
}
impl Hash for SharedString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}
impl Borrow<str> for SharedString {
    fn borrow(&self) -> &str {
        self.0.as_str()
    }
}
impl Clone for SharedString {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}
impl Deref for SharedString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl Display for SharedString{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl SharedString {
    pub fn new(s: &str) -> Self {
        SharedString(Rc::new(s.to_string()))
    }
}
pub(crate) struct StrTable(HashSet<SharedString>);

impl StrTable {
    pub(crate) fn new() -> Self {
        StrTable(HashSet::new())
    }
    pub(crate) fn insert(&mut self, s: &str) -> SharedString {
        if !self.0.contains(s) {
            let shared = SharedString::new(s);
            self.0.insert(shared.clone());
            shared
        } else {
            self.0.get(s).unwrap().clone()
        }
    }
}
