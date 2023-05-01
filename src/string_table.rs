use std::collections::HashMap;

pub(crate) struct StrTable {
    table: HashMap<String, usize>,
    index: usize,
}

pub(crate) trait StringTable {
    fn insert(&mut self, s: &str) -> usize;
    fn get(&self, index: usize) -> Option<&str>;
}

impl StrTable {
    pub(crate) fn new() -> StrTable {
        StrTable {
            table: HashMap::new(),
            index: 0,
        }
    }
}

impl StringTable for StrTable {
    fn insert(&mut self, s: &str) -> usize {
        if let Some(idx) = self.table.get(s) {
            *idx
        } else {
            let idx = self.index;
            self.table.insert(s.to_string(), idx);
            self.index += 1;
            idx
        }
    }

    fn get(&self, index: usize) -> Option<&str> {
        for (key, value) in self.table.iter(){
            if *value == index {
                return Some(key.as_str());
            }
        }
        None
    }
}
