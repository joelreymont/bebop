use serde::Serialize;
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::FromIterator;

#[derive(Debug, PartialEq, Serialize)]
pub struct Environment<K: Hash + Eq + Clone, V: PartialEq + Clone> {
    env: HashMap<K, V>,
}

impl<K: Hash + Eq + Clone, V: PartialEq + Clone> Clone for Environment<K, V> {
    fn clone(&self) -> Self {
        Self {
            env: self.env.clone(),
        }
    }
}

impl<K: Hash + Eq + Clone, V: PartialEq + Clone> Default for Environment<K, V> {
    fn default() -> Self {
        Self {
            env: HashMap::new(),
        }
    }
}

impl<K: Hash + Eq + Clone, V: PartialEq + Clone> Environment<K, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.env.insert(key, value);
    }

    /// Tries to find the value of a key in the Environment.
    pub fn get(&self, key: &K) -> Option<&V> {
        self.env.get(key)
    }

    pub fn is_empty(&self) -> bool {
        self.env.is_empty()
    }

    pub fn len(&self) -> usize {
        self.env.len()
    }
}

impl<K: Hash + Eq + Clone, V: PartialEq + Clone> FromIterator<(K, V)> for Environment<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self {
            env: HashMap::from_iter(iter),
        }
    }
}
