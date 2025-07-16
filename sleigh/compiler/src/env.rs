use ordermap::OrderMap;
use serde::{Serialize, Serializer};
use std::hash::Hash;
use std::iter::FromIterator;

#[derive(Debug, PartialEq)]
pub struct Environment<K, V>
where
    K: Hash + Eq + Clone + Ord + Serialize,
    V: PartialEq + Clone + Serialize,
{
    env: OrderMap<K, V>,
}

impl<K, V> Clone for Environment<K, V>
where
    K: Hash + Eq + Clone + Ord + Serialize,
    V: PartialEq + Clone + Serialize,
{
    fn clone(&self) -> Self {
        Self {
            env: self.env.clone(),
        }
    }
}

impl<K, V> Default for Environment<K, V>
where
    K: Hash + Eq + Clone + Ord + Serialize,
    V: PartialEq + Clone + Serialize,
{
    fn default() -> Self {
        Self {
            env: OrderMap::new(),
        }
    }
}

impl<K, V> Serialize for Environment<K, V>
where
    K: Hash + Eq + Clone + Ord + Serialize,
    V: PartialEq + Clone + Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut results: Vec<(&K, &V)> = self.env.iter().collect();
        results.sort_by(|x, y| x.0.cmp(y.0));
        results.serialize(serializer)
    }
}

impl<K, V> Environment<K, V>
where
    K: Hash + Eq + Clone + Ord + Serialize,
    V: PartialEq + Clone + Serialize,
{
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

impl<K, V> FromIterator<(K, V)> for Environment<K, V>
where
    K: Hash + Eq + Clone + Ord + Serialize,
    V: PartialEq + Clone + Serialize,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self {
            env: OrderMap::from_iter(iter),
        }
    }
}
