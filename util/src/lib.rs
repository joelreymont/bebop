use serde::Serialize;

pub mod id;
pub mod meta;

pub fn dump<T>(obj: &T) -> Result<String, ron::Error>
where
    T: Serialize,
{
    ron::ser::to_string_pretty(&obj, ron::ser::PrettyConfig::default())
}
