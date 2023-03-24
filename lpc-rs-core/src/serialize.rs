use once_cell::sync::OnceCell;
use serde::{Deserialize, Serialize, Serializer};

/// Serialize a [`OnceCell`]
pub fn serialize_once_cell<S, T>(cell: &OnceCell<T>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
    T: Serialize,
{
    if let Some(value) = cell.get() {
        value.serialize(serializer)
    } else {
        serializer.serialize_none()
    }
}

/// Deserialize a [`OnceCell`]
pub fn deserialize_once_cell<'de, D, T>(deserializer: D) -> Result<OnceCell<T>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: serde::Deserialize<'de>,
{
    let value = Option::<T>::deserialize(deserializer)?;
    let cell = OnceCell::new();
    if let Some(value) = value {
        let _ = cell.set(value);
    }
    Ok(cell)
}
