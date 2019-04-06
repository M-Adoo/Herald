#![feature(raw)]

pub mod events;
pub mod herald;
pub mod ident;
pub mod subscribe_change;

pub mod prelude {
  pub use crate::events;
  pub use crate::events::{Event, EventData};
  pub use crate::herald;
  pub use crate::herald::*;
  pub use crate::ident;
  pub use crate::ident::{Ident, UID};
  pub use crate::subscribe_change::SubscribeChange;
  pub use rxrust::prelude::*;
  pub use rxrust::subject::LocalSubject;
}
