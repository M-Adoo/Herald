use crate::ident::UID;

/// Event for host object,
/// `C` is the change type of type `T`
/// `U` is the user event type will emit
pub enum Event<C, U> {
  /// immediate change event, hold the changes
  ImmediateChange(C),
  /// batch change event, hold the batched changes
  BatchedChange(C),
  /// User custom  event.
  User(U),
  /// Event from embed struct.
  Pop(PopEvent<U>),
}

impl<'a, C, U> Event<C, U> {
  pub fn to_pop(self) -> PopEvent<U> {
    match self {
      Event::ImmediateChange(_) => PopEvent::ImmediateChange,
      Event::BatchedChange(_) => PopEvent::BatchedChange,
      Event::User(u) => PopEvent::User(u),
      Event::Pop(p) => p,
    }
  }
}

/// Event from embed struct
pub enum PopEvent<U> {
  ImmediateChange,
  BatchedChange,
  User(U),
}

pub struct EventData<'a, T, C, U> {
  pub host: &'a mut T,
  pub from: UID,
  pub event: Event<C, U>,
}
