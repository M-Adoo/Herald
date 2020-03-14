//! Herald is a library to help you to track change of your data struct.
//!
//! Herald require any change of your data struct should represent by a type,
//! and this type should implement `LastChange` trait.
//!
//! # Herald macro attribute
//!
//! `Herald` provide a macro attribute to auto implement `Herald`, and provide
//! a set of methods to update field. For example
//!
//! ```
//! use herald::prelude::*;
//!
//! #[herald]
//! struct Point {
//!     x: f32,
//!     y: f32
//! }
//!
//! let mut p = Point {
//!     x: 0.,
//!     y: 0.,
//!     herald_impl: Default::default(),
//! };
//!
//! p.change_stream().subscribe(|v| {
//!     println!("{:?}", v.borrow().changes)
//! });
//!
//! p.set_x(1.);
//! p.set_y(1.);
//!
//! // Will print like below
//! //
//! // PointChange { x: Some(Change { before: 0.0, after: 1.0 }), y: None }
//! // PointChange { x: None, y: Some(Change { before: 0.0, after: 1.0 })
//! ```
//!
//! ## what features `herald` attribute provide?
//!
//! todo
//!
//! ## what `herald` attr do ?
//!
//! `herald` attr will implement a `Herald` for your struct, and a set of set
//! method provide to update struct, any update across these method will be
//! tracked.
//!
//! Assume we have below code
//!
//! ```
//! # use herald::prelude::*;
//! #[herald]
//! struct Point {
//!     x: f32,
//!     y: f32
//! }
//! ```
//!
//! First, a `herald_impl`(HeraldImpl) field with `HeraldImpl<'_, Point>` into
//! point, it's a private field, but will be used to implement `Herald`.
//!
//! Second, Define a PointChange type to represent change of Point:
//! ```
//! # use herald::prelude::*;
//! struct PointChange {
//!     x: Option<Change<f32>>,
//!     y: Option<Change<f32>>,
//! }
//! ```
//! Third, implement `Herald` trait for Point by `PointChange` and
//! `herald_impl`. Fourth, `set_x` and `set_y` method provide on `Point`, use
//! these method to update `Point` will be tracked. Fifth, Define a `PointState`
//! type and impl `set_state` method to update multi fields at once and change
//! will be tracked.
//!
//! ```ignore
//! PointState {
//!     x: Option<f32>,
//!     y: Option<f32>,
//! }
//!
//! impl Point {
//!     fn set_state(&mut self, state: PointState) {
//!         ...
//!     }
//! }
//! ```
//!
//! ## Some field you not want to tracked.
//!
//! If you want not track some field, you can use `skip` argument, like
//!
//! ```
//! # use herald::prelude::*;
//! #[herald]
//! struct Point {
//!     x: f32,
//!     #[herald(skip)]
//!     y: f32,
//! }
//! ```
pub use herald_derive::herald;
pub use rxrust::{
  ops::box_it::LocalCloneBoxOp, prelude::*, subject::LocalSubject,
};
use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;

/// A interface for deal with change which from [Herald](Herald)
pub trait LastChange {
  fn merge(&mut self, other: &Self);
}

pub trait Herald<'a> {
  /// The type represented changed of `Self`.
  type C: LastChange + Default;

  /// A observable will emit change.
  fn change_stream(&mut self) -> LocalSubject<'a, RefChangeEvent<'a, Self>, ()>
  where
    Self: Sized;

  /// A observable will batched the change and emit it when `notifier` emit a
  /// value.
  fn batched_change_stream(
    &mut self,
    notifier: impl LocalObservable<'a, Err = ()> + Clone + 'a,
  ) -> LocalCloneBoxOp<'a, RefChangeEvent<'a, Self>, ()>
  where
    Self: Sized;
}

/// A struct help for some type to implement [`Herald`](Herald). Hold it as a
/// field and use it to emit `Change`, provide `change_stream` and
/// `batched_change_stream`.
pub struct HeraldImpl<'a, T: Herald<'a>> {
  changes: Option<LocalSubject<'a, RefChangeEvent<'a, T>, ()>>,
  batched_changes: Option<LocalCloneBoxOp<'a, RefChangeEvent<'a, T>, ()>>,
}

#[derive(Clone, Default, PartialEq, Debug)]
pub struct Change<T> {
  pub before: T,
  pub after: T,
}

pub struct ChangeEvent<'a, T: Herald<'a>> {
  pub host: &'a mut T,
  pub changes: T::C,
}

pub struct RefChangeEvent<'a, T: Herald<'a>>(Rc<RefCell<ChangeEvent<'a, T>>>);

impl<'a, T: Herald<'a>> Clone for RefChangeEvent<'a, T> {
  fn clone(&self) -> Self { RefChangeEvent(self.0.clone()) }
}

impl<'a, T: Herald<'a>> PayloadCopy for RefChangeEvent<'a, T> {}

impl<'a, T: Herald<'a>> RefChangeEvent<'a, T> {
  pub fn new(host: &'a mut T, changes: T::C) -> Self {
    RefChangeEvent(Rc::new(RefCell::new(ChangeEvent { host, changes })))
  }

  #[inline]
  pub fn borrow_mut(&self) -> RefMut<'_, ChangeEvent<'a, T>> {
    self.0.borrow_mut()
  }

  #[inline]
  pub fn borrow(&self) -> Ref<'_, ChangeEvent<'a, T>> { self.0.borrow() }
}

impl<'a, T: Herald<'a>> Default for HeraldImpl<'a, T> {
  #[inline]
  fn default() -> Self {
    HeraldImpl {
      changes: None,
      batched_changes: None,
    }
  }
}

impl<'a, T: Herald<'a> + 'a> HeraldImpl<'a, T> {
  /// return an observable, which emit the host type changes. Use this subscribe
  /// the host changes.
  #[inline]
  pub fn change_stream(
    &mut self,
  ) -> LocalSubject<'a, RefChangeEvent<'a, T>, ()> {
    let changes = if let Some(ref changes) = self.changes {
      changes.clone()
    } else {
      let changes = Subject::new();
      self.changes = Some(changes.clone());
      changes
    };
    changes
  }

  /// like change_stream, but changes has batched by the `notifier`.
  pub fn batched_change_stream(
    &mut self,
    notifier: impl LocalObservable<'a, Err = ()> + Clone + 'a,
  ) -> LocalCloneBoxOp<'a, RefChangeEvent<'a, T>, ()> {
    if let Some(ref batched) = self.batched_changes {
      batched.clone()
    } else {
      let batched = self
        .change_stream()
        .clone()
        .map(|v| Some(v))
        .merge(notifier.map(|_| None))
        // reset the scan operator when notify emit.
        .scan_initial(None, |acc: Option<RefChangeEvent<'a, T>>, chgs_event| {
          if let Some(chgs) = chgs_event {
            if let Some(acc) = acc {
              acc.borrow_mut().changes.merge(&chgs.borrow().changes);
              Some(acc)
            } else {
              Some(chgs)
            }
          } else {
            None
          }
        })
        .filter(|v| v.is_some())
        .map(|v| v.unwrap())
        // .sample(notifier)
        .share()
        .box_it();
      self.batched_changes = Some(batched.clone());
      batched
    }
  }

  pub fn emit_change(&mut self, host: &mut T, changes: T::C) {
    if let Some(ref mut subject) = self.changes {
      if subject.subscribed_size() > 0 {
        // unsafe introduce:
        // `events `(`Subject`) just emit the item, not care about `host` 's
        // lifetime,  require `host` lifetime longer than `'a` because
        // hold a PhantomData `T`.
        let host = unsafe { std::mem::transmute(host) };
        subject.next(RefChangeEvent::new(host, changes))
      }
    }
  }

  pub fn is_subscribed(&self) -> bool {
    self
      .changes
      .as_ref()
      .map_or(false, |s| s.subscribed_size() > 0)
  }
}

#[cfg(test)]
mod tests {
  use crate::prelude::*;

  #[derive(Default)]
  struct Button<'a> {
    click_times: i32,
    herald_impl: HeraldImpl<'a, Self>,
  }

  #[derive(Default, Clone, PartialEq, Debug)]
  struct ButtonChange {
    click_times: Change<i32>,
  }

  impl LastChange for ButtonChange {
    fn merge(&mut self, other: &Self) {
      self.click_times.after = other.click_times.after;
    }
  }

  impl<'a> Button<'a> {
    fn click(&mut self) {
      self.click_times += 1;
      if self.herald_impl.is_subscribed() {
        let info =
          unsafe { &mut *(&mut self.herald_impl as *mut HeraldImpl<'_, _>) };
        info.emit_change(
          self,
          ButtonChange {
            click_times: Change {
              after: self.click_times,
              before: self.click_times - 1,
            },
          },
        )
      }
    }
  }

  impl<'a> Herald<'a> for Button<'a> {
    type C = ButtonChange;

    fn change_stream(
      &mut self,
    ) -> LocalSubject<'a, RefChangeEvent<'a, Self>, ()>
    where
      Self: Sized,
    {
      self.herald_impl.change_stream()
    }

    /// A observable will batched the change and emit it when `notifier` emit a
    /// value.
    fn batched_change_stream(
      &mut self,
      notifier: impl LocalObservable<'a, Err = ()> + Clone + 'a,
    ) -> LocalCloneBoxOp<'a, RefChangeEvent<'a, Self>, ()>
    where
      Self: Sized,
    {
      self.herald_impl.batched_change_stream(notifier)
    }
  }

  #[test]
  fn change_stream() {
    let mut chgs = vec![];
    let mut batched: Option<_> = None;
    {
      let mut notify: LocalSubject<'_, (), ()> = Subject::new();
      let mut btn = Button::default();
      btn.change_stream().subscribe(|c| {
        chgs.push(c.borrow().changes.clone());
      });
      btn
        .batched_change_stream(notify.clone())
        .subscribe(|c| batched = Some(c.borrow().changes.clone()));

      btn.click();
      btn.click();
      notify.next(());
    }
    assert_eq!(chgs.len(), 2);
    assert_eq!(
      chgs[0],
      ButtonChange {
        click_times: Change {
          before: 0,
          after: 1
        }
      }
    );
    assert_eq!(
      chgs[1],
      ButtonChange {
        click_times: Change {
          before: 1,
          after: 2
        }
      }
    );

    assert_eq!(
      batched.unwrap(),
      ButtonChange {
        click_times: Change {
          before: 0,
          after: 2
        }
      }
    );
  }
}
