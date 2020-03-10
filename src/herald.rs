pub use herald_derive::herald;
pub use rxrust::{ops::box_it::LocalCloneBoxOp, prelude::*, subject::LocalSubject};
use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;

/// the latest changes from the self type.
pub trait Commit {
    fn merge(&mut self, other: &Self);
}

pub trait Herald<'a> {
    type C: Commit + Default;
    type State;
    fn commit(&mut self, state: Self::State);
    fn change_stream(&mut self) -> LocalSubject<'a, RefChangeEvent<'a, Self>, ()>
    where
        Self: Sized;
    fn batched_change_stream(
        &mut self,
        notifier: impl LocalObservable<'a, Err = ()> + Clone + 'a,
    ) -> LocalCloneBoxOp<'a, RefChangeEvent<'a, Self>, ()>
    where
        Self: Sized;
}

#[derive(Clone)]
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
    fn clone(&self) -> Self {
        RefChangeEvent(self.0.clone())
    }
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
    pub fn borrow(&self) -> Ref<'_, ChangeEvent<'a, T>> {
        self.0.borrow()
    }
}

pub struct HeraldImpl<'a, T: Herald<'a>> {
    changes: Option<LocalSubject<'a, RefChangeEvent<'a, T>, ()>>,
    batched_changes: Option<LocalCloneBoxOp<'a, RefChangeEvent<'a, T>, ()>>,
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
    /// return an observable, which emit the host type changes. Use this subscribe the
    /// host changes.
    #[inline]
    pub fn change_stream(&mut self) -> LocalSubject<'a, RefChangeEvent<'a, T>, ()> {
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
                // `events `(`Subject`) just emit the item, not care about `host` 's lifetime,
                //  require `host` lifetime longer than `'a` because hold a PhantomData `T`.
                let host = unsafe { std::mem::transmute(host) };
                subject.next(RefChangeEvent::new(host, changes))
            }
        }
    }

    pub fn is_subscribed(&self) -> bool {
        self.changes
            .as_ref()
            .map_or(false, |s| s.subscribed_size() > 0)
    }
}

#[cfg(test)]
mod tests {}
