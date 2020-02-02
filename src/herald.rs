use crate::{events::Event, events::EventData, ident::Ident};

pub use herald_derive::herald;
use rxrust::ops::filter_map::FilterMapOp;
use rxrust::ops::FilterMap;
use rxrust::prelude::*;
use rxrust::subject::{SubjectMutRefValue, SubjectValue};

pub trait Herald<'a, T, C, U> {
    fn herald(&mut self) -> &mut HeraldInfo<'a, T, C, U>;
}

#[derive(Clone)]
pub struct Change<T> {
    pub before: T,
    pub after: T,
}

pub struct ChangeEvent<'a, T, C> {
    host: &'a mut T,
    changes: &'a mut C,
}

impl<'a, T, C> ChangeEvent<'a, T, C> {
    #[inline]
    pub fn host(&mut self) -> &mut T {
        self.host
    }
    #[inline]
    pub fn changes(&mut self) -> &mut C {
        self.changes
    }
}

pub struct HeraldInfo<'a, T, C, U> {
    changes: Option<C>,
    events: LocalSubject<'a, SubjectMutRefValue<EventData<'a, T, C, U>>, SubjectValue<()>>,
}

impl<'a, T: Ident + 'a, C: 'a, U: 'a> Default for HeraldInfo<'a, T, C, U> {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

#[inline(always)]
pub fn constrain_change_event_callback<F, T, C>(f: F) -> F
where
    F: for<'r> FnMut(ChangeEvent<'r, T, C>),
{
    f
}

impl<'a, T: Ident + 'a, C: 'a, U: 'a> HeraldInfo<'a, T, C, U> {
    pub fn new() -> Self {
        HeraldInfo {
            changes: None,
            events: Subject::local(),
        }
    }

    /// return the batched changes, or defaults if no changes.
    pub fn batched(&mut self) -> &mut C
    where
        C: Default,
    {
        if let Some(ref mut batched) = self.changes {
            batched
        } else {
            self.changes = Some(C::default());
            self.changes.as_mut().unwrap()
        }
    }

    pub fn events_subject(
        &mut self,
    ) -> LocalSubject<'a, SubjectMutRefValue<EventData<'a, T, C, U>>, SubjectValue<()>> {
        self.events.fork()
    }

    pub fn immediate_changes_events(
        &self,
    ) -> FilterMapOp<
        LocalSubject<'a, SubjectMutRefValue<EventData<'a, T, C, U>>, SubjectValue<()>>,
        for<'r> fn(&'r mut EventData<'a, T, C, U>) -> Option<ChangeEvent<'r, T, C>>,
        &mut EventData<'a, T, C, U>,
    > {
        self.events.fork().filter_map(|ed| {
            if let Event::ImmediateChange(ref mut chgs) = ed.event {
                Some(ChangeEvent {
                    host: ed.host,
                    changes: chgs,
                })
            } else {
                None
            }
        })
    }

    pub fn batched_changes_events(
        &self,
    ) -> FilterMapOp<
        LocalSubject<'a, SubjectMutRefValue<EventData<'a, T, C, U>>, SubjectValue<()>>,
        for<'r> fn(&'r mut EventData<'a, T, C, U>) -> Option<ChangeEvent<'r, T, C>>,
        &mut EventData<'a, T, C, U>,
    > {
        self.events.fork().filter_map(|ed| {
            if let Event::BatchedChange(ref mut chgs) = ed.event {
                Some(ChangeEvent {
                    host: ed.host,
                    changes: chgs,
                })
            } else {
                None
            }
        })
    }

    pub fn send_event(&mut self, host: &mut T, event: Event<C, U>) {
        let from = host.uid();
        // unsafe introduce:
        // `events `(`Subject`) just emit the item, not care about `host` 's lifetime,
        //  require `host` lifetime longer than `'a` because hold a PhantomData `T`.
        let host = unsafe { std::mem::transmute(host) };
        let mut data = EventData { host, from, event };
        self.events.next(&mut data);
    }
}

#[test]
fn test_lifetime() {
    use crate::prelude::*;
    struct Btn;
    impl Ident for Btn {};
    let mut btn = Btn;
    let mut w: HeraldInfo<'_, Btn, (), ()> = HeraldInfo::new();

    w.immediate_changes_events()
        .subscribe((|_e: ChangeEvent<'_, _, _>| {}) as for<'r> fn(ChangeEvent<'r, _, _>));
    w.send_event(&mut btn, Event::User(()));
}

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    #[herald]
    struct Button {
        btns: f32,
    }

    #[test]
    fn event_lifetime() {
        let mut event_times = 0;
        {
            let mut btn = Button {
                btns: 0.,
                _herald_info: HeraldInfo::new(),
            };

            btn.herald().events_subject().subscribe(|_btns: &mut _| {
                event_times += 1;
            });

            btn.send_event(Event::BatchedChange(ButtonChanges::default()));
        }
        assert_eq!(event_times, 1);
    }

    #[test]
    fn update_state_should_emit_event() {
        let mut event_times = 0;
        {
            let mut btn = Button {
                btns: 0.,
                _herald_info: HeraldInfo::new(),
            };
            let on_immediate_change =
                constrain_change_event_callback(|chgs: ChangeEvent<'_, _, ButtonChanges>| {
                    event_times += 1;
                    let btns_chg = chgs.changes.btns.as_ref().unwrap();
                    &assert_eq!(btns_chg.before, 0.);
                    assert_eq!(btns_chg.after, 1.);
                });
            btn.herald()
                .immediate_changes_events()
                .subscribe(on_immediate_change);

            let on_batched_event =
                constrain_change_event_callback(|ce: ChangeEvent<'_, Button, ButtonChanges>| {
                    ce.host.btns = 3.;
                });
            btn.update_states(ButtonStates { btns: Some(1.) });
            btn.herald()
                .batched_changes_events()
                .subscribe(on_batched_event);
        }
        assert_eq!(event_times, 1);
    }
}
