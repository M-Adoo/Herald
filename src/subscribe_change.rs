use crate::herald::ChangeEvent;
use rxrust::prelude::*;

pub trait SubscribeChange<F, T, C>
where
  F: for<'r> FnMut(ChangeEvent<'r, T, C>),
{
  type Unsub;
  fn subscribe_change(self, next: F) -> Self::Unsub;
}

pub struct SubscribeChangeObserver<F>(F);

impl<F> ObserverComplete for SubscribeChangeObserver<F> {
  #[inline(always)]
  fn complete(&mut self) {}
}

impl<F> ObserverError<()> for SubscribeChangeObserver<F> {
  #[inline(always)]
  fn error(&mut self, _err: ()) {}
}

impl<F, Item> ObserverNext<Item> for SubscribeChangeObserver<F>
where
  F: FnMut(Item),
{
  #[inline(always)]
  fn next(&mut self, value: Item) {
    (self.0)(value);
  }
}

impl<S, F, T, C> SubscribeChange<F, T, C> for S
where
  S: RawSubscribable<Subscriber<SubscribeChangeObserver<F>, LocalSubscription>>,
  F: for<'r> FnMut(ChangeEvent<'r, T, C>),
{
  type Unsub = S::Unsub;
  fn subscribe_change(self, next: F) -> Self::Unsub
  where
    Self: Sized,
  {
    self.raw_subscribe(Subscriber::local(SubscribeChangeObserver(next)))
  }
}
