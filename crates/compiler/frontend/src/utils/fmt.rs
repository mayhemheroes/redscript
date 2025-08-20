use core::fmt;
use std::cell::RefCell;

#[derive(Debug, Clone)]
pub struct DisplayFn<F>(F);

impl<F> DisplayFn<F> {
    pub fn new(f: F) -> Self {
        Self(f)
    }
}

impl<F> fmt::Display for DisplayFn<F>
where
    F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0(f)
    }
}

pub fn lowercase(thing: &str) -> impl fmt::Display + Clone + use<'_> {
    DisplayFn::new(move |f: &mut fmt::Formatter<'_>| {
        thing
            .chars()
            .try_for_each(|c| write!(f, "{}", c.to_lowercase()))
    })
}

pub fn surrounded_by<'a, A>(
    thing: A,
    left: &'a str,
    right: &'a str,
) -> impl fmt::Display + Clone + use<'a, A>
where
    A: fmt::Display + Clone,
{
    DisplayFn::new(move |f: &mut fmt::Formatter<'_>| write!(f, "{left}{thing}{right}"))
}

pub fn sep_by<T, I>(iter: I, sep: &str) -> impl fmt::Display + use<'_, T, I>
where
    I: IntoIterator<Item = T>,
    I::Item: fmt::Display,
{
    let iter = RefCell::new(iter.into_iter());
    DisplayFn::new(move |f: &mut fmt::Formatter<'_>| {
        if let Some(first) = iter.borrow_mut().next() {
            write!(f, "{first}")?;
        }
        iter.borrow_mut()
            .try_for_each(|item| write!(f, "{sep}{item}"))?;
        Ok(())
    })
}
