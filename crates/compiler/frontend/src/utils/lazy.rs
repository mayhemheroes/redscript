use std::cell::RefCell;
use std::fmt;

pub struct Lazy<A, F> {
    get: F,
    value: RefCell<Option<A>>,
}

impl<A, F> Lazy<A, F> {
    #[inline]
    pub fn new(get: F) -> Self {
        Self {
            get,
            value: RefCell::new(None),
        }
    }

    #[inline]
    pub fn try_get(&self) -> Option<A>
    where
        A: Clone,
    {
        self.value.borrow().clone()
    }

    pub fn get<E>(&self, env: &E) -> Result<A, CycleError>
    where
        A: Clone,
        F: Fn(&E) -> A,
    {
        let mut borrow = match self.value.try_borrow_mut() {
            Ok(mut slot) => match &mut *slot {
                Some(var) => return Ok(var.clone()),
                None => slot,
            },
            Err(_) => return Err(CycleError),
        };

        let var = (self.get)(env);
        *borrow = Some(var.clone());
        Ok(var)
    }
}

impl<A, E> Lazy<A, Box<dyn Fn(&E) -> A>> {
    pub fn resolved(value: A) -> Self {
        Self {
            get: Box::new(move |_| unreachable!()),
            value: RefCell::new(Some(value)),
        }
    }
}

impl<A: fmt::Debug, F> fmt::Debug for Lazy<A, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Lazy")
            .field("value", &self.value.try_borrow())
            .finish()
    }
}

#[derive(Debug)]
pub struct CycleError;
