use core::marker::PhantomData;
use core::ptr::NonNull;

pub struct StackLL<'a, E> {
    pub parent: Option<NonNull<StackLL<'a, E>>>,
    pub item: E,
    pub marker: PhantomData<&'a mut u8>,
}

unsafe impl<E> Send for StackLL<'static, E> where E: Send {}
unsafe impl<E> Sync for StackLL<'static, E> where E: Sync {}

impl<'a, E> StackLL<'a, E> {
    pub fn new(item: E) -> Self {
        Self {
            parent: None,
            item,
            marker: PhantomData,
        }
    }

    pub fn parent(&mut self) -> Option<&mut Self> {
        return Some(unsafe { &mut *self.parent?.as_ptr() });
    }

    pub fn get(&self) -> &E {
        &self.item
    }

    pub fn child<'b>(&'b mut self, item: E) -> StackLL<'b, E> {
        StackLL {
            parent: Some(NonNull::from(self)),
            item,
            marker: PhantomData,
        }
    }
}

pub struct StackLLIter<'a, 'b, E>
where
    'b: 'a,
{
    pub ll: Option<&'a StackLL<'b, E>>,
}

impl<'a, 'b, E> Iterator for StackLLIter<'a, 'b, E> {
    type Item = &'a E;
    fn next(&mut self) -> Option<&'a E> {
        let ll = self.ll.take()?;
        let item = &ll.item;
        self.ll = ll.parent.map(|a| unsafe { &*a.as_ptr() });
        return Some(item);
    }
}

pub struct StackLLIterMut<'a, 'b, E>
where
    'b: 'a,
{
    pub ll: Option<&'a mut StackLL<'b, E>>,
}

impl<'a, 'b, E> Iterator for StackLLIterMut<'a, 'b, E> {
    type Item = &'a mut E;
    fn next(&mut self) -> Option<&'a mut E> {
        let ll = self.ll.take()?;
        let item = &mut ll.item;
        self.ll = ll.parent.map(|a| unsafe { &mut *a.as_ptr() });
        return Some(item);
    }
}

impl<'a, 'b, E> IntoIterator for &'a mut StackLL<'b, E> {
    type Item = &'a mut E;
    type IntoIter = StackLLIterMut<'a, 'b, E>;

    fn into_iter(self) -> Self::IntoIter {
        StackLLIterMut { ll: Some(self) }
    }
}

impl<'a, 'b, E> IntoIterator for &'a StackLL<'b, E> {
    type Item = &'a E;
    type IntoIter = StackLLIter<'a, 'b, E>;

    fn into_iter(self) -> Self::IntoIter {
        StackLLIter { ll: Some(self) }
    }
}

#[test]
fn stackll_validity() {
    let mut ll = StackLL::new(1);

    child(ll.child(2));

    let mut child = ll.child(2);

    let mut child2 = child.child(3);

    // println!("{}", ll.item);
    child2.item = 12;
}

#[cfg(test)]
fn child(stack: StackLL<usize>) {
    let expected = &[2, 1];
    for (idx, value) in stack.into_iter().enumerate() {
        assert_eq!(expected[idx], *value);
    }
}
