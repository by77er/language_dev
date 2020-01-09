/// A basic linked list
pub struct List {
    head: Link
}

enum Link {
    Empty,
    More(Box<Node>)
}

struct Node {
    elem: i32,
    next: Link
}

impl List {
    /// Returns a new, empty list
    pub fn new() -> Self {
        List { head: Link::Empty }
    }
    /// Adds an element to the beginning of the list
    pub fn push(&mut self, elem: i32) {
        let new_node = Link::More(Box::new(
            Node {
                elem: elem,
                next: std::mem::replace(&mut self.head, Link::Empty)
            }
        ));
        self.head = new_node;
    }
    /// Returns and removes an element from the front of the list (or, if it's empty, None)
    pub fn pop(&mut self) -> Option<i32> {
        match std::mem::replace(&mut self.head, Link::Empty) {
            Link::Empty => None,
            Link::More(node) => {
                self.head = node.next;
                Some(node.elem)
            }
        }
    }
}

impl Drop for List {
    /// Iteratively drops the list to avoid a recursion nightmare
    fn drop(&mut self) {
        let mut curr_node = std::mem::replace(&mut self.head, Link::Empty);
        while let Link::More(mut node) = curr_node {
            curr_node = std::mem::replace(&mut node.next, Link::Empty);
        }
    }
}

#[cfg(test)]
mod test {
    use super::List;
    #[test]
    fn basics() {
        let mut list = List::new();

        // Check empty list behaves right
        assert_eq!(list.pop(), None);

        // Populate list
        list.push(1);
        list.push(2);
        list.push(3);

        // Check normal removal
        assert_eq!(list.pop(), Some(3));
        assert_eq!(list.pop(), Some(2));

        // Push some more just to make sure nothing's corrupted
        list.push(4);
        list.push(5);

        // Check normal removal
        assert_eq!(list.pop(), Some(5));
        assert_eq!(list.pop(), Some(4));

        // Check exhaustion
        assert_eq!(list.pop(), Some(1));
        assert_eq!(list.pop(), None);
    }
}