use std::mem;

pub fn move_element<T>(vector: &mut Vec<Option<T>>, index: usize) -> Option<T> {
    mem::replace(&mut vector[index], None)
}

macro_rules! move_tuplet {
    { ($y:ident $(, $x:ident)*) = $v:expr } => {
        let ($y, $($x),*) = move_tuplet!($v ; 1 ; ($($x),*) ; (move_element(&mut $v, 0)) );
    };
    { $v:expr ; $j:expr ; ($y:ident $(, $x:ident)*) ; ($($a:expr),*) } => {
        move_tuplet!( $v ; $j+1 ; ($($x),*) ; ($($a),*,move_element(&mut $v, $j)) )
    };
    { $v:expr ; $j:expr ; () ; $accu:expr } => {
        $accu
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tuplet_test() {
        let mut v = vec![Some(1), Some(2), Some(3)];
        move_tuplet!((a, b, c) = v);

        assert_eq!(a, Some(1));
        assert_eq!(b, Some(2));
        assert_eq!(c, Some(3));
    }
}
