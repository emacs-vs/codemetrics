fn main() {
    let mut my_val = 0;
    'outer: loop {
        loop {
            my_val += 1;
            if 15 == my_val {
                break 'outer;
            }

            if 5 == my_val {
                continue;
            }
        }
    }

    let my_vec = vec![1, 2, 3, 4];
    'outer_for: for some_val in my_vec {
        loop {
            if some_val == 2 {
                continue 'outer_for;
            } else {
                break;
            }
        }
    }
}
