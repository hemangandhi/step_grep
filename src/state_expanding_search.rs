pub fn search<T, I, E>(
    initial_states: Vec<T>,
    inputs: impl std::iter::IntoIterator<Item = I>,
    expand: impl Fn(T, &I) -> Result<Vec<T>, E>,
) -> Result<Vec<T>, Vec<E>> {
    let mut states = initial_states;
    for input in inputs.into_iter() {
        let (new_states, errors): (Vec<Result<Vec<T>, E>>, Vec<Result<Vec<T>, E>>) = states
            .into_iter()
            .map(|state| expand(state, &input))
            .partition(Result::is_ok);
        if new_states.is_empty() {
            return Err(errors
                .into_iter()
                .filter_map(|e| match e {
                    Ok(_) => None,
                    Err(e) => Some(e),
                })
                .collect::<Vec<E>>());
        }
        states = new_states
            .into_iter()
            .flat_map(|s| s.unwrap_or_default())
            .collect();
    }
    Ok(states)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_nqueen_search() {
        fn expand_queens(q: Vec<u8>, c: &usize) -> Result<Vec<Vec<u8>>, ()> {
            let states: Vec<Vec<u8>> = (0..5u8)
                .filter_map(|r| {
                    if q.iter().enumerate().any(|(qc, qr)| {
                        let row_dist = if *qr < r { r - qr } else { qr - r };
                        let col_dist = if qc < *c { c - qc } else { qc - c };
                        qc == *c || *qr == r || (row_dist as usize) == col_dist
                    }) {
                        None
                    } else {
                        Some(q.iter().chain([&r]).cloned().collect())
                    }
                })
                .collect();
            if states.is_empty() {
                Err(())
            } else {
                Ok(states)
            }
        }

        let queens = search(
            (0..5u8).map(|r| vec![r]).collect(),
            1..5usize,
            expand_queens,
        );
        assert!(queens.is_ok());
        println!("Queens! {:?}", queens);
    }
}
