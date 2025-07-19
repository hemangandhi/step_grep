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

    fn expand_queens_upto(n: u8) -> impl Fn(Vec<u8>, &usize) -> Result<Vec<Vec<u8>>, ()> {
        move |q: Vec<u8>, c: &usize| -> Result<Vec<Vec<u8>>, ()> {
            let states: Vec<Vec<u8>> = (0..n)
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
    }

    #[test]
    fn test_nqueen_search() {
        const QUEENS_SOLUTIONS: [[u8; 5]; 10] = [
            [0, 2, 4, 1, 3],
            [0, 3, 1, 4, 2],
            [1, 3, 0, 2, 4],
            [1, 4, 2, 0, 3],
            [2, 0, 3, 1, 4],
            [2, 4, 1, 3, 0],
            [3, 0, 2, 4, 1],
            [3, 1, 4, 2, 0],
            [4, 1, 3, 0, 2],
            [4, 2, 0, 3, 1],
        ];

        let queens = search(
            (0..5u8).map(|r| vec![r]).collect(),
            1..5usize,
            expand_queens_upto(5),
        )
        .unwrap();
        assert!(queens.len() == 10);
        let missing: Vec<&[u8; 5]> = QUEENS_SOLUTIONS
            .iter()
            .filter(|s| !queens.contains(&s.iter().cloned().collect()))
            .collect();
        assert!(missing.is_empty());
        let extra: Vec<&Vec<u8>> = queens
            .iter()
            .filter(|s| s.len() != 5 || !QUEENS_SOLUTIONS.contains(&[s[0], s[1], s[2], s[3], s[4]]))
            .collect();
        assert!(extra.is_empty());
    }

    #[test]
    fn test_nqueens_not_found() {
        let no_queens = search(
            (0..3u8).map(|r| vec![r]).collect(),
            1..3usize,
            expand_queens_upto(3),
        )
        .unwrap_err();
        assert!(no_queens.len() == 2);
    }
}
