# Step Grep

## What?

A small binary to read DDR simfiles, search them for patterns, and annotate them with footing.

### When?

The features will probably implemented as:

- [ ] A CLI to search simfiles
- [ ] A small WASM frontend for the searcher
- [ ] Annotations added into the WASM frontend

## How?

For the CLI:

```
$ stepgrep <regex> .stepmania/songs
```

(Of course, `.stepmania/songs` can be replaced by any directory containing directories containing directories with the simfiles and song mp3s.

### The Regex Format

The regex will treat a DDR map as a string where sets of adjacent notes represent jumps and whitespaces represent intervals where nothing is pressed.
The notes are mapped to:

- **u**p
- **d**own
- **l**eft
- **r**ight

For holds, we use the uppercase (**U**, **D**, **L**, **R**) where the first one starts the hold and the second ends it.

Instead of using `s` for whitespace, we use `i` for "interval". Furthermore, `n` is about the prescence of notes, `h` is about any hold.
Intervals are also special: `i` is usually the shortest gap between the notes, but can be suffixed with a number to be that number of beats.
(This number can be a fraction with a `/` in it.)

To extend matches, we also use `+` and `*` as defined in regexes.

Furthermore, we define patterns with a special syntax: `name(args...)=regex` where `args` are:

- empty for a no-argument template (must still be invoked with `{}`).
- `name` = regex
- `name N` = any note
- `name H` = any hold
- `name I` = any interval (which can be extended by `+` and `*`  or numbers)

This lets us define templates of regexes that we can fill in. These can be filled in with a function call syntax: `name(args...)`.

`{}` can be used for grouping instead of `()` but `[]` can still be used for character sets.

All other characters are ignored.

#### Examples

A jack of at least two notes:

```
jack(gap I, note N) = note {gap+ note}+

```

Some jump sequence:

```
jumps() = nni+{nni+}*
```


