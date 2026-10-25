# kitty-keyboard

The [Kitty keyboard protocol](https://sw.kovidgoyal.net/kitty/keyboard-protocol/)
for terminal emulators. It does not depend on slt and has two parts:

**Progressive-enhancement state.** `make-keyboard-state` creates it, and
`keyboard-control-sequence` acts on the protocol's `CSI = > < ? … u`
sequences and returns the reply to a query. The main and alternate screens
each have their own flags and a bounded stack of saved flags, as the
protocol requires.

**Encoding.** `encode-key` returns the bytes for a key event under a set of
flags. It covers:

- the legacy encoding: C0 keys, Ctrl mapping, Alt as an ESC prefix,
  functional keys, and application cursor keys;
- every enhancement: disambiguation, event types, alternate keys, all keys
  as escape codes, and associated text.

```lisp
(kitty-keyboard:encode-key 1 #\a :modifiers '(:control))   ; => ESC [97;5u
(kitty-keyboard:encode-key 0 :up :application-cursor-keys t) ; => ESC O A
```

**Key names.** A key is the unshifted character (`#\a`) or a keyword from
the protocol's table:

- `:escape`, `:enter`, `:tab`, `:backspace`;
- `:f1` to `:f35`;
- `:kp-0`, `:kp-enter` and the other keypad keys;
- `:media-play` and the other media keys;
- `:left-shift` and the other modifier keys.

Modifiers are `:shift :alt :control :super :hyper :meta :caps-lock
:num-lock`.

Run the tests with `(asdf:test-system :kitty-keyboard)`.
