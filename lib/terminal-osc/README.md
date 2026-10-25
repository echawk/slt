# terminal-osc

Parsing of OSC (Operating System Command) payloads, and encoding of their
replies, for the control strings modern terminals share:

| OSC | | functions |
|---|---|---|
| 4, 10-19, 104, 110-119 | colors | `parse-color-spec` (`rgb:`, `#`, `rgbi:`), `format-color-spec`, `parse-palette-request`, `parse-dynamic-color-request`, `parse-palette-reset` |
| 52 | clipboard | `parse-clipboard-request`, `encode-clipboard-reply` |
| 8 | hyperlinks | `parse-hyperlink` |
| 7 | working directory | `parse-working-directory` (percent-decoded `file://` URLs) |
| 133 | shell integration | `parse-semantic-prompt` |
| 9, 777, 99 | notifications | `parse-notification`, and `notification-assembler` for kitty's chunked OSC 99 |

`parse-osc` splits a payload into its number and UTF-8 text, and
`encode-osc` builds replies. Nothing here acts on a request: what to do
with it is up to the terminal. The library does not depend on slt.

Run the tests with `(asdf:test-system :terminal-osc)`.
