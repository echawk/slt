# ecma48

An incremental scanner for ECMA-48 (ISO 6429) control functions in UTF-8
terminal output. It follows the DEC/xterm parser conventions: Paul Williams'
state machine, BEL ending OSC strings, CAN/SUB aborting a sequence, and 8-bit
C1 bytes treated as text in UTF-8. It does not depend on slt.

```lisp
(let ((scanner (ecma48:make-scanner)))
  (ecma48:scan scanner octets
               (lambda (event)
                 (case (ecma48:event-kind event)
                   (:csi (print (list (code-char (ecma48:event-final event))
                                      (ecma48:event-parameters event))))
                   (:osc (print (ecma48:event-data event)))))))
```

`scan` may be called with any split of the input: a sequence that spans
calls is reported once, whole.

**Events.** Each one has a kind:

- `:text` and `:control` (a C0 control or DEL);
- `:escape` (final and intermediate bytes);
- `:csi` (private marker, parameters with colon sub-parameters,
  intermediates, final);
- `:osc`, `:dcs` (with its header parsed like CSI), `:apc`, `:pm` and
  `:sos`, with their payload and terminator;
- `:raw` (aborted, overlong or declined sequences).

**Raw bytes.** Every event carries its raw bytes, so a caller can pass on
what it does not handle unchanged. Joined in order, the raw bytes of all
events reproduce the input, with one exception: a C0 control met inside a
sequence is reported before it, as terminals execute it at once.

**Control strings.** A `:string-policy` function can decline a string as
soon as its kind and first byte are known. A declined string is passed
through as `:raw` events as it arrives, instead of being collected.
`:max-string-length` bounds the strings that are collected.

**Allocation.** The scanner reuses one event object and allocates nothing
for ordinary output. Use `copy-event` to keep an event.

Run the tests with `(asdf:test-system :ecma48)`.
