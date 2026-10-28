# simd-base64

Base64 encoding, and incremental decoding into a vector the caller
provides; and cl-base64's functions by the same names and arguments. Input may
arrive in pieces split anywhere, even within a quartet of digits; padding
may be missing, or appear between separately padded pieces. It does not
depend on slt.

```lisp
(simd-base64:decode (map '(vector (unsigned-byte 8)) #'char-code "aGk="))
;; => #(104 105)

;; RGB pixels sent as base64, straight into RGBA, in chunks:
(let ((decoder (simd-base64:make-decoder
                (make-array (* 4 width height) :element-type '(unsigned-byte 8))
                :layout :rgba)))
  (dolist (chunk chunks)
    (simd-base64:decoder-feed decoder chunk))
  (simd-base64:decoder-finish decoder)   ; => octets decoded
  (simd-base64:decoder-output decoder))
```

**Layouts.** `:octets` stores the decoded octets in order. `:rgba` takes
every three as a pixel's RGB and stores it as four RGBA octets with alpha
255, which is how a terminal wants an image sent as base64 RGB (Kitty
graphics `f=24`).

**Bounds.** Octets beyond the output are counted (`decoder-fill`), not
stored, so a caller can tell a payload that was too long from one that fit.

**Errors.** `base64-error` is signalled at an octet that is neither a digit
nor `=`, and for a lone digit at the end of a group. The decoder is of no
further use after one.

**Speed.** Runs of 16 digits are decoded in one step when SBCL has the
sb-simd contrib: with NEON on arm64, for both layouts, and with SSE2 on
x86-64, for `:rgba` (packing 12 octets out of 16 needs a byte shuffle that
SSE2 lacks). Other input goes a quartet at a time through a lookup table.
On an M-series Mac, a 1280x720 RGB frame (4.9 MB of base64) decodes into
RGBA in about 1.3 ms.

## Encoding

```lisp
(simd-base64:encode (map '(vector (unsigned-byte 8)) #'char-code "hi"))
;; => #(97 71 107 61), "aGk="
```

`encode` takes `:start`, `:end` and `:uri` (the URI-safe alphabet,
`-_` padded with `.`) and returns octets; `encoded-length` says how many.
With NEON, 12 octets are encoded per step, in the standard and URI
alphabets; a 1280x720 RGBA frame (3.7 MB) encodes in about 1.5 ms.

## In place of cl-base64

These take the same arguments as cl-base64's and return the same results,
so a program can use `simd-base64` where it used `cl-base64`:

- `usb8-array-to-base64-string`, `usb8-array-to-base64-stream`,
  `string-to-base64-string`, `string-to-base64-stream`,
  `integer-to-base64-string`, `integer-to-base64-stream` (`:uri`,
  `:columns`);
- `base64-string-to-usb8-array`, `base64-string-to-string`,
  `base64-string-to-stream`, `base64-string-to-integer`,
  `base64-stream-to-integer` (`:uri`);
- `*uri-encode-table*`, `*uri-decode-table*` and `make-decode-table`.

The tests check them against cl-base64 itself. The differences:

- A character that is not a digit, padding or whitespace signals
  `base64-error` instead of a warning, and is not skipped.
- cl-base64 declares `base64-stream-to-string`, `stream-to-base64-string`
  and the like but does not define them, so they are not here either.
- A string to encode holds a character per octet; one past 255 signals an
  error, where cl-base64 silently gives wrong digits.

On a 1280x720 RGBA frame, `usb8-array-to-base64-string` takes 5.4 ms to
cl-base64's 12.6, and `base64-string-to-usb8-array` 5.3 ms to its 7.9.
Most of what remains is copying SBCL's 32-bit characters.

Tests: `(asdf:test-system :simd-base64)`.
