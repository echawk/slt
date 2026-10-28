# simd-base64

Incremental base64 decoding into a vector the caller provides. Input may
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

Tests: `(asdf:test-system :simd-base64)`.
