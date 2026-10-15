(in-package #:slt/conformance)

;;;; Expectations follow xterm and ECMA-48 behavior.  Where engines are
;;;; known to deviate, the case lists them under :known-failures so the
;;;; suite stays green while documenting the gap.

;;; Text, wrapping, carriage control

(define-vt-case plain-text () ("hello")
  (:screen "hello" "" "" "" "")
  (:cursor 0 5))

(define-vt-case cr-lf () ("ab" #\Return #\Linefeed "cd")
  (:screen "ab" "cd" "" "" "")
  (:cursor 1 2))

(define-vt-case bare-lf-keeps-column () ("ab" #\Linefeed "c")
  (:row 1 "  c"))

(define-vt-case autowrap (:columns 5) ("abcdefg")
  (:row 0 "abcde")
  (:row 1 "fg")
  (:cursor 1 2))

(define-vt-case pending-wrap-stays-on-last-column (:columns 5) ("abcde")
  (:row 1 "")
  (:cursor 0 4))

(define-vt-case pending-wrap-cleared-by-cr (:columns 5) ("abcde" #\Return "X")
  (:screen "Xbcde" "" "" "" ""))

(define-vt-case autowrap-disabled (:columns 5) (:csi "?7l" "abcdefg")
  (:screen "abcdg" "" "" "" ""))

(define-vt-case backspace () ("abc" (:bytes 8 8) "X")
  (:row 0 "aXc")
  (:cursor 0 2))

(define-vt-case backspace-stops-at-margin () ((:bytes 8 8) "X")
  (:row 0 "X"))

(define-vt-case lf-at-bottom-scrolls (:rows 3)
    ("1" #\Return #\Linefeed "2" #\Return #\Linefeed "3" #\Return #\Linefeed "4")
  (:screen "2" "3" "4"))

;;; Cursor movement

(define-vt-case cup () (:csi "2;3HX")
  (:row 1 "  X")
  (:cursor 1 3))

(define-vt-case cup-defaults-to-home () ("abc" :csi "HX")
  (:row 0 "Xbc"))

(define-vt-case cup-clamps-to-screen (:rows 3 :columns 4) (:csi "99;99HX")
  (:row 2 "   X"))

(define-vt-case cursor-relative-moves () (:csi "3;5H" :csi "A" :csi "2D" :csi "B" :csi "3C" "X")
  (:row 2 "     X"))

(define-vt-case cuu-clamps-at-top () (:csi "2;1H" :csi "9AX")
  (:row 0 "X"))

(define-vt-case cha-vpa-hpa () (:csi "4G" "a" :csi "3d" "b" :csi "2`" "c")
  (:row 0 "   a")
  (:row 2 " c  b"))

(define-vt-case cnl-cpl () ("ab" :csi "2E" "c" :csi "F" "d")
  (:row 1 "d")
  (:row 2 "c"))

(define-vt-case decsc-decrc () (:csi "2;3H" :esc "7" :csi "H" "a" :esc "8" "b")
  (:row 0 "a")
  (:row 1 "  b"))

;;; Erasing

(define-vt-case ed-below () ("aaaa" #\Return #\Linefeed "bbbb" #\Return #\Linefeed "cccc"
                             :csi "2;3H" :csi "J")
  (:screen "aaaa" "bb" "" "" ""))

(define-vt-case ed-above () ("aaaa" #\Return #\Linefeed "bbbb" #\Return #\Linefeed "cccc"
                             :csi "2;3H" :csi "1J")
  (:screen "" "   b" "cccc" "" ""))

(define-vt-case ed-all () ("aaaa" #\Return #\Linefeed "bbbb" :csi "2J")
  (:screen "" "" "" "" "")
  (:cursor 1 4))

(define-vt-case el-variants () ("abcdef" #\Return #\Linefeed "abcdef" #\Return #\Linefeed "abcdef"
                                :csi "1;3H" :csi "K"
                                :csi "2;3H" :csi "1K"
                                :csi "3;3H" :csi "2K")
  (:screen "ab" "   def" "" "" ""))

(define-vt-case ech () ("abcdef" :csi "1;2H" :csi "2X")
  (:row 0 "a  def")
  (:cursor 0 1))

(define-vt-case erase-uses-background-color () (:csi "41m" :csi "2J" :csi "0m")
  (:bg 2 2 (:palette 1)))

;;; Inserting and deleting

(define-vt-case ich () ("abcdef" :csi "1;2H" :csi "2@")
  (:row 0 "a  bcdef"))

(define-vt-case ich-pushes-text-off-the-edge (:columns 6) ("abcdef" :csi "1;2H" :csi "2@")
  (:row 0 "a  bcd"))

(define-vt-case dch () ("abcdef" :csi "1;2H" :csi "2P")
  (:row 0 "adef"))

(define-vt-case insert-mode () ("abc" :csi "1;2H" :csi "4h" "XY" :csi "4l")
  (:row 0 "aXYbc"))

(define-vt-case il-dl () ("1" #\Return #\Linefeed "2" #\Return #\Linefeed "3" #\Return #\Linefeed "4"
                          :csi "2H" :csi "L" "n"
                          :csi "4H" :csi "M")
  (:screen "1" "n" "2" "4" ""))

(define-vt-case rep () ("x" :csi "3b")
  (:row 0 "xxxx"))

;;; Scrolling regions

(define-vt-case decstbm-confines-scrolling (:rows 5)
    ("1" #\Return #\Linefeed "2" #\Return #\Linefeed "3" #\Return #\Linefeed "4" #\Return #\Linefeed "5"
     :csi "2;4r" :csi "4;1H" #\Linefeed "x")
  (:screen "1" "3" "4" "x" "5"))

(define-vt-case decstbm-homes-cursor () ("abc" :csi "2;4r")
  (:cursor 0 0))

(define-vt-case su-sd (:rows 4)
    ("1" #\Return #\Linefeed "2" #\Return #\Linefeed "3" #\Return #\Linefeed "4" :csi "S")
  (:screen "2" "3" "4" ""))

(define-vt-case sd (:rows 4)
    ("1" #\Return #\Linefeed "2" #\Return #\Linefeed "3" #\Return #\Linefeed "4" :csi "2T")
  (:screen "" "" "1" "2"))

(define-vt-case reverse-index-at-top-scrolls-down (:rows 3)
    ("1" #\Return #\Linefeed "2" :csi "H" :esc "M" "0")
  (:screen "0" "1" "2"))

(define-vt-case origin-mode (:rows 5) (:csi "2;4r" :csi "?6h" :csi "1;1H" "o")
  (:row 1 "o")
  (:cursor 1 1))

;;; Tabs

(define-vt-case default-tab-stops (:columns 20) (#\Tab "a" #\Tab "b")
  (:row 0 "        a       b"))

(define-vt-case set-and-clear-tab-stops (:columns 20)
    (:csi "3g" :csi "3G" :esc "H" #\Return #\Tab "a")
  (:row 0 "  a"))

(define-vt-case cbt (:columns 20) (:csi "12G" :csi "Z" "a")
  (:row 0 "        a"))

;;; Character sets

(define-vt-case dec-special-graphics () (:esc "(0" "qx" :esc "(B" "q")
  (:row 0 "─│q"))

(define-vt-case decaln (:rows 2 :columns 3) (:esc "#8")
  (:screen "EEE" "EEE"))

;;; Unicode

(define-vt-case utf8-multibyte () ("é→€😀")
  (:row 0 "é→€😀"))

(define-vt-case utf8-split-across-reads () ((:bytes #xE2) :split (:bytes #x86) :split (:bytes #x92) "x")
  (:row 0 "→x"))

(define-vt-case wide-character-occupies-two-cells () ("中x")
  (:row 0 "中x")
  (:cursor 0 3)
  (:has-attrs 0 0 ()))

(define-vt-case wide-character-wraps-early (:columns 5) ("abcd中")
  (:row 0 "abcd")
  (:row 1 "中"))

(define-vt-case combining-character (:known-failures (:3bst)) ("e" (:bytes #xCC #x81) "x")
  (:row 0 #.(format nil "e~cx" (code-char #x301)))
  (:cursor 0 2))

;;; SGR

(define-vt-case sgr-basic-colors () (:csi "31;42mA" :csi "0mB")
  (:fg 0 0 (:palette 1))
  (:bg 0 0 (:palette 2))
  (:fg 0 1 :default)
  (:bg 0 1 :default))

(define-vt-case sgr-bright-colors () (:csi "91;102mA")
  (:fg 0 0 (:palette 9))
  (:bg 0 0 (:palette 10)))

(define-vt-case sgr-256-colors () (:csi "38;5;196;48;5;21mA")
  (:fg 0 0 (:palette 196))
  (:bg 0 0 (:palette 21)))

(define-vt-case sgr-truecolor () (:csi "38;2;1;2;3;48;2;250;251;252mA")
  (:fg 0 0 (:rgb 1 2 3))
  (:bg 0 0 (:rgb 250 251 252)))

(define-vt-case sgr-default-color-reset () (:csi "31;42m" :csi "39;49mA")
  (:fg 0 0 :default)
  (:bg 0 0 :default))

(define-vt-case sgr-explicit-default-palette-entries (:known-failures (:3bst))
    (:csi "37;40mA")
  (:fg 0 0 (:palette 7))
  (:bg 0 0 (:palette 0)))

(define-vt-case sgr-attributes () (:csi "1mb" :csi "0;3mi" :csi "0;4mu" :csi "0;7mr" :csi "0;9ms"
                                   :csi "0;5mk")
  (:attrs 0 0 (:bold))
  (:attrs 0 1 (:italic))
  (:attrs 0 2 (:underline))
  (:attrs 0 3 (:inverse))
  (:attrs 0 4 (:strike))
  (:attrs 0 5 (:blink)))

(define-vt-case sgr-faint (:known-failures (:libvterm)) (:csi "2mf" :csi "22mn")
  (:attrs 0 0 (:faint))
  (:attrs 0 1 ()))

(define-vt-case sgr-attribute-resets () (:csi "1;3;4;7;9m" :csi "22;23;24;27;29mA")
  (:attrs 0 0 ()))

(define-vt-case sgr-double-underline (:known-failures (:3bst)) (:csi "21mA")
  (:attrs 0 0 (:double-underline)))

(define-vt-case sgr-0-resets-everything () (:csi "1;4;31;42m" :csi "mA")
  (:attrs 0 0 ())
  (:fg 0 0 :default)
  (:bg 0 0 :default))

;;; Modes

(define-vt-case hide-cursor () (:csi "?25l")
  (:cursor-visible nil))

(define-vt-case show-cursor () (:csi "?25l" :csi "?25h")
  (:cursor-visible t))

(define-vt-case alternate-screen-1049 ()
    ("main" :csi "?1049h" "alt" :csi "?1049l")
  (:row 0 "main")
  (:cursor 0 4))

(define-vt-case alternate-screen-is-blank () ("main" :csi "?1049h")
  (:row 0 ""))

(define-vt-case ris-resets () (:csi "31m" "abc" :esc "c" "x")
  (:screen "x" "" "" "" "")
  (:fg 0 0 :default))

;;; Replies and OSC

(define-vt-case primary-device-attributes () (:csi "c")
  (:reply-prefix #.(format nil "~c[?" (code-char 27))))

(define-vt-case cursor-position-report () (:csi "3;4H" :csi "6n")
  (:reply #.(format nil "~c[3;4R" (code-char 27))))

(define-vt-case device-status-report () (:csi "5n")
  (:reply #.(format nil "~c[0n" (code-char 27))))

(define-vt-case osc-title-bel () (:osc "2;hello" :bel)
  (:title "hello"))

(define-vt-case osc-title-st () (:osc "0;both" :st)
  (:title "both"))

(define-vt-case unknown-dcs-is-ignored () ("a" :esc "Pzz" :st "b")
  (:row 0 "ab"))

(define-vt-case unknown-csi-is-ignored () ("a" :csi "?9999z" "b")
  (:row 0 "ab"))
