(in-package #:lispvt)

;;;; Display width of codepoints: 0 for combining and zero-width
;;;; characters, 2 for East Asian wide/fullwidth and emoji presentation
;;;; characters, 1 otherwise.  These ranges are the source; CHAR-WIDTH looks
;;;; widths up in the table properties.lisp builds from them.

(defparameter *zero-width-ranges*
  #(#x0300 #x036F  #x0483 #x0489  #x0591 #x05BD  #x05BF #x05BF  #x05C1 #x05C2
    #x05C4 #x05C5  #x05C7 #x05C7  #x0610 #x061A  #x061C #x061C  #x064B #x065F
    #x0670 #x0670  #x06D6 #x06DC  #x06DF #x06E4  #x06E7 #x06E8  #x06EA #x06ED
    #x0711 #x0711  #x0730 #x074A  #x07A6 #x07B0  #x07EB #x07F3  #x07FD #x07FD
    #x0816 #x0819  #x081B #x0823  #x0825 #x0827  #x0829 #x082D  #x0859 #x085B
    #x0898 #x089F  #x08CA #x08E1  #x08E3 #x0902  #x093A #x093A  #x093C #x093C
    #x0941 #x0948  #x094D #x094D  #x0951 #x0957  #x0962 #x0963  #x0981 #x0981
    #x09BC #x09BC  #x09C1 #x09C4  #x09CD #x09CD  #x09E2 #x09E3  #x09FE #x09FE
    #x0A01 #x0A02  #x0A3C #x0A3C  #x0A41 #x0A42  #x0A47 #x0A48  #x0A4B #x0A4D
    #x0A51 #x0A51  #x0A70 #x0A71  #x0A75 #x0A75  #x0A81 #x0A82  #x0ABC #x0ABC
    #x0AC1 #x0AC5  #x0AC7 #x0AC8  #x0ACD #x0ACD  #x0AE2 #x0AE3  #x0AFA #x0AFF
    #x0B01 #x0B01  #x0B3C #x0B3C  #x0B3F #x0B3F  #x0B41 #x0B44  #x0B4D #x0B4D
    #x0B55 #x0B56  #x0B62 #x0B63  #x0B82 #x0B82  #x0BC0 #x0BC0  #x0BCD #x0BCD
    #x0C00 #x0C00  #x0C04 #x0C04  #x0C3C #x0C3C  #x0C3E #x0C40  #x0C46 #x0C48
    #x0C4A #x0C4D  #x0C55 #x0C56  #x0C62 #x0C63  #x0C81 #x0C81  #x0CBC #x0CBC
    #x0CBF #x0CBF  #x0CC6 #x0CC6  #x0CCC #x0CCD  #x0CE2 #x0CE3  #x0D00 #x0D01
    #x0D3B #x0D3C  #x0D41 #x0D44  #x0D4D #x0D4D  #x0D62 #x0D63  #x0D81 #x0D81
    #x0DCA #x0DCA  #x0DD2 #x0DD4  #x0DD6 #x0DD6  #x0E31 #x0E31  #x0E34 #x0E3A
    #x0E47 #x0E4E  #x0EB1 #x0EB1  #x0EB4 #x0EBC  #x0EC8 #x0ECE  #x0F18 #x0F19
    #x0F35 #x0F35  #x0F37 #x0F37  #x0F39 #x0F39  #x0F71 #x0F7E  #x0F80 #x0F84
    #x0F86 #x0F87  #x0F8D #x0FBC  #x0FC6 #x0FC6  #x102D #x1030  #x1032 #x1037
    #x1039 #x103A  #x103D #x103E  #x1058 #x1059  #x105E #x1060  #x1071 #x1074
    #x1082 #x1082  #x1085 #x1086  #x108D #x108D  #x109D #x109D  #x1160 #x11FF
    #x135D #x135F  #x1712 #x1714  #x1732 #x1733  #x1752 #x1753  #x1772 #x1773
    #x17B4 #x17B5  #x17B7 #x17BD  #x17C6 #x17C6  #x17C9 #x17D3  #x17DD #x17DD
    #x180B #x180F  #x1885 #x1886  #x18A9 #x18A9  #x1920 #x1922  #x1927 #x1928
    #x1932 #x1932  #x1939 #x193B  #x1A17 #x1A18  #x1A1B #x1A1B  #x1A56 #x1A56
    #x1A58 #x1A5E  #x1A60 #x1A60  #x1A62 #x1A62  #x1A65 #x1A6C  #x1A73 #x1A7C
    #x1A7F #x1A7F  #x1AB0 #x1ACE  #x1B00 #x1B03  #x1B34 #x1B34  #x1B36 #x1B3A
    #x1B3C #x1B3C  #x1B42 #x1B42  #x1B6B #x1B73  #x1B80 #x1B81  #x1BA2 #x1BA5
    #x1BA8 #x1BA9  #x1BAB #x1BAD  #x1BE6 #x1BE6  #x1BE8 #x1BE9  #x1BED #x1BED
    #x1BEF #x1BF1  #x1C2C #x1C33  #x1C36 #x1C37  #x1CD0 #x1CD2  #x1CD4 #x1CE0
    #x1CE2 #x1CE8  #x1CED #x1CED  #x1CF4 #x1CF4  #x1CF8 #x1CF9  #x1DC0 #x1DFF
    #x200B #x200F  #x202A #x202E  #x2060 #x2064  #x206A #x206F  #x20D0 #x20F0
    #x2CEF #x2CF1  #x2D7F #x2D7F  #x2DE0 #x2DFF  #x302A #x302D  #x3099 #x309A
    #xA66F #xA672  #xA674 #xA67D  #xA69E #xA69F  #xA6F0 #xA6F1  #xA802 #xA802
    #xA806 #xA806  #xA80B #xA80B  #xA825 #xA826  #xA82C #xA82C  #xA8C4 #xA8C5
    #xA8E0 #xA8F1  #xA8FF #xA8FF  #xA926 #xA92D  #xA947 #xA951  #xA980 #xA982
    #xA9B3 #xA9B3  #xA9B6 #xA9B9  #xA9BC #xA9BD  #xA9E5 #xA9E5  #xAA29 #xAA2E
    #xAA31 #xAA32  #xAA35 #xAA36  #xAA43 #xAA43  #xAA4C #xAA4C  #xAA7C #xAA7C
    #xAAB0 #xAAB0  #xAAB2 #xAAB4  #xAAB7 #xAAB8  #xAABE #xAABF  #xAAC1 #xAAC1
    #xAAEC #xAAED  #xAAF6 #xAAF6  #xABE5 #xABE5  #xABE8 #xABE8  #xABED #xABED
    #xD7B0 #xD7FF  #xFB1E #xFB1E  #xFE00 #xFE0F  #xFE20 #xFE2F  #xFEFF #xFEFF
    #xFFF9 #xFFFB
    #x101FD #x101FD  #x102E0 #x102E0  #x10376 #x1037A  #x10A01 #x10A0F
    #x10A38 #x10A3F  #x10AE5 #x10AE6  #x10D24 #x10D27  #x10EAB #x10EAC
    #x10F46 #x10F50  #x11001 #x11001  #x11038 #x11046  #x1107F #x11081
    #x110B3 #x110B6  #x110B9 #x110BA  #x11100 #x11102  #x11127 #x1112B
    #x1112D #x11134  #x11173 #x11173  #x11180 #x11181  #x111B6 #x111BE
    #x1D167 #x1D169  #x1D173 #x1D182  #x1D185 #x1D18B  #x1D1AA #x1D1AD
    #x1D242 #x1D244  #x1E000 #x1E02A  #x1E130 #x1E136  #x1E2EC #x1E2EF
    #x1E8D0 #x1E8D6  #x1E944 #x1E94A  #x1F3FB #x1F3FF  #xE0001 #xE0001
    #xE0020 #xE007F  #xE0100 #xE01EF)
  "Inclusive ranges of zero-width codepoints, as a flat vector of pairs.")

(defparameter *wide-ranges*
  #(#x1100 #x115F  #x231A #x231B  #x2329 #x232A  #x23E9 #x23EC  #x23F0 #x23F0
    #x23F3 #x23F3  #x25FD #x25FE  #x2614 #x2615  #x2648 #x2653  #x267F #x267F
    #x2693 #x2693  #x26A1 #x26A1  #x26AA #x26AB  #x26BD #x26BE  #x26C4 #x26C5
    #x26CE #x26CE  #x26D4 #x26D4  #x26EA #x26EA  #x26F2 #x26F3  #x26F5 #x26F5
    #x26FA #x26FA  #x26FD #x26FD  #x2705 #x2705  #x270A #x270B  #x2728 #x2728
    #x274C #x274C  #x274E #x274E  #x2753 #x2755  #x2757 #x2757  #x2795 #x2797
    #x27B0 #x27B0  #x27BF #x27BF  #x2B1B #x2B1C  #x2B50 #x2B50  #x2B55 #x2B55
    #x2E80 #x303E  #x3041 #x33FF  #x3400 #x4DBF  #x4E00 #x9FFF  #xA000 #xA4CF
    #xA960 #xA97F  #xAC00 #xD7A3  #xF900 #xFAFF  #xFE10 #xFE19  #xFE30 #xFE6F
    #xFF00 #xFF60  #xFFE0 #xFFE6
    #x16FE0 #x16FE4  #x16FF0 #x16FF1  #x17000 #x187F7  #x18800 #x18CD5
    #x18D00 #x18D08  #x1AFF0 #x1AFFE  #x1B000 #x1B122  #x1B132 #x1B132
    #x1B150 #x1B152  #x1B155 #x1B155  #x1B164 #x1B167  #x1B170 #x1B2FB
    #x1F004 #x1F004  #x1F0CF #x1F0CF  #x1F18E #x1F18E  #x1F191 #x1F19A
    #x1F200 #x1F202  #x1F210 #x1F23B  #x1F240 #x1F248  #x1F250 #x1F251
    #x1F260 #x1F265  #x1F300 #x1F320  #x1F32D #x1F335  #x1F337 #x1F37C
    #x1F37E #x1F393  #x1F3A0 #x1F3CA  #x1F3CF #x1F3D3  #x1F3E0 #x1F3F0
    #x1F3F4 #x1F3F4  #x1F3F8 #x1F43E  #x1F440 #x1F440  #x1F442 #x1F4FC
    #x1F4FF #x1F53D  #x1F54B #x1F54E  #x1F550 #x1F567  #x1F57A #x1F57A
    #x1F595 #x1F596  #x1F5A4 #x1F5A4  #x1F5FB #x1F64F  #x1F680 #x1F6C5
    #x1F6CC #x1F6CC  #x1F6D0 #x1F6D2  #x1F6D5 #x1F6D7  #x1F6DC #x1F6DF
    #x1F6EB #x1F6EC  #x1F6F4 #x1F6FC  #x1F7E0 #x1F7EB  #x1F7F0 #x1F7F0
    #x1F90C #x1F93A  #x1F93C #x1F945  #x1F947 #x1F9FF  #x1FA70 #x1FAFF
    #x20000 #x2FFFD  #x30000 #x3FFFD)
  "Inclusive ranges of double-width codepoints, as a flat vector of pairs.")

(defun range-member-p (code ranges)
  (declare (type simple-vector ranges)
           (type (unsigned-byte 32) code))
  (let ((low 0)
        (high (1- (floor (length ranges) 2))))
    (declare (type fixnum low high))
    (loop while (<= low high)
          do (let ((middle (floor (+ low high) 2)))
               (cond ((< code (the fixnum (svref ranges (* 2 middle))))
                      (setf high (1- middle)))
                     ((> code (the fixnum (svref ranges (1+ (* 2 middle)))))
                      (setf low (1+ middle)))
                     (t (return-from range-member-p t)))))
    nil))

(defun compute-char-width (code)
  (cond ((range-member-p code *zero-width-ranges*) 0)
        ((range-member-p code *wide-ranges*) 2)
        (t 1)))


