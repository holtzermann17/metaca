;;; 256ca.el

;; Copyright (C) 2014 Joseph Corneli

;; Maintainer: holtzermann17@gmail.com
;; Keywords: science, simulation

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This code creates "meta-" cellular automata spacetime diagrams.
;; That is, the CA's evolve their own rules locally, rather evolving
;; an abstract state according to one global rule.

;; Previous work with CAs feature evolving global rules.  This is the
;; first work that I am aware of that evolves the rule locally.

;; Alternate versions of the functions add an abstract "phenotype" on
;; top of the "genotype."  One research question of interest is: Can
;; we define a simple "Baldwin effect" that feeds back from the
;; phenotype to the genotype and leads to the production of rules with
;; edge-of-chaos behavior?  This can be seen as a simple analogue to
;; the cosmological question "How widespread is life in the universe?"

;; An example of a relevant Baldwin effect is:

;; XXX  XXO  XOO  OOO
;;  X    X    X    X 
;; ===  ===  ===  ===
;;  1    2    3    4 
;;
;; Condition 1: heavy mutation
;; Condition 2: medium mutation
;; Condition 3: low mutation
;; Condition 4: no mutation

;; When we say "mutation" what do we mean?

;;; Notes:

;; Related things: mirror image, complement, and mirror complement

;; 0000	0
;; 0001	1
;; 0010	2
;; 0011	3
;; 0100	4
;; 0101	5
;; 0110	6
;; 0111	7
;; 1000	8
;; 1001	9
;; 1010	A
;; 1011	B
;; 1100	C
;; 1101	D
;; 1110	E
;; 1111	F

;;; Code:

(require 'hexrgb)

;;; Convenience

(defvar hexcolour-keywords
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property (match-beginning 0)
                            (match-end 0)
			    'face (list :background 
				        (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook 'emacs-lisp-mode-hook 'hexcolour-add-to-font-lock)

;;; 00011101 11100010 10111000 01000111
;;; 01101110 01110110 10010001 10001001
;;; 00011110 11100001 01111000 10000111
;;; 01011010 10100101

(defvar truth-table-3 '("000" ; 0 1 1 0  0 0 1 1  0 1 0 1  0 1
			"001" ; 0 1 0 1  1 1 0 0  0 1 1 0  1 0
			"010" ; 0 1 1 0  1 1 0 0  0 1 1 0  0 1
			"100" ; 1 0 1 0  0 1 1 0  1 0 1 0  1 0
			"011" ; 1 0 1 0  1 0 0 1  1 0 1 0  1 0
			"101" ; 1 0 0 1  1 1 0 0  1 0 0 1  0 1
			"110" ; 0 1 0 1  1 1 0 0  1 0 0 1  1 0
			"111" ; 1 0 0 1  0 0 1 1  0 1 0 1  0 1
			))

(defvar truth-table-8
  '(("00000000" "!" "#000000")
    ("00000001" "#" "#010101")
    ("00000010" "$" "#020202")
    ("00000100" "%" "#040404")
    ("00000011" "&" "#030303")
    ("00000101" "[" "#050505")
    ("00000110" "]" "#060606")
    ("00000111" "*" "#070707")
    ("00001000" "0" "#080808")
    ("00001001" "1" "#090909")
    ("00001010" "2" "#0a0a0a")
    ("00001100" "3" "#0c0c0c")
    ("00001011" "4" "#0b0b0b")
    ("00001101" "5" "#0d0d0d")
    ("00001110" "6" "#0e0e0e")
    ("00001111" "7" "#0f0f0f")
    ("00010000" "8" "#101010")
    ("00010001" "9" "#111111")
    ("00010010" "@" "#121212")
    ("00010100" "A" "#141414")
    ("00010011" "B" "#131313")
    ("00010101" "C" "#151515")
    ("00010110" "D" "#161616")
    ("00010111" "E" "#171717")
    ("00011000" "F" "#181818")
    ("00011001" "G" "#191919")
    ("00011010" "H" "#1a1a1a")
    ("00011100" "I" "#1c1c1c")
    ("00011011" "J" "#1b1b1b")
    ("00011101" "K" "#00ff33") ; #1d1d1d 00011101 11100010 10111000 01000111
    ("00011110" "L" "#0033ff") ; #1e1e1e 00011110 11100001 01111000 10000111
    ("00011111" "M" "#1f1f1f")
    ("00100000" "N" "#202020")
    ("00100001" "O" "#212121")
    ("00100010" "P" "#222222")
    ("00100100" "Q" "#242424")
    ("00100011" "R" "#232323")
    ("00100101" "S" "#252525")
    ("00100110" "T" "#262626")
    ("00100111" "U" "#272727")
    ("00101000" "V" "#282828")
    ("00101001" "W" "#292929")
    ("00101010" "X" "#2a2a2a")
    ("00101100" "Y" "#2c2c2c")
    ("00101011" "Z" "#2b2b2b")
    ("00101101" "^" "#2d2d2d")
    ("00101110" "`" "#2e2e2e")
    ("00101111" "a" "#2f2f2f")
    ("00110000" "b" "#303030")
    ("00110001" "c" "#313131")
    ("00110010" "d" "#323232")
    ("00110100" "e" "#343434")
    ("00110011" "f" "#333333")
    ("00110101" "g" "#353535")
    ("00110110" "h" "#363636")
    ("00110111" "i" "#373737")
    ("00111000" "j" "#383838")
    ("00111001" "k" "#393939")
    ("00111010" "l" "#3a3a3a")
    ("00111100" "m" "#3c3c3c")
    ("00111011" "n" "#3b3b3b")
    ("00111101" "o" "#3d3d3d")
    ("00111110" "p" "#3e3e3e")
    ("00111111" "q" "#3f3f3f")
    ("01000000" "r" "#404040")
    ("01000001" "s" "#414141")
    ("01000010" "t" "#424242")
    ("01000100" "u" "#444444")
    ("01000011" "v" "#434343")
    ("01000101" "w" "#454545")
    ("01000110" "x" "#464646")
    ("01000111" "y" "#00ff33") ;#474747
    ("01001000" "z" "#484848")
    ("01001001" "~" "#494949")
    ("01001010" " " "#4a4a4a")
    ("01001100" "¡" "#4c4c4c")
    ("01001011" "¢" "#4b4b4b")
    ("01001101" "£" "#4d4d4d")
    ("01001110" "¤" "#4e4e4e")
    ("01001111" "¥" "#4f4f4f")
    ("01010000" "¦" "#505050")
    ("01010001" "§" "#515151")
    ("01010010" "¨" "#525252")
    ("01010100" "©" "#545454")
    ("01010011" "ª" "#535353")
    ("01010101" "«" "#555555")
    ("01010110" "¬" "#565656")
    ("01010111" "­" "#575757")
    ("01011000" "®" "#585858")
    ("01011001" "¯" "#595959")
    ("01011010" "°" "#ffcc00") ; #5a5a5a 01011010 10100101
    ("01011100" "±" "#5c5c5c")
    ("01011011" "²" "#5b5b5b")
    ("01011101" "³" "#5d5d5d")
    ("01011110" "´" "#5e5e5e")
    ("01011111" "µ" "#5f5f5f")
    ("01100000" "¶" "#606060")
    ("01100001" "·" "#616161")
    ("01100010" "¹" "#626262")
    ("01100100" "º" "#646464")
    ("01100011" "»" "#636363")
    ("01100101" "¼" "#656565")
    ("01100110" "½" "#666666")
    ("01100111" "¾" "#676767")
    ("01101000" "¿" "#686868")
    ("01101001" "À" "#696969")
    ("01101010" "Á" "#6a6a6a")
    ("01101100" "Â" "#6c6c6c")
    ("01101011" "Ã" "#6b6b6b")
    ("01101101" "Ä" "#6d6d6d")
    ("01101110" "Å" "#ff3300") ; #6e6e6e 01101110 01110110 10010001 10001001
    ("01101111" "Æ" "#6f6f6f")
    ("01110000" "Ç" "#707070")
    ("01110001" "È" "#717171")
    ("01110010" "É" "#727272")
    ("01110100" "Ê" "#747474")
    ("01110011" "Ë" "#737373")
    ("01110101" "Ì" "#757575")
    ("01110110" "Í" "#ff3300") ; #767676
    ("01110111" "Î" "#777777")
    ("01111000" "Ï" "#0033ff") ;#787878
    ("01111001" "Ð" "#797979")
    ("01111010" "Ñ" "#7a7a7a")
    ("01111100" "Ò" "#7c7c7c")
    ("01111011" "Ó" "#7b7b7b")
    ("01111101" "Ô" "#7d7d7d")
    ("01111110" "Õ" "#7e7e7e")
    ("01111111" "Ö" "#7f7f7f")
    ("10000000" "×" "#808080")
    ("10000001" "Ø" "#818181")
    ("10000010" "Ù" "#828282")
    ("10000100" "Ú" "#848484")
    ("10000011" "Û" "#838383")
    ("10000101" "Ü" "#858585")
    ("10000110" "Ý" "#868686")
    ("10000111" "Þ" "#0033ff") ;#878787
    ("10001000" "ß" "#888888")
    ("10001001" "à" "#ff3300") ;#898989
    ("10001010" "á" "#8a8a8a")
    ("10001100" "â" "#8c8c8c")
    ("10001011" "ã" "#8b8b8b")
    ("10001101" "ä" "#8d8d8d")
    ("10001110" "å" "#8e8e8e")
    ("10001111" "æ" "#8f8f8f")
    ("10010000" "ç" "#909090")
    ("10010001" "è" "#ff3300") ;#919191
    ("10010010" "é" "#929292")
    ("10010100" "ê" "#949494")
    ("10010011" "ë" "#939393")
    ("10010101" "ì" "#959595")
    ("10010110" "í" "#969696")
    ("10010111" "î" "#979797")
    ("10011000" "ï" "#989898")
    ("10011001" "ð" "#999999")
    ("10011010" "ñ" "#9a9a9a")
    ("10011100" "ò" "#9c9c9c")
    ("10011011" "ó" "#9b9b9b")
    ("10011101" "ô" "#9d9d9d")
    ("10011110" "õ" "#9e9e9e")
    ("10011111" "ö" "#9f9f9f")
    ("10100000" "÷" "#a0a0a0")
    ("10100001" "ø" "#a1a1a1")
    ("10100010" "ù" "#a2a2a2")
    ("10100100" "ú" "#a4a4a4")
    ("10100011" "û" "#a3a3a3")
    ("10100101" "ü" "#ffcc00")  ;#a5a5a5
    ("10100110" "ý" "#a6a6a6")
    ("10100111" "þ" "#a7a7a7")
    ("10101000" "ÿ" "#a8a8a8")
    ("10101001" "ā" "#a9a9a9")
    ("10101010" "Ă" "#aaaaaa")
    ("10101100" "Ą" "#acacac")
    ("10101011" "Ć" "#ababab")
    ("10101101" "Č" "#adadad")
    ("10101110" "Ď" "#aeaeae")
    ("10101111" "Đ" "#afafaf")
    ("10110000" "Ę" "#b0b0b0")
    ("10110001" "Ě" "#b1b1b1")
    ("10110010" "ī" "#b2b2b2")
    ("10110100" "Ĺ" "#b4b4b4")
    ("10110011" "Ľ" "#b3b3b3")
    ("10110101" "Ł" "#b5b5b5")
    ("10110110" "Ń" "#b6b6b6")
    ("10110111" "ņ" "#b7b7b7")
    ("10111000" "Ň" "#00ff33") ;#b8b8b8
    ("10111001" "Ő" "#b9b9b9")
    ("10111010" "Œ" "#bababa")
    ("10111100" "Ŕ" "#bcbcbc")
    ("10111011" "Ř" "#bbbbbb")
    ("10111101" "Ś" "#bdbdbd")
    ("10111110" "Ş" "#bebebe")
    ("10111111" "Š" "#bfbfbf")
    ("11000000" "Ţ" "#c0c0c0")
    ("11000001" "Ť" "#c1c1c1")
    ("11000010" "Ů" "#c2c2c2")
    ("11000100" "Ű" "#c4c4c4")
    ("11000011" "Ÿ" "#c3c3c3")
    ("11000101" "Ź" "#c5c5c5")
    ("11000110" "Ż" "#c6c6c6")
    ("11000111" "Ž" "#c7c7c7")
    ("11001000" "Α" "#c8c8c8")
    ("11001001" "Β" "#c9c9c9")
    ("11001010" "Γ" "#cacaca")
    ("11001100" "Δ" "#cccccc")
    ("11001011" "Ε" "#cbcbcb")
    ("11001101" "Ζ" "#cdcdcd")
    ("11001110" "Η" "#cecece")
    ("11001111" "Θ" "#cfcfcf")
    ("11010000" "Ι" "#d0d0d0")
    ("11010001" "Κ" "#d1d1d1")
    ("11010010" "Λ" "#d2d2d2")
    ("11010100" "Μ" "#d4d4d4")
    ("11010011" "Ν" "#d3d3d3")
    ("11010101" "Ξ" "#d5d5d5")
    ("11010110" "Ο" "#d6d6d6")
    ("11010111" "Π" "#d7d7d7")
    ("11011000" "Ρ" "#d8d8d8")
    ("11011001" "Σ" "#d9d9d9")
    ("11011010" "Τ" "#dadada")
    ("11011100" "Υ" "#dcdcdc")
    ("11011011" "Φ" "#dbdbdb")
    ("11011101" "Χ" "#dddddd")
    ("11011110" "Ψ" "#dedede")
    ("11011111" "Ω" "#dfdfdf")
    ("11100000" "α" "#e0e0e0")
    ("11100001" "β" "#0033ff") ;#e1e1e1
    ("11100010" "γ" "#00ff33") ;#e2e2e2
    ("11100100" "δ" "#e4e4e4")
    ("11100011" "ε" "#e3e3e3")
    ("11100101" "ζ" "#e5e5e5")
    ("11100110" "η" "#e6e6e6")
    ("11100111" "θ" "#e7e7e7") 
    ("11101000" "ι" "#e8e8e8")
    ("11101001" "κ" "#e9e9e9")
    ("11101010" "λ" "#eaeaea")
    ("11101100" "μ" "#ececec")
    ("11101011" "ν" "#ebebeb")
    ("11101101" "ξ" "#ededed")
    ("11101110" "ο" "#eeeeee")
    ("11101111" "π" "#efefef")
    ("11110000" "ρ" "#f0f0f0")
    ("11110001" "σ" "#f1f1f1")
    ("11110010" "τ" "#f2f2f2")
    ("11110100" "υ" "#f4f4f4")
    ("11110011" "φ" "#f3f3f3")
    ("11110101" "χ" "#f5f5f5")
    ("11110110" "ψ" "#f6f6f6")
    ("11110111" "ω" "#f7f7f7")
    ("11111000" "☀" "#f8f8f8")
    ("11111001" "☈" "#f9f9f9")
    ("11111010" "☉" "#fafafa")
    ("11111100" "☼" "#fcfcfc")
    ("11111011" "☽" "#fbfbfb")
    ("11111101" "☾" "#fdfdfd")
    ("11111110" "☿" "#fefefe")
    ("11111111" "♀" "#ffffff")))

(defun random-sigil ()
  (second (nth (random 256) truth-table-8)))

(defun random-sigil-string (len)
  (let ((res ""))
    (dotimes (i len) (setq res (concat res (random-sigil))))
    res))

(defun random-phenotype-string (len)
  (let ((res ""))
    (dotimes (i len) (setq res (concat res (int-to-string (random 2)))))
    res))

(defun get-genotype-from-sigil (sig)
  (car (member-if (lambda (elt) (string= (second elt) sig)) truth-table-8)))

(defun get-genotype-from-rule (rule)
  (car (member-if (lambda (elt) (string= (first elt) rule)) truth-table-8)))

(defun get-sigil-from-rule (rule)
  (second (car (member-if (lambda (elt) (string= (first elt) rule)) truth-table-8))))

(defun get-rule-from-sigil (sigil)
  (first (car (member-if (lambda (elt) (string= (second elt) sigil)) truth-table-8))))

; (get-rule-from-sigil "#")

(defun evolve-sigil (sig &optional pred next ignore)
  (let* ((p (if pred (first (get-genotype-from-sigil pred))
	      (first (get-genotype-from-sigil "!"))))
	 (s (first (get-genotype-from-sigil sig)))
	 (n (if next (first (get-genotype-from-sigil next))
	      (first (get-genotype-from-sigil "!"))))
	 (s-ints (map 'list (lambda (a) (string-to-int (char-to-string a))) s))
	 (local-rule (mapcar* #'list truth-table-3 s-ints))
	 (local-data (map 'list (lambda (a b c) (concat (char-to-string a)
							(char-to-string b)
							(char-to-string c)))
			  p s n))
	 (output ""))
    ;; combine the results...
    (mapc (lambda (num) (setq output (concat output (int-to-string num))))
	  ;; of looking up each element of the local data according to the local rule
	  (map 'list (lambda (data)
		       (second (car (member-if (lambda (elt) (string= (first elt) data))
					       local-rule))))
	       local-data))
    (get-genotype-from-rule output)))

; (evolve-sigil "«" "Å" "«") ;=> ("01101110" "Å" "#87afd7")

(defun evolve-sigil-with-blending (sig &optional pred next ignore)
  (let* ((p (if pred (first (get-genotype-from-sigil pred))
	      (first (get-genotype-from-sigil "!"))))
	 (s (first (get-genotype-from-sigil sig)))
	 (n (if next (first (get-genotype-from-sigil next))
	      (first (get-genotype-from-sigil "!"))))
	 (s-ints (map 'list (lambda (a) (string-to-int (char-to-string a))) s))
	 (local-rule (mapcar* #'list truth-table-3 s-ints))
	 (local-data (map 'list (lambda (a b c) (concat (char-to-string a)
							(char-to-string b)
							(char-to-string c)))
			  p s n))
	 (output ""))
    ;; combine the results...
    (mapc (lambda (num) (setq output (concat output (int-to-string num))))
	  ;; of first looking at blends and then defaulting to looking
	  ;; up each element of the local data according to the local rule
	  (map 'list (lambda (data)
		       (cond 
			((and (string= (substring data 0 1) "0")
			      (string= (substring data 2 3) "0"))
			 0)
			((and (string= (substring data 0 1) "1")
			      (string= (substring data 2 3) "1"))
			 1)
			(t
			 (second (car (member-if (lambda (elt)
						   (string= (first elt) data))
						 local-rule))))))
	       local-data))
    (get-genotype-from-rule output))) 

;; Here's a test case to show how the results of the two operations
;; can differ:

; (get-sigil-from-rule "01010100") "©"
; (get-sigil-from-rule "01101110") "Å"
; (get-sigil-from-rule "01010101") "«"

;; (evolve-sigil "©" "Å" "«") ;=> ("00101011" "Z" "#00d7d7")

;; (evolve-sigil-with-blending "©" "Å" "«");=> ("01101111" "Æ" "#87afff")

(defun evolve-sigil-with-blending-3 (sig &optional pred next ignore)
  "A more choosy version of `evolve-sigil-with-blending'."
  (let* ((p (if pred (first (get-genotype-from-sigil pred))
	      (first (get-genotype-from-sigil "!"))))
	 (s (first (get-genotype-from-sigil sig)))
	 (n (if next (first (get-genotype-from-sigil next))
	      (first (get-genotype-from-sigil "!"))))
	 (s-ints (map 'list (lambda (a) (string-to-int (char-to-string a))) s))
	 (local-rule (mapcar* #'list truth-table-3 s-ints))
	 (local-data (map 'list (lambda (a b c) (concat (char-to-string a)
							(char-to-string b)
							(char-to-string c)))
			  p s n))
	 (output ""))
    ;; combine the results...
    (mapc (lambda (num) (setq output (concat output (int-to-string num))))
	  ;; of first looking at blends and then defaulting to looking
	  ;; up each element of the local data according to the local rule
	  (map 'list (lambda (data)
		       (cond 
			((and (string= (substring data 0 1) "0")
			      (string= (substring data 1 2) "0")
			      (string= (substring data 2 3) "0"))
			 0)
			((and (string= (substring data 0 1) "1")
			      (string= (substring data 2 3) "1")
			      (string= (substring data 2 3) "1"))
			 1)
			(t
			 (second (car (member-if (lambda (elt)
						   (string= (first elt) data))
						 local-rule))))))
	       local-data))
    (get-genotype-from-rule output)))

(defun evolve-sigil-with-blending-flip (sig &optional pred next flip)
  (let* ((p (if pred (first (get-genotype-from-sigil pred))
	      (first (get-genotype-from-sigil "!"))))
	 (s (first (get-genotype-from-sigil sig)))
	 (n (if next (first (get-genotype-from-sigil next))
	      (first (get-genotype-from-sigil "!"))))
	 (s-ints (map 'list (lambda (a) (string-to-int (char-to-string a))) s))
	 (local-rule (mapcar* #'list truth-table-3 s-ints))
	 (local-data (map 'list (lambda (a b c) (concat (char-to-string a)
							(char-to-string b)
							(char-to-string c)))
			  p s n))
	 (output ""))
    ;; combine the results...
    (mapc (lambda (num) (setq output (concat output (int-to-string num))))
	  ;; of first looking at blends and then defaulting to looking
	  ;; up each element of the local data according to the local rule
	  ;; ... conditionally flipping the locally-determined elements elements
	  (map 'list (lambda (data)
		       (cond 
			((and (string= (substring data 0 1) "0")
			      (string= (substring data 2 3) "0"))
			 (if flip 1 0))
			((and (string= (substring data 0 1) "1")
			      (string= (substring data 2 3) "1"))
			 (if flip 0 1))
			(t
			 (second (car (member-if (lambda (elt)
						   (string= (first elt) data))
						 local-rule))))))
	       local-data))
    (get-genotype-from-rule output)))

(defun mutate-genotype-n (genotype n)  
  (dotimes (j n)
    (let* ((pos (random 8))
	   (elt (substring genotype pos (1+ pos))))
      (cond
       ((string= elt "0")
	(setq genotype (with-temp-buffer (insert genotype)
				       (goto-char pos)
				       (delete-char 1)
				       (insert "1")
				       (buffer-substring-no-properties (point-min)
								       (point-max))))
	
	)
       (t
	(setq genotype (with-temp-buffer (insert genotype)
				       (goto-char pos)
				       (delete-char 1)
				       (insert "0")
				       (buffer-substring-no-properties (point-min)
								       (point-max))))
	))))
  genotype)

(defun evolve-sigil-with-blending-mutation (sig &optional pred next ignore)
  (let* ((mutation 2)
	 (p (if pred (first (get-genotype-from-sigil pred))
	      (first (get-genotype-from-sigil "!"))))
	 (s (first (get-genotype-from-sigil sig)))
	 (n (if next (first (get-genotype-from-sigil next))
	      (first (get-genotype-from-sigil "!"))))
	 (s-ints (map 'list (lambda (a) (string-to-int (char-to-string a))) s))
	 (local-rule (mapcar* #'list truth-table-3 s-ints))
	 (local-data (map 'list (lambda (a b c) (concat (char-to-string a)
							(char-to-string b)
							(char-to-string c)))
			  p s n))
	 (output ""))
    ;; combine the results...
    (mapc (lambda (num) (setq output (concat output (int-to-string num))))
	  ;; of first looking at blends and then defaulting to looking
	  ;; up each element of the local data according to the local rule
	  (map 'list (lambda (data)
		       (cond 
			((and (string= (substring data 0 1) "0")
			      (string= (substring data 2 3) "0"))
			 0)
			((and (string= (substring data 0 1) "1")
			      (string= (substring data 2 3) "1"))
			 1)
			(t
			 (second (car (member-if (lambda (elt)
						   (string= (first elt) data))
						 local-rule))))))
	       local-data))
    ;; mutate the output in *mutation* places and return
    (get-genotype-from-rule (mutate-genotype-n output mutation))))

;; (evolve-sigil-with-blending-mutation "©" "Å" "«" 0) => ("01101111" "Æ" "#6f6f6f")
;; (evolve-sigil-with-blending-mutation "©" "Å" "«" 1) => ("01101111" "Æ" "#6f6f6f")
;; (evolve-sigil-with-blending-mutation "©" "Å" "«" 2) => ("01101011" "Ã" "#6b6b6b")
;; (evolve-sigil-with-blending-mutation "©" "Å" "«" 3) => ("01110101" "Ì" "#757575")

(defun evolve-sigil-with-blending-baldwin (sig &optional pred next context)
  (let* ((p (if pred (first (get-genotype-from-sigil pred))
	      (first (get-genotype-from-sigil "!"))))
	 (s (first (get-genotype-from-sigil sig)))
	 (n (if next (first (get-genotype-from-sigil next))
	      (first (get-genotype-from-sigil "!"))))
	 (s-ints (map 'list (lambda (a) (string-to-int (char-to-string a))) s))
	 (local-rule (mapcar* #'list truth-table-3 s-ints))
	 (local-data (map 'list (lambda (a b c) (concat (char-to-string a)
							(char-to-string b)
							(char-to-string c)))
			  p s n))
	 (output ""))
    ;; combine the results...
    (mapc (lambda (num) (setq output (concat output (int-to-string num))))
	  ;; of first looking at blends and then defaulting to looking
	  ;; up each element of the local data according to the local rule
	  (map 'list (lambda (data)
		       (cond 
			((and (string= (substring data 0 1) "0")
			      (string= (substring data 2 3) "0"))
			 0)
			((and (string= (substring data 0 1) "1")
			      (string= (substring data 2 3) "1"))
			 1)
			(t
			 (second (car (member-if (lambda (elt)
						   (string= (first elt) data))
						 local-rule))))))
	       local-data))
    ;; Count the number of times the old context matches the new
    ;; state, and then mutate output that many times, then return.
    ;; Note, it could be interesting to create a variant that would
    ;; only mutate the contextually underdetermined "alleles",
    ;; combining this step with what we have above.
    (if context
	(let ((mutations 0)
	      (to-match (car (last context))))
	  (map 'list (lambda (elt)
		       (when (eq elt to-match)
			 (setq mutations (1+ mutations))))
	       (nbutlast context))
	  (get-genotype-from-rule (mutate-genotype-n output (1- mutations))))
      (get-genotype-from-rule output))))

;; (evolve-sigil-with-blending-baldwin "n" "Æ" "Ö")

(defalias 'evolve-sigil-fn 'evolve-sigil-with-blending-baldwin
  "A function symbol that determines how to evolve a sigil.
Typical arguments are SIG, PRED, and NEXT, which specify the
sigil and its predecessor and successor.  The function takes a
fourth argument which is typically ignored, but which some
variants of the function use to do interesting things.  For
example, `evolve-sigil-with-blending-baldwin' takes a quadruple
specifying the phenotypic context as 3 old values and the new
state.")

; (evolve-sigil-fn "n" "Æ" "Ö")

(defun evolve-sigil-string (str &optional landscape)
  "Evolve sigil string STR, optionally subject to an effect determined by LANDSCAPE.
The exact nature of the effect is determined by passing data to `evolve-sigil-fn'."
  (let* ((letters (map 'list #'char-to-string (string-to-list str)))
	 (head (evolve-sigil-fn (first letters)
			     "!"
			     (or (second letters) "!")))
	 (tail (evolve-sigil-fn (car (last letters))
			     (or (nth (- (length letters) 2) letters)
				 "!")
			     "!"))
	 (landscape (if landscape (string-to-list landscape)
		      (let (empty-landscape)
			(dotimes (j (length str))
			  (setq empty-landscape (cons nil empty-landscape)))
			empty-landscape))))
    (concat (second head)
	    (map 'string
		 (lambda (mid pred next data)
		   (string-to-char (second (evolve-sigil-fn mid pred next data))))
		 (butlast (cdr letters) 1)
		 (butlast letters 2)
		 (cddr letters)
		 (butlast (cdr landscape) 1))
	    (second tail))))

(defun evolve-sigil-string-contextually (str &optional old-landscape new-landscape)
  "`evolve-sigil-string' variant with context of OLD-LANDSCAPE and NEW-LANDSCAPE.
Context is refined into quadruples with 3 old cells followed by 1
new status.  The exact nature of the effect of context is
determined by passing data to `evolve-sigil-fn'.  Only some
`evolve-sigil-fn' variants are prepared to process the context
intelligently, see its definition for more information."
  (let* ((letters (map 'list #'char-to-string (string-to-list str)))
	 (head (evolve-sigil-fn (first letters)
			     "!"
			     (or (second letters) "!")))
	 (tail (evolve-sigil-fn (car (last letters))
			     (or (nth (- (length letters) 2) letters)
				 "!")
			     "!"))
	 ;; TODO: The boundary conditions for the landscape need to be set to 0, 0
	 (landscape (if old-landscape (nconc
				       (cons 0 (string-to-list old-landscape))
				       (list 0))
		      (let (empty-landscape)
			(dotimes (j (length str))
			  (setq empty-landscape (cons nil empty-landscape)))
			empty-landscape)))
	 (landscape-triples (map 'list
				 (lambda (beg mid end) (list beg mid end))
				 (butlast landscape 2)
				 (butlast (cdr landscape) 1)
				 (cddr landscape)))
	 (landscape-quadruples (map 'list
				    (lambda (triple next) (nconc triple (list next)))
				    landscape-triples
				    (nconc
				       (cons 0 (string-to-list new-landscape))
				       (list 0))
				    )))
    (concat (second head)
	    (map 'string
		 (lambda (mid pred next data)
		   (string-to-char (second (evolve-sigil-fn mid pred next data))))
		 (butlast (cdr letters) 1)
		 (butlast letters 2)
		 (cddr letters)
		 landscape-quadruples)
	   (second tail))))

;; (evolve-sigil-string-contextually "nÆÖxιÇ" "101010" "111000")
;; (evolve-sigil-string-contextually "!#$%&" "00000")

(defun evolve-digits-by-rule (first-digit second-digit third-digit rule)
  "Look up the triple [FIRST-DIGIT SECOND-DIGIT THIRD-DIGIT] in look-up table RULE."
  (let ((triple (apply #'concat
		       (map 'list #'int-to-string
			    (list first-digit second-digit third-digit)))))
     (nth (position triple truth-table-3 :test #'string=)
	  (map 'list #'char-to-string (string-to-list rule)))))

;; (evolve-digits-by-rule 0 0 0 "10000000") => 1
;; (evolve-digits-by-rule 0 0 1 "10000000") => 0

;; Eventually we may want a "Baldwin effect" version of
;; `evolve-phenotype-against-genotype' this that takes a "meta-rule"
;; that says how the phenotype influences the genotype.  How exactly
;; we're supposed to find these meta-rules is currently a bit unclear.

(defun evolve-phenotype-against-genotype (gen phe)
  "Take PHE of 0's and 1's and evolve it according to the rules in the genotype, GEN."
  (let* ((digits (map 'list (lambda (digit)
			      (string-to-int
			       (char-to-string digit)))
		      (string-to-list phe)))
	 (letters (map 'list #'char-to-string (string-to-list gen))))
    (map 'string
	 (lambda (first-digit second-digit third-digit letter)
	   (let ((rule (get-rule-from-sigil letter)))
	     (string-to-char
	      (evolve-digits-by-rule first-digit second-digit third-digit rule))))
	 (cons 0 (butlast digits 1))
	 digits
	 (nconc (cdr digits) (list 0))
	 letters)))

(defun co-evolve-phenotype-and-genotype (gen phe)
  "Take PHE of 0's and 1's and genotype GEN, and evolve them against each other."
  ;; First calculate the new phenotype
  (let* ((new-phenotype (evolve-phenotype-against-genotype gen phe))
	 ;; new feed the new phenotype and the old genotype into `evolve-sigil-string'
	 ;; to calculate the new genotype
	 (new-genotype (evolve-sigil-string-contextually gen phe new-phenotype)))
    (list new-genotype new-phenotype)))

(defun run-for-generations (gen n)
  "Evolve the genotype GEN, N times."
  (let ((gens (list gen)))
    (dotimes (j n)
      (setq gens (nconc gens (list (evolve-sigil-string (car (last gens)))))))
    gens))

;; An effect of *current phenotype* on *next genotype* isn't exactly
;; a Baldwin effect.  
(defun run-for-generations-2 (gen phe n)
  "Evolve the genotype GEN and phenotype PHE, N times."
  (let ((gen-gens (list gen))
	(phe-gens (list phe)))
    (dotimes (j n)
      (setq gen-gens (nconc gen-gens (list (evolve-sigil-string
					    (car (last gen-gens))
					    ;; Current phenotype
					    (car (last phe-gens))))))
      (setq phe-gens (nconc phe-gens (list (evolve-phenotype-against-genotype
					    (car (last phe-gens))
					    (car (last gen-gens)))))))
    (list gen-gens phe-gens)))

;; (run-for-generations-2 "!#$%&" "01010" 5)

(defun run-for-generations-3 (gen phe n)
  "Evolve the genotype GEN together with the phenotype PHE, N times.
Depending on `evolve-sigil-fn', there may be a Baldwin effect, i.e. an
interaction between the current phenotype and the next genotype."
  (let ((gen-gens (list gen))
	(phe-gens (list phe)))
    (dotimes (j n)
      (let ((result (co-evolve-phenotype-and-genotype
		     (car (last gen-gens))
		     (car (last phe-gens)))))
	(setq gen-gens (nconc gen-gens (list (first result))))
	(setq phe-gens (nconc phe-gens (list (second result))))))
    (list gen-gens phe-gens)))

;; (run-for-generations-3 "!#$%&" "01010" 5)

(defun print-space-time (str n)
  (pop-to-buffer (get-buffer-create "*spacetime*"))
  (fundamental-mode)
  (erase-buffer)
  (insert (concat "P3" "\n"))
  (insert (concat "# " str "\n"))
  (insert (concat (int-to-string (length str)) " " (int-to-string n) "\n"))
  (insert "255\n")
  ;; for all time
  (mapc (lambda (era)
	  ;; for all space
	  (mapc (lambda (elt)
		  ;; insert relevant RGB triple
		  (mapc (lambda (num)
			  (insert (concat (int-to-string num) " ")))
			(hexrgb-hex-to-color-values
			 (third
			  (car (member-if (lambda (a)
					    (string= (char-to-string elt)
						     (second a)))
					  truth-table-8)))))
		  (insert " "))
		era)
	  (insert "\n"))
	(run-for-generations str n))
  (goto-char (point-max))
  (insert (concat "\n"))
  (image-mode))

(defun print-space-time-2 (gen phe n)
  (pop-to-buffer (get-buffer-create "*spacetime*"))
  (fundamental-mode)
  (erase-buffer)
  (insert (concat "P3" "\n"))
  (insert (concat "# " phe " " gen "\n"))
  (insert (concat (int-to-string (+ (* 2 (length gen)) 1)) " " (int-to-string n) "\n"))
  (insert "255\n")
  (let* ((data (run-for-generations-2 gen phe n)))
    ;; for all time
    (cl-mapc (lambda (gen-era phe-era)
	       ;; for all space (genotype edition)
	       (mapc (lambda (gen-elt)
		       ;; insert relevant RGB triple
		       (mapc (lambda (num)
			       (insert (concat (int-to-string num) " ")))
			     (hexrgb-hex-to-color-values
			      (third
			       (car (member-if (lambda (a)
						 (string= (char-to-string gen-elt)
							  (second a)))
					       truth-table-8)))))
		       (insert " "))
		     gen-era)
	       ;; Insert separator
	       (insert (concat "255 255 255" " "))
	       ;; for all space (phenotype edition)
	       (mapc (lambda (phe-elt)
		       (if (string= (char-to-string phe-elt) "0")
			   (mapc (lambda (num)
				   (insert (concat (int-to-string num) " ")))
				 (hexrgb-hex-to-color-values "#000000"))
			 (mapc (lambda (num)
				 (insert (concat (int-to-string num) " ")))
			       (hexrgb-hex-to-color-values "#ffffff")))
		       (insert " "))
		     phe-era)
	       (insert "\n"))
	     (first data)
	     (second data))
    (goto-char (point-max))
    (insert (concat "\n"))
    (image-mode)))

;; (print-space-time-2 "01010" "!#$%&" 10)

(defun print-space-time-3 (gen phe n)
  (pop-to-buffer (get-buffer-create "*spacetime*"))
  (fundamental-mode)
  (erase-buffer)
  (insert (concat "P3" "\n"))
  (insert (concat "# " phe " " gen "\n"))
  (insert (concat (int-to-string (+ (* 2 (length gen)) 1)) " " (int-to-string n) "\n"))
  (insert "255\n")
  (let* ((data (run-for-generations-3 gen phe n)))
    ;; for all time
    (cl-mapc (lambda (gen-era phe-era)
	       ;; for all space (genotype edition)
	       (mapc (lambda (gen-elt)
		       ;; insert relevant RGB triple
		       (mapc (lambda (num)
			       (insert (concat (int-to-string num) " ")))
			     (hexrgb-hex-to-color-values
			      (third
			       (car (member-if (lambda (a)
						 (string= (char-to-string gen-elt)
							  (second a)))
					       truth-table-8)))))
		       (insert " "))
		     gen-era)
	       ;; Insert separator
	       (insert (concat "255 255 255" " "))
	       ;; for all space (phenotype edition)
	       (mapc (lambda (phe-elt)
		       (if (string= (char-to-string phe-elt) "0")
			   (mapc (lambda (num)
				   (insert (concat (int-to-string num) " ")))
				 (hexrgb-hex-to-color-values "#000000"))
			 (mapc (lambda (num)
				 (insert (concat (int-to-string num) " ")))
			       (hexrgb-hex-to-color-values "#ffffff")))
		       (insert " "))
		     phe-era)
	       (insert "\n"))
	     (first data)
	     (second data))
    (goto-char (point-max))
    (insert (concat "\n"))
    (image-mode)))

(defun multiverse-files (m n)
  "Produce M examples of the evolution of an N x N genotype space."
  (setq default-directory "~")
  (dotimes (j m)
    (let ((jth (format "%03d" (1+ j))))
      (print-space-time (random-sigil-string n) n)
      (write-file (concat "time" jth ".ppm"))
      (kill-buffer)
      (shell-command (concat
		      "convert -scale 1000 time" jth ".ppm st" jth ".ppm"))))
  (shell-command "montage -adjoin st*.ppm times.ppm"))

; (multiverse-files 36 50)

(defun multiverse-files-2 (m n) 
  "Produce M examples of the evolution of an N x N genotype and N x N phenotype space."
  (setq default-directory "~")
  (dotimes (j m) 
    (let ((jth (format "%03d" (1+ j))))
      (print-space-time-2 (random-sigil-string n) (random-phenotype-string n)  n)
      (write-file (concat "dtime" jth ".ppm"))
      (kill-buffer)
      (shell-command (concat
		      "convert -scale 1000 dtime" jth ".ppm dst" jth ".ppm"))))
  (shell-command "montage -adjoin dst*.ppm dtimes.ppm"))

;; (multiverse-files-2 20 50)

(defun multiverse-files-3 (m n) 
  "Produce M examples of the evolution of an N x N genotype and N x N phenotype space."
  (setq default-directory "~")
  (dotimes (j m) 
    (let ((jth (format "%03d" (1+ j))))
      (print-space-time-3 (random-sigil-string n) (random-phenotype-string n) n)
      (write-file (concat "dtime" jth ".ppm"))
      (kill-buffer)
      (shell-command (concat
		      "convert -scale 1000 dtime" jth ".ppm dst" jth ".ppm"))))
  (shell-command "montage -adjoin dst*.ppm dtimes.ppm"))

;; (multiverse-files-3 20 50)

;;; end 256ca.el
