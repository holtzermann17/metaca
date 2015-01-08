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

;; This could be done in Latin but Chinese has nicer spacing!
;; List and definitions are from 
;; http://www.commonchinesecharacters.com/Lists/MostCommon2500ChineseCharacters
(defvar truth-table-8
  '(("00000000" "一" "#000000") ; one 
    ("00000001" "乙" "#010101") ; second
    ("00000010" "二" "#020202") ; two
    ("00000100" "十" "#040404") ; ten
    ("00000011" "丁" "#030303") ; fourth
    ("00000101" "厂" "#050505") ; factory
    ("00000110" "七" "#060606") ; seven
    ("00000111" "卜" "#070707") ; see
    ("00001000" "八" "#080808") ; eight
    ("00001001" "人" "#090909") ; man
    ("00001010" "入" "#0a0a0a") ; enter
    ("00001100" "儿" "#0c0c0c") ; son
    ("00001011" "九" "#0b0b0b") ; nine
    ("00001101" "几" "#0d0d0d") ; how much
    ("00001110" "了" "#0e0e0e") ; finish
    ("00001111" "乃" "#0f0f0f") ; so
    ("00010000" "刀" "#101010") ; knife
    ("00010001" "力" "#111111") ; power
    ("00010010" "又" "#121212") ; again
    ("00010100" "三" "#141414") ; three
    ("00010011" "干" "#131313") ; trunk
    ("00010101" "于" "#151515") ; in
    ("00010110" "亏" "#161616") ; deficit
    ("00010111" "士" "#171717") ; scholar
    ("00011000" "土" "#181818") ; earth
    ("00011001" "工" "#191919") ; work
    ("00011010" "才" "#1a1a1a") ; ability
    ("00011100" "下" "#1c1c1c") ; down
    ("00011011" "寸" "#1b1b1b") ; inch
    ("00011101" "丈" "#00ff33") ; ten feet  #1d1d1d 00011101 11100010 10111000 01000111
    ("00011110" "大" "#0033ff") ; big  #1e1e1e 00011110 11100001 01111000 10000111
    ("00011111" "与" "#1f1f1f") ; doubt
    ("00100000" "万" "#202020") ; see
    ("00100001" "上" "#212121") ; above
    ("00100010" "小" "#222222") ; small
    ("00100100" "口" "#242424") ; mouth
    ("00100011" "山" "#232323") ; mountain
    ("00100101" "巾" "#252525") ; cloth
    ("00100110" "千" "#262626") ; thousand
    ("00100111" "乞" "#272727") ; beg
    ("00101000" "川" "#282828") ; plain
    ("00101001" "亿" "#292929") ; 100 million
    ("00101010" "个" "#2a2a2a") ; this
    ("00101100" "么" "#2c2c2c") ; what
    ("00101011" "久" "#2b2b2b") ; long time
    ("00101101" "勺" "#2d2d2d") ; spoon
    ("00101110" "丸" "#2e2e2e") ; pill
    ("00101111" "夕" "#2f2f2f") ; dusk
    ("00110000" "凡" "#303030") ; ordinary
    ("00110001" "及" "#313131") ; and
    ("00110010" "广" "#323232") ; wide
    ("00110100" "亡" "#343434") ; lose
    ("00110011" "门" "#333333") ; door
    ("00110101" "义" "#353535") ; justice
    ("00110110" "之" "#363636") ; him/her
    ("00110111" "尸" "#373737") ; corpse
    ("00111000" "已" "#383838") ; then
    ("00111001" "弓" "#393939") ; bow
    ("00111010" "己" "#3a3a3a") ; self
    ("00111100" "卫" "#3c3c3c") ; guard
    ("00111011" "子" "#3b3b3b") ; child
    ("00111101" "也" "#3d3d3d") ; also
    ("00111110" "女" "#3e3e3e") ; female
    ("00111111" "飞" "#3f3f3f") ; fly
    ("01000000" "刃" "#404040") ; edge of blade
    ("01000001" "习" "#414141") ; practice
    ("01000010" "叉" "#424242") ; cross
    ("01000100" "马" "#444444") ; horse
    ("01000011" "乡" "#434343") ; home
    ("01000101" "丰" "#454545") ; luxuriant
    ("01000110" "王" "#464646") ; king
    ("01000111" "井" "#00ff33") ; neat  #474747
    ("01001000" "开" "#484848") ; open
    ("01001001" "夫" "#494949") ; that
    ("01001010" "天" "#4a4a4a") ; heaven
    ("01001100" "元" "#4c4c4c") ; money
    ("01001011" "无" "#4b4b4b") ; no
    ("01001101" "云" "#4d4d4d") ; cloud
    ("01001110" "专" "#4e4e4e") ; special
    ("01001111" "扎" "#4f4f4f") ; tie
    ("01010000" "艺" "#505050") ; skill
    ("01010001" "木" "#515151") ; tree
    ("01010010" "五" "#525252") ; five
    ("01010100" "支" "#545454") ; support
    ("01010011" "厅" "#535353") ; hall
    ("01010101" "不" "#555555") ; not
    ("01010110" "太" "#565656") ; highest
    ("01010111" "犬" "#575757") ; dog
    ("01011000" "区" "#585858") ; area
    ("01011001" "历" "#595959") ; undergo
    ("01011010" "友" "#ffcc00") ; friend #5a5a5a 01011010 10100101
    ("01011100" "尤" "#5c5c5c") ; particular
    ("01011011" "匹" "#5b5b5b") ; mate
    ("01011101" "车" "#5d5d5d") ; car
    ("01011110" "巨" "#5e5e5e") ; huge
    ("01011111" "牙" "#5f5f5f") ; tooth
    ("01100000" "屯" "#606060") ; village
    ("01100001" "比" "#616161") ; near
    ("01100010" "互" "#626262") ; mutual
    ("01100100" "切" "#646464") ; definitely
    ("01100011" "瓦" "#636363") ; tile
    ("01100101" "止" "#656565") ; stop
    ("01100110" "少" "#666666") ; young
    ("01100111" "日" "#676767") ; sun
    ("01101000" "中" "#686868") ; hit
    ("01101001" "贝" "#696969") ; shellfish
    ("01101010" "内" "#6a6a6a") ; inside
    ("01101100" "水" "#6c6c6c") ; water
    ("01101011" "冈" "#6b6b6b") ; ridge
    ("01101101" "见" "#6d6d6d") ; meet
    ("01101110" "手" "#ff3300") ; hand #6e6e6e 01101110 01110110 10010001 10001001
    ("01101111" "午" "#6f6f6f") ; noon
    ("01110000" "牛" "#707070") ; ox
    ("01110001" "毛" "#717171") ; hair
    ("01110010" "气" "#727272") ; air
    ("01110100" "升" "#747474") ; ascend
    ("01110011" "长" "#737373") ; long
    ("01110101" "仁" "#757575") ; humane
    ("01110110" "什" "#ff3300") ; assorted #767676
    ("01110111" "片" "#777777") ; flake
    ("01111000" "仆" "#0033ff") ; servant #787878
    ("01111001" "化" "#797979") ; change
    ("01111010" "仇" "#7a7a7a") ; hatred
    ("01111100" "币" "#7c7c7c") ; money
    ("01111011" "仍" "#7b7b7b") ; still
    ("01111101" "仅" "#7d7d7d") ; barely
    ("01111110" "斤" "#7e7e7e") ; half kilo
    ("01111111" "爪" "#7f7f7f") ; claw
    ("10000000" "反" "#808080") ; contrary
    ("10000001" "介" "#818181") ; introduce
    ("10000010" "父" "#828282") ; father
    ("10000100" "从" "#848484") ; from
    ("10000011" "今" "#838383") ; today
    ("10000101" "凶" "#858585") ; terrible
    ("10000110" "分" "#868686") ; part
    ("10000111" "乏" "#0033ff") ; depleted #878787
    ("10001000" "公" "#888888") ; public 
    ("10001001" "仓" "#ff3300") ; barn #898989
    ("10001010" "月" "#8a8a8a") ; moon
    ("10001100" "氏" "#8c8c8c") ; name
    ("10001011" "勿" "#8b8b8b") ; do not
    ("10001101" "风" "#8d8d8d") ; wind
    ("10001110" "欠" "#8e8e8e") ; owe
    ("10001111" "丹" "#8f8f8f") ; red
    ("10010000" "匀" "#909090") ; even
    ("10010001" "乌" "#ff3300") ; crow #919191
    ("10010010" "勾" "#929292") ; affair
    ("10010100" "凤" "#949494") ; phoenix
    ("10010011" "六" "#939393") ; six
    ("10010101" "文" "#959595") ; language
    ("10010110" "方" "#969696") ; square
    ("10010111" "火" "#979797") ; fire
    ("10011000" "为" "#989898") ; take
    ("10011001" "斗" "#999999") ; fight
    ("10011010" "忆" "#9a9a9a") ; remember
    ("10011100" "计" "#9c9c9c") ; calculate
    ("10011011" "订" "#9b9b9b") ; agree
    ("10011101" "户" "#9d9d9d") ; household
    ("10011110" "认" "#9e9e9e") ; recognize
    ("10011111" "心" "#9f9f9f") ; heart
    ("10100000" "尺" "#a0a0a0") ; foot
    ("10100001" "引" "#a1a1a1") ; pull
    ("10100010" "丑" "#a2a2a2") ; clown
    ("10100100" "巴" "#a4a4a4") ; wish
    ("10100011" "孔" "#a3a3a3") ; hole
    ("10100101" "队" "#ffcc00") ; team #a5a5a5
    ("10100110" "办" "#a6a6a6") ; manage
    ("10100111" "以" "#a7a7a7") ; because
    ("10101000" "允" "#a8a8a8") ; fair
    ("10101001" "予" "#a9a9a9") ; give
    ("10101010" "劝" "#aaaaaa") ; advise
    ("10101100" "双" "#acacac") ; pair
    ("10101011" "书" "#ababab") ; document
    ("10101101" "幻" "#adadad") ; fantasy
    ("10101110" "玉" "#aeaeae") ; jade
    ("10101111" "刊" "#afafaf") ; publish
    ("10110000" "末" "#b0b0b0") ; end
    ("10110001" "未" "#b1b1b1") ; not yet
    ("10110010" "示" "#b2b2b2") ; show
    ("10110100" "击" "#b4b4b4") ; hit
    ("10110011" "打" "#b3b3b3") ; dozen
    ("10110101" "巧" "#b5b5b5") ; opportunely
    ("10110110" "正" "#b6b6b6") ; straight
    ("10110111" "扑" "#b7b7b7") ; devote
    ("10111000" "扒" "#00ff33") ; cling #b8b8b8
    ("10111001" "功" "#b9b9b9") ; achievement
    ("10111010" "扔" "#bababa") ; throw
    ("10111100" "去" "#bcbcbc") ; go
    ("10111011" "甘" "#bbbbbb") ; sweet
    ("10111101" "世" "#bdbdbd") ; life
    ("10111110" "古" "#bebebe") ; ancient
    ("10111111" "节" "#bfbfbf") ; festival
    ("11000000" "本" "#c0c0c0") ; source
    ("11000001" "术" "#c1c1c1") ; method
    ("11000010" "可" "#c2c2c2") ; see
    ("11000100" "丙" "#c4c4c4") ; third
    ("11000011" "左" "#c3c3c3") ; unorthodox
    ("11000101" "厉" "#c5c5c5") ; strict
    ("11000110" "石" "#c6c6c6") ; stone
    ("11000111" "右" "#c7c7c7") ; right
    ("11001000" "布" "#c8c8c8") ; spread
    ("11001001" "龙" "#c9c9c9") ; dragon
    ("11001010" "平" "#cacaca") ; flat
    ("11001100" "灭" "#cccccc") ; extinguish
    ("11001011" "轧" "#cbcbcb") ; crush
    ("11001101" "东" "#cdcdcd") ; east
    ("11001110" "卡" "#cecece") ; block
    ("11001111" "北" "#cfcfcf") ; north
    ("11010000" "占" "#d0d0d0") ; occupy
    ("11010001" "业" "#d1d1d1") ; job
    ("11010010" "旧" "#d2d2d2") ; worn
    ("11010100" "帅" "#d4d4d4") ; smart
    ("11010011" "归" "#d3d3d3") ; return
    ("11010101" "目" "#d5d5d5") ; eye
    ("11010110" "旦" "#d6d6d6") ; dawn
    ("11010111" "且" "#d7d7d7") ; moreover
    ("11011000" "叮" "#d8d8d8") ; sting
    ("11011001" "叶" "#d9d9d9") ; harmony
    ("11011010" "甲" "#dadada") ; first
    ("11011100" "申" "#dcdcdc") ; extend
    ("11011011" "号" "#dbdbdb") ; roar
    ("11011101" "电" "#dddddd") ; electric
    ("11011110" "田" "#dedede") ; field
    ("11011111" "由" "#dfdfdf") ; follow
    ("11100000" "只" "#e0e0e0") ; only
    ("11100001" "央" "#0033ff") ; center #e1e1e1
    ("11100010" "史" "#00ff33") ; history #e2e2e2
    ("11100100" "兄" "#e4e4e4") ; elder brother
    ("11100011" "叼" "#e3e3e3") ; hold in the mouth
    ("11100101" "叫" "#e5e5e5") ; shout
    ("11100110" "叨" "#e6e6e6") ; garrulous
    ("11100111" "另" "#e7e7e7") ; another
    ("11101000" "叹" "#e8e8e8") ; sigh
    ("11101001" "四" "#e9e9e9") ; four
    ("11101010" "生" "#eaeaea") ; life
    ("11101100" "失" "#ececec") ; lose
    ("11101011" "禾" "#ebebeb") ; grain
    ("11101101" "丘" "#ededed") ; mound
    ("11101110" "付" "#eeeeee") ; pay
    ("11101111" "仗" "#efefef") ; weild
    ("11110000" "代" "#f0f0f0") ; substitute
    ("11110001" "仙" "#f1f1f1") ; immortl
    ("11110010" "们" "#f2f2f2") ; plural
    ("11110100" "仪" "#f4f4f4") ; apparatus
    ("11110011" "白" "#f3f3f3") ; free
    ("11110101" "仔" "#f5f5f5") ; meticulous
    ("11110110" "他" "#f6f6f6") ; him
    ("11110111" "斥" "#f7f7f7") ; blame
    ("11111000" "瓜" "#f8f8f8") ; melon
    ("11111001" "乎" "#f9f9f9") ; in
    ("11111010" "丛" "#fafafa") ; cluster
    ("11111100" "令" "#fcfcfc") ; see
    ("11111011" "用" "#fbfbfb") ; use
    ("11111101" "甩" "#fdfdfd") ; throw
    ("11111110" "印" "#fefefe") ; mark
    ("11111111" "乐" "#ffffff"))) ; happy

;; Further convenience for rendering results

(setq sigil-keywords
  (map 'list (lambda (elt) (list (second elt) 
				 `(0 (put-text-property (match-beginning 0)
							(match-end 0)
							'face (list :background 
								    ,(third elt))))))
       truth-table-8))

(defun sigil-add-to-font-lock ()
  (font-lock-add-keywords nil sigil-keywords))

(add-hook 'emacs-lisp-mode-hook 'sigil-add-to-font-lock)

;; Functions:

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
	      (first (get-genotype-from-sigil "一"))))
	 (s (first (get-genotype-from-sigil sig)))
	 (n (if next (first (get-genotype-from-sigil next))
	      (first (get-genotype-from-sigil "一"))))
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
	      (first (get-genotype-from-sigil "一"))))
	 (s (first (get-genotype-from-sigil sig)))
	 (n (if next (first (get-genotype-from-sigil next))
	      (first (get-genotype-from-sigil "一"))))
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
	      (first (get-genotype-from-sigil "一"))))
	 (s (first (get-genotype-from-sigil sig)))
	 (n (if next (first (get-genotype-from-sigil next))
	      (first (get-genotype-from-sigil "一"))))
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
	      (first (get-genotype-from-sigil "一"))))
	 (s (first (get-genotype-from-sigil sig)))
	 (n (if next (first (get-genotype-from-sigil next))
	      (first (get-genotype-from-sigil "一"))))
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

(defun mutate-rule-n (genotype n)  
  (dotimes (j n)
    (let* ((pos (random 8))
	   (elt (substring genotype pos (1+ pos))))
      (cond
       ((string= elt "0")
	(setq genotype (with-temp-buffer (insert genotype)
					 (goto-char (1+ pos))
					 (delete-char 1)
					 (insert "1")
					 (buffer-substring-no-properties (point-min)
									 (point-max)))))
       (t
	(setq genotype (with-temp-buffer (insert genotype)
					 (goto-char (1+ pos))
					 (delete-char 1)
					 (insert "0")
					 (buffer-substring-no-properties (point-min)
									 (point-max))))
	))))
  genotype)

; (mutate-rule-n "10101010" 1) ;=> "10101110"

(defun evolve-sigil-with-blending-mutation (sig &optional pred next ignore)
  (let* ((mutation 1)
	 (p (if pred (first (get-genotype-from-sigil pred))
	      (first (get-genotype-from-sigil "一"))))
	 (s (first (get-genotype-from-sigil sig)))
	 (n (if next (first (get-genotype-from-sigil next))
	      (first (get-genotype-from-sigil "一"))))
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
    (get-genotype-from-rule (mutate-rule-n output mutation))))

;; (evolve-sigil-with-blending-mutation "©" "Å" "«" 0) => ("01101111" "Æ" "#6f6f6f")
;; (evolve-sigil-with-blending-mutation "©" "Å" "«" 1) => ("01101111" "Æ" "#6f6f6f")
;; (evolve-sigil-with-blending-mutation "©" "Å" "«" 2) => ("01101011" "Ã" "#6b6b6b")
;; (evolve-sigil-with-blending-mutation "©" "Å" "«" 3) => ("01110101" "Ì" "#757575")

(defun evolve-sigil-with-blending-baldwin (sig &optional pred next context)
  (let* ((p (if pred (first (get-genotype-from-sigil pred))
	      (first (get-genotype-from-sigil "一"))))
	 (s (first (get-genotype-from-sigil sig)))
	 (n (if next (first (get-genotype-from-sigil next))
	      (first (get-genotype-from-sigil "一"))))
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
    ;;
    ;; Try mutating only 1/3 of the time or so...
    (if (and context (< (random 3) 1))
	(let* ((mutations 0)
	       (context-seq (string-to-list context))
	       (to-match (car (last context-seq))))
	  (map 'list (lambda (elt)
		       (when (eq elt to-match)
			 (setq mutations (1+ mutations))))
	       (nbutlast context-seq))
	  (get-genotype-from-rule (mutate-rule-n output (+ mutations 2))))
      (get-genotype-from-rule output))))

;; (evolve-sigil-with-blending-baldwin "n" "Æ" "Ö") ;=> ("01111111" "Ö" "#7f7f7f")
;; (evolve-sigil-with-blending-baldwin "n" "Æ" "Ö" "0010") ;=> ("00111111" "q" "#3f3f3f")

(defun evolve-sigil-with-blending-baldwin-2 (sig &optional pred next context)
  (let* ((p (if pred (first (get-genotype-from-sigil pred))
	      (first (get-genotype-from-sigil "一"))))
	 (s (first (get-genotype-from-sigil sig)))
	 (n (if next (first (get-genotype-from-sigil next))
	      (first (get-genotype-from-sigil "一"))))
	 (s-ints (map 'list (lambda (a) (string-to-int (char-to-string a))) s))
	 (local-rule (mapcar* #'list truth-table-3 s-ints))
	 (local-data (map 'list (lambda (a b c) (concat (char-to-string a)
							(char-to-string b)
							(char-to-string c)))
			  p s n))
	 (output "")
	 (mutations 0))
    ;; Count the number of times the old context matches the new
    ;; state.  We will mutate the output that many times, but
    ;; only where there are contextually underdetermined "alleles".
    (if context
	(let* ((context-seq (string-to-list context))
	       (to-match (car (last context-seq))))
	  (map 'list (lambda (elt)
		       (when (eq elt to-match)
			 (setq mutations (1+ mutations))))
	       (nbutlast context-seq))))
    ;; Now, combine the results...
    ;; TODO: finish this
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
	       local-data))))

(defalias 'evolve-sigil-fn 'evolve-sigil-with-blending-baldwin
  "A function symbol that determines how to evolve a sigil.
Typical arguments are SIG, PRED, and NEXT, which specify the
sigil and its predecessor and successor.  The function takes a
fourth argument which is typically ignored, but which some
variants of the function use to do interesting things.  For
example, `evolve-sigil-with-blending-baldwin' takes a quadruple
specifying the phenotypic context as 3 old values and the new
state.")

; (evolve-sigil-fn "n" "Æ" "Ö") ;=> ("01111111" "Ö" "#7f7f7f")

(defun evolve-sigil-string (str &optional landscape)
  "Evolve sigil string STR, optionally subject to an effect determined by LANDSCAPE.
The exact nature of the effect is determined by passing data to `evolve-sigil-fn'."
  (let* ((letters (map 'list #'char-to-string (string-to-list str)))
	 (head (evolve-sigil-fn (first letters)
			     "一"
			     (or (second letters) "一")))
	 (tail (evolve-sigil-fn (car (last letters))
			     (or (nth (- (length letters) 2) letters)
				 "一")
			     "一"))
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
				"一"
				(or (second letters) "一")))
	 (tail (evolve-sigil-fn (car (last letters))
				(or (nth (- (length letters) 2) letters)
				    "一")
				"一"))
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
		   (string-to-char
		    (second (evolve-sigil-fn mid pred next
					     (apply #'concat
						    (map 'list
							 #'char-to-string data))))))
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

;; An effect of *current phenotype* on *next genotype* isn't exactly a
;; Baldwin effect: see `run-for-generations-3' for something a bit
;; more Baldwin-like.
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
Depending on `evolve-sigil-fn', there may be a Baldwin effect,
i.e. a context-driven interaction between the change in phenotype
and the next genotype."
  (let ((gen-gens (list gen))
	(phe-gens (list phe)))
    (dotimes (j n)
      (let ((result (co-evolve-phenotype-and-genotype
		     (car (last gen-gens))
		     (car (last phe-gens)))))
	(setq gen-gens (nconc gen-gens (list (first result))))
	(setq phe-gens (nconc phe-gens (list (second result))))))
    (message "%s" (last gen-gens))
    (list gen-gens phe-gens)))

;; (run-for-generations-3 "!#$%&" "01010" 5)

(defun print-space-time (str n)
  "Basic space-time CA simulator."
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
  ; (insert (concat "# " phe " " gen "\n"))
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

(defun print-space-time-4 (gen phe n)
  (let* ((data (run-for-generations-3 gen phe n)))
    ;; Print image file
    (pop-to-buffer (get-buffer-create "*spacetime*"))
    (fundamental-mode)
    (erase-buffer)
    (insert (concat "P3" "\n"))
					; (insert (concat "# " phe " " gen "\n"))
    (insert (concat (int-to-string (+ (* 2 (length gen)) 1)) " " (int-to-string n) "\n"))
    (insert "255\n")
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
    (image-mode)

    ;; NOW PRINT TEXT FILE:
    (pop-to-buffer (get-buffer-create "*spacetime-text*"))
    (fundamental-mode)
    (erase-buffer)
    (insert "GENOTYPE:\n")
    ;; for all time (genotype edition)
    (mapc (lambda (gen-era)
	    ;; for all space (genotype edition)
	    (insert gen-era)
	    (insert "\n"))
	  (first data))
    ;; Insert separator
    (insert "\nPHENOTYPE:\n")
    ;; for all time (phenotype edition)
    (mapc (lambda (phe-era)
	    ;; for all space (phenotype edition)
	    (mapc (lambda (phe-elt)
		    (if (string= (char-to-string phe-elt) "0")
			(insert "〇"))
		    (if (string= (char-to-string phe-elt) "1")
			(insert "一")))
		  phe-era)
	    (insert "\n"))
	  (second data))
    ;; Insert newline at end-of-file
    (insert "\n")
    ))

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

;; (multiverse-files-3 1 200)

(defun multiverse-files-4 (m n) 
  "Produce M examples of the evolution of an N x N genotype and N x N phenotype space."
  (setq default-directory "~")
  (dotimes (j m) 
    (let ((jth (format "%03d" (1+ j))))
      (print-space-time-4 (random-sigil-string n) (random-phenotype-string n) n)
      (set-buffer "*spacetime-text*")
      (write-file (concat "dtime-data" jth ".txt"))
      (kill-buffer)
      (set-buffer "*spacetime*")
      (write-file (concat "dtime" jth ".ppm"))
      (kill-buffer)
      (shell-command (concat
		      "convert -scale 1000 dtime" jth ".ppm dst" jth ".ppm"))))
  (shell-command "montage -adjoin dst*.ppm dtimes.ppm"))

;; (multiverse-files-4 1 20)

;;; end 256ca.el
