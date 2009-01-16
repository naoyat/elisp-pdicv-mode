;;; nt-english.el --- English
;;
;; Copyright (C) 2005 Naoya TOZUKA. All Rights Reserved.
;;
;; Author: Naoya TOZUKA <pdicviewer@gmail.com>
;; Maintainer: Naoya TOZUKA <pdicviewer@gmail.com>
;; Primary distribution site: http://pdicviewer.naochan.com/el/
;;
;; Created: 23 Dec 2005
;; Last modified: 23 Dec 2005
;; Version: 0.2
;; Keywords:

(provide 'nt-english)

;;; Commentaries:

; (nt:english-guess-original-form WORD)
;    - 推定される単語の原形をリストで得る。
;      ※返り値のリストには WORD は含まれない。
;
; (nt:skipit-p WORD)
;    - スキップしたい単語なら t, そうでなければ nil

;; Code
(defvar nt-skip-words '("the" "a" "an"
                        "i" "you" "he" "she" "it" "we" "they"
                        "am" "are" "is" "was" "were"
                        "this" "that" "these" "those"
                        "or" "and" "but"))

(defvar nt-english-irreg-verbs-list
      '(("abode" . "abide")
        ("alit" . "alight")
        ("arose" . "arise") ("arisen" . "arise")
        ("awoke" . "awake") ("awoken" . "awake")
        ("backbit" . "backbite") ("backbitten" . "backbite")
        ("backslid" . "backslide")
        ("was" . "be") ("were" . "be") ("been" . "be")
        ("bore" . "bear") ("borne" . "bear") ("born" . "bear")
        ("beaten" . "beat")
        ("became" . "become")
        ("befell" . "befall") ("befallen" . "befall")
        ("begot" . "beget") ("begotten" . "beget")
        ("began" . "begin") ("begun" . "begin")
        ("beheld" . "behold")
        ("bent" . "bend")
        ("bereft" . "bereave")
        ("besought" . "beseech")
        ("bade" . "bid") ("bidden" . "bid")
        ("bade" . "bide")
        ("bound" . "bind")
        ("bit" . "bite") ("bitten" . "bite")
        ("bled" . "bleed")
        ("blent" . "blend")
        ("blest" . "bless")
        ("blew" . "blow") ("blown" . "blow")
        ("broke" . "break") ("broken" . "break")
        ("bred" . "breed")
        ("brought" . "bring")
        ("built" . "build")
        ("burnt" . "burn")
        ("bought" . "buy")
        ("caught" . "catch")
        ("chose" . "choose") ("chosen" . "choose")
        ("clove" . "cleave") ("cleft" . "cleave") ("cloven" . "cleave")
        ("clung" . "cling")
        ("clad" . "clothe")
        ("came" . "come")
        ("crept" . "creep")
        ("crew" . "crow")
        ("durst" . "dare")
        ("dealt" . "deal")
        ("dug" . "dig")
        ("did" . "do") ("done" . "do")
        ("drew" . "draw") ("drawn" . "draw")
        ("dreamt" . "dream")
        ("drank" . "drink") ("drunk" . "drink")
        ("drove" . "drive") ("driven" . "drive")
        ("dwelt" . "dwell")
        ("ate" . "eat") ("eaten" . "eat")
        ("fell" . "fall") ("fallen" . "fall")
        ("fed" . "feed")
        ("felt" . "feel")
        ("fought" . "fight")
        ("found" . "find")
        ("fled" . "flee")
        ("flung" . "fling")
        ("flew" . "fly") ("flown" . "fly")
        ("forbade" . "forbid") ("forbad" . "forbid") ("forbidden" . "forbid")
        ("forgot" . "forget") ("forgotten" . "forget")
        ("forgave" . "forgive") ("forgiven" . "forgive")
        ("forsook" . "forsake") ("forsaken" . "forsake")
        ("froze" . "freeze") ("frozen" . "freeze")
        ("gelt" . "geld")
        ("got" . "get") ("gotten" . "get")
        ("gilt" . "gild")
        ("girt" . "gird")
        ("gave" . "give") ("given" . "give")
        ("gnawn" . "gnaw")
        ("went" . "go") ("gone" . "go")
        ("graven" . "grave")
        ("ground" . "grind")
        ("gript" . "grip")
        ("grew" . "grow") ("grown" . "grow")
        ("hamstrung" . "hamstring")
        ("hung" . "hang") ("hung" . "hang")
        ("had" . "have")
        ("heard" . "hear")
        ("hove" . "heave")
        ("hewn" . "hew")
        ("hid" . "hide") ("hidden" . "hide")
        ("held" . "hold")
        ("inlaid" . "inlay")
        ("kept" . "keep")
        ("knelt" . "kneel")
        ("knew" . "know") ("known" . "know")
        ("laid" . "lay")
        ("led" . "lead")
        ("leant" . "lean")
        ("leapt" . "leap")
        ("learnt" . "learn")
        ("left" . "leave")
        ("lent" . "lend")
        ("lay" . "lie") ("lain" . "lie")
        ("lit" . "light")
        ("lost" . "lose")
        ("made" . "make")
        ("meant" . "mean")
        ("met" . "meet")
        ("molten" . "melt")
        ("misled" . "mislead")
        ("mistook" . "mistake") ("mistaken" . "mistake")
        ("misunderstood" . "misunderstand")
        ("mowed" . "mow") ("mown" . "mow")
        ("outdid" . "outdo") ("outdone" . "outdo")
        ("outwent" . "outgo") ("outgone" . "outgo")
        ("outgrew" . "outgrow") ("outgrown" . "outgrow")
        ("outran" . "outrun")
        ("overcame" . "overcome")
        ("overdid" . "overdo") ("overdone" . "overdo")
        ("overdrew" . "overdraw") ("overdrawn" . "overdraw")
        ("overtook" . "overtake") ("overtaken" . "overtake")
        ("overthrew" . "overthrow") ("overthrown" . "overthrow")
        ("paid" . "pay")
        ("pent" . "pen")
        ("pled" . "plead")
        ("proven" . "prove")
        ("rent" . "rend")
        ("repaid" . "repay")
        ("retold" . "retell")
        ("rewound" . "rewind")
        ("rewrote" . "rewrite") ("rewritten" . "rewrite")
        ("rode" . "ride") ("ridden" . "ride")
        ("rang" . "ring") ("rung" . "ring")
        ("rose" . "rise") ("risen" . "rise")
        ("ran" . "run")
        ("sawn" . "saw")
        ("said" . "say")
        ("saw" . "see") ("seen" . "see")
        ("sought" . "seek")
        ("sold" . "sell")
        ("sent" . "send")
        ("sewn" . "sew")
        ("shook" . "shake") ("shaken" . "shake")
        ("shaven" . "shave")
        ("shorn" . "shear")
        ("shone" . "shine") ("shone" . "shine")
        ("shat" . "shit")
        ("shod" . "shoe")
        ("shot" . "shoot")
        ("shown" . "show")
        ("shrank" . "shrink") ("shrunk" . "shrink") ("shrunken" . "shrink")
        ("shrove" . "shrive") ("shriven" . "shrive")
        ("sang" . "sing") ("sung" . "sing")
        ("sank" . "sink") ("sunk" . "sink") ("sunken" . "sink")
        ("sat" . "sit")
        ("slew" . "slay") ("slain" . "slay")
        ("slept" . "sleep")
        ("slid" . "slide")
        ("slung" . "sling")
        ("slunk" . "slink")
        ("smelt" . "smell")
        ("smote" . "smite") ("smitten" . "smite")
        ("sown" . "sow")
        ("spoke" . "speak") ("spoken" . "speak")
        ("sped" . "speed")
        ("spelt" . "spell")
        ("spellbound" . "spellbind")
        ("spent" . "spend")
        ("spilt" . "spill")
        ("spun" . "spin") ("span" . "spin")
        ("spat" . "spit")
        ("spoilt" . "spoil")
        ("sprang" . "spring") ("sprung" . "spring")
        ("stood" . "stand")
        ("stove" . "stave")
        ("stole" . "steal") ("stolen" . "steal")
        ("stuck" . "stick")
        ("stung" . "sting")
        ("stank" . "stink") ("stunk" . "stink")
        ("strewn" . "strew")
        ("strode" . "stride") ("stridden" . "stride")
        ("struck" . "strike") ("stricken" . "strike")
        ("strung" . "string")
        ("strove" . "strive") ("striven" . "strive")
        ("swore" . "swear") ("sworn" . "swear")
        ("swept" . "sweep")
        ("swollen" . "swell")
        ("swam" . "swim") ("swum" . "swim")
        ("swung" . "swing")
        ("took" . "take") ("taken" . "take")
        ("taught" . "teach")
        ("tore" . "tear") ("torn" . "tear")
        ("telecasted" . "telecast")
        ("told" . "tell")
        ("thought" . "think")
        ("throve" . "thrive") ("thriven" . "thrive")
        ("threw" . "throw") ("thrown" . "throw")
        ("thrust" . "thrust") ("thrust" . "thrust")
        ("trod" . "tread") ("trodden" . "tread")
        ("unbent" . "unbend")
        ("unbound" . "unbind")
        ("underwent" . "undergo") ("undergone" . "undergo")
        ("understood" . "understand")
        ("undertook" . "undertake") ("undertaken" . "undertake")
        ("undid" . "undo") ("undone" . "undo")
        ("woke" . "wake") ("woken" . "wake")
        ("wore" . "wear") ("worn" . "wear")
        ("wove" . "weave") ("woven" . "weave")
        ("wept" . "weep")
        ("won" . "win")
        ("wound" . "wind")
        ("withdrew" . "withdraw") ("withdrawn" . "withdraw")
        ("withheld" . "withhold")
        ("withstood" . "withstand")
        ("wrought" . "work")
        ("wrung" . "wring")
        ("wrote" . "write") ("written" . "write")
;;
        ("does" . "do") ("did" "do")
        ("could" . "can")
        ("would" . "will")
        ("should" . "shall")
        ("might" . "may")
;       ("ain't" "be")
        ))

(defvar nt-english-irreg-nouns-list
      '(
        ("children" . "child")
        ("boxen" . "box") ("oxen" . "ox")
        ("men" . "man") ("women" . "woman")
        ("geese" . "goose") ("teeth" . "tooth") ("feet" . "foot") ("mice" . "mouse")
                                        ; ("those" . "that") ("these" . "this")
        ))

(defun nt:english-guess-original-form (word)
  (catch 'block
    (cond
     ((null word) (throw 'block nil))
     ((string= word "") (throw 'block nil))
     (t nil)
     )
    
    (let* ((word-len (length word))
           (body-1 nil) (suffix-1 nil)
           (body-2 nil) (suffix-2 nil)
           (body-3 nil) (suffix-3 nil)
           (body-4 nil) (suffix-4 nil) (tmp))

      (setq body-1 (substring word 0 -1))
      (setq suffix-1 (substring word -1 nil))

      (if (>= word-len 2)
          (progn
           (setq body-2 (substring word 0 -2))
           (setq suffix-2 (substring word -2 nil))
           
           (if (>= word-len 3)
               (progn
                (setq body-3 (substring word 0 -3))
                (setq suffix-3 (substring word -3 nil))
                
                (if (>= word-len 4)
                    (progn
                     (setq body-4 (substring word 0 -4))
                     (setq suffix-4 (substring word -4 nil))
                     ))
                ))
           ))
      
      ;; irregular verbs/nouns first.
      (setq tmp (cdr (assoc word nt-english-irreg-verbs-list)))
      (if tmp (throw 'block (list tmp)))
      (setq tmp (cdr (assoc word nt-english-irreg-nouns-list)))
      (if tmp (throw 'block (list tmp)))
      
      (cond
       ((string= suffix-3 "ves") (list (concat body-3 "fe") (concat body-3 "f") body-1)) ;; -f > -ves
       ((string= suffix-3 "ies") (list (concat body-3 "y") body-1)) ;; -y > -ies
       ((string= suffix-3 "oes") (list body-2)) ;; o > o(e)s
       ((string= suffix-2 "es") (list (concat body-2 "is") body-1 body-2)) ;; (e)s, -is
       ((string= suffix-1 "s") (list body-1))
       ((string= suffix-1 "i") (list (concat body-1 "us"))) ;; -us > -i
       ((string= suffix-1 "a") (list (concat body-1 "um") (concat body-1 "on"))) ;; -um > -a
       ((string= suffix-2 "ae") (list body-1)) ;; -a > -ae
       
                                        ; verb -ed
       ((string= suffix-4 "nned") (list body-3)) ;; -n > -nned
       ((string= suffix-4 "dded") (list body-3)) ;; -d > -dded
       ((string= suffix-4 "tted") (list body-3)) ;; -t > -tted
       ((string= suffix-4 "pped") (list body-3)) ;; -p > -pped
       ((string= suffix-4 "gged") (list body-3)) ;; -g > -gged
       ((string= suffix-4 "cked") (list body-2 body-3)) ;; -c[k] > -cked
       ((string= suffix-3 "ied") (list (concat body-3 "y"))) ;; -子音+y > -ied ※-母音+y > yed
       ((string= suffix-2 "ed") (list body-1 body-2)) ;; (default) -(e) > -ed
       ((string= suffix-2 "id") (list (concat body-2 "y"))) ;; pay > paid, say > said
                                        ; verb -ing
       ((string= suffix-4 "ying") (list (concat body-4 "ie") body-3))
       ((string= suffix-3 "ing") (list (concat body-3 "e") body-3))

       ((string= suffix-3 "n't") (list body-3))
       ((string= suffix-3 "'ll") (list body-3))
       ((string= suffix-3 "'re") (list body-3))
       ((string= suffix-2 "'m") (list body-2))
       ((string= suffix-2 "'d") (list body-2))
       ((string= suffix-2 "'s") (list body-2))
      
       (t nil) ;; 推測候補がない場合は 空リスト を返す。
       ) ;cond
      ) ;let
    );caught
  )

(defmacro nt:skipit-p (word)
  `(if (member (downcase ,word) nt-skip-words) t nil))

;;; nt-english.el ends here
