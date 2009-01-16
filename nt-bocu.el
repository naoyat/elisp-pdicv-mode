;;; nt-bocu.el --- decode/encode BOCU-1 string (via utf-8, so requires Mule-UCS)
;;
;; Copyright (C) 2005 Naoya TOZUKA. All Rights Reserved.
;;
;; Author: Naoya TOZUKA <pdicviewer@gmail.com>
;; Maintainer: Naoya TOZUKA <pdicviewer@gmail.com>
;; Primary distribution site: http://pdicviewer.naochan.com/el/
;;
;; Created: 12 Feb 2005
;; Last modified: 15 Dec 2005 (defun --> defsubst)
;; Version: 1.0.1
;; Keywords: BOCU-1 encode decode

(provide 'nt-bocu)

;;; Commentaries:

;; internal
; (nt:bocu-decode-trail-char TR)
; (nt:bocu-encode-trail-char NUM)

; (nt:bocustr-to-rawcode-list STR)
; (nt:diff-to-bocustr DIFF) - used by #'bocu-rawcode-list-to-bocustr
; (nt:rawcode-list-to-bocustr STR)

; (nt:bocu-decode STR)
;   - BOCU-1 文字列をデコードし、文字列（Mule内部形式）に変換
; (nt:bocu-encode STR)
;   - 文字列（Mule内部形式）を BOCU-1 エンコード

;;; Code:
(require 'nt-utf8)

(defsubst nt:bocu-decode-trail-char (tr)
  "[BOCU] decode trail char"
  (cond
   ((>  tr #x20) (- tr 13)) ;21- >> 14-
   ((>= tr #x1c) (- tr 12)) ;1C ... 1F >> 10 ... 13
   ((>= tr #x10) (- tr 10)) ;10 ... 19 >> 06 ... 0F
   (t (1- tr))              ;01 ... 06 >> 00 ... 05
   ); cond
  )
(defsubst nt:bocu-encode-trail-char (c)
  "[BOCU] encode trail char"
  (cond
   ((>  c #x13) (+ c 13)) ;14- >> 21-
   ((>= c #x10) (+ c 12)) ;10 ... 13 >> 1C ... 1F
   ((>= c #x06) (+ c 10)) ;06 ... 0F >> 10 ... 19
   (t (1+ c))             ;00 ... 05 >> 01 ... 06
   ); cond
  )

(defun nt:bocustr-to-rawcode-list (s)
  "[BOCU] BOCU-1 string --> rawcode-list"
  (let ((l ()) (len (length s)) (i 0)
        (pc #x40) (lead 0) (tr 0) (code 0) (diff 0))
    (while (< i len)
      (setq lead (aref s i) i (1+ i))
      (cond ((<= lead #x20) (setq code lead))
            ((= lead #x21) ;21    (L T T T)
             (setq diff (+ -187660 (* 243 243 243)))
                                        ; trail 3
             (setq tr (nt:bocu-decode-trail-char (aref s i)) i (1+ i))
             (setq diff (+ diff (* tr 243 243)))
                                        ; trail 2
             (setq tr (nt:bocu-decode-trail-char (aref s i)) i (1+ i))
             (setq diff (+ diff (* tr 243)))
                                        ; trail 1
             (setq tr (nt:bocu-decode-trail-char (aref s i)) i (1+ i))
             (setq diff (+ diff tr))
             )
            ((< lead #x25) ;22-24 (L T T)
             (setq diff (+ -10513 (* (- lead #x25) 243 243)))
                                        ; trail 2
             (setq tr (nt:bocu-decode-trail-char (aref s i)) i (1+ i))
             (setq diff (+ diff (* tr 243)))
                                        ; trail 1
             (setq tr (nt:bocu-decode-trail-char (aref s i)) i (1+ i))
             (setq diff (+ diff tr))
             )
            ((< lead #x50) ;25-4f (L T)
             (setq diff (+ -64 (* (- lead #x50) 243)))
                                        ; trail 1
             (setq tr (nt:bocu-decode-trail-char (aref s i)) i (1+ i))
             (setq diff (+ diff tr))
             )
            ((< lead #xd0) ;50-cf (L)
             (setq diff (- lead #x90))
             )
            ((< lead #xfb) ;d0-fa (L T)
             (setq diff (+ 64 (* (- lead #xd0) 243)))
                                        ; trail 1
             (setq tr (nt:bocu-decode-trail-char (aref s i)) i (1+ i))
             (setq diff (+ diff tr))
             )
            ((< lead #xfe) ;fb-fd (L T T)
             (setq diff (+ 10513 (* (- lead #xfb) 243 243)))
                                        ; trail 2
             (setq tr (nt:bocu-decode-trail-char (aref s i)) i (1+ i))
             (setq diff (+ diff (* tr 243)))
                                        ; trail 1
             (setq tr (nt:bocu-decode-trail-char (aref s i)) i (1+ i))
             (setq diff (+ diff tr))
             )
            ((= lead #xfe) ;fe    (L T T T)
             (setq diff 187660)
                                        ; trail 3
             (setq tr (nt:bocu-decode-trail-char (aref s i)) i (1+ i))
             (setq diff (+ diff (* tr 243 243)))
                                        ; trail 2
             (setq tr (nt:bocu-decode-trail-char (aref s i)) i (1+ i))
             (setq diff (+ diff (* tr 243)))
                                        ; trail 1
             (setq tr (nt:bocu-decode-trail-char (aref s i)) i (1+ i))
             (setq diff (+ diff tr))
             )
            ((= lead #xff) ; reset
             )
            ); end of cond.

      (cond
       ((<= lead #x20)
        (push lead l)
                                        ;	(setq r (concat r (string lead)))
        (if (< lead #x20) (setq pc #x40)) ;#x20ならそのまま
        )
       ((< lead #xff)
        (progn
          (setq code (+ pc diff))
          (if (< code 0) (setq code 0));; error recovery

          (push code l)
                                        ;	  (setq r (concat r (if (> code 0) (code-to-utf8 code) "?")))

          (setq pc (cond 
                    ((< code #x20) #x40)
                    ((= code #x20) pc) ; keep pc
                    ((and (<= #x3040 code) (<= code #x309f)) #x3070)
                    ((and (<= #x4e00 code) (<= code #x9fa5)) #x7711)
                    ((and (<= #xac00 code) (<= code #xd7a3)) #xc1d1)
                    (t (+ (logand code (lognot #x7f)) #x40))
                    )); pc
          ))
       (t (setq pc #x40)); #xFF: reset
       )
      ); wend
    (nreverse l)
    ); let
  )

(defun nt:diff-to-bocustr (diff)
  "[BOCU] diff --> BOCU-1 string"
  (catch 'bocu-encode-diff
    (let ((s "") (t0 0) (t1 0) (t2 0) (t3 0))
      (cond
       ((< diff -14536567) (throw 'bocu-encode-diff 'underflow-exception))
       ((< diff -187660)  ; [-14536567,-187660) : 21
        (progn
          (setq diff (- diff -14536567))
          (setq t3 (% diff 243)) (setq diff (/ diff 243))
          (setq t2 (% diff 243)) (setq diff (/ diff 243))
          (setq t1 (% diff 243)) (setq diff (/ diff 243))
                                        ;(setq t0 diff)
          (string #x21 (nt:bocu-encode-trail-char t1) (nt:bocu-encode-trail-char t2) (nt:bocu-encode-trail-char t3))
          )
        )
       ((< diff -10513)   ; [-187660,-10513) : 22-24
        (progn
          (setq diff (- diff -187660))
          (setq t2 (% diff 243)) (setq diff (/ diff 243))
          (setq t1 (% diff 243)) (setq diff (/ diff 243))
          (setq t0 diff)
          (string (+ #x22 t0) (nt:bocu-encode-trail-char t1) (nt:bocu-encode-trail-char t2))
          )
        )
       ((< diff -64)      ; [-10513,-64) : 25-4F
        (progn
          (setq diff (- diff -10513))
          (setq t1 (% diff 243)) (setq diff (/ diff 243))
          (setq t0 diff)
          (string (+ #x25 t0) (nt:bocu-encode-trail-char t1))
          )
        )
       ((< diff 64)       ; [-64,63) : 50-CF
        (progn
          (setq diff (- diff -64))
          (setq t0 diff)
          (string (+ #x50 t0))
          )
        )
       ((< diff 10513)    ; [64,10513) : D0-FA
        (progn
          (setq diff (- diff 64))
          (setq t1 (% diff 243)) (setq diff (/ diff 243))
          (setq t0 diff)
          (string (+ #xD0 t0) (nt:bocu-encode-trail-char t1))
          )
        )
       ((< diff 187660)   ; [10513,187660) : FB-FD
        (progn
          (setq diff (- diff 10513))
          (setq t2 (% diff 243)) (setq diff (/ diff 243))
          (setq t1 (% diff 243)) (setq diff (/ diff 243))
          (setq t0 diff)
          (string (+ #xFB t0) (nt:bocu-encode-trail-char t1) (nt:bocu-encode-trail-char t2))
          )
        )
       ((< diff 14536567) ; [187660,14536567) : FE
        (progn
          (setq diff (- diff 187660))
          (setq t3 (% diff 243)) (setq diff (/ diff 243))
          (setq t2 (% diff 243)) (setq diff (/ diff 243))
          (setq t1 (% diff 243)) (setq diff (/ diff 243))
                                        ;(setq t0 diff)
          (string #xFE (nt:bocu-encode-trail-char t1) (nt:bocu-encode-trail-char t2) (nt:bocu-encode-trail-char t3))
          )
        )
       (t (throw 'bocu-encode-diff 'overflow-exception))
       ); cond
      ); let
    ); caught
  )

(defun nt:rawcode-list-to-bocustr (l) ; not tested much
  "rawcode list --> BOCU-1 string"
  (let* ((s "") (pc #x40))
    (while l
      (let* ((code (car l))
             (diff (- code pc)) )
        (setq l (cdr l))
        (setq s (concat s (nt:diff-to-bocustr diff)))
        (setq pc (cond
                  ((< code #x20) #x40)
                  ((= code #x20) pc) ;keep pc
                  ((and (<= #x3040 code) (<= code #x309F)) #x3070)
                  ((and (<= #x4E00 code) (<= code #x9FA5)) #x7711)
                  ((and (<= #xAC00 code) (<= code #xD7A3)) #xC1D1)
                  (t (+ (logand code #xffff80) #x40))))
        ); let*
      ); wend
    s
    ); let*
  )

(defsubst nt:bocu-decode (s)
  "decode BOCU-1 string (via utf-8)"
  (let* ((rawcode-list (nt:bocustr-to-rawcode-list s))
         (utf8str (nt:rawcode-list-to-utf8str rawcode-list)))
    (decode-coding-string utf8str 'utf-8)
    )
  )

(defsubst nt:bocu-encode (s)
  "encode a string into BOCU-1 (via utf-8)"
  (let* ((utf8str (encode-coding-string s 'utf-8))
         (rawcode-list (nt:utf8str-to-rawcode-list utf8str))
         (bocustr (nt:rawcode-list-to-bocustr rawcode-list))
         )
    bocustr
    )
  )

;;; nt-bocu.el ends here
