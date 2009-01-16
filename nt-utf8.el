;;; nt-utf8.el --- converter between utf-8 and rawcode
;;;             some functions require Mule-UCS
;;
;; Copyright (C) 2005 Naoya TOZUKA. All Rights Reserved.
;;
;; Author: Naoya TOZUKA <pdicviewer@gmail.com>
;; Maintainer: Naoya TOZUKA <pdicviewer@gmail.com>
;; Primary distribution site: http://pdicviewer.naochan.com/el/
;;
;; Created: 14 Feb 2005
;; Last modified: 15 Dec 2005
;; Version: 1.0
;; Keywords: UTF-8

(provide 'nt-utf8)

;;; Commentaries:
;
;; core functions
; (nt:utf8-rawcode-to-utf8str CODE)
; (nt:utf8-rawcode-list-to-utf8str LIST)
; (nt:utf8-utf8str-to-rawcode-list STR)

;; applied functions
; (nt:utf8-mulestr-to-unicode-list STR)  - requires Mule-UCS
;   - Mule内部文字列を、文字列を構成する各文字のunicodeコードリストに変換
; (nt:utf8-unicode-list-to-mulestr LIST) - requires Mule-UCS
;   - 文字列を構成する各文字のunicodeコードリストを、Mule内部文字列に変換

;;; Code:
(defun nt:rawcode-to-utf8str (code)
  "convert a raw-code to utf-8 string"
  (cond
   ((<= code #x007f) (string code))
   ((<= code #x07ff) (string (logior #xc0 (lsh code -6))
                             (logior #x80 (logand code #x3f))
                             ))
   ((<= code #xffff) (string (logior #xe0 (lsh code -12))
                             (logior #x80 (logand (lsh code -6) #x3f))
                             (logior #x80 (logand code #x3f))
                             ))
   ((<= code #x10ffff) (string (logior #xf0 1 (lsh code -18)
                                       (logior #x80 2 (logand (lsh code -12) #x3f))
                                       (logior #x80 3 (logand (lsh code -6) #x3f))
                                       (logior #x80 4 (logand code #x3f))
                                       ))
    )
   );cond
  )

(defun nt:rawcode-list-to-utf8str (l)
  "convert raw-code list to utf-8 string"
  (let ((s ""))
    (while l
      (setq s (concat s (nt:rawcode-to-utf8str (car l))))
      (setq l (cdr l))
      )
    s
    )
  )

(defun nt:utf8str-to-rawcode-list (s)
  "convert utf-8 string to raw-code list"
  (let ((result ())
        (len (length s)) (i 0))
    (while (< i len)
      (let ((c (aref s i))
            (code -1))
        (setq i (1+ i))
        (setq code
              (cond
               ((zerop (logand c #x80))   ; 0xxxxxxx
                c)                        ;  > 000000000xxxxxxx : 0000-007F
               ((= (logand c #xe0) #xc0)  ; 110yyyyy 10xxxxxx
                (let ((t1 (aref s i)))    ;  > 00000yyyyyxxxxxx : 0080-07FF
                  (setq i (1+ i))
                  (+ (lsh (logand #x1f c) 6)
                     (logand #x3f t1))
                  ))
               ((= (logand c #xf0) #xe0)  ; 1110zzzz 10yyyyyy 10xxxxxx
                (let ((t1 (aref s i))     ;  > zzzzyyyyyyxxxxxx : 0800-FFFF
                      (t2 (aref s (1+ i))))
                  (setq i (+ i 2))
                  (+ (lsh (logand #x0f c) 12)
                     (lsh (logand #x3f t1) 6)
                     (logand #x3f t2))
                  ))
               ((= (logand c #xf8) #xf0)  ; 11110uuu 10uuzzzz 10yyyyyy 10xxxxxx
                (let ((t1 (aref s i))     ;  > 000uuuuuzzzzyyyyyyxxxxxx : 10000-10FFFF
                      (t2 (aref s (1+ i)))
                      (t3 (aref s (+ i 2))))
                  (setq i (+ i 3))
                  (+ (lsh (logand #x07 c) 16)
                     (lsh (logand #x3f t1) 12)
                     (lsh (logand #x3f t2) 6)
                     (logand #x3f t3))
                  ))
               (t -1)
               )); code
        (setq result (cons code result))
        );let
      ); wend
    (nreverse result)
    ); let
  )

;
; applied codes
;
(defun nt:mulestr-to-unicode-list (s)
  "convert any Emacs-string to a Unicode raw-code list"
  (nt:utf8str-to-rawcode-list (encode-coding-string s 'utf-8))
  )

(defun nt:unicode-list-to-mulestr (l)
  "convert a Unicode raw-code list to an Emacs-string"
  (decode-coding-string (nt:rawcode-list-to-utf8str l) 'utf-8)
  )

;;; nt-utf8.el ends here
