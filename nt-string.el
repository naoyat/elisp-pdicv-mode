;;; nt-string.el --- NT's string utilities

;;
;; Copyright (C) 2005 Naoya TOZUKA. All Rights Reserved.
;;
;; Author: Naoya TOZUKA <pdicviewer@gmail.com>
;; Maintainer: Naoya TOZUKA <pdicviewer@gmail.com>
;; Primary distribution site: http://pdicviewer.naochan.com/el/
;;
;; Created: 16 Feb 2005
;; Last modified: 15 Dec 2005
;; Version: 1.0
;; Keywords: replace-in-string strcmp trim

(provide 'nt-string)

;;; Commentary:
;
; (nt:strcmp S1 S2)
; (nt:strncmp S1 S2 N)
;   - C言語の strcmp(), strncmp() 関数をエミュレートする関数。
;
; (nt:replace-all STR REGEXP SUBST)
; (nt:replace-in-string*DEPRECATED STR REGEXP SUBST)
;   - 文字列 STR 内の、正規表現 REGEXP に一致する部分をすべて
;     代替文字列 SUBST に置換する。
;
; (nt:rtrim STR)
; (nt:ltrim STR)
; (nt:trim STR)
;   - 文字列 STR の右側、左側、左右両方の空白を除去する。

;;; Code:
(require 'nt-macros)

(defmacro nt:strncmp (s1 s2 n)
  "strncmp()"
  `(nt:strcmp ,s1 ,s2 ,n))

(defun nt:strcmp (s1 s2 &optional n)
  "strcmp()"
  (catch 'strcmp
    (let* ((s1-length (length s1))
           (s2-length (length s2))
           (strncmp-p n) ; (not (null n))) ; t/nil
           (i 0))

      (if n
          (if (or (> n s1-length) (> n s2-length))
              (setq n nil strncmp-p nil)))
      (if (null n) 
          (setq n (min s1-length s2-length)))

      (if (zerop n) (throw 'strcmp 0))


      (while (< i n)
        (let ((s1-i (aref s1 i))
              (s2-i (aref s2 i)))
          (if (/= s1-i s2-i) (throw 'strcmp (- s1-i s2-i)))
          );let
        (setq i (1+ i))
        );wend

      ;n文字目までs1=s2
      (if strncmp-p
          ;; strncmp()
          0
        ;; strcmp()
        (throw 'strcmp (- s1-length s2-length)))
      );let
    0
    );caught
  )

(defun nt:replace-in-string*DEPRECATED (str r n)
  "replace /r/ in str --> n"
  (catch 'replace-in-string
    (let* ((result "")
           (s-len (length str))
           (r-len (length r))
           (till (- s-len r-len))
           (i 0) (at 0))
      (while (<= i till)
        (setq at (string-match r str i))
        (if (null at) (throw 'replace-in-string (concat result (substring str i))))

        (setq result (concat result (substring str i at) n))
        (setq i (+ at r-len))
        ); wend
      result
      ); let
    ); caught
  )

(defun nt:replace-all (str regex subst)
  "replace /regex/ in str --> subst"
  (let ((ofs 0)
        (last (length str))
        (result ""))
    (catch 'while
      (while (< ofs last)
        (let ((found-at nil))
          (if (setq found-at (string-match regex str ofs))
              (progn
                (setq result (concat result (substring str ofs found-at) subst))
                (setq ofs (match-end 0))
                )
            (progn
              (setq result (concat result (substring str ofs last)))
              (throw 'while nil)
              ))
          );let
        );wend
      );caught
    result
    );let
  )

(defun nt:rtrim (str)
  "rtrim"
  (catch 'rtrim
    (let ((i (1- (length str))))
      (while (> i 0)
        (if (> (aref str i) #x20) (throw 'rtrim (substring str 0 (1+ i))))
        (-- i)
        );wend
      );let
    );caught
  )

(defun nt:ltrim (str)
  "ltrim"
  (catch 'ltrim
    (let ((len (length str)) (i 0))
      (while (< i len)
        (if (> (aref str i) #x20) (throw 'ltrim (substring str i len)))
        (++ i)
        );wend
      );let
    );caught
  )

;(defun nt:trim (str)
;  "trim"
;  (nt:ltrim (nt:rtrim str))
;  )
(defmacro nt:trim (str)
  "trim"
  `(nt:ltrim (nt:rtrim ,str))
  )

;;; nt-string.el ends here
