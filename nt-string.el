;;; nt-string.el --- NT's string utilities
;;
;; Copyright (C) 2005-2009 naoya_t. All Rights Reserved.
;;
;; Author: naoya_t <naoya.t@aqua.plala.or.jp>
;; Maintainer: naoya_t <naoya.t@aqua.plala.or.jp>
;; Primary distribution site:
;;   http://lambdarepos.svnrepository.com/svn/share/lang/elisp/pdicv-mode/trunk
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

      (when n
		(when (or (> n s1-length) (> n s2-length))
		  (setq n nil strncmp-p nil)))
      (when (null n) 
		(setq n (min s1-length s2-length)))

      (when (zerop n) (throw 'strcmp 0))

      (while (< i n)
        (let ((s1-i (aref s1 i))
              (s2-i (aref s2 i)))
          (when (/= s1-i s2-i) (throw 'strcmp (- s1-i s2-i))))
        (setq i (1+ i)))

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
        (when (null at) (throw 'replace-in-string (concat result (substring str i))))

        (setq result (concat result (substring str i at) n))
        (setq i (+ at r-len)))
      result)))

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
                (setq ofs (match-end 0)))
            (progn
              (setq result (concat result (substring str ofs last)))
              (throw 'while nil))))))
    result))

(defun nt:rtrim (str)
  "rtrim"
  (catch 'rtrim
    (let ((i (1- (length str))))
      (while (> i 0)
        (when (> (aref str i) #x20) (throw 'rtrim (substring str 0 (1+ i))))
        (-- i)))))

(defun nt:ltrim (str)
  "ltrim"
  (catch 'ltrim
    (let ((len (length str)) (i 0))
      (while (< i len)
        (when (> (aref str i) #x20) (throw 'ltrim (substring str i len)))
        (++ i)))))

;(defun nt:trim (str)
;  "trim"
;  (nt:ltrim (nt:rtrim str)))
(defmacro nt:trim (str)
  "trim"
  `(nt:ltrim (nt:rtrim ,str)))

;;; nt-string.el ends here
