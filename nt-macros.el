;;; nt-macros.el --- useful (at least for NT) macros
;;
;; Copyright (C) 2005 Naoya TOZUKA. All Rights Reserved.
;;
;; Author: Naoya TOZUKA <pdicviewer@gmail.com>
;; Maintainer: Naoya TOZUKA <pdicviewer@gmail.com>
;; Primary distribution site: http://pdicviewer.naochan.com/el/
;;
;; Created: 16 Feb 2005 (formerly nt-utils.el)
;; Last modified: 15 Dec 2005
;; Version: 1.0
;; Keywords: ++ -- ->

(provide 'nt-macros)

;;; Commentary:

; (++ VAR)
;   - 変数 VAR の値を１増やす。
;     (setq VAR (1+ VAR)) のマクロ。

; (-- VAR)
;   - 変数 VAR の値を１減らす。
;     (setq VAR (1- VAR)) のマクロ。

; (string< S1 S2)
; (string> S1 S2)
; (string<= S1 S2)
; (string>= S1 S2)
;   - (string< S1 S2) を補う関数群。


;;; Code:
(defmacro ++ (var)
  "increment the specified variable"
  `(setq ,var (1+ ,var)))

(defmacro -- (var)
  "decrement the specified variable"
  `(setq ,var (1- ,var)))

(defmacro += (var num)
  `(setq ,var (+ ,var ,num)))

(defmacro -= (var num)
  `(setq ,var (- ,var ,num)))

(defmacro *= (var num)
  `(setq ,var (* ,var ,num)))

;(defmacro /= (var num)
;  `(setq ,var (/ ,var ,num)))

(defmacro != (a b)
  "/="
  `(not (= ,a ,b)))

(defmacro <<= (var num)
  `(setq ,var (lsh ,var ,num)))

(defmacro >>= (var num)
  `(setq ,var (lsh ,var (- ,num))))

(defmacro cdr= (list-var)
  `(setq ,list-var (cdr ,list-var)))

(defmacro concat= (str-var str)
  `(setq ,str-var (concat ,str-var ,str)))

;;
(defmacro -> (array key)
  ""
  `(cdr (assoc ,key ,array)))

;;
(defmacro string> (s1 s2)
  "string>"
  `(string< ,s2 ,s1))

(defmacro string<= (s1 s2)
  "string<="
  `(not (string< ,s2 ,s1)))

(defmacro string>= (s1 s2)
  "string>="
  `(not (string< ,s1 ,s2)))

;;; nt-macros.el ends here
