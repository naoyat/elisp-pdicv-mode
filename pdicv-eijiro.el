;;; pdicv-eijiro.el --- around eijiro
;;
;; Copyright (C) 2005 Naoya TOZUKA. All Rights Reserved.
;;
;; Author: Naoya TOZUKA <pdicviewer@gmail.com>
;; Maintainer: Naoya TOZUKA <pdicviewer@gmail.com>
;; Primary distribution site: http://pdicviewer.naochan.com/el/
;;
;; Created: 06 Feb 2005
;; Last modified: 23 Dec 2005
;; Version: 0.9.1
;; Keywords: eijiro waeijiro

(provide 'pdicv-eijiro)

;;; Commentary:

; (pdicv-eijiro-search WORD-TO-SEARCH [REGEXP-P])
;   - 英辞郎/和英辞郎から単語を検索（正規表現検索も可）
;
; (pdicv-eijiro-search-interactive WORD-TO-SEARCH)
;   - <interactive> ミニバッファから入力した単語を検索
;
; (pdicv-eijiro-search-region FROM TO)
;   - <interactive> 指定範囲の文字列を検索

;;; Code:

(require 'pdicv-search)

;
; applied functions
;
(defun pdicv-eijiro-search (word-to-search &optional regexp-p)
  "search in EIJIRO/WAEIJIRO"
  (if (>= (aref word-to-search 0) 128)
      (pdicv-search 'waeijiro word-to-search regexp-p)
    (pdicv-search 'eijiro word-to-search regexp-p))
  )

(defun pdicv-eijiro-search-interactive (word-to-search)
  (interactive "sWord to search: ")
  (if (> (length word-to-search) 0)
      (pdicv-eijiro-search word-to-search)))

(defun pdicv-eijiro-search-region (from to)
  ""
  (interactive "r")
  (pdicv-eijiro-search (buffer-substring from to)))

;;; pdicv-search.el ends here
