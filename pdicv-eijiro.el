;;; pdicv-eijiro.el --- around eijiro
;;
;; Copyright (C) 2005-2009 naoya_t. All Rights Reserved.
;;
;; Author: naoya_t <naoya.t@aqua.plala.or.jp>
;; Maintainer: naoya_t <naoya.t@aqua.plala.or.jp>
;; Primary distribution site:
;;   http://lambdarepos.svnrepository.com/svn/share/lang/elisp/pdicv-mode/trunk
;;
;; Created: 06 Feb 2005
;; Last modified: 23 Dec 2005
;; Version: 0.9.2
;; Keywords: eijiro waeijiro

(provide 'pdicv-eijiro)

;;; Commentary:

; (pdicv-eijiro-search WORD-TO-SEARCH [REGEXP-P])
;   - �Ѽ�Ϻ/�±Ѽ�Ϻ����ñ��򸡺�������ɽ��������ġ�
;
; (pdicv-eijiro-search-interactive WORD-TO-SEARCH)
;   - <interactive> �ߥ˥Хåե��������Ϥ���ñ��򸡺�
;
; (pdicv-eijiro-search-region FROM TO)
;   - <interactive> �����ϰϤ�ʸ����򸡺�

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
