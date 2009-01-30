;; pdicviewer.el - PDIC Viewer for Emacs
;;
;; Copyright (C) 2005-2009 naoya_t. All Rights Reserved.
;;
;; Author: naoya_t <naoya.t@aqua.plala.or.jp>
;; Maintainer: naoya_t <naoya.t@aqua.plala.or.jp>
;; Primary distribution site:
;;   http://lambdarepos.svnrepository.com/svn/share/lang/elisp/pdicv-mode/trunk
;;
;; Created: 14 Feb 2005
;; Last modified: 23 Dec 2005
;; Version: 0.9.2
;; Keywords: PDIC dictionary search eijiro
;;

;;; Commentary:
;;
;; PDICフォーマットの辞書から検索します。
;; 利用及び再配布の際は、GNU 一般公用許諾書の適当なバージョンに従って下さい。
;;
;; 一次配布元
;;    http://pdicviewer.naochan.com/el/

(provide 'pdicv-mode)

;;; Code:
(require 'pdicv-core)
(require 'pdicv-search)
(require 'pdicv-eijiro)

;; minor-mode
(easy-mmode-define-minor-mode
 ;; mode
 pdicv-mode
 ;; doc
 "Toggle pdicv mode."
 ;; init-value (変数 pdicv-mode の初期値)
 nil ;;initial
 ;; mode indicator
 " pdicv"
 ;; keymap
 '(("\C-c\C-d" . pdicv-set-target-dictionary)
   ("\C-c\C-a" . pdicv-search-current-word)
   ("\C-c\C-c" . pdicv-search-next-word)
   ("\C-c\C-b" . pdicv-search-previous-word))
 )

; (eword-encoding pron-encoding jword-encoding example-encoding) )
(defvar pdicv-dictionary-list '()) ;; pdicviewer.el にて設定
(defvar pdicv-inited-p nil)

;(pdicv-init)

;;;###autoload
(defun pdicviewer ()
  ""
  (interactive)
  (pdicv-init)
  )

;;; pdicviewer.el ends here
