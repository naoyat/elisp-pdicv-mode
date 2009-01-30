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
;; PDIC�ե����ޥåȤμ��񤫤鸡�����ޤ���
;; ���ѵڤӺ����ۤκݤϡ�GNU ���̸��ѵ������Ŭ���ʥС������˽��äƲ�������
;;
;; �켡���۸�
;;    http://pdicviewer.naochan.com/el/

;;; Code:
;(require 'pdicv-core)
(require 'pdicv-search)
;(require 'pdicv-eijiro)
(require 'pdicv-mode)

; ����Υ��󥹥ȡ��������ꤷ�ޤ�
; (eword-encoding pron-encoding jword-encoding example-encoding) )
(setq pdicv-dictionary-list
      '(
;        (sample "~/pdic/SAMPLE.DIC" ;
;                (nil nil sjis sjis) t)
;        (cj2 "~/pdic/cj2.dic" ;
;             bocu nil)
		(eijiro "~/Library/EIJIRO 4th Edition/Eijiro112.dic" bocu nil)
		(waeijiro "~/Library/EIJIRO 4th Edition/Waeiji112.dic" bocu nil)
;        (eijiro "~/pdic/eijiro81/EIJIRO81.DIC"
;                (nil nil sjis sjis))
;        (waeijiro "~/pdic/eijiro81/WAEIJI81.DIC"
;                  (sjis nil sjis sjis) t)
;        (fr "~/pdic/fr.dic"
;            (latin1 nil sjis latin1) nil)
;        (ej
;         (eijiro waeijiro))
        )
      )

;;;###autoload
(defun pdicv ()
  ""
  (interactive)
  (pdicv-init)
  )

;;; pdicviewer.el ends here
