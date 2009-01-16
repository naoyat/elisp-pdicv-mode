;;; pdicv-search.el --- upper layer
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
;; Keywords: read-from-file

(provide 'pdicv-search)

;;; Copmmentary:
;;
; (pdicv-init)
;   - 初期化。辞書インデックスを読み込んでおく。
;
; (pdicv-search-regexp DICNAME REGEXP-TO-SEARCH [FIELD-TO-SEARCH])
;   - 正規表現検索
; (pdicv-search-just DICNAME WORD-TO-SEARCH [FIELD-TO-SEARCH])
;   - exact検索
; (pdicv-search DICNAME WORD-TO-SEARCH [JUST-P REGEXP-P FILED-TO-SEARCH])
;   - 辞書検索
;
; (pdicv-search-interactive)
;   - <interactive> ミニバッファから入力した単語を検索
; (pdicv-search-region FROM TO)
;   - <interactive> 指定した範囲の文字列を検索
; (pdicv-set-target-dictionary)
;   - <interactive> 検索対象辞書の設定
; (pdicv-search-current-word)
;   - <interactive> カーソル位置にある単語を検索
; (pdicv-search-next-word)
;   - <interactive> １つ後の単語を検索
; (pdicv-search-previous-word)
;   - <interactive> １つ前の単語を検索
;

;;; Code:
(require 'nt-macros)
(require 'nt-bocu)
(require 'nt-english)
(require 'pdicv-core)

(defvar pdicv-dictionary-list ())
(defvar pdicv-index-table-list ())

(defvar pdicv-target-dictionary nil)
(defvar pdicv-inited-p nil)

(defun pdicv-init ()
  "preload index-tables"
  (catch 'pdicv-init
    (if pdicv-inited-p (throw 'pdicv-init nil))
    (setq pdicv-index-table-list '())
    (garbage-collect)
    (let ((diclist pdicv-dictionary-list))
      (while diclist
        (let* ((dicinfo (car diclist))
               (dicname (car dicinfo))
               (path (nth 1 dicinfo))
               (encoding-list (nth 2 dicinfo))
               (word-encoding (if (atom encoding-list) encoding-list (car encoding-list)))
               (index-table nil))
          (if (atom path)
              (progn
                (message "Loading index-table for %s (\"%s\")..." dicname path)
                (setq index-table (pdicv-get-index-list path word-encoding))
                (push (cons dicname index-table) pdicv-index-table-list))
            );fi
          );let*
        (setq diclist (cdr diclist))
        );wend
      (setq pdicv-inited-p t)
      (message "Done.")
      ); let
    );caught
  )

(defun pdicv-search-regexp (dicname regexp-to-search &optional field-to-search)
  ""
;  (pdicv-search dicname nil regexp-to-search t field-to-search)
  (pdicv-search dicname regexp-to-search nil t field-to-search)
  )

(defun pdicv-search-just (dicname word-to-search &optional field-to-search)
  ""
  (pdicv-search dicname word-to-search t nil field-to-search)
  )

(defun pdicv-search (dicname word-to-search &optional just-p regexp-p field-to-search)
  ""
  (if (null just-p) (setq just-p nil))
  (if (null regexp-p) (setq regexp-p nil))
  (if (null field-to-search) (setq field-to-search 'e))

  (catch 'pdicv-search
    (let ((candidates
           (if just-p (cons (downcase word-to-search) (nt:english-guess-original-form word-to-search))))
          (candidate word-to-search)
          (first-round-p t)
          (dicinfo (assoc dicname pdicv-dictionary-list)))

      (if (null dicinfo) (throw 'pdicv-search 'dictionary-not-found))

;      (push (downcase word-to-search) candidates)
      (if (not (string= (downcase word-to-search) word-to-search))
          (push word-to-search candidates))

      (while (setq candidate (pop candidates))
                                        ;path
        (if (listp (cadr dicinfo))
            (let ((dicname-list (cadr dicinfo)))
              (while dicname-list
                (pdicv-search (car dicname-list) candidate just-p regexp-p field-to-search)
                (setq dicname-list (cdr dicname-list))
                )
              )
                                        ;else...
          (let* ((encoding-list (nth 2 dicinfo))
                 (word-encoding (if (listp encoding-list) (car encoding-list) encoding-list))
                 (word-in-dic-encoding (cond
                                        ((eq word-encoding 'bocu)
                                         (nt:bocu-encode candidate))
                                        ((eq word-encoding 'sjis)
                                         (encode-coding-string candidate 'japanese-shift-jis-dos))
                                        ((eq word-encoding 'latin1)
                                         (encode-coding-string candidate 'iso-latin-1-dos))
                                        (word-encoding
                                         (encode-coding-string candidate word-encoding))
                                        (t candidate)))
                 (needle1 word-in-dic-encoding)
                 (needle1-len (length needle1))
                 (needle2 (concat (substring needle1 0 -1)
                                  (string (1+ (aref needle1 (1- needle1-len))))))
                 (simple-mode-p just-p)

                 (criteria
                  (if regexp-p (list
                                (concat "/" candidate "/")
                                (if (string-match "^^[^[]" candidate) ; optimizable
                                    (cons (substring candidate 1 2)
                                          (string (1+ (aref candidate 1))))
                                  nil)
;				'(lambda (ix) (let ((word (cadr (car ix))))
;						(string-match needle1 word)))
;				)
                                `(lambda (e p j x) (string-match ,needle1 ,field-to-search))
                                );list
                    (list
                     candidate    ; 検索文字列（これは結果表示にしか用いられない）
                     (cons needle1 needle2) ; index検索用。nilなら全文検索
                                        ;		   '(lambda (ix) (let ((word (cadr (car ix))))
                                        ;				   (and (not (string< word needle1))
                                       ;					(string< word needle2))))
                     (if just-p
                         `(lambda (e p j x) (zerop (nt:strcmp ,field-to-search ,needle1)))
                       `(lambda (e p j x) (and (not (string< ,field-to-search ,needle1))
                                               (string< ,field-to-search ,needle2))) ; データブロック検索用。
                       );just-p
                     );list
                    ));fi,criteria

                 );let*
                                        ;	  (insert (format "%s" criteria))

            (pdicv-core-search dicinfo criteria simple-mode-p (not first-round-p)) ; clear only at the first time
            );let*
          );fi
        (setq first-round-p nil)
        );wend
      );let
    );caught
  )

(defun pdicv-search-interactive ()
  (interactive)
  (catch 'block
    (let ((dicname (completing-read "Target dictionary: " pdicv-dictionary-list nil t ""))
          (word-to-search nil))
;        (completing-read "Target dictionary:" (mapcar 'car pdicv-dictionary-list) nil t ""))
      (if (null dicname) (throw 'block nil))

      (setq word-to-search
            (read-from-minibuffer "Word to search: "))
      (if (> (length word-to-search) 0)
          (pdicv-search (intern dicname) word-to-search))
      );let
    );caught
  )


(defun pdicv-search-region (from to)
  ""
  (interactive "r")
  (let ((dicname (completing-read "Target dictionary: " pdicv-dictionary-list nil t "")))
    (if dicname (pdicv-search (intern dicname) (buffer-substring from to)))
    );let
  )

(defun pdicv-set-target-dictionary ()
  ""
  (interactive)
  (let ((dicname (completing-read "Target dictionary: " pdicv-dictionary-list nil t "")))
    (if dicname (setq pdicv-target-dictionary (intern dicname)))
    );let
  )

(defun pdicv-search-current-word ()
  ""
  (interactive)
  (if (null pdicv-target-dictionary) (pdicv-set-target-dictionary))

  (let ((word (thing-at-point 'word)))
    (if word;(and word (not (nt:skipit-p word-to-search)))
        (pdicv-search-just pdicv-target-dictionary word)
      (message "no word at cursor"))
    );let
  )

(defun pdicv-search-next-word ()
  ""
  (interactive)
  (if (null pdicv-target-dictionary) (pdicv-set-target-dictionary))

  (forward-word 1) (forward-char)
;  (pdicv-search-current-word)

  (let ((word (thing-at-point 'word)))
    (if (and word (not (nt:skipit-p word)))
        (pdicv-search-just pdicv-target-dictionary word)
      (progn
        (forward-word 1) (forward-char)
        (setq word (thing-at-point 'word))
        (if (and word (not (nt:skipit-p word)))
            (pdicv-search-just pdicv-target-dictionary word)
          (message "no word at cursor"));fi
        );progn
      );fi
    );let
  )

(defun pdicv-search-previous-word ()
  ""
  (interactive)
  (forward-word -1) ; (backward-word 1)
  (pdicv-search-current-word)
  )

;;; pdicv-search.el ends here
