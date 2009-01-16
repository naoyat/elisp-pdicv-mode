;;; pdicv-core.el --- core functions for PDIC-formatted dictionaries
;;
;; Copyright (C) 2005 Naoya TOZUKA. All Rights Reserved.
;;
;; Author: Naoya TOZUKA <pdicviewer@gmail.com>
;; Maintainer: Naoya TOZUKA <pdicviewer@gmail.com>
;; Primary distribution site: http://pdicviewer.naochan.com/el/
;;
;; Created: 14 Feb 2005
;; Last modified: 23 Dec 2005
;; Version: 0.9.1
;; Keywords: PDIC dictionary search

(provide 'pdicv-core)
;(put 'pdicv-core 'version "0.9.1")

;;; Commentary:

; (pdicv-get-header-info FILENAME)
;    - ヘッダ情報を読み取る
;
; (pdicv-get-index-list FILENAME [WORD-ENCODING])
;    - PDIC辞書ファイルから、インデックスリストを取得
;
; (pdicv-scan-datablock FILENAME PHYS CRITERIA-FUNC)
;    - データブロックをスキャン
;
; (pdicv-core-search DICINFO CRITERIA [SIMPLE-MODE-P DONT-CLEAR-P])
;    - PDIC検索（コアルーチン）
;

;;; Code:
(require 'nt-macros)
(require 'nt-readval)
(require 'nt-string)
(require 'nt-bocu)
(require 'nt-file)
(require 'nt-english)

; decoder
(defvar pdicv-null-decoder (lambda (s) s))
(defvar pdicv-sjis-decoder (lambda (s) (decode-coding-string s 'japanese-shift-jis-dos)))
(defvar pdicv-latin1-decoder (lambda (s) (decode-coding-string s 'iso-latin-1-dos)))
(defvar pdicv-bocu-decoder (lambda (s) (nt:bocu-decode s)))
(defmacro pdicv-create-decoder (encoding)
  "create a decoder from user-specified encoding"
  `(lambda (s) (decode-coding-string s ,encoding)))

(defvar pdicv-index-table-list ())

(defvar pdicv-result-height 8)
;
; ヘッダ情報を読み取る
;
(defun pdicv-get-header-info (filename)
  "[PDIC] Get Header Info"
  (catch 'pdicv-get-header-info
    (let* ((header-buf (nt:read-from-file filename 0 256))
           ;
           (headername nil);(substring header-buf 1 100))
           (dictitle nil);(substring header-buf 101 140))
           (version (nt:read-short header-buf 140))
           (lword (nt:read-short header-buf 142))
           (ljapa (nt:read-short header-buf 144))
           (block-size (nt:read-short header-buf 146))
           (index-block (nt:read-short header-buf 148))
           (header-size (nt:read-short header-buf 150))
           (index-size (nt:read-ushort header-buf 152))
           (empty-block (nt:read-short header-buf 154))
           (nindex (nt:read-short header-buf 156))
           (nblock (nt:read-short header-buf 158))
           (nword (nt:read-ulong header-buf 160))
           (dicorder (nt:read-uchar header-buf 164))
           (dictype (nt:read-uchar header-buf 165)) (dictype* nil)
           (attrlen (nt:read-uchar header-buf 166))
                                        ; NEWDIC2-
           (olenumber 0) (os nil)
           (lid-word 0) (lid-japa 0) (lid-exp 0) (lid-pron 0) (lid-other 0)
                                        ; NEWDIC3-
           (extheader 0) (index-blkbit 0) (cypt nil) (update-count 0)
           (dicident nil)
           ;;
           (major-version (/ version 256))
           (datablock-size (* nblock block-size))
           (bocu nil)
           )

      (setq version
            (nth major-version '(not-supported not-supported newdic1 newdic2 newdic3 newdic4)))

      (setq dicorder
            (nth (nt:read-uchar header-buf 164) '(code-order ignore-case dictionary-order order-descendant)))

      (if (> (logand dictype 128) 0) (setq dictype* (cons 'tree-view-mode dictype*)))
      (if (> (logand dictype 64) 0) (setq dictype* (cons 'crypted dictype*)))
;     (if (> (logand dictype 32) 0) (setq dictype* (cons 'multilingual dictype*)))
      (if (> (logand dictype 16) 0) (setq dictype* (cons 'unicode dictype*)))
      (if (> (logand dictype 8) 0) (setq dictype* (cons 'bocu dictype*)))
      (if (> (logand dictype 1) 0) (setq dictype* (cons 'ar-compressed dictype*)))

      (if (= major-version 5)
          (progn "HyperDIC, Ver 5.00"
                 (setq os (nt:read-char header-buf 167))
                 (setq os (cond ((= os 0) 'sjis-crlf)
                                ((= os 1) 'sjis-cr)
                                ((= os 2) 'sjis-lf)
                                ((= os 3) 'euc-lf)
                                ((= os 4) 'jis-lf)
                                ((= os 32) 'bocu)
                                ))
                 (if (eq os 'bocu) (setq bocu t))
                 (setq olenumber (nt:read-long header-buf 168))
			       ;(setq lid-word (short header-buf 172))
			       ;(setq lid-japa (short header-buf 174))
				;(setq lid-exp (short header-buf 176))
			       ;(setq lid-pron (short header-buf 178))
			      ;(setq lid-other (short header-buf 180))
                 (setq index-blkbit (if (= (nt:read-uchar header-buf 182) 1) 32 16))
		 ; dummy0 @185
                 (setq extheader (nt:read-ulong header-buf 184))
                 (setq empty-block (nt:read-long header-buf 188)) ;overwrite
                 (setq nindex (nt:read-long header-buf 192)) ;overwrite
                 (setq nblock (nt:read-long header-buf 196)) ;overwrite
                 (setq datablock-size (* nblock block-size))
                 (setq cypt (substring header-buf 200 208)) ;- reserved[8]
                 (setq update-count (nt:read-ulong header-buf 208))
                                        ; dummy00 @212[4]
                 (setq dicident (substring header-buf 216 224))
                                        ;(setq dummy (substring header-buf 224 256))
                 (setq index-size (* index-block block-size)) ;overwrite

                 );progn
        (progn "< 5.0"
               (if (>= major-version 3)
                   (progn "NEWDIC2-"
                          (setq olenumber (nt:read-long header-buf 167))
                                        ;(setq os (byte (substring header-buf 172 173)))
                          (setq os (nth (nt:read-char header-buf 171) '(sjis-crlf)))
                                        ;(setq lid-word (short header-buf 172))
                                        ;(setq lid-japa (short header-buf 174))
                                        ;(setq lid-exp (short header-buf 176))
                                        ;(setq lid-pron (short header-buf 178))
                                        ;(setq lid-other (short header-buf 180))
                          ))
               (if (>= major-version 4)
                   (progn "NEWDIC3-"
                          (setq extheader (nt:read-ulong header-buf 182))
                          (setq empty-block (nt:read-long header-buf 186)) ;overwrite
                          (setq nindex (nt:read-long header-buf 190)) ;overwrite
                          (setq nblock (nt:read-long header-buf 194)) ;overwrite
                          (setq datablock-size (* nblock block-size))
                          (setq index-blkbit (if (= (nt:read-uchar header-buf 198) 1) 32 16))
                          (setq cypt (substring header-buf 200 208))
                          (setq update-count (nt:read-ulong header-buf 207))
                                        ;(setq dummy (substring header-buf 212 256))
                          (setq index-size (* index-block block-size)) ;overwrite
                          ))
	       ); < 5.0
	);fi
      
      (list
;       (cons 'headername headername)   ;
;       (cons 'dictitle dictitle)       ;
       (cons 'version version)         ;
       (cons 'lword lword)             ;
       (cons 'ljapa ljapa)             ;
       (cons 'block-size block-size)   ;
       (cons 'index-block index-block) ;
       (cons 'header-size header-size) ;
       (cons 'index-size index-size)   ;
       (cons 'empty-block empty-block) ;
       (cons 'nindex nindex)           ;
       (cons 'nblock nblock)           ;
       (cons 'nword nword)             ;
       (cons 'dicorder dicorder)       ;
       (cons 'dictype dictype*)        ;
       (cons 'attrlen attrlen)         ;
       (cons 'os os)                   ;
                                        ;       (cons 'lid-word lid-word) ;
                                        ;       (cons 'lid-japa lid-japa) ;
                                        ;       (cons 'lid-exp lid-exp) ;
                                        ;       (cons 'lid-pron lid-pron) ;
                                        ;       (cons 'lid-other lid-other) ;
       (cons 'extheader extheader) ;
       (cons 'index-blkbit index-blkbit) ;(0=16,1=32)
       (cons 'cypt cypt) ;
       (cons 'update-count update-count) ;
       
       (cons 'index-begins-at (+ header-size extheader))
       (cons 'datablock-begins-at (+ header-size extheader index-size))
       (cons 'datablock-ends-at (+ header-size extheader index-size datablock-size))
       (cons 'datablock-size datablock-size)
       (cons 'bocu bocu)
       ); list
      ); let*
    ); caught
  )

(defun pdicv-get-index-list (filename &optional word-encoding)
  "[PDICV] Get the index list from PDIC file"
  (let* (
         (header (pdicv-get-header-info filename))
         (index-buf (nt:read-from-file filename
				    (-> header 'index-begins-at) (-> header 'index-size)))

         (32bit-address-mode (if (= (-> header 'index-blkbit) 32) t nil))

         (ix 0) (ix-max (-> header 'nindex))
         (ofs 0)
         (index-list ())
         )

    (while (< ix ix-max)
      (let ((phys -1) (word "") (word* nil))
        (if 32bit-address-mode
            (progn (setq phys (nt:read-ulong index-buf ofs)) (setq ofs (+ ofs 4)))
          (progn (setq phys (nt:read-ushort index-buf ofs)) (setq ofs (+ ofs 2)))
          )
        (setq word* (nt:read-cstring index-buf ofs)) (setq ofs (+ ofs (cdr word*) 1))
        (setq word (car word*))
;	(cond
;	 ((eq word-encoding 'bocu)
;	  (setq word (nt:bocu-decode word)))
;	 ((eq word-encoding 'sjis)
;	  (setq word (decode-coding-string word 'japanese-shift-jis-dos)))
;	 (word-encoding
;	  (setq word (decode-coding-string word word-encoding)))
;	 (t nil))

;	(setq index-list (cons (cons phys word) index-list))
        (push (cons phys word) index-list)
        (setq ix (1+ ix))
        );let
      )
    (nreverse index-list)
    )
  )

(defface pdicv-face-dummy
  '((( (class color) (background light) )
     (:foreground "green" :background "SlateGray1" :weight bold))
    (t
     (:foreground "red" :background "black"))) ; :weight bold
    "Face for caption")
(defface pdicv-face-caption-red
  '((t (:foreground "red" :background "black")))
  "Face for caption")
(defface pdicv-face-caption-blue
  '((t (:foreground "blue" :background "black")))
  "Face for caption")
(defface pdicv-face-caption-green
  '((t (:foreground "green" :background "black")))
  "Face for caption")
(defface pdicv-face-gray
  '((t (:foreground "gray")))
  "Face for text")

(defvar pdicv-default-inserter
      (lambda (eword pron jword example)
        (progn
          (set-text-properties 0 (length eword) '(face bold) eword)
                                        ;	(set-text-properties 0 (length eword) '(face pdicv-face-caption-green) eword)
                                        ;	(set-text-properties 0 (length jword) '(face pdicv-face-caption-gray) jword)

          (setq jword (nt:replace-all jword "〓●" " // "))
          (setq jword (nt:replace-all jword "\n" "\n  "))

          (let ((buf ""))
            (setq buf eword)
            (if (string< "" pron) (setq buf (concat buf " [" pron "]")))
                                        ;		(setq result (concat result " : " jword))
            (setq buf (concat buf "\n  " jword))
            (if (string< "" example) (setq buf (concat buf "\n  - " example))
                                        ;	      (setq buf (concat buf "\n"))
              )
;            (setq buf (concat buf "\n\n"))
            (setq buf (concat buf "\n"))

            (insert buf)
            ); let
          ); progn
        );lambda
      )
;;
;;
;;
(defun pdicv-scan-datablock (filename phys criteria-func)
  "[PDICV] scan a datablock"
;  (insert (format "pdicv-scan-datablock (%s %d ...)\n" filename phys))
  (catch 'pdicv-scan-datablock
    (let* ((result ()) ;(match-count 0)
           (header (pdicv-get-header-info filename))
           (offset (+ (-> header 'datablock-begins-at) (lsh phys 8)))
	   (aligned (if (eq (-> header 'version) 'newdic4) t nil))
;	   (bocu (-> header 'bocu))
           (head-word (nt:read-ushort (nt:read-from-file filename offset 2)))
           (blocks (logand 32767 head-word))
           (block-length (- (lsh blocks 8) 2))
           (field-size (if (zerop (logand 32768 head-word)) 2 4))
           (datablock (nt:read-from-file filename (+ offset 2) block-length))
                                        ;    (list blocks field-size datablock)
           (p 0)
           (field-length 0)
           (compress-length 0)
           (rest nil)
           (eword "") (eword-attrib 0)
           )

      (while (< p block-length) ;    (while (< p field-size)
        (setq field-length
              (if (= field-size 2) (nt:read-ushort datablock p) (nt:read-ulong datablock p)) )
        (if (zerop field-length) (throw 'pdicv-scan-datablock (nreverse result))); sfield-list))
        (setq p (+ p field-size)) ;2ないし4バイト
        (setq compress-length (nt:read-uchar datablock p)) ; 圧縮長
        (setq p (1+ p))

        (if aligned (progn
                      (setq eword-attrib (nt:read-uchar datablock p)) ; 見出し語属性
                      (setq p (1+ p))
                      ))
                                        ; 見出し語以降をとりあえず rest に入れる
        (setq rest (substring datablock p (+ p field-length)))
        (setq p (+ p field-length))
                                        ; 見出し語 (NULL終端)
        (let* ((eword-cstr (nt:read-cstring rest))
               (eword-compressed (car eword-cstr)) (eword-len (cdr eword-cstr))
               (q 0)
               (level 0)
               (extended nil)
               (jword-cstr nil) (jword "") (jword-len 0)
               (ext-list nil)
               (example "") (pron "") (link "")
               )

          (setq eword (if (zerop compress-length)
                          eword-compressed
                        (concat (substring eword 0 compress-length) eword-compressed)
                        ))
          (setq q (1+ eword-len))
                                        ; 見出し語属性
          (if (not aligned) (progn
                              (setq eword-attrib (nt:read-uchar rest q))
                              (setq q (1+ q))
                              ))
          (setq level (logand eword-attrib 15))
;	  (insert (format ": %s %d %d\n" eword eword-len eword-attrib))
;;	  (if (zerop (logand eword-attrib 128))
;;	      (throw 'pdicv-scan-datablock ()); 'illegal)
          (setq eword-attrib (logand eword-attrib 127))

          (setq extended (if (zerop (logand eword-attrib 16)) nil t))
          (if extended
              (progn ;拡張
                (setq jword-cstr (nt:read-cstring rest q))
                (setq jword (car jword-cstr)) (setq jword-len (cdr jword-cstr))
                (setq q (+ q jword-len 1))
                (setq ext-list nil)
                (catch 'while
                  (while (< q field-length)
                    (let* ((ex-attrib (nt:read-uchar rest q))
                           (ex-attrib-sub (logand ex-attrib 15))
                           (exdata-cstr nil)
                           (exdata "") (exdata-len 0) )
                      (if (= (logand ex-attrib 128) 128) (throw 'while t))
                      (setq q (1+ q))
                      (setq exdata-cstr (nt:read-cstring rest q))
                      (setq exdata (car exdata-cstr))
                      (setq exdata-len (cdr exdata-cstr))
                      (cond
                       ((= ex-attrib-sub 1) (setq example exdata))
                       ((= ex-attrib-sub 2) (setq pron exdata))
                       ((= ex-attrib-sub 4) (setq link exdata))
                       (t nil))
                      (setq q (+ q exdata-len 1))
                      ) ; let*
                    ) ; while
                  ) ; catch while2
                ) ; progn
            (progn ;標準
              (setq jword (substring rest q))
              (setq pron "")
              (setq example "")
              ) ; progn
            ) ; if extended

                                        ;	  (insert (format "- %s\n" eword))
          (if (funcall criteria-func eword pron jword example)
              (push (list eword pron jword example) result))
          );let
        ); wend
      (nreverse result)
      ); let*
    ) ;catch(0)
  )

(defun pdicv-core-search (dicinfo criteria &optional simple-mode-p dont-clear-p)
  "search in PDIC"
  (let* ((dicname (car dicinfo))
         (dicfile (nth 1 dicinfo))
         (encoding-list (nth 2 dicinfo))
         (decoder-list ())
         (index-table (-> pdicv-index-table-list dicname))
         )
;    (if (null index-table) (setq index-table (pdicv-get-index-list dicfile)))

    (if (atom encoding-list) ;; expand encoding-list
        (setq encoding-list (list encoding-list encoding-list encoding-list encoding-list)))
 
    (while encoding-list ;; build the decoder-list
      (let ((encoding (car encoding-list)))
        (cond
         ((eq encoding 'bocu) (push pdicv-bocu-decoder decoder-list))
         ((eq encoding 'sjis) (push pdicv-sjis-decoder decoder-list))
         ((eq encoding 'latin1) (push pdicv-latin1-decoder decoder-list))
         (encoding (push (pdicv-create-decoder encoding) decoder-list))
         (t (push pdicv-null-decoder decoder-list))
         );cond
        );let
      (setq encoding-list (cdr encoding-list))
      );wend
    (setq decoder-list (nreverse decoder-list))

    (catch 'pdicv-core-search
;      (if (null original-word-to-search) (setq original-word-to-search word-to-search))
      (let* (;(buffer-name (generate-new-buffer-name *buffer-name))
                                        ;               (pdicv-buffer-name "*PDIC Viewer*")
                                        ;               (dummy (if (get-buffer pdicv-buffer-name) (kill-buffer pdicv-buffer-name)))
             (pdicv-buffer (get-buffer-create "*PDIC Viewer*"))
                                        ;criteria
             (word-to-search (car criteria))

             (index-needles (nth 1 criteria))
             (needle1 (car index-needles))
             (needle2 (cdr index-needles))

             (datablock-criteria-func (nth 2 criteria))

             (ix index-table) (index-size (length ix)) (curr-size index-size)
             (ix+ (cadr ix)); next one
             (match-count 0)
             )

;	  (switch-to-buffer pdicv-buffer-name)
        (save-current-buffer
          (set-buffer pdicv-buffer)
          (if (null dont-clear-p) (erase-buffer))

          (if (not simple-mode-p)
              (progn
                                        ;(pop-to-buffer pdicv-buffer-name)
                                        ;              (set-buffer pdicv-buffer-name)
                (insert (format "検索文字列: %s\n" word-to-search))
                (insert (format "該当件数: ????\n"))
                (newline))
                                        ;(insert "\n"))
            )
          (if index-needles
              (setq ix
                    (let ((p ix) (last-p nil))
                      (catch 'pdicv-search-in-index
                        (while p
                          (let* ((elem (car p))
                                        ;(phys (car elem))
                                 (word (cdr elem)) )

                            (if (string< needle1 word) (throw 'pdicv-search-in-index last-p))
                                        ; (if (string< needle2 word) (throw 'pdicv-search-in-index last-p))

                            (setq last-p p)
                            (setq p (cdr p))
                            ); let
                          ); wend
                        last-p
                        ); caught

                      ); let
                    ))

          (catch 'while
            (while ix
              (let* ((curr (car ix))
                     (phys (car curr)) (word (cdr curr))
                                        ; (x (insert (format "* current ix: (%d %s)\n" phys word)))
                     (result (pdicv-scan-datablock dicfile phys datablock-criteria-func)); decoder-list nil))
                     (result-count (length result))
                     (inserter pdicv-default-inserter)
                     )

                (if index-needles
                    (if (string>= word needle2) (throw 'while t)))
                                        ;		    (if (not (string< word (cdr index-needles))) (throw 'while t)))

                                        ;		(insert (format "(%s with index %s ... %s)\n" 
                                        ;				word-to-search
                                        ;				(funcall (nth 0 decoder-list) word) result))

                (if result (progn
                             (while result
                               (let ((rec (pop result)))
                                 (funcall inserter
                                          (funcall (nth 0 decoder-list) (nth 0 rec)); eword
                                          (funcall (nth 1 decoder-list) (nth 1 rec)); pron
                                          (funcall (nth 2 decoder-list) (nth 2 rec)); jword
                                          (funcall (nth 3 decoder-list) (nth 3 rec)); example
                                          )
                                 (setq match-count (1+ match-count))
                                 )
                               );wend
                             (message "%5d/%5d:%7d" curr-size index-size match-count)
                             (sit-for 0)
                             )
                  (progn ;else
                    (if (zerop (% curr-size 128)) ;;128は適当な数
                        (message "%5d/%5d:%7d" curr-size index-size match-count))
                    ));fi
                );let*
              (setq ix (cdr ix))
              (setq curr-size (1- curr-size))
              );wend
            );caught

                                        ;(insert (pdicv-scan-datablock dicfile (car (car ix)) decoder-list nil needle1 needle2))
          (goto-char 1)

          (if (not simple-mode-p)
              (if (re-search-forward ": [?][?][?][?]" nil t nil)
                  (replace-match (format ": %d" match-count) t t nil 0))
            );fi

          ); save-current-buffer

;      (pop-to-buffer (current-buffer))
;        (setq split-height-threshold 6)
        (if (one-window-p)
            (set-window-buffer (split-window-vertically (- pdicv-result-height)) pdicv-buffer)
          )
        ); let*
      ); caught
    );let*
  )

;;; pdicv-core.el ends here
