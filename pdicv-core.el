;;; pdicv-core.el --- core functions for PDIC-formatted dictionaries
;;
;; Copyright (C) 2005-2009 naoya_t. All Rights Reserved.
;;
;; Author: naoya_t <naoya.t@aqua.plala.or.jp>
;; Maintainer: naoya_t <naoya.t@aqua.plala.or.jp>
;; Primary distribution site:
;;   http://lambdarepos.svnrepository.com/svn/share/lang/elisp/pdicv-mode/trunk
;;
;; Created: 14 Feb 2005
;; Last modified: 30 Jan 2009
;; Version: 0.9.2
;; Keywords: PDIC dictionary search eijiro

(provide 'pdicv-core)
;(put 'pdicv-core 'version "0.9.2")

;;; Commentary:

; (pdicv-get-header-info FILENAME)
;    - ’¥Ø’¥Ã’¥À’¾ð’Êó’¤ò’ÆÉ’¤ß’¼è’¤ë
;
; (pdicv-get-index-list FILENAME [WORD-ENCODING])
;    - PDIC’¼­’½ñ’¥Õ’¥¡’¥¤’¥ë’¤«’¤é’¡¢’¥¤’¥ó’¥Ç’¥Ã’¥¯’¥¹’¥ê’¥¹’¥È’¤ò’¼è’ÆÀ
;
; (pdicv-scan-datablock FILENAME PHYS CRITERIA-FUNC)
;    - ’¥Ç’¡¼’¥¿’¥Ö’¥í’¥Ã’¥¯’¤ò’¥¹’¥­’¥ã’¥ó
;
; (pdicv-core-search DICINFO CRITERIA [SIMPLE-MODE-P DONT-CLEAR-P])
;    - PDIC’¸¡’º÷’¡Ê’¥³’¥¢’¥ë’¡¼’¥Á’¥ó’¡Ë
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
; ’¥Ø’¥Ã’¥À’¾ð’Êó’¤ò’ÆÉ’¤ß’¼è’¤ë
;
(defun pdicv-get-header-info (filename)
  "[PDIC] Get Header Info"
  (catch 'pdicv-get-header-info
    (let* ((header-buf (nt:read-from-file filename 0 256))
           ;
           (headername nil); (substring header-buf 1 100))
           (dictitle nil); (substring header-buf 101 140))
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
            (nth major-version '(not-supported not-supported newdic1 newdic2 newdic3 newdic4 unicode-bocu-6)))

      (setq dicorder
            (nth (nt:read-uchar header-buf 164) '(code-order ignore-case dictionary-order order-descendant)))

      (when (> (logand dictype 128) 0) (setq dictype* (cons 'tree-view-mode dictype*)))
      (when (> (logand dictype 64) 0) (setq dictype* (cons 'crypted dictype*)))
;     (when (> (logand dictype 32) 0) (setq dictype* (cons 'multilingual dictype*)))
      (when (> (logand dictype 16) 0) (setq dictype* (cons 'unicode dictype*)))
      (when (> (logand dictype 8) 0) (setq dictype* (cons 'bocu dictype*)))
      (when (> (logand dictype 1) 0) (setq dictype* (cons 'ar-compressed dictype*)))

	  (case major-version
		(6 "Ver 6.xx"
		   (setq os (nt:read-char header-buf 167))
		   (setq os (cond ((= os 0) 'sjis-crlf)
						  ((= os 1) 'sjis-cr)
						  ((= os 2) 'sjis-lf)
						  ((= os 3) 'euc-lf)
						  ((= os 4) 'jis-lf)
						  ((= os 32) 'bocu)
						  ))
		   (when (eq os 'bocu) (setq bocu t))
		   (setq olenumber (nt:read-long header-buf 168))
		   ;; dummy_lid, 10 bytes
		   (setq index-blkbit (if (= (nt:read-uchar header-buf 182) 1) 32 16))
		   ;; dummy0 @185
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
		   );6
		(5 "HyperDIC, Ver 5.00"
		   (setq os (nt:read-char header-buf 167))
		   (setq os (cond ((= os 0) 'sjis-crlf)
						  ((= os 1) 'sjis-cr)
						  ((= os 2) 'sjis-lf)
						  ((= os 3) 'euc-lf)
						  ((= os 4) 'jis-lf)
						  ((= os 32) 'bocu)
						  ))
		   (when (eq os 'bocu) (setq bocu t))
		   (setq olenumber (nt:read-long header-buf 168))
		   (setq index-blkbit (if (= (nt:read-uchar header-buf 182) 1) 32 16))
		   ;; dummy0 @185
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
		   );5
		(t "< 5.0"
		   (when (>= major-version 3)
			 "NEWDIC2-"
			 (setq olenumber (nt:read-long header-buf 167))
                                        ;(setq os (byte (substring header-buf 172 173)))
			 (setq os (nth (nt:read-char header-buf 171) '(sjis-crlf)))
                                        ;(setq lid-word (short header-buf 172))
                                        ;(setq lid-japa (short header-buf 174))
                                        ;(setq lid-exp (short header-buf 176))
                                        ;(setq lid-pron (short header-buf 178))
                                        ;(setq lid-other (short header-buf 180))
			 )
		   (when (>= major-version 4)
			 "NEWDIC3-"
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
			 )
	       )); esac
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
       (cons 'bocu bocu)))))

(defun pdicv-get-index-list (filename &optional word-encoding)
  "[PDICV] Get the index list from PDIC file"
  (let* ((header (pdicv-get-header-info filename))
         (index-buf (nt:read-from-file filename
				    (-> header 'index-begins-at) (-> header 'index-size)))

         (32bit-address-mode (if (= (-> header 'index-blkbit) 32) t nil))
		 (tab-sep-p (eq 'unicode-bocu-6 (-> header 'version)))

         (ix 0) (ix-max (-> header 'nindex))
         (ofs 0)
         (index-list ()))
    (while (< ix ix-max)
      (let ((phys -1) (word "") (word* nil))
        (if 32bit-address-mode
            (progn (setq phys (nt:read-ulong index-buf ofs))
				   (setq ofs (+ ofs 4)))
          (progn (setq phys (nt:read-ushort index-buf ofs))
				 (setq ofs (+ ofs 2))))
        (setq word* (nt:read-cstring index-buf ofs)) (setq ofs (+ ofs (cdr word*) 1))
        (setq word (car word*))

		(when tab-sep-p
		  (let ((tsv (split-string word "\t")))
			(when (consp tsv)
			  (setq word (car tsv)))))
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
        ))
    (nreverse index-list) ))

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

	  (setq jword (nt:replace-all jword "’¢®’¡ü" " // "))
	  (setq jword (nt:replace-all jword "\n" "\n  "))

	  (let ((buf ""))
		(setq buf eword)
		(when (string< "" pron) (setq buf (concat buf " [" pron "]")))
                                        ;		(setq result (concat result " : " jword))
		(setq buf (concat buf "\n  " jword))
		(when (string< "" example) (setq buf (concat buf "\n  - " example)))
                                        ;	      (setq buf (concat buf "\n"))
;            (setq buf (concat buf "\n\n"))
		(setq buf (concat buf "\n"))

		(insert buf)))))
;;
;;
;;
(defun pdicv-scan-datablock (filename phys criteria-func)
  "[PDICV] scan a datablock"
  (catch 'pdicv-scan-datablock
    (let* ((result ()) ;(match-count 0)
           (header (pdicv-get-header-info filename))
		   (tab-sep-p (eq 'unicode-bocu-6 (-> header 'version)))
		   (block-size (-> header 'block-size))
           (offset (+ (-> header 'datablock-begins-at) (* phys block-size)))
		   (aligned (and (member (-> header 'version) '(newdic4 unicode-bocu-6)) t))
		   ;; (bocu (-> header 'bocu))
           (head-word (nt:read-ushort (nt:read-from-file filename offset 2)))
           (blocks (logand 32767 head-word))
           (block-length (- (* blocks block-size) 2))
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
        (when (zerop field-length) (throw 'pdicv-scan-datablock (nreverse result))); sfield-list))
        (setq p (+ p field-size)) ;2’¤Ê’¤¤’¤·4’¥Ð’¥¤’¥È
        (setq compress-length (nt:read-uchar datablock p)) ; ’°µ’½Ì’Ä¹
        (setq p (1+ p))

        (when aligned
		  (setq eword-attrib (nt:read-uchar datablock p)) ; ’¸«’½Ð’¤·’¸ì’Â°’À­
		  (setq p (1+ p)))
                                        ; ’¸«’½Ð’¤·’¸ì’°Ê’¹ß’¤ò’¤È’¤ê’¤¢’¤¨’¤º rest ’¤Ë’Æþ’¤ì’¤ë
        (setq rest (substring datablock p (+ p field-length)))
        (setq p (+ p field-length))
                                        ; ’¸«’½Ð’¤·’¸ì (NULL’½ª’Ã¼)
        (let* ((eword-cstr (nt:read-cstring rest))
               (eword-compressed (car eword-cstr)) (eword-len (cdr eword-cstr))
               (q 0)
               (level 0)
               (extended nil)
               (jword-cstr nil) (jword "") (jword-len 0)
               (ext-list nil)
               (example "") (pron "") (link ""))

          (setq eword (if (zerop compress-length)
                          eword-compressed
                        (concat (substring eword 0 compress-length) eword-compressed) ))
          (setq q (1+ eword-len))
		  ;; ’¸«’½Ð’¤·’¸ì’Â°’À­
          (when (not aligned)
			(setq eword-attrib (nt:read-uchar rest q))
			(setq q (1+ q)))

          (setq level (logand eword-attrib 15))
;	  (insert (format ": %s %d %d\n" eword eword-len eword-attrib))
;;	  (if (zerop (logand eword-attrib 128))
;;	      (throw 'pdicv-scan-datablock ()); 'illegal)
          (setq eword-attrib (logand eword-attrib 127))

          (setq extended (if (zerop (logand eword-attrib 16)) nil t))
          (if extended
              (progn ;’³È’Ä¥
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
                      (when (= (logand ex-attrib 128) 128) (throw 'while t))
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
            (progn ;’É¸’½à
              (setq jword (substring rest q))
              (setq pron "")
              (setq example ""))
            ) ; if extended

		  (if tab-sep-p
			  (let* ((splitted (split-string eword "\t"))
					 (eword (car splitted))
					 (entry (cadr splitted)))
				(when (funcall criteria-func entry eword pron jword example)
				  (push (list entry pron jword example) result)))
			(when (funcall criteria-func eword eword pron jword example)
			  (push (list eword pron jword example) result)))
		  ;;(when (funcall criteria-func eword pron jword example)
		  ;;	(push (list eword pron jword example) result))
          );let
        ); wend
      (nreverse result))))

(defun pdicv-core-search (dicinfo criteria &optional simple-mode-p dont-clear-p)
  "search in PDIC"
  (let* ((dicname (car dicinfo))
         (dicfile (nth 1 dicinfo))
         (encoding-list (nth 2 dicinfo))
         (decoder-list ())
         (index-table (-> pdicv-index-table-list dicname)))
;    (if (null index-table) (setq index-table (pdicv-get-index-list dicfile)))

    (when (atom encoding-list) ;; expand encoding-list
	  (setq encoding-list (list encoding-list encoding-list encoding-list encoding-list)))
 
    (while encoding-list ;; build the decoder-list
      (let ((encoding (car encoding-list)))
        (cond
         ((eq encoding 'bocu) (push pdicv-bocu-decoder decoder-list))
         ((eq encoding 'sjis) (push pdicv-sjis-decoder decoder-list))
         ((eq encoding 'latin1) (push pdicv-latin1-decoder decoder-list))
         (encoding (push (pdicv-create-decoder encoding) decoder-list))
         (t (push pdicv-null-decoder decoder-list))))
      (setq encoding-list (cdr encoding-list)))
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
             (match-count 0))

;	  (switch-to-buffer pdicv-buffer-name)
        (save-current-buffer
          (set-buffer pdicv-buffer)
          (when (null dont-clear-p) (erase-buffer))

          (when (not simple-mode-p)
                                        ;(pop-to-buffer pdicv-buffer-name)
                                        ;              (set-buffer pdicv-buffer-name)
			(insert (format "’¸¡’º÷’Ê¸’»ú’Îó: %s\n" word-to-search))
			(insert (format "’³º’Åö’·ï’¿ô: ????\n"))
			(newline))
                                        ;(insert "\n"))
          (when index-needles
			(setq ix
				  (let ((p ix) (last-p nil))
					(catch 'pdicv-search-in-index
					  (while p
						(let* ((elem (car p)) ;(phys (car elem))
							   (word (cdr elem)))
						  (if (string< needle1 word) (throw 'pdicv-search-in-index last-p))
						  ;; (if (string< needle2 word) (throw 'pdicv-search-in-index last-p))
                            (setq last-p p)
                            (setq p (cdr p)) ))
					  last-p))))
          (catch 'while
            (while ix
              (let* ((curr (car ix))
                     (phys (car curr)) (word (cdr curr))
					 ;; (x (insert (format "* current ix: (%d %s)\n" phys word)))
                     (result (pdicv-scan-datablock dicfile phys datablock-criteria-func)); decoder-list nil))
                     (result-count (length result))
                     (inserter pdicv-default-inserter))
                (when index-needles
				  (when (string>= word needle2) (throw 'while t)))
				;;  (if (not (string< word (cdr index-needles))) (throw 'while t)))

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
                                 ))
                             (message "%5d/%5d:%7d" curr-size index-size match-count)
                             (sit-for 0))
                  ;;else
				  (when (zerop (% curr-size 128)) ;;128’¤Ï’Å¬’Åö’¤Ê’¿ô
					(message "%5d/%5d:%7d" curr-size index-size match-count))))
              (setq ix (cdr ix))
              (setq curr-size (1- curr-size))
              );wend
            );caught

		  ;;(insert (pdicv-scan-datablock dicfile (car (car ix)) decoder-list nil needle1 needle2))
          (goto-char 1)

          (when (not simple-mode-p)
			(when (re-search-forward ": [?][?][?][?]" nil t nil)
			  (replace-match (format ": %d" match-count) t t nil 0)))
          ); save-current-buffer

;      (pop-to-buffer (current-buffer))
;        (setq split-height-threshold 6)
        (when (one-window-p)
		  (set-window-buffer (split-window-vertically (- pdicv-result-height)) pdicv-buffer))
        ))))

;;; pdicv-core.el ends here
