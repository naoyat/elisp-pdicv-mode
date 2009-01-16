;;; nt-readval.el --- read value or a string from buffer
;;
;; Copyright (C) 2005 Naoya TOZUKA. All Rights Reserved.
;;
;; Author: Naoya TOZUKA <pdicviewer@gmail.com>
;; Maintainer: Naoya TOZUKA <pdicviewer@gmail.com>
;; Primary distribution site: http://pdicviewer.naochan.com/el/
;;
;; Created: 06 Feb 2005
;; Last modified: 15 Dec 2005 (defun --> defsubst)
;; Version: 1.0.1
;; Keywords: char uchar short ushort long ulong cstring pstring bcd

(provide 'nt-readval)

;;; Commentary:
;; this package enables you to read an integer value
;; such as (u)char,(u)short,(u)long, or a string value
;; such as C-string, Pascal-string, from the specified buffer.

;;; Code:
;;============================================================
;; uchar - read unsigned char value (1-byte) from buffer
;;       /// 1バイトのデータを unsigned char 値として読み取る
;;============================================================
(defsubst nt:read-uchar (s &optional index)
  "1-byte string --> unsigned char"
  (catch 'uchar
    (if (not index) (setq index 0))
    (if (or (< index 0) (<= (length s) index)) (throw 'uchar 'out-of-bounds-exception))
    (aref s index)
    )
  )

;;===========================================================
;; char - read (signed) char value (1-byte) from buffer
;;      /// 1バイトのデータを (signed) char 値として読み取る
;;===========================================================
(defsubst nt:read-char (s &optional index)
  "1-byte string --> signed char"
  (catch 'char
;    (let ((uc (uchar s index)))
;      (if (eq uc 'out-of-bounds-exception) (throw 'char uc))
;      (if (< uc 128) uc (- uc 256))
;      ) ; let
    (let ((c 0))
      (if (not index) (setq index 0))
      (if (or (< index 0) (<= (length s) index)) (throw 'char 'out-of-bounds-exception))
      (setq c (aref s index))
      (if (< c 128) c (- c 256)) ; =result
      ); let
    ); caught
  )

;;==============================================================
;; ushort - read unsigned short value (2-byte) from buffer
;;        /// 2バイトのデータを unsigned short 値として読み取る
;;==============================================================
(defsubst nt:read-ushort (s &optional index)
  "2-byte string (little-endian) --> unsigned short"
  (catch 'ushort
    (if (not index) (setq index 0))
    (if (or (< index 0) (< (- (length s) 2) index)) (throw 'ushort 'out-of-bounds-exception))
    (+ (lsh (aref s (1+ index)) 8)
       (aref s index))
    )
  )

(defsubst nt:read-ushort-bigendian (s &optional index)
  "2-byte string (big-endian) --> unsigned short"
  (catch 'ushort
    (if (not index) (setq index 0))
    (if (or (< index 0) (< (- (length s) 2) index)) (throw 'ushort 'out-of-bounds-exception))
    (+ (lsh (aref s index) 8)
       (aref s (1+ index)))
    )
  )

(defmacro nt:read-ushort-littleendian (s &optional index)
  "2-byte string (little-endian as default) --> unsigned short"
  `(nt:read-ushort ,s ,index))

;;==============================================================
;; short - read (signed) short value (2-byte) from buffer
;;        ///２バイトのデータを (signed) short 値として読み取る
;;==============================================================
(defsubst nt:read-short (s &optional index)
  "2-byte string (little-endian) --> signed short"
  (catch 'short
    (let ((us (nt:read-ushort s index)))
      (if (eq us 'out-of-bounds-exception) (throw 'short us))
      (if (< us 32768) us (- us 65536))
      ) ; let
    );caught
  )

(defsubst nt:read-short-bigendian (s &optional index)
  "2-byte string (big-endian) --> signed short"
  (catch 'short
    (let ((us (nt:read-ushort-bigendian s index)))
      (if (eq us 'out-of-bounds-exception) (throw 'short us))
      (if (< us 32768) us (- us 65536))
      ) ; let
    )
  )

(defmacro nt:read-short-littleendian (s &optional index)
  "2-byte string (little-endian as default) --> signed short"
  `(nt:read-short ,s ,index))

;;==============================================================
;; long - read (signed) long int value (4-byte) from buffer
;;        # emacs-lisp treates less than 28-bit value
;;        # -268435456 <= x <= 268435455 (2^28-1)
;;        ///４バイトのデータを (signed) long 値として読み取る
;;        ///※elispでは28ビットしか扱えないので注意
;;==============================================================
(defsubst nt:read-long (s &optional index)
  "4-byte string (little-endian) --> signed long
-268435456 <= x <= 268435455 (2^28-1)"
  (catch 'long
    (if (not index) (setq index 0))
    (if (or (< index 0) (< (- (length s) 4) index)) (throw 'long 'out-of-bounds-exception))

    (let* (
           (hh (aref s (+ index 3)))
           (h0 (lsh hh -4))
           )

      (cond ((zerop h0) nil) ; plus
            ((= h0 15) nil) ; minus
                                        ;      (t (setq hh (logand 15 hh)))
            ((< h0 8) (throw 'long 'overflow-exception))
            ((>= h0 8) (throw 'long 'underflow-exception))
            )
                                        ;      (logior (lsh (aref s (+ index 3)) 24)
      (logior (lsh hh 24)
              (lsh (aref s (+ index 2)) 16)
              (lsh (aref s (1+ index)) 8)
              (aref s index))
      )
    )
  )

(defsubst nt:read-long-bigendian (s &optional index)
  "4-byte string (big-endian) --> signed long
-268435456 <= x <= 268435455 (2^28-1)"
  (catch 'long
    (if (not index) (setq index 0))
    (if (or (< index 0) (< (- (length s) 4) index)) (throw 'long 'out-of-bounds-exception))

    (let* (
           (hh (aref s index))
           (h0 (lsh hh -4))
           )

      (cond ((zerop h0) nil) ; plus
            ((= h0 15) nil) ; minus
                                        ;      (t (setq hh (logand 15 hh)))
            ((< h0 8) (throw 'long 'overflow-exception))
            ((>= h0 8) (throw 'long 'underflow-exception))
            )
                                        ;      (logior (lsh (aref s (+ index 3)) 24)
      (logior (lsh hh 24)
              (lsh (aref s (1+ index)) 16)
              (lsh (aref s (+ index 2)) 8)
              (aref s (+ index 3)))
      )
    )
  )

(defmacro nt:read-long-littleendian (s &optional index)
  "4-byte string (little-endian as default) --> signed long"
  `(nt:read-long ,s ,index))

;;==============================================================
;; ulong - read unsigned long int value (4-byte) from buffer
;;        # emacs-lisp treates less than 28-bit value
;;        # 0 <= x <= 268435455 (2^28-1)
;;        ///４バイトのデータを unsigned long 値として読み取る
;;        ///※elispでは28ビットしか扱えないので注意
;;==============================================================
(defsubst nt:read-ulong (s &optional index)
  "4-byte string (little-endian) --> unsigned long
0 <= x <= 268435455 (2^28-1)"
  (catch 'ulong
    (if (not index) (setq index 0))
    (if (or (< index 0) (< (- (length s) 4) index)) (throw 'ulong 'out-of-bounds-exception))

    (let* (
           (hh (aref s (+ index 3)))
           (h0 (lsh hh -4))
           )

      (cond ((zerop h0) nil) ; plus
                                        ;      (t (setq hh (logand 15 hh)))
            (t (throw 'ulong 'overflow-exception)))
                                        ;      (logior (lsh (aref s (+ index 3)) 24)
      (logior (lsh hh 24)
              (lsh (aref s (+ index 2)) 16)
              (lsh (aref s (1+ index)) 8)
              (aref s index))
      )
    )

;    (let ((sl (long s index)))
;    (if (>= sl 0) sl 0)
;    )
  )

(defsubst nt:read-ulong-bigendian (s &optional index)
  "4-byte string (big-endian) --> unsigned long
0 <= x <= 268435455 (2^28-1)"
  (catch 'ulong
    (if (not index) (setq index 0))
    (if (or (< index 0) (< (- (length s) 4) index)) (throw 'ulong 'out-of-bounds-exception))

    (let* (
           (hh (aref s index))
           (h0 (lsh hh -4))
           )

      (cond ((zerop h0) nil) ; plus
                                        ;      (t (setq hh (logand 15 hh)))
            (t (throw 'ulong 'overflow-exception)))
                                        ;      (logior (lsh (aref s (+ index 3)) 24)
      (logior (lsh hh 24)
              (lsh (aref s (1+ index)) 16)
              (lsh (aref s (+ index 2)) 8)
              (aref s (+ index 3)))
      )
    )

;    (let ((sl (long s index)))
;    (if (>= sl 0) sl 0)
;    )
  )

(defmacro nt:read-ulong-littleendian (s &optional index)
  "4-byte string (little-endian as default) --> unsigned long"
  `(nt:read-ulong ,s ,index))

;;==============================================================
;; cstring - read a C-string (NULL-terminated) from buffer
;;        # ie. read the data until just before '\0'
;;        ///バッファからC文字列（NULL終端）を読み取る
;;==============================================================
(defsubst nt:read-cstring (s &optional index)
  "pick up a C-string.
returns (string . length)"
  (catch 'cstring
    (if (not index) (setq index 0))
    (if (or (< index 0) (>= index (length s))) (throw 'cstring 'out-of-bounds-exception))
    (let ( (ofs 0) (ofs-max (- (length s) index)) )
;     (if (> ofs-max 248) (setq ofs-max 248))
      (while (< ofs ofs-max)
        (if (zerop (aref s (+ index ofs))) 
            (throw 'cstring (cons (substring s index (+ index ofs)) ofs) )
          )
        (setq ofs (1+ ofs))
        )
      (cons (substring s index nil) ofs-max)
      )
    )
  )

;;==============================================
;; pstring - read a Pascal-string from buffer
;;        ///バッファからPascal文字列を読み取る
;;==============================================
(defsubst nt:read-pstring (s &optional index)
  "pick up a Pascal-string.
returns (string . length)"
  (catch 'pstring
    (if (not index) (setq index 0))
    (if (or (< index 0) (>= index (length s))) (throw 'pstring 'out-of-bounds-exception))
    (let ( (ofs 0) (ofs-max (- (length s) index))
           (size (nt:read-uchar s index)) )
      (if (> (1+ size) ofs-max) (throw 'pstring 'out-of-bounds-exception))
      (throw 'pstring (cons (substring s (1+ index) (+ index 1 size)) size) )
      ); let
    ); caught
  )

(defsubst nt:read-bcd (s ofs bytes)
  "read BCD value"
  (let ((i 0) (n 0) (c 0))
    (while (< i bytes)
      (setq c (aref s (+ ofs i)))
      (setq n (+ (* n 100) (* (lsh c -4) 10) (logand c #x0f)))
      (setq i (1+ i)) ;; (++ i)
      );wend
    n
    );let
  )

;;; nt-readval.el ends here
