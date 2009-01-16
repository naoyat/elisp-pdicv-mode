;;; nt-file.el --- file-related functions
;;
;; Copyright (C) 2005 Naoya TOZUKA. All Rights Reserved.
;;
;; Author: Naoya TOZUKA <pdicviewer@gmail.com>
;; Maintainer: Naoya TOZUKA <pdicviewer@gmail.com>
;; Primary distribution site: http://pdicviewer.naochan.com/el/
;;
;; Created: 16 Feb 2005 (formerly nt-utils)
;; Last modified: 15 Dec 2005
;; Version: 1.0
;; Keywords: read-from-file

(provide 'nt-file)

;;; Commentary:

; (read-from-file FNAME FROM LENGTH)
;   - ファイル FNAME の、先頭からの位置 FROM  から
;     LENGTH バイト分を読み取り、文字列として返す。

; (scan-latest-version PATHNAME-FMT MIN MAX)
;   - フォーマット PATHNAME-FMT で指定されたパス名のファイルのうち
;     バージョン番号が MIN 以上 MAX 以下の範囲内で最も大きいものを探す。
;     (例) (scan-latest-version "EIJIRO%d.DIC" 50 100)
;   - 見つからなければ nil を返す。

;;; Code:
(defun nt:read-from-file (filename read-from read-length)
  "read a part of file"
  (save-current-buffer
    (let ((buffer-name (generate-new-buffer-name "*read-from-file*"))
          (my-buffer nil))
      (generate-new-buffer buffer-name)
      (set-buffer buffer-name)
      (insert-file-contents-literally filename nil read-from (+ read-from read-length) nil)
                                        ;    (setq buffer-read-only t)
      (setq my-buffer (buffer-substring 1 (+ 1 read-length)))
      (kill-buffer buffer-name)
      my-buffer
      ) ; let
    ) ; save-current-buffer
  )

(defun nt:scan-latest-version (filename-format min max)
  (catch 'scan-latest-version
    (let ((version max))
      (while (>= version min)
        (let ((file (format filename-format version)))
          (if (file-readable-p file) (throw 'scan-latest-version (list file version))
            (setq version (1- version))
            );fi
          );let
        );wend
      nil
      );let
    );caught
  )

;;; nt-file.el ends here
