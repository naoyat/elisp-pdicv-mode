;;; nt-file.el --- file-related functions
;;
;; Copyright (C) 2005-2009 naoya_t. All Rights Reserved.
;;
;; Author: naoya_t <naoya.t@aqua.plala.or.jp>
;; Maintainer: naoya_t <naoya.t@aqua.plala.or.jp>
;; Primary distribution site:
;;   http://lambdarepos.svnrepository.com/svn/share/lang/elisp/pdicv-mode/trunk
;;
;; Created: 16 Feb 2005 (formerly nt-utils)
;; Last modified: 15 Dec 2005
;; Version: 1.0
;; Keywords: read-from-file

(provide 'nt-file)

;;; Commentary:

; (read-from-file FNAME FROM LENGTH)
;   - �ե����� FNAME �Ρ���Ƭ����ΰ��� FROM  ����
;     LENGTH �Х���ʬ���ɤ߼�ꡢʸ����Ȥ����֤���

; (scan-latest-version PATHNAME-FMT MIN MAX)
;   - �ե����ޥå� PATHNAME-FMT �ǻ��ꤵ�줿�ѥ�̾�Υե�����Τ���
;     �С�������ֹ椬 MIN �ʾ� MAX �ʲ����ϰ���ǺǤ��礭����Τ�õ����
;     (��) (scan-latest-version "EIJIRO%d.DIC" 50 100)
;   - ���Ĥ���ʤ���� nil ���֤���

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
      my-buffer)))

(defun nt:scan-latest-version (filename-format min max)
  (catch 'scan-latest-version
    (let ((version max))
      (while (>= version min)
        (let ((file (format filename-format version)))
          (if (file-readable-p file) (throw 'scan-latest-version (list file version))
            (setq version (1- version)))))
      nil)))

;;; nt-file.el ends here
