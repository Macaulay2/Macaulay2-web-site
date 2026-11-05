;;; make-archive-contents.el --- generate a package.el archive index -*- lexical-binding: t -*-

;; Usage: emacs --batch -l make-archive-contents.el OUTPUT PACKAGE...
;;
;; Writes OUTPUT (normally archive-contents) describing the newest
;; version of each package found among the PACKAGEs.  Each PACKAGE is
;; either a package.el tar package (NAME-VERSION.tar, e.g., produced
;; by "make dist" in M2-emacs) or a single-file package
;; (NAME-VERSION.el, with the usual package headers).

(require 'package)
(require 'tar-mode)

(let ((output (pop command-line-args-left))
      entries)
  (dolist (file command-line-args-left)
    (let* ((desc (with-temp-buffer
                   (insert-file-contents-literally file)
                   (if (string-suffix-p ".tar" file)
                       (progn (tar-mode) (package-tar-file-info))
                     (decode-coding-region (point-min) (point-max) 'utf-8)
                     (package-buffer-info))))
           (name (package-desc-name desc))
           (old (cdr (assq name entries))))
      ;; package.el downloads NAME-VERSION.tar or NAME-VERSION.el using
      ;; the version from the package metadata, not the file name
      (unless (equal (file-name-nondirectory file)
                     (concat (package-desc-full-name desc)
                             (package-desc-suffix desc)))
        (error "%s should be named %s%s" file
               (package-desc-full-name desc) (package-desc-suffix desc)))
      (when (or (null old)
                (version-list-< (aref old 0) (package-desc-version desc)))
        (setq entries
              (cons (cons name (vector (package-desc-version desc)
                                       (package-desc-reqs desc)
                                       (package-desc-summary desc)
                                       (package-desc-kind desc)
                                       (package-desc-extras desc)))
                    (assq-delete-all name entries))))))
  (setq command-line-args-left nil)
  ;; write to a temporary file first so that clients never see a
  ;; partially written archive-contents
  (with-temp-file (concat output ".tmp")
    (let ((print-length nil)
          (print-level nil))
      (pp (cons 1 entries) (current-buffer))))
  (rename-file (concat output ".tmp") output t))
