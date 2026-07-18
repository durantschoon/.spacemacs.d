;;; defadvice-patch-advanced.el --- Suppress obsolete defadvice warnings -*- lexical-binding: t -*-

;; Author: Durant Schoon
;; Version: 2.0
;; Keywords: package, defadvice, warnings

;;; Commentary:

;; Silences the `defadvice', `destructuring-bind', and `callf' obsolescence
;; warnings emitted by older packages, by clearing their `byte-obsolete-info'
;; property once startup is complete.
;;
;; History: v1.0 also carried ~370 lines that walked `package-user-dir' and
;; rewrote package .el files on disk to insert these same `put' forms. That
;; machinery never ran -- an unbalanced paren made the file fail to load from
;; the start, so `require' always signalled and the caller swallowed it. When
;; the paren was fixed the code was found to be unsound independently of that:
;;
;;   - it called `package--pending-actions', `package--busy',
;;     `package--download-in-progress' and `comp-async-compilation', none of
;;     which exist in Emacs, so readiness detection signalled void-function
;;   - it hung its logic off `package-post-install-hook' and
;;     `package-post-update-hook', which package.el does not provide
;;   - its "skip critical files" guard called `(return)', a Common Lisp form
;;     unbound in elisp, so the guard crashed instead of returning
;;
;; The approach was also wrong on its own terms: Emacs loads .elc, not .el, so
;; patching source after installation has no effect; the warnings it targeted
;; are emitted at byte-compile time, before any such patch could apply; and
;; rewriting .el files leaves them newer than their .elc, which invites a
;; different warning. Any package reinstall reverted the edits regardless.
;;
;; The `put' forms below achieve the intended result directly.

;;; Code:

(defun defadvice-patch--suppress-warnings ()
  "Suppress obsolete-macro warnings from packages still using old forms."
  (put 'defadvice 'byte-obsolete-info nil)
  (put 'destructuring-bind 'byte-obsolete-info nil)
  (put 'callf 'byte-obsolete-info nil))

;; Deferred to startup completion so it cannot race package initialization
(add-hook 'emacs-startup-hook #'defadvice-patch--suppress-warnings)

(provide 'defadvice-patch-advanced)

;;; defadvice-patch-advanced.el ends here
