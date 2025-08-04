;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded before init.el and before any packages are loaded.
;; Minimal warning suppression for deprecation warnings.

;; Suppress deprecation warnings from outdated packages
(setq warning-suppress-types
      '((defadvice obsolete deprecated callf destructuring-bind 
          define-minor-mode case invalid-face)))
(setq warning-suppress-log-types
      '((defadvice obsolete deprecated callf destructuring-bind 
          define-minor-mode case invalid-face)))

;; Set warning level to emergency to suppress most warnings
(setq warning-minimum-level :emergency)

;; Suppress byte-compilation warnings
(setq byte-compile-warnings '(cl-functions))
(setq native-comp-async-report-warnings-errors 'silent) 