;;; defadvice-patch-advanced.el --- Advanced defadvice patching with warning suppression -*- lexical-binding: t -*-

;; Author: Durant Schoon
;; Version: 1.0
;; Keywords: package, defadvice, warnings

;; This script automatically patches defadvice warnings in downloaded packages
;; by adding warning suppression to files that contain defadvice calls.
;; It handles conditional defadvice calls intelligently.

;; Suppress obsolete warnings globally - but only after Emacs is fully loaded
(defun defadvice-patch--suppress-warnings ()
  "Suppress obsolete warnings after Emacs is fully loaded."
  (put 'defadvice 'byte-obsolete-info nil)
  (put 'destructuring-bind 'byte-obsolete-info nil)
  (put 'callf 'byte-obsolete-info nil))

;; Only suppress warnings after startup is complete
(add-hook 'emacs-startup-hook #'defadvice-patch--suppress-warnings)

;; Add a more sophisticated approach using multiple hooks
(defun defadvice-patch--setup-smart-hooks ()
  "Set up intelligent hooks that detect when packages are truly ready."
  
  ;; Use after-init-hook for initial startup
  (add-hook 'after-init-hook 
    (lambda ()
      (when (and (not defadvice-patch--processing)
                 (defadvice-patch--packages-fully-loaded-p))
        (defadvice-patch-all-packages))
      ;; Set up file system monitoring for ongoing changes
      (defadvice-patch--monitor-package-changes)))
  
  ;; Use package hooks with smart detection
  (add-hook 'package-post-install-hook #'defadvice-patch--smart-after-package)
  (add-hook 'package-post-update-hook #'defadvice-patch--smart-after-package))

(defun defadvice-patch--packages-fully-loaded-p ()
  "Check if packages are fully loaded using comprehensive checks."
  (and
   ;; Basic package system checks
   (boundp 'package-alist)
   (not (package--pending-actions))
   (not (package--busy))
   
   ;; Check if key packages are loaded
   (featurep 'package)
   (featurep 'cl-lib)
   
   ;; Check if file system is stable
   (defadvice-patch--file-system-stable)
   
   ;; Check if no active downloads or installations
   (not (package--download-in-progress))
   
   ;; Check if package directories exist and are accessible
   (file-directory-p package-user-dir)
   (file-writable-p package-user-dir)))

(defun defadvice-patch--smart-after-package ()
  "Smart package hook that waits for true readiness."
  (unless defadvice-patch--processing
    (defadvice-patch--wait-for-packages-ready)))

;; Set up the smart hooks
(defadvice-patch--setup-smart-hooks)

(defun defadvice-patch--find-elpa-files ()
  "Find all .el files in elpa directories."
  (let ((files '()))
    ;; Check if package-user-dir is already a develop directory
    (if (string-match "develop/?$" package-user-dir)
        ;; package-user-dir is already the develop directory, look for packages directly
        (dolist (dir (directory-files package-user-dir t "^[^.]"))
          (when (file-directory-p dir)
            (dolist (file (directory-files dir t "\\.el$"))
              (push file files))))
      ;; Standard structure: look for elpa/*/develop/
      (dolist (dir (directory-files package-user-dir t "^[^.]"))
        (let ((develop-dir (expand-file-name "develop" dir)))
          (when (file-directory-p develop-dir)
            (dolist (file (directory-files develop-dir t "\\.el$"))
              (push file files))))))

    files))

(defun defadvice-patch--file-contains-defadvice-p (file)
  "Check if FILE contains defadvice calls."
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "\\bdefadvice\\b" nil t))))

(defun defadvice-patch--file-contains-obsolete-calls-p (file)
  "Check if FILE contains any obsolete function calls."
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (goto-char (point-min))
      (or (re-search-forward "\\bdefadvice\\b" nil t)
          (re-search-forward "\\bdestructuring-bind\\b" nil t)
          (re-search-forward "\\bcallf\\b" nil t)))))

(defun defadvice-patch--insert-warning-suppression (file)
  "Insert warning suppression into FILE.
Looks for a good place to insert it, typically after the header comment."
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (goto-char (point-min))
      ;; Look for the end of the header comment
      (if (re-search-forward "^;;;.*\\.el.*---" nil t)
          (progn
            ;; Find the end of the header comment block
            (re-search-forward "^[^;]" nil t)
            (forward-line -1)
            (end-of-line)
            (insert "\n\n;; Suppress obsolete warnings\n(put 'defadvice 'byte-obsolete-info nil)\n(put 'destructuring-bind 'byte-obsolete-info nil)\n(put 'callf 'byte-obsolete-info nil)\n"))
        ;; If no header found, insert at the beginning
        (goto-char (point-min))
        (insert ";; Suppress obsolete warnings\n(put 'defadvice 'byte-obsolete-info nil)\n(put 'destructuring-bind 'byte-obsolete-info nil)\n(put 'callf 'byte-obsolete-info nil)\n\n")))
    (write-region (point-min) (point-max) file)))

(defun defadvice-patch--patch-file (file)
  "Patch a single file to eliminate defadvice warnings."
  ;; Safety checks to prevent modifying critical files
  (when (or (string-match "defadvice-patch" file)
            (string-match "init\\.el$" file)
            (string-match "early-init\\.el$" file)
            (string-match "spacemacs" file))
    (message "⚠️ Skipping critical file: %s" (file-name-nondirectory file))
    (return))
  
  (message "Patching %s..." (file-name-nondirectory file))
  (condition-case err
      (progn
        ;; Only insert warning suppression if it's not already there
        (unless (defadvice-patch--file-contains-warning-suppression-p file)
          (defadvice-patch--insert-warning-suppression file))
        (message "✅ Successfully patched %s" (file-name-nondirectory file)))
    (error
     (message "❌ Failed to patch %s: %s" (file-name-nondirectory file) err))))

(defun defadvice-patch--file-contains-warning-suppression-p (file)
  "Check if FILE already contains the warning suppression."
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward ";; Suppress obsolete warnings" nil t))))

(defun defadvice-patch-all-packages ()
  "Patch all downloaded packages to eliminate obsolete warnings."
  (interactive)
  ;; Prevent recursive calls
  (when defadvice-patch--processing
    (message "⚠️ defadvice-patch already processing, skipping...")
    (return))
  
  (setq defadvice-patch--processing t)
  (unwind-protect
      (let ((files (defadvice-patch--find-elpa-files))
            (count 0)
            (max-files 100)) ; Safety limit to prevent infinite processing
        (message "🔍 Found %d files to check for obsolete warnings" (length files))
        (dolist (file files)
          (when (and (< count max-files)
                     (defadvice-patch--file-contains-obsolete-calls-p file))
            (defadvice-patch--patch-file file)
            (setq count (1+ count))))
        (message "✅ Patched %d files to eliminate obsolete warnings" count))
    (setq defadvice-patch--processing nil)))

(defun defadvice-patch--after-package-install ()
  "Hook function to run after package installation."
  ;; Use a more sophisticated approach to detect when packages are ready
  (defadvice-patch--wait-for-packages-ready))

;; Add hook to run after package operations - but only if not already processing
(defvar defadvice-patch--processing nil
  "Flag to prevent recursive processing.")

(defun defadvice-patch--wait-for-packages-ready ()
  "Wait for packages to be fully loaded using multiple detection methods."
  (let ((max-attempts 20)
        (attempt 0)
        (check-interval 0.5))
    
    (defun defadvice-patch--check-packages-ready ()
      (setq attempt (1+ attempt))
      
      ;; Check multiple conditions to ensure packages are ready
      (let ((packages-ready
             (and
              ;; 1. No pending package operations
              (not (package--pending-actions))
              
              ;; 2. Package system is not busy
              (not (package--busy))
              
              ;; 3. No active package downloads
              (not (package--download-in-progress))
              
              ;; 4. All required packages are available
              (defadvice-patch--all-required-packages-available)
              
              ;; 5. File system is stable (no recent file changes)
              (defadvice-patch--file-system-stable))))
        
        (if packages-ready
            ;; Packages are ready, run the patching
            (progn
              (message "🔍 Packages are ready, starting defadvice patching...")
              (condition-case err
                  (defadvice-patch-all-packages)
                (error (message "⚠️ Error in defadvice patching: %S" err))))
          
          ;; Not ready yet, check again or give up
          (if (< attempt max-attempts)
              (run-with-timer check-interval nil #'defadvice-patch--check-packages-ready)
            (message "⚠️ Timeout waiting for packages to be ready, skipping defadvice patching")))))
    
    ;; Start the checking process
    (run-with-timer check-interval nil #'defadvice-patch--check-packages-ready)))

(defun defadvice-patch--all-required-packages-available ()
  "Check if all required packages are available and loaded."
  (let ((required-packages '(package cl-lib))
        (all-available t))
    (dolist (pkg required-packages)
      (unless (or (featurep pkg) (require pkg nil t))
        (setq all-available nil)))
    all-available))

(defun defadvice-patch--file-system-stable ()
  "Check if the file system is stable (no recent changes to package files)."
  (let ((elpa-dir (expand-file-name "elpa" package-user-dir))
        (stable t)
        (current-time (float-time)))
    (when (file-directory-p elpa-dir)
      ;; Check if any .el files have been modified in the last 3 seconds
      (dolist (file (directory-files elpa-dir t "\\.el$" t))
        (let ((mod-time (file-attribute-modification-time (file-attributes file))))
          (when (< (- current-time mod-time) 3)
            (setq stable nil)))))
    stable))

(defun defadvice-patch--monitor-package-changes ()
  "Monitor package directory for changes and trigger patching when stable."
  (let ((elpa-dir (expand-file-name "elpa" package-user-dir)))
    (when (file-directory-p elpa-dir)
      ;; Use file system monitoring if available
      (if (fboundp 'file-notify-add-watch)
          (progn
            (message "🔍 Setting up file system monitoring for package changes...")
            (file-notify-add-watch elpa-dir 
              '(change) 
              (lambda (event)
                (when (eq (nth 1 event) 'stopped)
                  (run-with-timer 2 nil #'defadvice-patch--check-and-patch-if-ready)))))
        ;; Fallback to periodic checking
        (run-with-timer 5 5 #'defadvice-patch--check-and-patch-if-ready)))))

(defun defadvice-patch--check-and-patch-if-ready ()
  "Check if packages are ready and patch if so."
  (when (and (not defadvice-patch--processing)
             (defadvice-patch--packages-fully-loaded-p))
    (message "🔍 Package changes detected, checking if ready for patching...")
    (defadvice-patch-all-packages)))

(defun defadvice-patch--safe-after-package-install ()
  "Safe wrapper for package install hook."
  (unless defadvice-patch--processing
    (setq defadvice-patch--processing t)
    (defadvice-patch--after-package-install)
    (setq defadvice-patch--processing nil)))

;; Old hooks removed - now using smart hooks above

(defun defadvice-patch--cleanup-old-patches (file)
  "Remove old warning suppression from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (goto-char (point-min))
      ;; Remove old defadvice-only suppression
      (while (re-search-forward ";; Suppress defadvice warnings" nil t)
        (let ((start (match-beginning 0))
              (end (save-excursion
                     (forward-line 1)
                     (point))))
          (delete-region start end)))
      ;; Remove legacy-defadvice macro definitions if any
      (while (re-search-forward "(defmacro legacy-defadvice[^)]*)" nil t)
        (let ((start (match-beginning 0))
              (end (save-excursion
                     (forward-sexp 1)
                     (point))))
          (delete-region start end)
          (delete-blank-lines)))
      ;; Replace legacy-defadvice calls back to defadvice
      (while (re-search-forward "\\blegacy-defadvice\\b" nil t)
        (replace-match "defadvice")))
    (write-region (point-min) (point-max) file)))

;; Interactive command to manually patch all packages
(defun defadvice-patch-manual ()
  "Manually patch all packages to eliminate obsolete warnings."
  (interactive)
  (defadvice-patch-all-packages))

;; Interactive command to cleanup old patches and reapply new approach
(defun defadvice-patch-cleanup-and-reapply ()
  "Clean up old patches and reapply the new warning suppression approach."
  (interactive)
  (let ((files (defadvice-patch--find-elpa-files))
        (count 0))
    (message "Cleaning up old patches...")
    (dolist (file files)
      (when (or (defadvice-patch--file-contains-defadvice-p file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "\\blegacy-defadvice\\b" nil t))))
        (defadvice-patch--cleanup-old-patches file)
        (setq count (1+ count))))
    (message "Cleaned up %d files" count)
    (message "Applying new warning suppression approach...")
    (defadvice-patch-all-packages)))

;; Provide the package
(provide 'defadvice-patch-advanced)
