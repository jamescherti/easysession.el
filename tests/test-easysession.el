;;; test-easysession.el --- Easysession tests -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/easysession.el
;; Keywords: convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Test the easysession package.

;;; Code:

(require 'ert)
(require 'dired)
(require 'easysession)


(defvar test-easysession--before-load-hook-triggered nil
  "Flag indicating whether `easysession-before-load-hook' has been executed.")

(defvar test-easysession--after-load-hook-triggered nil
  "Flag indicating whether `easysession-after-load-hook' has been executed.")

(defvar test-easysession--before-save-hook-triggered nil
  "Flag indicating whether `easysession-before-save-hook' has been executed.")

(defvar test-easysession--after-save-hook-triggered nil
  "Flag indicating whether `easysession-after-save-hook' has been executed.")

(defvar test-easysession--new-session-hook-triggered nil
  "Flag indicating whether `easysession-new-session-hook' has been executed.")

(defvar test-easysession--file-buffer1-path "~/buffer1"
  "Path to the first test file editing buffer.")

(defvar test-easysession--file-buffer2-path "~/buffer2"
  "Path to the second test file editing buffer.")

(defvar test-easysession--dired-buffer-path "~/"
  "Path to the directory for the Dired buffer.")

(defvar test-easysession--dired-buffer nil
  "Reference to the Dired buffer.")

(defvar test-easysession--file-buffer1 nil
  "Reference to the first test buffer.")

(defvar test-easysession--file-buffer2 nil
  "Reference to the second test buffer.")

(defvar test-easysession--indirect-buffer1-name "indirect-buffer1"
  "Name of the indirect test buffer.")

(defvar test-easysession--indirect-buffer1 nil
  "Reference to the indirect test buffer.")

(defun test-easysession--tree-contains-p (tree value)
  "Return non-nil when TREE contains VALUE as a subtree."
  (cond
   ((equal tree value) t)
   ((consp tree)
    (or (test-easysession--tree-contains-p (car tree) value)
        (test-easysession--tree-contains-p (cdr tree) value)))
   ((vectorp tree)
    (seq-some (lambda (item)
                (test-easysession--tree-contains-p item value))
              tree))))

(defun test-easysession--add-hooks ()
  "Add and configure hooks for testing `easysession`.
Tracks the execution of session-related hooks and performs checks
to ensure expected buffer states before and after loading or saving."
  (interactive)
  (when test-easysession--new-session-hook-triggered
    (error (concat "The `easysession-new-session-hook` should not be "
                   "triggered before the switch to another session.")))

  (add-hook 'easysession-new-session-hook
            #'(lambda () (setq test-easysession--new-session-hook-triggered t)))

  (add-hook 'easysession-before-load-hook
            #'(lambda ()
                (when (get-file-buffer test-easysession--file-buffer1-path)
                  (error "Before-load: Buffer 1 should not be open"))
                (setq test-easysession--before-load-hook-triggered t)))

  (add-hook 'easysession-after-load-hook
            #'(lambda ()
                (setq test-easysession--after-load-hook-triggered t)))

  (add-hook 'easysession-before-save-hook
            #'(lambda ()
                (setq test-easysession--before-save-hook-triggered t)))

  (add-hook 'easysession-after-save-hook
            #'(lambda ()
                (setq test-easysession--after-save-hook-triggered t))))

(defun test-easysession--switch-session ()
  "Test the `easysession-switch-to' function.
Test the `easysession-switch-to' function by switching to a test session. Checks
if the `easysession-new-session-hook' is correctly executed and verifies the
session name before and after the switch."
  (interactive)
  ;; Verify the initial session name
  (unless (string= "main" (easysession-get-session-name))
    (error "Expected the initial session to be named 'main', but found '%s'"
           (easysession-get-session-name)))

  (setq easysession--session-loaded t)

  ;; Switch to the test session
  (easysession-save)
  (ignore-errors
    (easysession-delete "test"))
  (let ((easysession-confirm-new-session nil))
    (easysession-switch-to "test"))

  ;; Verify the session name after switching
  (unless (string= "test" (easysession-get-session-name))
    (error "Expected the session to be named 'test', but found '%s'"
           (easysession-get-session-name)))

  ;; Check if the new session hook was executed
  (unless test-easysession--new-session-hook-triggered
    (error (concat "The `easysession-new-session-hook` was not triggered "
                   "after switching sessions."))))

(defun test-easysession--add-remove-handlers ()
  "Test adding and removing easysession save and load handlers.
This function ensures that handlers are correctly removed and re-added, and
validates the handler lists after each operation."
  (interactive)
  ;; Remove existing save and load handlers
  (setq easysession--save-handlers
        '(easysession--handler-save-file-editing-buffers
          easysession--handler-save-indirect-buffers))
  (setq easysession--load-handlers
        '(easysession--handler-load-file-editing-buffers
          easysession--handler-load-indirect-buffers))
  (easysession-remove-save-handler 'easysession--handler-save-file-editing-buffers)
  (easysession-remove-save-handler 'easysession--handler-save-indirect-buffers)
  (easysession-remove-load-handler 'easysession--handler-load-file-editing-buffers)
  (easysession-remove-load-handler 'easysession--handler-load-indirect-buffers)

  ;; Validate that handler lists are empty after removal
  (unless (null easysession--load-handlers)
    (error "Load handlers list is not empty after removal: %s"
           easysession--load-handlers))
  (unless (null easysession--save-handlers)
    (error "Save handlers list is not empty after removal: %s"
           easysession--save-handlers))

  ;; Re-add save and load handlers
  (easysession-add-save-handler 'easysession--handler-save-file-editing-buffers)
  (easysession-add-save-handler 'easysession--handler-save-indirect-buffers)
  (easysession-add-load-handler 'easysession--handler-load-file-editing-buffers)
  (easysession-add-load-handler 'easysession--handler-load-indirect-buffers)

  ;; Validate that handlers were correctly added
  (unless (equal easysession--load-handlers
                 '(easysession--handler-load-file-editing-buffers
                   easysession--handler-load-indirect-buffers))
    (error "Load handlers were not added correctly"))
  (unless (equal easysession--save-handlers
                 '(easysession--handler-save-file-editing-buffers
                   easysession--handler-save-indirect-buffers))
    (error "Save handlers were not added correctly"))

  ;; Transfer them to builtin and empty user handlers
  (setq easysession--builtin-load-handlers
        (copy-sequence easysession--builtin-load-handlers))
  (setq easysession--builtin-save-handlers
        (copy-sequence easysession--builtin-save-handlers))
  (setq easysession--load-handlers nil)
  (setq easysession--save-handlers nil))

(defun test-easysession--create-buffers ()
  "Create and set up test buffers for easysession.
This function creates file buffers, a Dired buffer, and an indirect buffer,
storing them in respective variables for later use."
  (interactive)
  ;; File editing buffers
  (with-temp-buffer
    (insert "hello world")
    (write-file test-easysession--file-buffer1-path))

  (with-temp-buffer
    (insert "hello world2")
    (write-file test-easysession--file-buffer2-path))

  (setq test-easysession--file-buffer1
        (find-file-noselect test-easysession--file-buffer1-path))
  (unless test-easysession--file-buffer1
    (error "Failed to create test-easysession--file-buffer1"))

  (setq test-easysession--file-buffer2
        (find-file-noselect test-easysession--file-buffer2-path))
  (unless test-easysession--file-buffer2
    (error "Failed to create test-easysession--file-buffer2"))

  ;; Dired buffer
  (setq test-easysession--dired-buffer
        (dired-noselect test-easysession--dired-buffer-path))
  (unless test-easysession--dired-buffer
    (error "Failed to create test-easysession--dired-buffer"))

  ;; Indirect buffer
  (with-current-buffer test-easysession--file-buffer1
    (clone-indirect-buffer test-easysession--indirect-buffer1-name nil))
  (setq test-easysession--indirect-buffer1
        (get-buffer test-easysession--indirect-buffer1-name))
  (unless test-easysession--indirect-buffer1
    (error "Failed to create test-easysession--indirect-buffer1")))

(defun test-easysession--save-load ()
  "Test persisting and restoring: file editing buffers and indirect-buffer."
  (interactive)
  (unless (get-file-buffer test-easysession--file-buffer1-path)
    (error "Before-save: Buffer 1 should be open"))

  ;; Save session and kill buffers
  (easysession-save)
  (unless test-easysession--after-save-hook-triggered
    (error "The easysession-after-save-hook was not triggered"))
  (unless test-easysession--before-save-hook-triggered
    (error "The easysession-before-save-hook was not triggered"))
  (when test-easysession--file-buffer1
    (kill-buffer test-easysession--file-buffer1))
  (when (get-file-buffer test-easysession--file-buffer1-path)
    (error "The second buffer is still open"))

  (when test-easysession--file-buffer2
    (kill-buffer test-easysession--file-buffer2))
  (when (get-file-buffer test-easysession--file-buffer2-path)
    (error "The second buffer is still open"))

  (when test-easysession--dired-buffer
    (kill-buffer test-easysession--dired-buffer))
  (when (buffer-live-p test-easysession--dired-buffer)
    (error "The Dired buffer is still open"))

  (when test-easysession--indirect-buffer1
    (kill-buffer test-easysession--indirect-buffer1))
  (when (get-buffer test-easysession--indirect-buffer1-name)
    (error "The indirect buffer is still open"))

  ;; Load session
  (easysession-load)
  (unless test-easysession--after-load-hook-triggered
    (error "The easysession-after-load-hook was not triggered"))
  (unless test-easysession--before-load-hook-triggered
    (error "The easysession-before-load-hook was not triggered"))

  (setq test-easysession--file-buffer1
        (get-file-buffer test-easysession--file-buffer1-path))
  (when (not test-easysession--file-buffer1)
    (error "Failed to first buffer"))

  (setq test-easysession--file-buffer2
        (get-file-buffer test-easysession--file-buffer2-path))
  (when (not test-easysession--file-buffer2)
    (error "Failed to the second buffer"))

  (setq test-easysession--indirect-buffer1
        (get-buffer test-easysession--indirect-buffer1-name))
  (when (not test-easysession--indirect-buffer1)
    (error
     "Failed to restore the indirect buffer"))

  (setq test-easysession--dired-buffer
        (dired-noselect test-easysession--dired-buffer-path))
  (when (not test-easysession--dired-buffer)
    (error
     "Failed to restore the Dired buffer"))
  (with-current-buffer test-easysession--dired-buffer
    (unless (string= (expand-file-name test-easysession--dired-buffer-path)
                     (expand-file-name default-directory))
      (error
       "The Dired buffer points to the wrong path"))))

(defun test-easysession--get-all-names ()
  "Test: `easysession--get-all-names'."
  (interactive)
  (unless (equal (sort (easysession--get-all-names) #'string<)
                 (sort (list "main" "test") #'string<))
    (error "The easysession--get-all-names failed")))

(defun test-easysession-save-mode-predicate ()
  "Test save-mode predicate."
  (interactive)
  (defun my-easysession-nothing-saved ()
    "Nothing is saved."
    nil)
  (setq easysession-save-mode-predicate 'my-easysession-nothing-saved)
  (easysession--auto-save)

  (when test-easysession--file-buffer1
    (kill-buffer test-easysession--file-buffer1))
  (setq easysession-before-load-hook nil)
  (easysession-load)
  (setq test-easysession--file-buffer1
        (get-buffer test-easysession--file-buffer1-path))
  (when test-easysession--file-buffer1
    (error (concat "easysession--auto-save or the "
                   "easysession-save-mode-predicate do not seem to "
                   "be working"))))

(ert-deftest test-easysession-load-coalesces-buffer-list-update-hook ()
  "Default buffer list observers see only the final restored frameset."
  (let* ((temporary-directory (make-temp-file "easysession-test-" t))
         (easysession-directory (file-name-as-directory temporary-directory))
         (easysession--builtin-load-handlers nil)
         (easysession--load-handlers nil)
         (easysession--current-session-name nil)
         (easysession--session-loaded nil)
         (easysession-enable-frameset-restore t)
         (easysession-fontify nil)
         (before-load-hook-called nil)
         (after-load-hook-called nil)
         (restore-running nil)
         (restore-finished nil)
         (hook-calls-during-restore 0)
         (hook-calls-after-restore 0)
         (hook-calls-outside-load 0)
         (local-hook-calls 0)
         stable-hook-buffer
         (first-buffer (generate-new-buffer " *easysession-first*"))
         (second-buffer (generate-new-buffer " *easysession-second*"))
         (easysession-before-load-hook
          (list (lambda () (setq before-load-hook-called t))))
         (easysession-after-load-hook
          (list (lambda () (setq after-load-hook-called t))))
         (buffer-list-update-hook
          (list (lambda ()
                  (cond
                   (restore-running
                    (setq hook-calls-during-restore
                          (1+ hook-calls-during-restore)))
                   (restore-finished
                    (if easysession-load-in-progress
                        (setq hook-calls-after-restore
                              (1+ hook-calls-after-restore)
                              stable-hook-buffer (current-buffer))
                      (setq hook-calls-outside-load
                            (1+ hook-calls-outside-load))))
                   (t
                    (setq hook-calls-outside-load
                          (1+ hook-calls-outside-load))))))))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer first-buffer)
          (let ((source-window (selected-window))
                (restored-window (split-window-right)))
            (set-window-buffer restored-window second-buffer)
            (dolist (buffer (list first-buffer second-buffer))
              (with-current-buffer buffer
                (setq-local
                 buffer-list-update-hook
                 (list (lambda ()
                         (setq local-hook-calls (1+ local-hook-calls)))
                       t))))
            (with-temp-file
                (expand-file-name "hook-test" easysession-directory)
              (prin1 '(("frameset" . test-frameset)) (current-buffer)))
            (cl-letf (((symbol-function 'frameset-restore)
                       (lambda (&rest _)
                         (setq restore-running t)
                         (select-window restored-window)
                         (select-window source-window)
                         (select-window restored-window)
                         (setq restore-running nil
                               restore-finished t))))
              (easysession-load "hook-test")))

          (should before-load-hook-called)
          (should after-load-hook-called)
          (should (= hook-calls-during-restore 0))
          (should (= hook-calls-after-restore 1))
          ;; Buffer-local hook values retain their normal semantics, including
          ;; their `t' element that continues with the default hook value.
          (should (= local-hook-calls 3))
          (should (eq stable-hook-buffer second-buffer))
          (let ((outside-calls hook-calls-outside-load))
            (run-hooks 'buffer-list-update-hook)
            (should (= hook-calls-outside-load (1+ outside-calls)))
            (should (= local-hook-calls 4)))

          ;; Do not invent a notification when frameset restoration made no
          ;; buffer-list change.
          (setq restore-finished nil
                hook-calls-after-restore 0
                stable-hook-buffer nil)
          (cl-letf (((symbol-function 'frameset-restore)
                     (lambda (&rest _)
                       (setq restore-finished t))))
            (easysession-load "hook-test"))
          (should (= hook-calls-after-restore 0))
          (should (= local-hook-calls 4)))
      (when (buffer-live-p first-buffer)
        (kill-buffer first-buffer))
      (when (buffer-live-p second-buffer)
        (kill-buffer second-buffer))
      (delete-directory temporary-directory t))))

(ert-deftest test-easysession-load-restores-hook-after-frameset-error ()
  "A failed frameset restore replays and restores the default hook safely."
  (let* ((temporary-directory (make-temp-file "easysession-test-" t))
         (easysession-directory (file-name-as-directory temporary-directory))
         (easysession--builtin-load-handlers nil)
         (easysession--load-handlers nil)
         (easysession--current-session-name nil)
         (easysession--session-loaded nil)
         (easysession-enable-frameset-restore t)
         (easysession-fontify nil)
         (easysession-before-load-hook nil)
         (easysession-after-load-hook nil)
         (restore-running nil)
         (hook-calls-during-restore 0)
         (hook-calls-after-error 0)
         (hook-calls-outside-load 0)
         (observer-error-enabled t)
         (first-buffer (generate-new-buffer " *easysession-error-first*"))
         (second-buffer (generate-new-buffer " *easysession-error-second*"))
         (buffer-list-update-hook
          (list (lambda ()
                  (if restore-running
                      (setq hook-calls-during-restore
                            (1+ hook-calls-during-restore))
                    (if easysession-load-in-progress
                        (setq hook-calls-after-error
                              (1+ hook-calls-after-error))
                      (setq hook-calls-outside-load
                            (1+ hook-calls-outside-load)))))
                (lambda ()
                  (when (and observer-error-enabled
                             easysession-load-in-progress)
                    (error "synthetic observer failure"))))))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer first-buffer)
          (let ((restored-window (split-window-right)))
            (set-window-buffer restored-window second-buffer)
            (with-temp-file
                (expand-file-name "hook-error-test" easysession-directory)
              (prin1 '(("frameset" . test-frameset)) (current-buffer)))
            (cl-letf (((symbol-function 'frameset-restore)
                       (lambda (&rest _)
                         (setq restore-running t)
                         (select-window restored-window)
                         (setq restore-running nil)
                         (error "synthetic frameset failure"))))
              (let ((error-data
                     (should-error
                      (easysession-load "hook-error-test")
                      :type 'error)))
                (should
                 (string-match-p "synthetic frameset failure"
                                 (error-message-string error-data)))
                (should-not
                 (string-match-p "synthetic observer failure"
                                 (error-message-string error-data))))))

          (should (= hook-calls-during-restore 0))
          (should (= hook-calls-after-error 1))
          (should-not easysession-load-in-progress)
          (setq observer-error-enabled nil)
          (let ((outside-calls hook-calls-outside-load))
            (run-hooks 'buffer-list-update-hook)
            (should (= hook-calls-outside-load (1+ outside-calls)))))
      (setq observer-error-enabled nil)
      (when (buffer-live-p first-buffer)
        (kill-buffer first-buffer))
      (when (buffer-live-p second-buffer)
        (kill-buffer second-buffer))
      (delete-directory temporary-directory t))))

(ert-deftest test-easysession-load-keeps-side-window-parameters-persistent ()
  "A restored side window remains serializable by the next session save."
  (let* ((temporary-directory (make-temp-file "easysession-test-" t))
         (easysession-directory (file-name-as-directory temporary-directory))
         (easysession--builtin-load-handlers nil)
         (easysession--load-handlers nil)
         (easysession--current-session-name nil)
         (easysession--session-loaded nil)
         (easysession-before-load-hook nil)
         (easysession-after-load-hook nil)
         (easysession-enable-frameset-restore t)
         (easysession-fontify nil)
         (window-persistent-parameters
          (assq-delete-all
           'window-side
           (assq-delete-all 'window-slot
                            (copy-tree window-persistent-parameters))))
         frameset-restored)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "side-window-test"
                                            easysession-directory)
            (prin1 '(("frameset" . test-frameset)) (current-buffer)))
          (cl-letf (((symbol-function 'frameset-restore)
                     (lambda (&rest _)
                       (setq frameset-restored t))))
            (easysession-load "side-window-test"))

          (should frameset-restored)
          (let ((window (selected-window)))
            (unwind-protect
                (progn
                  (set-window-parameter window 'window-side 'right)
                  (set-window-parameter window 'window-slot 0)
                  (let ((next-frameset
                         (easysession--save-frameset "next-session")))
                    (should
                     (test-easysession--tree-contains-p
                      next-frameset '(window-side . right)))
                    (should
                     (test-easysession--tree-contains-p
                      next-frameset '(window-slot . 0)))))
              (set-window-parameter window 'window-side nil)
              (set-window-parameter window 'window-slot nil))))
      (delete-directory temporary-directory t))))

(defun test-easysession--init ()
  "Init test."
  (easysession--set-current-session "main")

  ;; Init
  (test-easysession--add-hooks)
  (test-easysession--switch-session)
  (test-easysession--add-remove-handlers)
  (test-easysession--create-buffers))

(ert-deftest test-easysession ()
  "Test EasySession."
  (test-easysession--init)
  (test-easysession--save-load)
  (test-easysession--get-all-names)
  (test-easysession-save-mode-predicate))

(provide 'test-easysession)

;; Local variables:
;; byte-compile-warnings: (not lexical)
;; End:

;;; test-easysession.el ends here
