;;; test-easysession.el --- Easysession tests -*- lexical-binding: t -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
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
    (error ("Expected the initial session to be named 'main', but found '%s'"
            (easysession-get-session-name))))

  ;; Switch to the test session
  (easysession-save)
  (easysession-switch-to "test")

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
  (easysession-remove-save-handler 'easysession--handler-save-file-editing-buffers)
  (easysession-remove-save-handler 'easysession--handler-save-indirect-buffers)
  (easysession-remove-load-handler 'easysession--handler-load-file-editing-buffers)
  (easysession-remove-load-handler 'easysession--handler-load-indirect-buffers)

  ;; Validate that handler lists are empty after removal
  (unless (null easysession--load-handlers)
    (error "Load handlers list is not empty after removal"))
  (unless (null easysession--save-handlers)
    (error "Save handlers list is not empty after removal"))

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
    (error "Save handlers were not added correctly")))

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
  (kill-buffer test-easysession--file-buffer1)
  (when (get-file-buffer test-easysession--file-buffer1-path)
    (error "The second buffer is still open"))

  (kill-buffer test-easysession--file-buffer2)
  (when (get-file-buffer test-easysession--file-buffer2-path)
    (error "The second buffer is still open"))

  (kill-buffer test-easysession--dired-buffer)
  (when (buffer-live-p test-easysession--dired-buffer)
    (error "The Dired buffer is still open"))

  (kill-buffer test-easysession--indirect-buffer1)
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
  (unless (equal (easysession--get-all-names) '("main" "test"))
    (error "The easysession--get-all-names failed")))

(defun test-easysession ()
  "Test easysession."
  (interactive)
  (test-easysession--add-hooks)
  (test-easysession--switch-session)
  (test-easysession--add-remove-handlers)
  (test-easysession--create-buffers)
  (test-easysession--save-load)
  (test-easysession--get-all-names)
  (message "Success: test-easysession"))

(provide 'test-easysession)
;;; test-easysession.el ends here
