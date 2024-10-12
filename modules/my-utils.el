;;; my-utils.el --- My utility functions  -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Meow King <mr.meowking@anche.no>

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;; Indentation ================================================================
(defun others/indent-buffer ()
  "Automatic format current buffer."
  (interactive)
  (if (derived-mode-p 'python-mode)
      (message "Don't indent python buffer, it will mess up the code syntax.")
    (save-excursion
      (indent-region (point-min) (point-max) nil)
      (delete-trailing-whitespace)
      (untabify (point-min) (point-max)))))

(defun others/indent-comment-buffer ()
  "Indent comment of buffer."
  (interactive)
  (others/indent-comment-region (point-min) (point-max)))

(defun others/indent-comment-region (start end)
  "Indent region.
START END."
  (interactive "r")
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (while (< (point) end)
      (if (comment-search-forward end t)
          (comment-indent)
        (goto-char end)))))

(defun mk/refresh-file (arg)
  "Refresh current file.  Indentation / Save / Load and other stuffs.
ARG: prefix argument."
  (interactive "P")
  (unless (fboundp 'apheleia--get-formatters)
    (require 'apheleia-core))
  (let ((formatter (apheleia--get-formatters)))
    (if formatter
        (apheleia-format-buffer formatter)
      (others/indent-buffer)
      (others/indent-comment-buffer)))
  (save-buffer)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (mk/better-emacs-lisp-byte-compile-and-load))
   (t)))

(defun mk/better-emacs-lisp-byte-compile-and-load ()
  ;; erase byte compile buffer
  (when (get-buffer byte-compile-log-buffer)
    (kill-buffer byte-compile-log-buffer))
  (emacs-lisp-byte-compile-and-load)
  ;; remove byte-compile file
  (delete-file (byte-compile-dest-file buffer-file-name)))

;;; Buffer =====================================================================
(defun mk/smart-buffer-switch-no-hidden (&optional arg)
  "Buffer switch according to project existence without showing hidden buffers.
ARG: prefix argument.  When ARG is non-nil, then force use
`mk/consult-buffer-no-hidden' command instead."
  (interactive "P")
  (if (and (not arg) (project-current))
      (mk/consult-project-buffer-no-hidden)
    (mk/consult-buffer-no-hidden)))

(defun mk/smart-buffer-switch (&optional arg)
  "Smart buffer switch according to project existence.
ARG: prefix argument.  When ARG is non-nil, then force use
`consult-buffer' command instead."
  (interactive "P")
  (if (and (not arg) (project-current))
      (consult-project-buffer)
    (consult-buffer)))


(defun mk/switch-to-compilation-buffer()
  "Switch to compilation buffer"
  (interactive)
  (switch-to-buffer "*compilation*"))

(defun mk/switch-to-eww-buffer ()
  (interactive)
  (switch-to-buffer "*eww*"))

(defun mk/toggle-documentation-buffer-display ()
  (interactive)
  (let ((buf (if lsp-bridge-mode
                 lsp-bridge-buffer-documentation-buffer
               eldoc--doc-buffer)))
    (let ((window (get-buffer-window buf)))
      (if window
          (quit-window nil window)
        (display-buffer buf)))))

(defun mk/better-kill-buffer (&optional arg)
  (interactive "P")
  (if arg
      (call-interactively #'kill-buffer-and-window)
    (call-interactively #'kill-current-buffer)))

;;; File =======================================================================

(defun mk/delete-file ()
  "Delete the current buffer file."
  (interactive)
  (if (not buffer-file-name)
      (message "[Error] This buffer havn't been saved to file.")
    (let ((whether-to-delete (yes-or-no-p "Whether to delete this file?")))
      (if whether-to-delete
          (progn
            (move-file-to-trash buffer-file-name)
            (kill-buffer))
        nil)
      )))

;; NOTE: deprecated since 29.1 because of the builtin function rename-visited-file
(defun mk/rename-file ()
  "Rename the current buffer file."
  (interactive)
  (if (not buffer-file-name)
      (message "[Error] This buffer havn't been saved to file.")
    (let ((new-file-name (read-string "Enter a new name:" (file-name-nondirectory (buffer-file-name))))
          (old-buffer (current-buffer)))
      (rename-file buffer-file-name new-file-name)
      (find-file new-file-name)
      (kill-buffer old-buffer))))

(defun mk/reload-buffer ()
  "Use find-file to reload buffer if the file is changed by other programs."
  (interactive)
  ;; (find-file (buffer-file-name))
  (revert-buffer nil t))

(defun others/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))


(defun mk/find-file-other-window ()
  "Open a window at right for file selected by `find-file'."
  (interactive)
  (split-window-right)
  (other-window 1)
  (call-interactively #'find-file))

(defun mk/smart-find-file ()
  "Context intelligent Find file."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (let ((default-directory (dired-current-directory)))
        (call-interactively #'find-file))
    (call-interactively #'find-file)))

;;; Window =====================================================================
(defun mk/ace-window-balance-window ()
  "Balance window after `ace-window`"
  (interactive)
  (ace-window 0)
  (if (solaire-mode-real-buffer-p)
      (balance-windows)
    (maximize-window)))

(defun mk/split-window-horizontally ()
  "Split window horizontally & Move to spawned window."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun mk/split-window-vertically ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun others/window-split-toggle ()
  "Toggle window layout: vertical <-> horizontal."
  (interactive)
  (if (eq (length (window-list)) 2)
      (let ((func (if (window-full-height-p)
                      #'split-window-vertically
                    #'split-window-horizontally)))
        ;; to make sure the other buffer has been selected once
        (other-window 1)
        (other-window 1)
        (delete-other-windows)
        (funcall func)
        (save-selected-window
          (other-window 1)
          (switch-to-buffer (other-buffer))))
    (error "Can't toggle with more than 2 windows!")))

(defun mk/ace-copy-window ()
  "Ace copy window."
  (interactive)
  (aw-select " Ace - Copy Window"
             #'aw-copy-window))

(defun mk/toggle-follow-mode ()
  (interactive)
  (require 'follow)
  (if follow-mode
      (follow-mode -1)
    (follow-delete-other-windows-and-split)))

;;; Trivial Functions ==========================================================

(defun mk/open-terminal (&optional arg)
  "Open terminal at project root if in a project, otherwise current folder.
ARG: universal argument."
  (interactive "P")
  ;; hyprctl dispatch exec '[workspace 1 slien; float; size 90% 40%; move 5% 58%]  kitty -d 
  (let ((command-prefix "foot -a floating -D "))
    (if (and (project-current) (not arg))
        (start-process-shell-command "open terminal" "*terminal*"
                                     (concat command-prefix (project-root (project-current))))
      (start-process-shell-command
       "open terminal" "*terminal*"
       (concat command-prefix (file-name-directory buffer-file-name))))))

(defun mk/project-compile (&optional confirm)
  "Save & Compile Project.
CONFIRM: universal argument. Whether a confirm is needed."
  (interactive "P")
  (save-buffer)
  (if confirm
      (project-compile)
    (let ((compilation-read-command nil))
      (project-compile))))

(defvar-local mk/search-engines
    '(("direct" . "%s")
      ("github" . "https://github.com/search?q=%s")
      ("google" . "https://www.google.com/search?q=%s")
      ("bing" . "https://www.bing.com/search?q=%s"))
  "Search engines used for function mk/search-online.")

(defun mk/search-online()
  "Search online, using word at point as default."
  (interactive)
  (let* ((word (current-word))
         (word (read-string "Search: " word))
         (engine-names (mapcar #'car mk/search-engines))
         (engine (completing-read "Choose a search engine:" engine-names))
         (search-url (cdr (assoc engine mk/search-engines)))
         (url (format search-url word)))
    (if url
        (progn
          ;; directly call Firefox instead of using browse-url to make parameters("?a=b") can be passed to local url
          ;; (browse-url url)
          (call-process "firefox-developer-edition" nil 0 nil url)
          (call-process "swaymsg" nil 0 nil "workspace" "3")
          (message "open url: %s" url))
      (message "Invalid search engine!"))))

(defun mk/copy-path-smart()
  "Copy current path/project path into clipboard."
  (interactive)
  (if (project-current)
      (mk/lib/copy-string-to-clipboard (project-root (project-current)))
    (mk/lib/copy-string-to-clipboard (file-name-directory buffer-file-name))))

(defun mk/browse-emacs-devel()
  "Use eww to browse the emacs devel thread."
  (interactive)
  (eww (format "https://lists.gnu.org/archive/html/emacs-devel/%s/threads.html"
               (format-time-string "%Y-%0m"))))

(defun mk/elpaca-update()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively #'elpaca-merge)))

(defun mk/elpaca-update-all()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively #'elpaca-merge-all)))

(defun mk/open-emacs.d ()
  "Open emacs.d project."
  (interactive)
  (project-switch-project user-emacs-directory))

(defun mk/hs-hide-level-samrt()
  "Calling hs-hide-level based on line numbers."
  (interactive)
  (when hs-minor-mode
    (let ((n (car (buffer-line-statistics)))
          (l3 500)
          (l2 600)
          (l1 700)
          (l0 1000))
      (cond
       ((> n l0)
        (hs-hide-all)
        ;; (outline-show-only-headings)
        )
       ((> n l1) (hs-hide-all))     ;; also hide long comment
       ((> n l2) (hs-hide-level 1)) ;; show root function
       ((> n l3) (hs-hide-level 2))))))

(defun mk/global-read-only-mode ()
  "`find-file-hook'."
  (interactive)
  (if (memq 'read-only-mode (default-value 'find-file-hook)) ;; diff-hl change the local value
      (progn
        ;; will remove all buffer's read-only-mode including those originally supposed to be
        (dolist (buf (buffer-list))
          (unless (string-match-p (rx (or " " "*") (* anychar)) (buffer-name buf))
            (with-current-buffer buf
              (read-only-mode -1))))
        (remove-hook 'find-file-hook #'read-only-mode))
    
    ;; add read-only-mode to all already existing normal buffers
    (dolist (buf (buffer-list))
      (unless (string-match-p (rx (or " " "*") (* anychar)) (buffer-name buf))
        (with-current-buffer buf
          (read-only-mode 1))))
    (add-hook 'find-file-hook #'read-only-mode)))

(defun mk/ace-ielm ()
  (interactive)
  (ielm)
  (when-let ((ielm-window (selected-window))
             (w (ace-select-window)))
    (select-window ielm-window)
    (ielm-change-working-buffer (window-buffer w))))

(defun mk/share-0x0 ()
  (interactive)
  (when (region-active-p)
    (let* ((mode major-mode)
           (file-name (substring-no-properties (breadcrumb-project-crumbs)))
           (content (buffer-substring-no-properties
                     (region-beginning)
                     (region-end)))
           (tempfile (make-temp-file "emacs-share-0x0-" nil ".txt")))
      (with-temp-file tempfile
        (insert
         "================================================================================\n"
         "Submitter: " user-full-name "\n"
         "Time: " (current-time-string) "\n"
         "In File: " file-name"\n"
         "Lang(Emacs Major Mode): " (symbol-name mode) "\n"
         "================================================================================\n\n")
        (insert content)
        (mk/lib/buffer-remove-left-common-paddings))
      (deactivate-mark)
      (request "https://0x0.st"
        :type "POST"
        :files `(("file" . ,tempfile))
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (when data
                      (let ((res (string-trim-right data)))
                        (mk/lib/copy-string-to-clipboard res)
                        (message "File uploaded: %s" res)))))
        :error (cl-function
                (lambda (&rest args &key error-thrown &allow-other-keys)
                  (message "Error: %S" error-thrown)))
        :complete (lambda (&rest _)
                    (delete-file tempfile))))))

(defun mk/run-just-command (&optional command)
  (interactive
   (list (completing-read
          "Choose a recipe:"
          (seq-map #'(lambda (str)
                       (car (split-string str)))
                   (cdr (process-lines "just" "--list"))))))
  (compile (concat "just " command)))

(defun mk/better-align-regexp (start end)
  (interactive "r")
  (let ((re-adjustable (rx-to-string '(group (* space))))
        (re-identifier (rx-to-string (read--expression "Enter RX expression: "
                                                       "(seq )")))
        (repeat (yes-or-no-p "Repeat through the line? ")))
    (align-regexp
     start end
     (concat re-adjustable re-identifier) 1 0 repeat)))

;; https://emacs.stackexchange.com/a/80549
(defun others/visual-diff-strings (old-string new-string)
  (let ((old-buffer (get-buffer-create (make-temp-name "old-buffer-")))
        (new-buffer (get-buffer-create (make-temp-name "new-buffer-")))
        (output-buffer (get-buffer-create (make-temp-name "*output-diff*"))))
    (with-current-buffer old-buffer
      (insert old-string))
    (with-current-buffer new-buffer
      (insert new-string))
    (diff-no-select old-buffer new-buffer "-u" t output-buffer)
    (kill-buffer old-buffer)
    (kill-buffer new-buffer)
    (display-buffer-pop-up-frame output-buffer '((pop-up-frame-parameters (width . 80))))))

(defun mk/diff-string()
  (interactive)
  (let ((str1 (read-string "String1: "))
        (str2 (read-string "String2: ")))
    (others/visual-diff-strings str1 str2)))

(provide 'my-utils)

;;; my-utils.el ends here

