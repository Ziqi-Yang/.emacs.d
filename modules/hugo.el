;;; hugo.el --- hugo -*- lexical-binding: t -*-
;;; Commentary:
;; Make editing hugo blog more convenient
;;; Code:
(defvar mk/hugo-root "~/Documents/blog-meow/" ;; must start with ~
  "Hugo blog root.")

(defvar mk/hugo-content-dir (expand-file-name "content" mk/hugo-root)
  "Hugo blog content directory.")

(defvar mk/hugo-preview-command "firefox --new-window http://127.0.0.1:1313/blog_meow && hugo server -D"
  "Hugo preview command & process-name.")

(defvar mk/hugo-tags-map (make-hash-table :test #'equal)
  "Tag-filepaths hash table for all org blogs.")

(defun mk/hugo/execute-command (command &optional wait force-cd get-result)
  "Execute a command at hugo root directory.
Arguments:
command: command executed at hugo blog root.
wait: whether or not should we wait the command execution.
force-cd:
  t: temporarily change into the blog directory and execute the command
  nil: should be in blog directory.
get-result:
  t: ignore wait and force-cd(functions like all they are true).
     Execute command and return the result.
  nil: nil"
  (interactive "sCommand:")
  (let ((default-directory default-directory))
    (if (or force-cd get-result)
      (setq default-directory mk/hugo-root)
      nil)
    (if (and (project-current) (string= (project-root (project-current)) mk/hugo-root))
      (let ((default-directory mk/hugo-root))
        (if get-result
          (shell-command-to-string command)
          (progn
            (if wait
              (unless (eq (shell-command command "*hugo*" "*hugo-error*") 0)
                (user-error "execute command failed"))
              (start-process-shell-command command "*hugo*" command))
            (message command))))
      (user-error "Please make sure that you are in the blog (sub)directory."))))

(defun mk/hugo/cd-project()
  "Use project-switch-project to change into blog path."
  (interactive)
  (project-switch-project mk/hugo-root))

(defun mk/hugo/preview()
  "Start hugo server to preview your site(with draft)."
  (interactive)
  (mk/hugo/execute-command mk/hugo-preview-command nil t))

(defun mk/hugo/stop-preview()
  "Stop hugo server process."
  (interactive)
  (delete-process mk/hugo-preview-command)
  (message "preview disabled"))

(defun mk/hugo/toggle-preview()
  "Toggle perview (with draft)."
  (interactive)
  (if (get-process mk/hugo-preview-command)
    (mk/hugo/stop-preview)
    (mk/hugo/preview)))

(defun mk/hugo/build()
  "Build without draft."
  (interactive)
  (mk/hugo/execute-command "hugo --minify --gc --cleanDestinationDir"))

(defun mk/hugo/edit-or-create ()
  "Edit or create a blog."
  (interactive)
  (let* ( (direname (completing-read "Section/FileName.<Ext>:" (directory-files mk/hugo-content-dir nil "\\`[^.]*\\'")))
          (filename (concat direname "/" (completing-read "File:"  (directory-files (expand-file-name direname mk/hugo-content-dir) nil "\\`[^.].*\\'"))))
          (file (expand-file-name filename mk/hugo-content-dir)))
    (unless (file-exists-p file) ;; create file if not exists
      (mk/hugo/execute-command (concat "hugo new " filename) t t))
    (find-file file)))

(defun mk/hugo/goto-draft()
  "Goto draft."
  (interactive)
  (let* ((draft (completing-read "draft:"
                  (split-string (mk/hugo/execute-command "hugo list drafts" nil nil t) "\n" t)))
          (draft-file (expand-file-name draft mk/hugo-root)))
    (find-file draft-file)))

(defun mk/hugo/get-all-orgs-files ()
  "Get all blogs absolute path recursively under mk/hugo-content-dir"
  (directory-files-recursively mk/hugo-content-dir "\\`.*\\.org\\'"))

(defun mk/hugo/read-tags-from-org-file (file)
  "Read the tags from the header of an org-mode FILE.
Return value example: ('rust' 'hi')"
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
		(car
		  (org-element-map (org-element-parse-buffer 'greater-element) 'keyword
        (lambda (keyword)
				  (message "%s" keyword)
          (when (string= (org-element-property :key keyword) "TAGS[]")
            (split-string (org-element-property :value keyword) " ")))))))

(defun mk/hugo/establish-tags-map ()
  "Scan all the org files in hugo content directories. Establish tag-file hashmap."
  (interactive)
  (setq mk/hugo-tags-map (make-hash-table :test #'equal)) ;; reinitialize hash map
  (dolist (blog-path (mk/hugo/get-all-orgs-files))
    (dolist (tag (mk/hugo/read-tags-from-org-file blog-path))
      (let ((filepaths (gethash tag mk/hugo-tags-map)))
        (if filepaths
          (progn
            (add-to-list 'filepaths blog-path)
            (puthash tag filepaths mk/hugo-tags-map))
          (puthash tag (list blog-path) mk/hugo-tags-map))))))

(defun mk/hugo/get-all-org-tags()
  "Get all tags from your org blog files under mk/hugo-content-dir."
  (when (eq (hash-table-count mk/hugo-tags-map) 0)
    (mk/hugo/establish-tags-map))
  (hash-table-keys mk/hugo-tags-map))

(defun mk/hugo/find-blog-using-tag-search ()
  "Use tag completion to find blog file."
  (interactive)
  (let* ((path-abbrev-symbol "$content")
          (pre-directory-abbrev-alist directory-abbrev-alist)
          (directory-abbrev-alist (cons `(,(expand-file-name mk/hugo-content-dir) . ,path-abbrev-symbol) directory-abbrev-alist))
          (tags (mk/hugo/get-all-org-tags))
          (tag (completing-read "Tag:" tags))
          (file (completing-read "File:" (mapcar #'abbreviate-file-name (gethash tag mk/hugo-tags-map))))
          (abs-file-path  (string-replace path-abbrev-symbol (expand-file-name mk/hugo-content-dir) file))
          (directory-abbrev-alist pre-directory-abbrev-alist)) ;; find-file use directory-abbrev-alist, so we recover this variable
    (find-file abs-file-path)))

;; TODO drop cape warper of emacs buildin completion framework
(defun mk/hugo/complete-tag-at-point (&optional interactive)
  "Use Cape Backend to complete tags."
  (interactive (list t))
  (if interactive
    (cape-interactive #'mk/hugo/complete-tag-at-point)
    (let ((bounds (cape--bounds 'word)))
      `(,(car bounds) ,(cdr bounds)
         ,(cape--table-with-properties (mk/hugo/get-all-org-tags) :sort nil)
         nil))))

(provide 'hugo)
