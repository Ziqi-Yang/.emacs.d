;;; my-tasks.el --- tasks -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Meow King <mr.meowking@posteo.com>

;;; Commentary:
;;; Code:

(transient-define-argument mk/tasks-infix/rust/package ()
  :class 'transient-option
  :argument "--package=")

(use-package cargo-mode
  :ensure (:host github :repo "ayrat555/cargo-mode"))

(use-package tasks
  :ensure (:host codeberg :repo "meow_king/tasks")
  :after cargo-mode
  :config
  (tasks-transient-define-prefix mk/tasks/rust ()
    "Tasks for Rust language."
    ["Options"
     ("-p" "package" mk/tasks-infix/rust/package)]
    [["check"
      ("c" "check" (concat "cargo check " (tasks-transient-get-arg "--package=")))
      ("C" "clippy" (concat "cargo clippy " (tasks-transient-get-arg "--package=")))
      ("f" "clippy fix" (concat "cargo clippy fix " (tasks-transient-get-arg "--package=")))]
     ["Compile"
      ("bb" "build" (concat "cargo build " (tasks-transient-get-arg "--package=")))
      ("r" "run" (concat "cargo run" (tasks-transient-get-arg "--package=")))]
     ["Test"
      ("ta" "all" (concat "cargo test " (tasks-transient-get-arg "--package=")))
      ("tb" "cur buf" cargo-mode-test-current-buffer)
      ("tc" "cur test" cargo-mode-test-current-test)]
     ["Misc"
      ("be" "benchmark" (concat "cargo bench " (tasks-transient-get-arg "--package=")))
      ("d" "doc" (concat "cargo doc " (tasks-transient-get-arg "--package=")))]])
  
  (tasks-transient-define-prefix mk/tasks/python ()
    "Tasks for Python language."
    [["Misc"
      ("r" "python " (concat "python " relative-file-name))]])

  (tasks-transient-define-prefix mk/tasks/zig ()
    "Tasks for Zig language."
    [["Misc"
      ("b" "build " "zig build")
      ("r" "run" "zig build run")]])
  
  (tasks-transient-define-prefix mk/tasks/kotlin ()
    "Tasks for Kotlin language."
    [["Misc"
      ("b" "build & run"
       (concat "kotlinc " rel-file-path " -include-runtime -d app.jar && kotlin ./app.jar"))]])

  (tasks-transient-define-prefix mk/tasks/c ()
    "Tasks for C language."
    ;; https://nrk.neocities.org/articles/c-static-analyzers
    ;; https://nullprogram.com/blog/2023/04/29/
    [["Misc"
      ("c" "compile"
       (concat
        "cc -g3 -Wall -Wextra -Wconversion -Wdouble-promotion "
        "-Wno-unused-parameter -Wno-unused-function -Wno-sign-conversion "
        "-fsanitize=undefined -fsanitize-trap "
        rel-file-path " -o " bare-rel-file-path))
      ("r" "compile & run"
       (concat
        "cc -g3 -Wall -Wextra -Wconversion -Wdouble-promotion "
        "-Wno-unused-parameter -Wno-unused-function -Wno-sign-conversion "
        "-fsanitize=undefined -fsanitize-trap "
        rel-file-path " -o " bare-rel-file-path " && ./" bare-rel-file-path))
      ]])

  (defun mk/tasks/run ()
    (interactive)
    (let ((file-extension (file-name-extension buffer-file-name)))
      (cond
       ((derived-mode-p '(c-mode c-ts-mode))
        (mk/tasks/c))
       ((derived-mode-p '(python-base-mode))
        (mk/tasks/python))
       ((derived-mode-p '(rust-ts-mode))
        (mk/tasks/rust))
       ((derived-mode-p '(zig-mode zig-ts-mode))
        (mk/tasks/zig))
       ((derived-mode-p '(kotlin-ts-mode))
        (mk/tasks/kotlin))
       ((string-match-p (regexp-opt '("puml" "plantuml")) file-extension)
        (tasks-wrap-compile-command
         '(concat "env PLANTUML_LIMIT_SIZE=327680 plantuml " rel-file-name
                  " && imv " bare-rel-file-name ".png")))
       (t (message "No tasks defined for current condition.")))))

  (defun mk/tasks/project-run ()
    (interactive)
    (let ((project-root-path (project-root (project-current))))
      (cond
       ((file-exists-p (expand-file-name "Cargo.toml" project-root-path))
        (mk/tasks/rust))
       ((file-exists-p (expand-file-name "build.zig" project-root-path))
        (mk/tasks/zig))
       (t (mk/tasks/run)))))

  (setq tasks-function #'mk/tasks/run)
  (setq tasks-project-function #'mk/tasks/project-run))

(provide 'my-tasks)

;;; my-tasks.el ends here
