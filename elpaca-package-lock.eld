((ace-window :source "elpaca-menu-lock-file" :recipe
             (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                        "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth treeless :ref
                       "77115afc1b0b9f633084cf7479c767988106c196"))
 (aggressive-indent :source "elpaca-menu-lock-file" :recipe
                    (:package "aggressive-indent" :repo "Malabarba/aggressive-indent-mode"
                              :fetcher github :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit t :depth treeless
                              :ref "a437a45868f94b77362c6b913c5ee8e67b273c42"))
 (apheleia :source "elpaca-menu-lock-file" :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia" :files
                     (:defaults ("scripts" "scripts/formatters")) :source "MELPA"
                     :protocol https :inherit t :depth treeless :ref
                     "426616cf1799dbce89800ae2fe9f155311436503"))
 (async :source "elpaca-menu-lock-file" :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                   "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth treeless :ref
                  "31cb2fea8f4bc7a593acd76187a89075d8075500"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                           "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth treeless :ref
                "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (breadcrumb :source "elpaca-menu-lock-file" :recipe
             (:package "breadcrumb" :repo "joaotavora/breadcrumb" :files
                       ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https :inherit
                       t :depth treeless :host github :ref
                       "da34d030e6d01db2bba45b30080204b23a714c9f"))
 (c-ts-prototype :source "elpaca-menu-lock-file" :recipe
                 (:source nil :protocol https :inherit t :depth treeless :type git :host
                          sourcehut :repo "meow_king/c-ts-protoype" :package
                          "c-ts-prototype" :ref "b67911e3cca43cd7a7c51d97a065acfcfcf1aa95"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                            "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "c675d8c142fc3265e3123e5e492b48e748846577"))
 (cargo-mode :source "elpaca-menu-lock-file" :recipe
             (:package "cargo-mode" :fetcher github :repo "ayrat555/cargo-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                        "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth treeless :host
                       github :ref "b1fb87c17fcd22d798bb04115e65ecf83e8c929a"))
 (casual :source "elpaca-menu-lock-file" :recipe
         (:package "casual" :fetcher github :repo "kickingvegas/casual" :old-names
                   (casual-agenda casual-bookmarks casual-calc casual-dired casual-editkit
                                  casual-ibuffer casual-info casual-isearch
                                  cc-isearch-menu casual-lib casual-re-builder)
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                    "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth treeless :host github
                   :ref "f38fb5e5d850d96d57559526bff973f41cf73940"))
 (citre :source "elpaca-menu-lock-file" :recipe
        (:package "citre" :repo "universal-ctags/citre" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                   "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth treeless :ref
                  "300596a34b61b486530ce3759e8fb722d9534325"))
 (color-rg :source "elpaca-menu-lock-file" :recipe
           (:source nil :protocol https :inherit t :depth treeless :host github :repo
                    "manateelazycat/color-rg" :package "color-rg" :ref
                    "41ad8fda5b6579a841889b2395c26cd642a006de"))
 (colorful-mode :source "elpaca-menu-lock-file" :recipe
                (:package "colorful-mode" :repo "DevelopmentCool2449/colorful-mode" :files
                          ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https
                          :inherit t :depth treeless :type git :host github :ref
                          "524240feb0cd4c385bab2e6f3345ef8ad52c112d"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                     "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "9bb753c6fdf82c4e66f002a9399d4bd9657ee51a"))
 (consult-todo :source "elpaca-menu-lock-file" :recipe
               (:package "consult-todo" :fetcher github :repo "liuyinz/consult-todo"
                         :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth treeless :type
                         git :host github :ref "f9ba063a6714cb95ddbd886786ada93771f3c140"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files ("*.el" "extensions/*.el") :fetcher
                  github :source "MELPA" :protocol https :inherit t :depth treeless :host
                  github :ref "0198f34f29a598c05c4b95dc3dfb8391f572d7d4"))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
           (:package "csv-mode" :repo
                     ("https://github.com/emacsmirror/gnu_elpa" . "csv-mode") :branch
                     "externals/csv-mode" :files ("*" (:exclude ".git")) :source
                     "GNU ELPA" :protocol https :inherit t :depth treeless :ref
                     "ba5dc934b9dbdc2b57ab1917a669cdfd7d1838d3"))
 (dape :source "elpaca-menu-lock-file" :recipe
       (:package "dape" :repo "svaante/dape" :files ("*" (:exclude ".git")) :source
                 "GNU ELPA" :protocol https :inherit t :depth treeless :type git :host
                 github :ref "35f56abdd050932550ff1c3aca436d7c056aa464"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "MELPA" :protocol https :inherit t :depth
                 treeless :ref "fb443e7a6e660ba849cafcd01021d9aac3ac6764"))
 (delight :source "elpaca-menu-lock-file" :recipe
          (:package "delight" :repo
                    ("https://git.savannah.gnu.org/git/delight.git" . "delight") :files
                    ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https :inherit t
                    :depth treeless :ref "5a0cd5ccb650d7bba1c1ea02cf67b71d7cfa6e9a"))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                     "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "e79aa49ad3cbbe85379cf6646db3aaacd3b04708"))
 (dired-hacks-utils :source "elpaca-menu-lock-file" :recipe
                    (:package "dired-hacks-utils" :fetcher github :repo
                              "Fuco1/dired-hacks" :files ("dired-hacks-utils.el") :source
                              "MELPA" :protocol https :inherit t :depth treeless :ref
                              "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (dired-subtree :source "elpaca-menu-lock-file" :recipe
                (:package "dired-subtree" :fetcher github :repo "Fuco1/dired-hacks" :files
                          ("dired-subtree.el") :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (dockerfile-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "dockerfile-mode" :fetcher github :repo
                            "spotify/dockerfile-mode" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                             "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                             "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth treeless
                            :host github :ref "97733ce074b1252c1270fd5e8a53d178b66668ed"))
 (eat :source "elpaca-menu-lock-file" :recipe
      (:package "eat" :repo ("https://codeberg.org/akib/emacs-eat" . "eat") :files
                ("*.el" ("term" "term/*.el") "*.texi" "*.ti" ("terminfo/e" "terminfo/e/*")
                 ("terminfo/65" "terminfo/65/*") ("integration" "integration/*")
                 (:exclude ".dir-locals.el" "*-tests.el"))
                :source "NonGNU ELPA" :protocol https :inherit t :depth treeless :url
                "https://codeberg.org/akib/emacs-eat" :ref
                "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99"))
 (edit-indirect :source "elpaca-menu-lock-file" :recipe
                (:package "edit-indirect" :fetcher github :repo "Fanael/edit-indirect"
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth treeless :host
                          github :ref "82a28d8a85277cfe453af464603ea330eae41c05"))
 (eee :source "elpaca-menu-lock-file" :recipe
      (:source nil :protocol https :inherit t :depth treeless :type git :host github :repo
               "eval-exec/eee.el" :files (:defaults "*.el" "*.sh") :package "eee" :ref
               "a2b74abd56cf7d60a03bea188504099e20d84e1f"))
 (eldoc-box :source "elpaca-menu-lock-file" :recipe
            (:package "eldoc-box" :repo "casouri/eldoc-box" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless :ref
                      "595262ec8ff56e8f86ef77d8e69339e84117e5f0"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
                       (:defaults (:exclude "elisp-refs-bench.el")) :source "MELPA"
                       :protocol https :inherit t :depth treeless :ref
                       "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :protocol https :inherit ignore :depth 1 :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "b5ef5f19ac1224853234c9acdac0ec9ea1c440a1" :files
            (:defaults "elpaca-test.el" (:exclude "extensions")) :build
            (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el") :main
                               "extensions/elpaca-use-package.el" :build
                               (:not elpaca--compile-info) :source "Elpaca extensions"
                               :protocol https :inherit t :depth treeless :ref
                               "b5ef5f19ac1224853234c9acdac0ec9ea1c440a1"))
 (embark :source "elpaca-menu-lock-file" :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github :files
                   ("embark.el" "embark-org.el" "embark.texi") :source "MELPA" :protocol
                   https :inherit t :depth treeless :ref
                   "7b3b2fa239c34c2e304eab4367a4f5924c047e2b"))
 (embark-collection :source "elpaca-menu-lock-file" :recipe
                    (:source nil :protocol https :inherit t :depth treeless :type git
                             :host sourcehut :repo "meow_king/embark-collection" :package
                             "embark-collection" :ref
                             "61106ed49e9cb9e6d5ced5d1bee918702ac84264"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
                 (:package "embark-consult" :repo "oantolin/embark" :fetcher github :files
                           ("embark-consult.el") :source "MELPA" :protocol https :inherit
                           t :depth treeless :ref
                           "7b3b2fa239c34c2e304eab4367a4f5924c047e2b"))
 (expreg :source "elpaca-menu-lock-file" :recipe
         (:package "expreg" :repo "casouri/expreg" :files ("*" (:exclude ".git")) :source
                   "GNU ELPA" :protocol https :inherit t :depth treeless :type git :host
                   github :ref "53fd5a4812b77e49d3ab45fc16bb2998abaf1d86"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
               "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                         "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (git-link :source "elpaca-menu-lock-file" :recipe
           (:package "git-link" :fetcher github :repo "sshaw/git-link" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless :host
                     github :ref "12caebc0982d3401a0b74ccddc2d5a651122de8a"))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                     "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                     "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "1856d5010dbd44f18afbb3a86ac18be59066202b"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                          "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth treeless :ref
               "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (htmljinja2-ts-mode :source "elpaca-menu-lock-file" :recipe
                     (:source nil :protocol https :inherit t :depth treeless :host
                              codeberg :repo "meow_king/htmljinja2-ts-mode" :package
                              "htmljinja2-ts-mode" :ref
                              "2178250e32a932db1783d17016b02e4c7abdc551"))
 (ibuffer-project :source "elpaca-menu-lock-file" :recipe
                  (:package "ibuffer-project" :fetcher github :repo
                            "muffinmad/emacs-ibuffer-project" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                             "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                             "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth treeless
                            :host github :ref "9002abd9cb4c8753fe4f6c522d9407b4d52e7873"))
 (janet-ts-mode :source "elpaca-menu-lock-file" :recipe
                (:source nil :protocol https :inherit t :depth treeless :host github :repo
                         "sogaiu/janet-ts-mode" :package "janet-ts-mode" :ref
                         "0e4d04d6487486104c3d65576e7a1480ce52b3fd"))
 (just-ts-mode :source "elpaca-menu-lock-file" :recipe
               (:package "just-ts-mode" :fetcher github :repo
                         "leon-barrett/just-ts-mode.el" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth treeless :type
                         git :host github :ref "9dd136bc809de85fa66a4665312eb0f55b1c8094"))
 (kotlin-ts-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "kotlin-ts-mode" :fetcher gitlab :repo
                           "bricka/emacs-kotlin-ts-mode" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                           :source "MELPA" :protocol https :inherit t :depth treeless
                           :host gitlab :ref "051c9ef534956c235343fb41546623ff87a1695b"))
 (lice :source "elpaca-menu-lock-file" :recipe
       (:package "lice" :fetcher github :repo "buzztaiki/lice-el" :files
                 ("*.el" "template") :source "MELPA" :protocol https :inherit t :depth
                 treeless :type git :host github :ref
                 "0b69ba54057146f1473e85c0760029e584e3eb13"))
 (llvm-mode :source "elpaca-menu-lock-file" :recipe
            (:source nil :protocol https :inherit t :depth treeless :host github :repo
                     "nverno/llvm-mode" :package "llvm-mode" :ref
                     "9741e5391ea711474474d8d4aacd5abaae2ba501"))
 (lsp-bridge :source "elpaca-menu-lock-file" :recipe
             (:source nil :protocol https :inherit t :depth treeless :type git :host
                      github :repo "manateelazycat/lsp-bridge" :files
                      (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver"
                                 "resources")
                      :build (:not elpaca--byte-compile) :package "lsp-bridge" :ref
                      "0623720a4a5617e50c7f3b0ccb88da1fba57bbbe"))
 (lsp-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github :files
                     (:defaults "clients/*.*") :source "MELPA" :protocol https :inherit t
                     :depth treeless :ref "c6e3660d32813b02ba3de60045de734e680bbcc7"))
 (lspx :source "elpaca-menu-lock-file" :recipe
       (:source nil :protocol https :inherit t :depth treeless :host codeberg :repo
                "meow_king/lspx" :package "lspx" :ref
                "ff2e16a0904c601c94d6e157e9b0f3d0dd3a7be5"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files ("lv.el") :source "MELPA"
               :protocol https :inherit t :depth treeless :ref
               "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                        "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth treeless :ref
                       "61bf01985aea982db351002951882438fcada08b"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode"
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth treeless :ref
                          "92802fae9ebbc8c2e4c281c06dcdbd74b8bca80e"))
 (md-ts-mode :source "elpaca-menu-lock-file" :recipe
             (:source nil :protocol https :inherit t :depth treeless :host github :repo
                      "eki3z/md" :package "md-ts-mode" :ref
                      "134d80ff9cbc5ebe66e6d76604a823bf588545f3"))
 (meow :source "elpaca-menu-lock-file" :recipe
       (:package "meow" :repo "meow-edit/meow" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                            "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "2246f593552c6208bac533de9af04b085fafa6fa"))
 (meow-tree-sitter :source "elpaca-menu-lock-file" :recipe
                   (:package "meow-tree-sitter" :fetcher github :repo
                             "skissue/meow-tree-sitter" :files (:defaults "queries")
                             :source "MELPA" :protocol https :inherit t :depth treeless
                             :ref "f050dc0867b7a37efc2c6432b33305a0f14b1029"))
 (mermaid-ts-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "mermaid-ts-mode" :fetcher github :repo
                            "JonathanHope/mermaid-ts-mode" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                             "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                             "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth treeless
                            :type git :host github :ref
                            "973e442cbed980cf51afc256c90ef133c4d02141"))
 (multiple-cursors :source "elpaca-menu-lock-file" :recipe
                   (:package "multiple-cursors" :fetcher github :repo
                             "magnars/multiple-cursors.el" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit t :depth treeless
                             :type git :host github :ref
                             "9017f3be6b00c1d82e33409db4a178133fb39d47"))
 (neocaml :source "elpaca-menu-lock-file" :recipe
          (:source nil :protocol https :inherit t :depth treeless :host github :repo
                   "bbatsov/neocaml" :package "neocaml" :ref
                   "930e4aa9ad50977eefe296f35d0c5c1e6e112398"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
             (:package "nerd-icons" :repo "rainstormstudio/nerd-icons.el" :fetcher github
                       :files (:defaults "data") :source "MELPA" :protocol https :inherit
                       t :depth treeless :ref "081f6f4f99b9460b63b5a2a6087b62fefd06a5d0"))
 (nerd-icons-dired :source "elpaca-menu-lock-file" :recipe
                   (:package "nerd-icons-dired" :repo "rainstormstudio/nerd-icons-dired"
                             :fetcher github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit t :depth treeless
                             :ref "3265d6c4b552eae457d50d423adb10494113d70b"))
 (nix-ts-mode :source "elpaca-menu-lock-file" :recipe
              (:package "nix-ts-mode" :fetcher github :repo "nix-community/nix-ts-mode"
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth treeless :type
                        git :host github :ref "706bbfaf7f344d7c5914bf7aa5beb2d904eb53b3"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless :ref
                      "26a384894678a1e51e3bf914af3699a61794fb57"))
 (org-appear :source "elpaca-menu-lock-file" :recipe
             (:package "org-appear" :fetcher github :repo "awth13/org-appear" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                        "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth treeless
                       :includes org :ref "32ee50f8fdfa449bbc235617549c1bccb503cb09"))
 (package-lint :source "elpaca-menu-lock-file" :recipe
               (:package "package-lint" :fetcher github :repo "purcell/package-lint"
                         :files (:defaults "data" (:exclude "*flymake.el")) :source
                         "MELPA" :protocol https :inherit t :depth treeless :ref
                         "1c37329703a507fa357302cf6fc29d4f2fe631a8"))
 (request :source "elpaca-menu-lock-file"
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github :files ("request.el")
             :source "MELPA" :protocol https :inherit t :depth treeless :ref
             "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
               "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                         "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo
                    ("https://github.com/Malabarba/spinner.el" . "spinner") :files
                    ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https :inherit t
                    :depth treeless :ref "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (string-inflection :source "elpaca-menu-lock-file" :recipe
                    (:package "string-inflection" :fetcher github :repo
                              "akicho8/string-inflection" :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit t :depth treeless
                              :type git :host github :ref
                              "02ab7b2ea8530c63c20556c1afb795924e08dfca"))
 (substitute :source "elpaca-menu-lock-file" :recipe
             (:package "substitute" :repo "protesilaos/substitute" :files
                       ("*" (:exclude ".git" "COPYING" "doclicense.texi")) :source
                       "GNU ELPA" :protocol https :inherit t :depth treeless :type git
                       :host sourcehut :ref "f1eb36c9e14df99e7ce8918a0add8893b79108da"))
 (surround :source "elpaca-menu-lock-file" :recipe
           (:package "surround" :fetcher github :repo "mkleehammer/surround" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless :type git
                     :host github :ref "6807bf69be1591a419a009adf8a5071b1cdcc76e"))
 (svelte-ts-mode :source "elpaca-menu-lock-file" :recipe
                 (:source nil :protocol https :inherit t :depth treeless :host github
                          :repo "leafOfTree/svelte-ts-mode" :branch "emacs-master"
                          :package "svelte-ts-mode" :ref
                          "5f4addd7fffc9595fdc0798f0aa18a4a5415c7d0"))
 (tasks :source "elpaca-menu-lock-file" :recipe
        (:source nil :protocol https :inherit t :depth treeless :host codeberg :repo
                 "meow_king/tasks" :package "tasks" :ref
                 "2f0fe5d63a1f3246541c9e94ab877480168a367a"))
 (tempel :source "elpaca-menu-lock-file" :recipe
         (:package "tempel" :repo "minad/tempel" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                    "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth treeless :ref
                   "506a8d570c145f4df63a18921e34bac6bef3ebe0"))
 (ts-query-highlight :source "elpaca-menu-lock-file" :recipe
                     (:source nil :protocol https :inherit t :depth treeless :type git
                              :host sourcehut :repo "meow_king/ts-query-highlight"
                              :package "ts-query-highlight" :ref
                              "fea63ef4edd82665bbe962ece2fa98a75730bcf6"))
 (typst-ts-mode :source "elpaca-menu-lock-file" :recipe
                (:package "typst-ts-mode" :repo "meow_king/typst-ts-mode" :files
                          ("*" (:exclude ".git")) :source "NonGNU ELPA" :protocol https
                          :inherit t :depth treeless :type git :host codeberg :branch
                          "develop" :ref "7c2ef0d5bd2b5a8727fe6d00938c47ba562e0c94"))
 (ultra-scroll :source "elpaca-menu-lock-file" :recipe
               (:source nil :protocol https :inherit t :depth treeless :host github :repo
                        "jdtsmith/ultra-scroll" :package "ultra-scroll" :ref
                        "21c568b1a26e597714ad65b40f246dd6e9f71fdd"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files ("*.el" "extensions/*.el")
                    :fetcher github :source "MELPA" :protocol https :inherit t :depth
                    treeless :host github :ref "9a48b8cb98d36011d8b00eb3ff3fe027d8852e37"))
 (vundo :source "elpaca-menu-lock-file" :recipe
        (:package "vundo" :repo ("https://github.com/casouri/vundo" . "vundo") :files
                  ("*" (:exclude ".git" "test")) :source "GNU ELPA" :protocol https
                  :inherit t :depth treeless :ref
                  "e0af8c5845abf884a644215a9cac37f39c13cd5a"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
        (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep" :files
                  ("wgrep.el") :source "MELPA" :protocol https :inherit t :depth treeless
                  :host github :ref "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github :files
                      ("yasnippet.el" "snippets") :source "MELPA" :protocol https :inherit
                      t :depth treeless :ref "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (zig-ts-mode :source "elpaca-menu-lock-file" :recipe
              (:package "zig-ts-mode" :fetcher codeberg :repo "meow_king/zig-ts-mode"
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth treeless :host
                        codeberg :branch "develop" :ref
                        "327353895240953026ec36ae6f17dd6563730e54"))
 (zoxide :source "elpaca-menu-lock-file" :recipe
         (:package "zoxide" :fetcher sourcehut :repo "vonfry/zoxide.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                    "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth treeless :ref
                   "0ec359c83b041c8ffe2ec0e70b16c5d218d9bfd9")))
