((plz-media-type :source #1="elpaca-menu-lock-file" :recipe
                 (:package "plz-media-type" :repo
                           ("https://github.com/r0man/plz-media-type" . "plz-media-type")
                           :files ("*" (:exclude ".git")) :source "GNU ELPA" :protocol
                           https :inherit t :depth treeless :ref
                           "ab69de91ae93ab6650175fdf0c4bb394d698b49a"))
 (plz-event-source :source #1# :recipe
                   (:package "plz-event-source" :repo
                             ("https://github.com/r0man/plz-event-source"
                              . "plz-event-source")
                             :files ("*" (:exclude ".git")) :source "GNU ELPA" :protocol
                             https :inherit t :depth treeless :ref
                             "79e726da2e697f99c8f1196e4badabac6ae3176b"))
 (plz :source #1# :recipe
      (:package "plz" :repo ("https://github.com/alphapapa/plz.el.git" . "plz") :files
                ("*" (:exclude ".git" "LICENSE")) :source "GNU ELPA" :protocol https
                :inherit t :depth treeless :ref "9fb22d84d9e39692392276d7b7b9979ca6ca1c01"))
 (llm :source #1# :recipe
      (:package "llm" :repo ("https://github.com/ahyatt/llm" . "llm") :files
                ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https :inherit t
                :depth treeless :ref "3e209ee7c29017601a8057d09b1f60a941445aa9"))
 (triples :source #1# :recipe
          (:package "triples" :repo ("https://github.com/ahyatt/triples" . "triples")
                    :files ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https
                    :inherit t :depth treeless :ref
                    "2e7cae76ac60227704c9631d27edf409b85e67c7"))
 (markdown-mode :source #1# :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode"
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth treeless :ref
                          "d2d960bec164e91e1a3315b176e2bcc324e63c95"))
 (dired-hacks-utils :source #1# :recipe
                    (:package "dired-hacks-utils" :fetcher github :repo
                              "Fuco1/dired-hacks" :files ("dired-hacks-utils.el") :source
                              "MELPA" :protocol https :inherit t :depth treeless :ref
                              "e9e408e8571aee5574ca0a431ef15cac5a3585d4"))
 (elisp-refs :source #1# :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
                       (:defaults (:exclude "elisp-refs-bench.el")) :source "MELPA"
                       :protocol https :inherit t :depth treeless :ref
                       "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (f :source #1# :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
               "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                         "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (s :source #1# :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
               "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                         "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (dash :source #1# :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "MELPA" :protocol https :inherit t :depth
                 treeless :ref "fcb5d831fc08a43f984242c7509870f30983c27c"))
 (just-ts-mode :source #1# :recipe
               (:package "just-ts-mode" :fetcher github :repo
                         "leon-barrett/just-ts-mode.el" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth treeless :type
                         git :host github :ref "acb598f1edabae9f679a507c8e22c21b3f2da132"))
 (zig-ts-mode :source #1# :recipe
              (:package "zig-ts-mode" :fetcher codeberg :repo "meow_king/zig-ts-mode"
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth treeless :type
                        git :host codeberg :branch "develop" :ref
                        "020500ac3c9ac2cadedccb5cd6c506eb38327443"))
 (typst-ts-mode :source #1# :recipe
                (:source nil :protocol https :inherit t :depth treeless :type git :host
                         codeberg :repo "meow_king/typst-ts-mode" :branch "develop"
                         :package "typst-ts-mode" :ref
                         "34d522c0a0d8eec9a8b3a6855cf394e7d5c8fb84"))
 (cargo :source #1# :recipe
        (:package "cargo" :repo "kwrooijen/cargo.el" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                   "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth treeless :host github
                  :ref "7f8466063381eed05d4e222ce822b1dd44e3bf17"))
 (md-ts-mode :source #1# :recipe
             (:source nil :protocol https :inherit t :depth treeless :host github :repo
                      "eki3z/md" :package "md-ts-mode" :ref
                      "cd1586e7f00a42e4657876f610db14ef869b8a02"))
 (markdown-ts-mode :source #1# :recipe
                   (:package "markdown-ts-mode" :repo "LionyxML/markdown-ts-mode" :fetcher
                             github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit t :depth treeless
                             :ref "2f1ee8b94cdf53cebc31ae08ecfbba846193d5e1"))
 (edit-indirect :source #1# :recipe
                (:package "edit-indirect" :fetcher github :repo "Fanael/edit-indirect"
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth treeless :host
                          github :ref "82a28d8a85277cfe453af464603ea330eae41c05"))
 (kotlin-ts-mode :source #1# :recipe
                 (:package "kotlin-ts-mode" :fetcher gitlab :repo
                           "bricka/emacs-kotlin-ts-mode" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                           :source "MELPA" :protocol https :inherit t :depth treeless
                           :host gitlab :ref "a25d56cecac9160ba7c140f982ec16ca7b2fe97f"))
 (mermaid-ts-mode :source #1# :recipe
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
                            "4f95d4544d5ca2d48ab93c0a133c48d94aed86d3"))
 (nix-ts-mode :source #1# :recipe
              (:package "nix-ts-mode" :fetcher github :repo "nix-community/nix-ts-mode"
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth treeless :type
                        git :host github :ref "62ce3a2dc39529c5db3516427e84b2c96b8efcfd"))
 (c-ts-prototype :source #1# :recipe
                 (:source nil :protocol https :inherit t :depth treeless :type git :host
                          sourcehut :repo "meow_king/c-ts-protoype" :package
                          "c-ts-prototype" :ref "b67911e3cca43cd7a7c51d97a065acfcfcf1aa95"))
 (llvm-mode :source #1# :recipe
            (:source nil :protocol https :inherit t :depth treeless :host github :repo
                     "nverno/llvm-mode" :package "llvm-mode" :ref
                     "9741e5391ea711474474d8d4aacd5abaae2ba501"))
 (twind :source #1# :recipe
        (:source nil :protocol https :inherit t :depth treeless :host github :repo
                 "akirak/twind.el" :package "twind" :ref
                 "da29a5f560438e886525f07059938724c5473c2a"))
 (web-mode :source #1# :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless :ref
                     "be2d59c8fa02b1a45ae54ce4079e502e659cefe6"))
 (colorful-mode :source #1# :recipe
                (:package "colorful-mode" :repo "DevelopmentCool2449/colorful-mode" :files
                          ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https
                          :inherit t :depth treeless :type git :host github :ref
                          "a086428f794d2b261073698bba481bd05f559922"))
 (org-appear :source #1# :recipe
             (:package "org-appear" :fetcher github :repo "awth13/org-appear" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                        "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth treeless
                       :includes org :ref "32ee50f8fdfa449bbc235617549c1bccb503cb09"))
 (git-link :source #1# :recipe
           (:package "git-link" :fetcher github :repo "sshaw/git-link" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless :host
                     github :ref "8d0f98cf36f6b9c31285329b054ae77f9a3d9b33"))
 (diff-hl :source #1# :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                     "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "685e99135001da13caecdff71acea1ee20bed373"))
 (jinx :source #1# :recipe
       (:package "jinx" :repo "minad/jinx" :files ("*.el" "*.h" "*.c") :fetcher github
                 :source "MELPA" :protocol https :inherit t :depth treeless :host github
                 :ref "9c778357ffaac972c86f4acd39d70c547abcaf46"))
 (wgrep :source #1# :recipe
        (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep" :files
                  ("wgrep.el") :source "MELPA" :protocol https :inherit t :depth treeless
                  :host github :ref "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (consult-todo :source #1# :recipe
               (:package "consult-todo" :fetcher github :repo "liuyinz/consult-todo"
                         :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth treeless :type
                         git :host github :ref "b50df0da6243d7a91016e1b8c9a208fb167695be"))
 (consult :source #1# :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                     "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "d557305b730f7666d46bc3eb04c87cfcc493a8e5"))
 (avy :source #1# :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                           "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth treeless :ref
                "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (color-rg :source #1# :recipe
           (:source nil :protocol https :inherit t :depth treeless :host github :repo
                    "manateelazycat/color-rg" :package "color-rg" :ref
                    "41ad8fda5b6579a841889b2395c26cd642a006de"))
 (zoxide :source #1# :recipe
         (:package "zoxide" :fetcher sourcehut :repo "vonfry/zoxide.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                    "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth treeless :ref
                   "0ec359c83b041c8ffe2ec0e70b16c5d218d9bfd9"))
 (substitute :source #1# :recipe
             (:package "substitute" :repo "protesilaos/substitute" :files
                       ("*" (:exclude ".git" "COPYING" "doclicense.texi")) :source
                       "GNU ELPA" :protocol https :inherit t :depth treeless :type git
                       :host sourcehut :ref "f1eb36c9e14df99e7ce8918a0add8893b79108da"))
 (ekg :source #1# :recipe
      (:package "ekg" :fetcher github :repo "ahyatt/ekg" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                           "LICENSE" "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth treeless :ref
                "eb79cc31ec6ee93ef1db97868b6e2d7593321441"))
 (lice :source #1# :recipe
       (:package "lice" :fetcher github :repo "buzztaiki/lice-el" :files
                 ("*.el" "template") :source "MELPA" :protocol https :inherit t :depth
                 treeless :type git :host github :ref
                 "0b69ba54057146f1473e85c0760029e584e3eb13"))
 (marginalia :source #1# :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                        "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth treeless :ref
                       "a527fb03b76a2bce1e360c6e73a095e06922c3f3"))
 (embark-collection :source #1# :recipe
                    (:source nil :protocol https :inherit t :depth treeless :type git
                             :host sourcehut :repo "meow_king/embark-collection" :package
                             "embark-collection" :ref
                             "61106ed49e9cb9e6d5ced5d1bee918702ac84264"))
 (embark-consult :source #1# :recipe
                 (:package "embark-consult" :repo "oantolin/embark" :fetcher github :files
                           ("embark-consult.el") :source "MELPA" :protocol https :inherit
                           t :depth treeless :ref
                           "d5df0eff182b014ab49328a4dbb1d69eb7faafbd"))
 (embark :source #1# :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github :files
                   ("embark.el" "embark-org.el" "embark.texi") :source "MELPA" :protocol
                   https :inherit t :depth treeless :ref
                   "d5df0eff182b014ab49328a4dbb1d69eb7faafbd"))
 (apheleia :source #1# :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia" :files
                     (:defaults ("scripts" "scripts/formatters")) :source "MELPA"
                     :protocol https :inherit t :depth treeless :ref
                     "2c8e8229cbe26c7fd264c2ffe3fbeb9435dad3ae"))
 (dape :source #1# :recipe
       (:package "dape" :repo "svaante/dape" :files ("*" (:exclude ".git")) :source
                 "GNU ELPA" :protocol https :inherit t :depth treeless :type git :host
                 github :ref "4348c957986dbca5c1537b392a4220558ad8be3f"))
 (eldoc-box :source #1# :recipe
            (:package "eldoc-box" :repo "casouri/eldoc-box" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless :ref
                      "ebc0e2c13791f5a22cf81be050b32f0ebf726855"))
 (citre :source #1# :recipe
        (:package "citre" :repo "universal-ctags/citre" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                   "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth treeless :ref
                  "2c8284d3eb2651b0a27e3bbfe6d278b05b95f79d"))
 (lsp-bridge :source #1# :recipe
             (:source nil :protocol https :inherit t :depth treeless :type git :host
                      github :repo "manateelazycat/lsp-bridge" :files
                      (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver"
                                 "resources")
                      :build (:not elpaca--byte-compile) :package "lsp-bridge" :ref
                      "16cf1522b1f981e503aac6e322060fdee1b05e20"))
 (yasnippet :source #1# :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github :files
                      ("yasnippet.el" "snippets") :source "MELPA" :protocol https :inherit
                      t :depth treeless :ref "5b315f1753480ebe669b157b8242448b5eafcfea"))
 (eglot-hierarchy :source #1# :recipe
                  (:source nil :protocol https :inherit t :depth treeless :host github
                           :repo "dolmens/eglot-hierarchy" :package "eglot-hierarchy" :ref
                           "fe7c03139f291ed9ddae289dca4ca905d7dd71ff"))
 (eglot-booster :source #1# :recipe
                (:source nil :protocol https :inherit t :depth treeless :host github :repo
                         "jdtsmith/eglot-booster" :package "eglot-booster" :ref
                         "e6daa6bcaf4aceee29c8a5a949b43eb1b89900ed"))
 (ts-query-highlight :source #1# :recipe
                     (:source nil :protocol https :inherit t :depth treeless :type git
                              :host sourcehut :repo "meow_king/ts-query-highlight"
                              :package "ts-query-highlight" :ref
                              "fea63ef4edd82665bbe962ece2fa98a75730bcf6"))
 (package-lint :source #1# :recipe
               (:package "package-lint" :fetcher github :repo "purcell/package-lint"
                         :files (:defaults "data" (:exclude "*flymake.el")) :source
                         "MELPA" :protocol https :inherit t :depth treeless :ref
                         "6170c1e5b79f9b9f606ab17ab4d9ffb9bce3ebde"))
 (multiple-cursors :source #1# :recipe
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
                             "89f1a8df9b1fc721b1672b4c7b6d3ab451e7e3ef"))
 (expreg :source #1# :recipe
         (:package "expreg" :repo "casouri/expreg" :files ("*" (:exclude ".git")) :source
                   "GNU ELPA" :protocol https :inherit t :depth treeless :type git :host
                   github :ref "9950c07ec90293964baa33603f4a80e764b0a847"))
 (aggressive-indent :source #1# :recipe
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
 (surround :source #1# :recipe
           (:package "surround" :fetcher github :repo "mkleehammer/surround" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless :type git
                     :host github :ref "32a63da201064bb0787fd2e7a79e41878e0223a4"))
 (string-inflection :source #1# :recipe
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
                              "617df25e91351feffe6aff4d9e4724733449d608"))
 (dired-subtree :source #1# :recipe
                (:package "dired-subtree" :fetcher github :repo "Fuco1/dired-hacks" :files
                          ("dired-subtree.el") :source "MELPA" :protocol https :inherit t
                          :depth treeless :ref "e9e408e8571aee5574ca0a431ef15cac5a3585d4"))
 (nerd-icons-dired :source #1# :recipe
                   (:package "nerd-icons-dired" :repo "rainstormstudio/nerd-icons-dired"
                             :fetcher github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit t :depth treeless
                             :ref "c0b0cda2b92f831d0f764a7e8c0c6728d6a27774"))
 (tempel :source #1# :recipe
         (:package "tempel" :repo "minad/tempel" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                    "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth treeless :ref
                   "12a2072ce97d7489296dbb950da946b77d215506"))
 (cape :source #1# :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                            "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "2e86b6deed2844fc1345ff01bc92c3a849a33778"))
 (corfu :source #1# :recipe
        (:package "corfu" :repo "minad/corfu" :files ("*.el" "extensions/*.el") :fetcher
                  github :source "MELPA" :protocol https :inherit t :depth treeless :host
                  github :ref "c302b0526ad9d0c99d1d0639df63f6ca65accfc3"))
 (orderless :source #1# :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless :ref
                      "c7cb04499d94ee1c17affb29b1cfcd2a45116c97"))
 (vertico :source #1# :recipe
          (:package "vertico" :repo "minad/vertico" :files ("*.el" "extensions/*.el")
                    :fetcher github :source "MELPA" :protocol https :inherit t :depth
                    treeless :host github :ref "026a81a9c893b1d73cdbcb12436a0fad3ebdeb5f"))
 (aidermacs :source #1# :recipe
            (:source nil :protocol https :inherit t :depth treeless :host github :repo
                     "MatthewZMD/aidermacs" :package "aidermacs" :ref
                     "96e732d770b9dd5f109aa606a72c1d62ebad6fc5"))
 (breadcrumb :source #1# :recipe
             (:package "breadcrumb" :repo "joaotavora/breadcrumb" :files
                       ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https :inherit
                       t :depth treeless :host github :ref
                       "da34d030e6d01db2bba45b30080204b23a714c9f"))
 (nerd-icons :source #1# :recipe
             (:package "nerd-icons" :repo "rainstormstudio/nerd-icons.el" :fetcher github
                       :files (:defaults "data") :source "MELPA" :protocol https :inherit
                       t :depth treeless :ref "43178575201e3d2ef8c4a507ed4c281b0936f39a"))
 (delight :source #1# :recipe
          (:package "delight" :repo
                    ("https://git.savannah.gnu.org/git/delight.git" . "delight") :files
                    ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https :inherit t
                    :depth treeless :ref "5a0cd5ccb650d7bba1c1ea02cf67b71d7cfa6e9a"))
 (apropospriate-theme :source #1# :recipe
                      (:package "apropospriate-theme" :fetcher github :repo
                                "waymondo/apropospriate-theme" :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                                 "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                                           "*-test.el" "*-tests.el" "LICENSE" "README*"
                                           "*-pkg.el"))
                                :source "MELPA" :protocol https :inherit t :depth treeless
                                :ref "1e8daa3e21e44a2fbc138db6fb38a915b4288479"))
 (casual :source #1# :recipe
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
                   :ref "ea0d69f5fd0aeb3530a7fcbd2e356123f4fdbc59"))
 (meow-tree-sitter :source #1# :recipe
                   (:package "meow-tree-sitter" :fetcher github :repo
                             "skissue/meow-tree-sitter" :files (:defaults "queries")
                             :source "MELPA" :protocol https :inherit t :depth treeless
                             :ref "d8e80d5ab97c85340b21be125f1e17edec2f21dd"))
 (meow :source #1# :recipe
       (:package "meow" :repo "meow-edit/meow" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                            "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "96fecf530d7b29dfb66f327933ebe37f016df132"))
 (eee :source #1# :recipe
      (:source nil :protocol https :inherit t :depth treeless :type git :host github :repo
               "eval-exec/eee.el" :files (:defaults "*.el" "*.sh") :package "eee" :ref
               "f5d425714df59763d8b456c50e426e3ae3a3f0ab"))
 (ultra-scroll :source #1# :recipe
               (:source nil :protocol https :inherit t :depth treeless :host github :repo
                        "jdtsmith/ultra-scroll" :package "ultra-scroll" :ref
                        "b72c507f6702db18d971a6b6bdc692e260f21159"))
 (eat :source #1# :recipe
      (:package "eat" :repo ("https://codeberg.org/akib/emacs-eat" . "eat") :files
                ("*.el" ("term" "term/*.el") "*.texi" "*.ti" ("terminfo/e" "terminfo/e/*")
                 ("terminfo/65" "terminfo/65/*") ("integration" "integration/*")
                 (:exclude ".dir-locals.el" "*-tests.el"))
                :source "NonGNU ELPA" :protocol https :inherit t :depth treeless :url
                "https://codeberg.org/akib/emacs-eat" :ref
                "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99"))
 (vundo :source #1# :recipe
        (:package "vundo" :repo ("https://github.com/casouri/vundo" . "vundo") :files
                  ("*" (:exclude ".git" "test")) :source "GNU ELPA" :protocol https
                  :inherit t :depth treeless :ref
                  "288892ee93214ddcaf65fdd9f3134c1bd10100ee"))
 (hl-todo :source #1# :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                     "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "0ce21c329b686802121df45bf4ae15ae201137bf"))
 (helpful :source #1# :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                     "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "6a4d5e0760ba96a24cd13052233c6edfb34d022c"))
 (ace-window :source #1# :recipe
             (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                        "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth treeless :ref
                       "77115afc1b0b9f633084cf7479c767988106c196"))
 (ibuffer-project :source #1# :recipe
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
 (request :source #1#
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github :files ("request.el")
             :source "MELPA" :protocol https :inherit t :depth treeless :ref
             "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6"))
 (elpaca-use-package :source #1# :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el") :main
                               "extensions/elpaca-use-package.el" :build
                               (:not elpaca--compile-info) :source "Elpaca extensions"
                               :protocol https :inherit t :depth treeless :ref
                               "180a2cd9ba12f970292c929d13c782587114a71b"))
 (elpaca :source
   #1# :recipe
   (:source nil :protocol https :inherit ignore :depth 1 :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "180a2cd9ba12f970292c929d13c782587114a71b" :files
            (:defaults "elpaca-test.el" (:exclude "extensions")) :build
            (:not elpaca--activate-package) :package "elpaca")))
