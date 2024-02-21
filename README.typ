= Meow Emacs
  _Note: the configuration is meant to be used for myself, you'd better only copy/paste the only part you want._

  == Requirement
    + Basic
        ```bash
        sudo pacman -S ripgrep
        # for plantum-mode, zoxide.el, 
        sudo pacman -S plantuml zoxide  
        ```
    
    + language servers: check #link("https://github.com/manateelazycat/lsp-bridge")[lsp-bridge] and `eglot-server-programs` variable \
        Also note that some programs have optional dependencies, which can be viewed by `pacman -Qi`. \
        To use `jdtls`, `java-17` environment is needed, which can be achieved by the following code:
        ```bash
    sudo pacman -S jdk17-openjdk # jdtls needs at least jdk17
    sudo archlinux-java set java-17-openjdk # set jdk17 as default jdk
        ```

    + #link("https://github.com/minad/jinx?tab=readme-ov-file#installation")[jinx], for reference:
        ```bash
    sudo pacman -S enchant hunspell hunspell-en_us pkgconf
        ```

    + dictd
        ```bash
    # note: change the dictionary order in /etc/dict/dictd.conf
    sudo pacman -S dictd
    gfw paru -S dict-gcide dict-moby-thesaurus dict-wn dict-foldoc dict-devils
    sudo systemctl enable --now dictd
        ```

    + #link("https://wakatime.com/emacs")[wakatime]

    + mail
        ```bash
    # msmtp: send mail; offlineimap: imap receive email; mu: index email and provide mu4e package for email
    sudo pacman -S msmtp offlineimap
    paru -S mu
    # (optional) python-pysocks is required for offlineiamp to support proxy, but now I use `dae` to achieve transparent proxy
        ```

    + citre
        ```bash
        sudo pacman -S ctags global # ctags and gtags(needed for finding reference)
        ```

    + apheleia(formatter): check `apheleia-mode-alist` and `apheleia-formatters` variables
    
    
  == Note
    The dictionary file `en_US-large.dic` in `dicts` directory is stolen from `/usr/share/hunspell/en_US-large.dic` with
    ```bash
sed 's/\/.*//' en_US-large.dic > en_US-large_mod.dic
    ```

= Other Awesome Emacs Configurations
  + #link("https://protesilaos.com/emacs/dotemacs")[Protesilaos Stavrou]
  + #link("Likhon-baRoy/.emacs.d")[Likhon-baRoy/.emacs.d]
  + #link("https://github.com/daviwil/emacs-from-scratch")[emacs-from-scratch - daviwil]
  + #link("面向产品经理的Emacs系列教程配套配置文件")[面向产品经理的Emacs系列教程配套配置文件]


