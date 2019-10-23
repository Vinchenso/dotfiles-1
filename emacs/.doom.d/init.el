;;;  -*- lexical-binding: t; -*-

(doom! :feature
                                        ;;debugger
       ;; +docsets)
       ;; syntax-checker ;;+childframe)

       :emacs
       dired
       electric
       vc

       :term
       term

       :completion
       (company +childframe)

                                        ;;the ultimate code completion backend
       ;; (helm +fuzzy)                    ;;+childframe)
                                        ;;ido
       (ivy
        +childframe
        ;; +fuzzy
        +prescient)


       :ui
       doom
       doom-dashboard
       modeline
       doom-quit
       ophints
       hl-todo
       workspaces
       nav-flash
       indent-guides
                                        ;;tabbar
       vc-gutter

       vi-tilde-fringe
       window-select
       (popup
        +all
        +defaults)
                                        ;;neotree
       treemacs
       (pretty-code +fira)

       :tools
       eval
       (lookup
        +devdocs)
       lsp
       gist
                                        ;;macos
       make
       magit                            ;;
       pass
       pdf
       prodigy
                                        ;;rgb
       tmux
       upload
       editorconfig
       (flycheck +childframe)
       direnv
       ;; wakatime

       :lang
       assembly
       cc
       crystal
       clojure
       csharp
       data
       elixir
       elm
       emacs-lisp
       ;ess
       go
       (haskell +dante)
       hy
                                        ;;(java +meghanada)
       (javascript +lsp)
       julia
       latex
       ledger
       lua
       markdown
       idris
       nix
       ocaml
       (org
        +attach
        +babel
        +capture
        +export
        +present
        +publish)
       perl
                                        ;;php
       plantuml
       purescript
       python
       rest
                                        ;;ruby
       rust
       scala
       sh
       swift
       web

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
                                        ;;(email +gmail)
       irc
                                        ;;(rss +org)
       twitter
       (write
        +wordnut
        +langtool)

       :editor
       (evil +everywhere)
       file-templates
       snippets
       ;; lispyville
       parinfer
       rotate-text
       fold
       multiple-cursors
       (format +onsave)

       :completion
       ;; (lsp
       ;;  ;; +javascript
       ;;  +go
       ;;  +css
       ;;  ;+rust
       ;;  +cpp
       ;;  +ocaml
       ;;  +java
       ;;  +python
       ;;  +sh)

       :term

       eshell

       :config
       ;; The default module set reasonable defaults for Emacs. It also provides
       ;; a Spacemacs-inspired keybinding scheme, a custom yasnippet library,
       ;; and additional ex commands for evil-mode. Use it as a reference for
       ;; your own modules.
       (default +bindings +snippets +evil-commands +smartparens))
