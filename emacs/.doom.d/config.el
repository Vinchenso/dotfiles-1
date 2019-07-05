;;;  -*- lexical-binding: t; -*-
(setq-default evil-shift-width 2 ;; I normally use 2wide for my projects.
              tab-width 2)

(setq user-mail-address "aria@ar1as.space"
      user-full-name "Aria Edm")

(add-hook 'prog-mode-hook #'goto-address-mode) ;; Linkify links!

;; Format elixir on save
(add-hook 'elixir-mode-hook (lambda () (add-hook 'before-save-hook #'elixir-format nil t)))

;; Load snippets
(after! yasnippet
  (push (expand-file-name "snippets/" doom-private-dir) yas-snippet-dirs))

;; (after! auth-source
;;   (let ((auth (plist-get (nth 0 (auth-source-search :host "wakatime.com")) :secret)))
;;     (if (functionp auth)
;;         (setq wakatime-api-key (funcall auth)))))


(def-package! prettier-js
  :commands (prettier-js-mode)
  :init
  (defun setup-prettier-js ()
    "Sets up arguments and the mode."
    (interactive)
    (setq prettier-js-args '("--single-quote"))

    (prettier-js-mode))
  (add-hook! (typescript-mode
              js2-mode)
    #'setup-prettier-js)
  (add-hook! web-mode (enable-minor-mode '("\\.tsx\\'" . setup-prettier-js))))

(after! typescript-mode
  (add-hook 'typescript-mode-hook #'flycheck-mode)
  (setq typescript-indent-level 2))


(after! js2-mode
  ;; use eslintd-fix so when i save it fixes dumb shit
  ;; (add-hook 'js2-mode-hook #'eslintd-fix-mode)

  ;; Indent shit
  (setq js2-basic-offset 2))

(setq js-indent-level 2)

(setq +set-eslint-checker nil)
(after! lsp-ui
  ;; for whatever reason, this was running twice.
  (when (not +set-eslint-checker)
    (progn
      (setq +set-eslint-checker t)
      (flycheck-add-mode 'javascript-eslint 'web-mode)
      (flycheck-add-next-checker 'lsp-ui '(warning . javascript-eslint)))))

(after! emmet-mode
  ;; this is already done in emmet :config but it doesn't seem to get set on my
  ;; machine, so let's do it again
  (map! :map emmet-mode-keymap
        :v [tab] #'emmet-wrap-with-markup
        :i [tab] #'+web/indent-or-yas-or-emmet-expand
        :i "M-E" #'emmet-expand-line))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename matches the regexp.
  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(setq company-idle-delay nil) ;; TODO: remove this line once
                              ;; +company/toggle-auto-completion has fixed its (unless)
(+company/toggle-auto-completion)

(after! web-mode
  (add-hook 'web-mode-hook #'flycheck-mode)

  (setq web-mode-markup-indent-offset 2 ;; Indentation
        web-mode-code-indent-offset 2
        web-mode-enable-auto-quoting nil ;; disbale adding "" after an =
        web-mode-auto-close-style 2))

(after! elm
  (setq elm-tags-on-save t
        elm-sort-imports-on-save t))

(map! :leader
     :prefix "f"
     "n" (lambda! (find-file "~/dotfiles/nixos/home.nix"))
     "N" (lambda! (find-file (concat "~/dotfiles/nixos/" (system-name) ".nix"))))

(setq lsp-haskell-process-wrapper-function
  (lambda (argv)
    (append
      (append (list "nix-shell" "-I" "." "--command")
              (list (mapconcat 'identity argv " ")))

      (list (concat (lsp-haskell--get-root) "/shell.nix")))))

;; Possibly useless
(setq haskell-process-wrapper-function
      (lambda (argv) (append (list "nix-shell" "-I" "." "--command")
                             (list (mapconcat 'identity argv " ")))))


(after! lsp
  ;; These take up a lot of space on my big font size
  (setq lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-signature-render-all nil))

(after! web-mode
  (remove-hook 'web-mode-hook #'+javascript|init-lsp-or-tide-maybe)
  (add-hook 'web-mode-local-vars-hook #'+javascript|init-lsp-or-tide-maybe))

;; damn home-manager making it cabal not show up in --pure!
(after! dante
  ;; I use `nix-impure' instead of nix. I think this is because --pure won't
  ;; include home-manager stuff but that's how i install most of my packages
  (setq dante-methods-alist (delq (assoc 'nix dante-methods-alist) dante-methods-alist))
  (defun +haskell*restore-modified-state (orig-fn &rest args)
    (let ((modified-p (buffer-modified-p)))
      (apply orig-fn args)
      (when modified-p
        (set-buffer-modified-p t))))
  (advice-add #'dante-async-load-current-buffer :around #'+haskell*restore-modified-state)
  (setq ws-butler-global-exempt-modes
        (append ws-butler-global-exempt-modes '(hindent-mode))))

(after! idris-mode
  (setq idris-show-help-text nil))

(after! helm
  ;; I want backspace to go up a level, like ivy
  (add-hook! 'helm-find-files-after-init-hook
    (map! :map helm-find-files-map
          "<DEL>" #'helm-find-files-up-one-level)))

;; Set twitter edit buffer to be 15 lines high so I can actually see what im
;; editing. FIXME this will be fixed upstream, remove me when it is
(after! twittering-mode
  (set-popup-rule! "^\\*twittering-edit"
    '((size . 15))
    '((transient) (quit) (select . t))))

                   ;; Modules
(load! "+ruby")    ;; Custom ruby mode. Make sure you disable ruby in init.el
(load! "+ui")      ;; My ui mods. Also contains ligature stuff.
(load! "+music")   ;; Music stuff, visible through SPC-a-m. Not perfect.
(load! "+ranger")  ;; File manager stuff
(load! "+reason")  ;; ReasonML stuff
(load! "+mail")    ;; Mail stuff
(load! "+org")     ;; Org mode stuff like todos and rebindings
(load! "+irc")     ;; Irc config
