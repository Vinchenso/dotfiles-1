;;;  -*- lexical-binding: t; -*-

(setq doom-theme 'doom-snazzy)

;; Fonts
(setq doom-font (font-spec :family "Source Code Pro" :size 18))
(setq doom-big-font (font-spec :family "Source Code Pro" :size 40))
;; (add-hook! 'window-setup-hook #'doom-big-font-mode)
;; Dash highlighting
(after! dash (dash-enable-font-lock))

(load! "+magit")
