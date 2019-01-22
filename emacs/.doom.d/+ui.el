;;;  -*- lexical-binding: t; -*-

(setq doom-theme 'doom-snazzy)

;; Fonts
(setq doom-font (font-spec :family "Iosevka" :size 18))
;; On my 1920x1200 monitor, 28pt Iosevka gives me about 33 lines and maybe 135
;; columns, which seems like a good zoomed in size for eye strain
(setq doom-big-font (font-spec :family "Iosevka" :size 28))
(add-hook! 'window-setup-hook #'doom-big-font-mode)
;; Dash highlighting
(after! dash (dash-enable-font-lock))

(load! "+magit")
