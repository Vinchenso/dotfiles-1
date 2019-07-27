;;;  -*- lexical-binding: t; -*-

(setq doom-theme 'doom-snazzy)

;; Fonts
;; 1920x1080, half screen at size 22 gives 80 columns 35 lines
(setq doom-font (font-spec :family "Source Code Pro" :size 22))
;; On my 1920x1080, full screen gives about 90 columns 19 lines
(setq doom-big-font (font-spec :family "Source Code Pro" :size 40))
(add-hook! 'window-setup-hook #'doom-big-font-mode)
;; Dash highlighting
(after! dash (dash-enable-font-lock))

(load! "+magit")
