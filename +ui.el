;;;  -*- lexical-binding: t; -*-

;; title bar translucent
;; no idea if this works, ju#2672 suggested it
(add-to-list 'default-frame-alist '(ns-transparent-tilebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Fonts
;; https://github.com/adobe-fonts/source-code-pro/releases/tag/variable-fonts
(setq doom-font (font-spec :family "Source Code Variable" :size 13))
(setq doom-variable-pitch-font (font-spec :family "Source Code Variable"))
(setq doom-unicode-font (font-spec :family "Source Code Variable"))
(setq doom-big-font (font-spec :family "Source Code Variable" :size 19))

;; Change theme.
;; Nord is pretty cool: https://github.com/arcticicestudio/nord
;; A nice alternative (although brighter) could be: nove - https://trevordmiller.com/projects/nova
(setq dooom-theme 'doom-nord)
