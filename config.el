;;;  -*- lexical-binding: t; -*-

;; Bind TAB to emmet expand because what monster uses M-e?

;; Author: hlissner
(defun emmet-or-snippet-expand ()
  (interactive)
  (call-interactively
   (cond ((and (bound-and-true-p yas-minor-mode)
               (yas--templates-for-key-at-point))
          #'yas-expand)
         ((emmet-expr-on-line)
          (if (bound-and-true-p yas-minor-mode)
              #'emmet-expand-yas
            #'emmet-expand-line))
         (#'indent-for-tab-command))))

(map! :after rjsx-mode
      :map rjsx-mode-map
      :i "TAB" #'emmet-or-snippet-expand)
(map! :after web-mode
      :map web-mode-map
      :i "TAB" #'emmet-or-snippet-expand)

;; Readd in rjsx's autotag shit
(map! :after rjsx-mode
      :map rjsx-mode-map
      "<" #'rjsx-electric-lt
      "C-d" #'rjsx-delete-creates-full-tag)


;; TODO removed me one hlissner does it
(after! web-mode
  (remove-hook 'web-mode-hook #'turn-off-smartparens-mode)
  )

;; Load snippets
(after! yasnippet
  (push (expand-file-name "snippets/" doom-private-dir) yas-snippet-dirs))

(load! +ruby)
(load! +lsp)
(load! +ui)

