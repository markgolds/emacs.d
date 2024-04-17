;; -*- lexical-binding: t; outline-regexp: ";;;" -*-

(use-package ace-window
  :bind (("M-o" . ace-window)
	 ("C-c o" . ace-window)
	 ("C-c d" . ace-delete-window)
	 ("C-c s" . ace-swap-window)
	 ("C-c w" . aw-show-dispatch-help)
	 ("<f9>" . ace-window))
  :config
  (set-face-attribute
   'aw-leading-char-face nil
   :foreground "deep sky blue"
   :weight 'bold
   :height 2.0)
  (setq
   aw-background nil
   aw-keys '(?w ?p ?a ?o ?e ?l)
   ;; When there are only two windows present,
   ;; "other-window" is called (unless aw-dispatch-always is set non-nil)
   aw-dispatch-always t
   aw-dispatch-alist
   '((?d aw-delete-window "Delete a window")
     (?s aw-swap-window "Swap windows")
     (?r aw-flip-window)			;; Can't add extra string here?
     (?v aw-split-window-horz "Vertical split") ;; I prefer this way
     (?h aw-split-window-vert "Horzontal split")
     (?f aw-split-window-fair "Fair split")
     (?m delete-other-windows "Maximize")
     (?b balance-windows)
     (?t aw-transpose-frame "Transpose")
     (?u winner-undo)
     ;; (?i aw-show-dispatch-help)
     ))
  ;; (setq aw-reverse-frame-list t)
  ;; (setq aw-ignore-current t)
  ;; (setq cursorp-in-non-selected-windows t)
  ;; (setq aw-keys '(?a ?s  ?f ?g ?h ?j ?k ?l))
  ;; (setq aw-dispatch-always t)
  (ace-window-display-mode) ;; Show labels in modelines
  )

(provide 'myacewindow)
