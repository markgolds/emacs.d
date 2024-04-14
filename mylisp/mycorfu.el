;; -*- lexical-binding: t; outline-regexp: ";;;" -*-

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator, use C-spc
  ;; (corfu-separator (kbd "SPC"))         ;;   ;; Space as separator
  (corfu-auto-delay  0.1) ;; TOO SMALL - NOT RECOMMENDED
  (corfu-auto-prefix 1) ;; TOO SMALL - NOT RECOMMENDED
  (completion-styles '(orderless-fast basic))
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  
  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  (corfu-indexed-mode)
  ;; (corfu-popupinfo-mode)
  (corfu-history-mode)
  (corfu-echo-mode))

;; SPC as separator  --> So M-SPC for orderless autocomplete
(setq corfu-separator 32)

;; highly recommanded to use corfu-separator with "32" (space)
(define-key corfu-map (kbd "SPC")
	    (lambda ()
	      (interactive)
	      (if current-prefix-arg
		  ;;we suppose that we want leave the word like that, so do a space
		  (progn
		    (corfu-quit)
		    (insert " "))
		(if (and (= (char-before) corfu-separator)
			 (or
			  ;; check if space, return or nothing after
			  (not (char-after))
			  (= (char-after) ?\s)
			  (= (char-after) ?\n)))
		    (progn
		      (corfu-insert)
		      (insert " "))
		  (corfu-insert-separator)))))


(provide 'mycorfu)
