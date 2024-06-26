;; -*- lexical-binding: t; outline-regexp: ";;;" -*-



;;;****************************** GENERAL STUFF ******************************
;; (setq warning-minimum-level :error)

;; no start-up screen
(setq inhibit-startup-message t)

;; Turn beep off
(setq visible-bell nil)

(setq gc-cons-threshold (* 128 1024 1024))
(setq read-process-output-max (* 1 1024 1024))
(setq process-adaptive-read-buffering nil)

;; if higher you'll notice a delay in syntax highlighting when scrolling
(setq jit-lock-defer-time 0)  

;; y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

					; turon off externals
;; Remove scrollbars, menu bars, and toolbars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(set-fringe-mode '(12 . 0))  ; extra space on left, none on right

;; Remembering recently edited files, recentf-open
(recentf-mode 1)

;; Remembering minibuffer prompt history
(setq history-length 25)
(savehist-mode 1)

;; where to store back-up files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq custom-file (make-temp-file "emacs-custom"))

;; use shift to move around windows
(windmove-default-keybindings 'shift)

(setq show-paren-delay 0) ;; this needs to come first
(show-paren-mode t)

(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-idle-delay 0)

;; ls -alFh when opening dired
(setq dired-listing-switches "-alFh")
(setq dired-kill-when-opening-new-dired-buffer t)  ;; don't accumulate buffers

(winner-mode 1)
;; Setup straight --------------------------------------------------

(setq straight-repository-branch "develop")

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package git)
;; End straight.el


;;;****************************** ORG STUFF  ******************************

(when (file-exists-p "~/.emacs.d/mylisp/myorg.el")
  (load "~/.emacs.d/mylisp/myorg.el"))

(define-key global-map (kbd "C-c l") #'org-store-link)
(define-key global-map (kbd "C-c a") #'org-agenda)
(define-key global-map (kbd "C-c c") #'org-capture)
(define-key global-map (kbd "C-c b") #'org-switchb)

;; (setq org-src-preserve-indentation nil)

;; (setq org-src-tab-acts-natively t)

;;;****************************** MAIN PACKAGES ******************************;;

;; ediff
(setq ediff-keep-variants nil)
(setq ediff-make-buffers-readonly-at-startup nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-show-clashes-only t)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package aggressive-indent)

(use-package markdown-mode)

(use-package company-statistics)
(add-hook 'after-init-hook 'company-statistics-mode)

(use-package diminish)

(use-package magit
  :bind (("C-x g" . 'magit-status)))

(use-package rainbow-delimiters
  :diminish
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package yasnippet
  :diminish)

(use-package yasnippet-snippets
  ;; :diminish yas-global-mode
  :diminish yas-minor-mode
  :init (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"                 ;; personal snippets
	  )))

;; god-mode
(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'bar 'box)))

(use-package god-mode
  :bind ("<escape>" . 'god-mode-all)
  :config
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  (custom-set-faces '(god-mode-lighter ((t (:inherit error)))))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type))
;; end god-mode

(use-package drag-stuff
  :diminish
  :bind (("M-<up>" . 'drag-stuff-up)
	 ("M-<down>" . 'drag-stuff-down))
  :config (drag-stuff-global-mode 1)
  )

;; ------------------------------IVY------------------------------

;; (use-package counsel
;;   :diminish
;;   :config
;;   (counsel-mode 1))

;; (use-package ivy-prescient
;;   :config
;;   ;; Uncomment the following line to have sorting remembered across sessions!
;;   (prescient-persist-mode 1)
;;   )

;; (use-package company-prescient
;;   :config
;;   ;; Uncomment the following line to have sorting remembered across sessions!
;;   (prescient-persist-mode 1)
;;   )

;; ;; swiper is an ivy enhanced version of isearch.
;; (use-package swiper
;;   :bind (("M-s" . counsel-grep-or-swiper)))

;; (use-package ivy-hydra)

;; (use-package ivy
;;   ;; :bind (("TAB" . ivy-alt-done))
;;   :diminish
;;   :config
;;   (ivy-mode 1)
;;   (ivy-prescient-mode 1)
;;   (setq ivy-initial-inputs-alist nil)  ;; removes ‘^’ in things like counsel-M-x and other ivy/counsel prompts.
;;   )

;; ;; The following line 
;; ;; The default ‘^’ string means that if you type something immediately after this string only completion candidates that begin with what you typed are shown.  Most of the time, I’m searching for a command without knowing what it begins with though.

;; (use-package ivy-rich
;;   :after ivy
;;   :custom
;;   (ivy-virtual-abbreviate 'full
;;    ivy-rich-switch-buffer-align-virtual-buffer t
;;    ivy-rich-path-style 'abbrev)
;;   :config
;;   (ivy-set-display-transformer 'ivy-switch-buffer
;;                                'ivy-rich-switch-buffer-transformer)
;;   (ivy-rich-mode 1)) ;; this gets us descriptions in M-x.
;; ------------------------------ENDIVY

;; Enable vertico
(use-package vertico
  :straight (vertico
	     :files (:defaults "extensions/*")
             :includes (vertico-indexed vertico-multiform vertico-directory vertico-quick))
  :init
  (vertico-mode)
  (vertico-indexed-mode)
  (vertico-multiform-mode)
  ;; ~/ will delete prev dir:
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)  
  :bind
  (:map vertico-map
	;; ("<tab>" #'vertico-insert) ; Set manually otherwise setting `vertico-quick-insert' overrides this
	;; ("<escape>" #'minibuffer-keyboard-quit)
	("?" . #'minibuffer-completion-help)
	("C-M-n" . #'vertico-next-group)
	("C-M-p" . #'vertico-previous-group)
	;; Multiform toggles
	("<backspace>" . #'vertico-directory-delete-char)
	("C-w" . #'vertico-directory-delete-word)
	("C-<backspace>" . #'vertico-directory-delete-word)
	("RET" . #'vertico-directory-enter)
	;; ("C-i" #'vertico-quick-insert)
	;; ("C-o" #'vertico-quick-exit)
	;; ("M-o" #'kb/vertico-quick-embark)
	("M-G" . #'vertico-multiform-grid)
	("M-F" . #'vertico-multiform-flat)
	("M-R" . #'vertico-multiform-reverse)
	("M-U" . #'vertico-multiform-unobtrusive)
	("C-l" . #'kb/vertico-multiform-flat-toggle)
	("M-q" . #'vertico-quick-insert)
	("C-q" . #'vertico-quick-insert)
	)
  :config 
  ;; Different scroll margin
  (setq vertico-scroll-margin 0)
  
  ;; Show more candidates
  (setq vertico-count 5)
  
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  
  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)
  
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; ------------------------------ MARGINALIA ------------------------------
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

;;------------------------------CONSULT------------------------------
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; for consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; for consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  :config
  (setq consult-grep-args
	'("grep"
	  ;; this is default, but makes it hard to add options in searches
	  ;; (consult--grep-exclude-args)  
	  "--null --line-buffered --color=never --ignore-case --with-filename --line-number -I -r")))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; keep embarking after killing buffer, otherwise default behaviour
  (setq embark-quit-after-action '((kill-buffer . nil) (t . t)))
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; Embark to choose which window to open buffer/file in
  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(eval-when-compile
  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
	 (require 'ace-window)
	 (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

(when (file-exists-p "~/.emacs.d/mylisp/mycorfu.el")
  (load "~/.emacs.d/mylisp/mycorfu.el"))


;; ;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 2)
       (cons 'orderless-literal-prefix word)))

(orderless-define-completion-style orderless-fast
  (orderless-style-dispatchers '(orderless-fast-dispatch))
  (orderless-matching-styles '(orderless-literal orderless-regexp)))


(use-package dired-open)

(use-package peep-dired
  :config
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4", "gz", "zip"))
  )

;; (defun my-dired-preview-to-the-right ()
;;   "My preferred `dired-preview-display-action-alist-function'."
;;   '((display-buffer-in-side-window)
;;     (side . right)
;;     (width . 0.5)
;;     ))
;; (use-package dired-preview
;;   :config
;;   ;; (setq dired-preview-display-action-alist-function #'my-dired-preview-to-the-right)
;;   (setq dired-preview-delay 0.1)
;;   (setq dired-preview-ignored-extensions-regexp
;; 	"\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a\\|flac\\|wav\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip\\|iso\\|epub\\)")
;;   )

;; icons----------------------------------------------
(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :diminish
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-corfu)

(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

;; icons----------------------------------------------

(use-package transpose-frame
  :bind ("C-S-t" . transpose-frame))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; csv viewing
(use-package csv-mode)
(use-package color)
(defun csv-highlight (&optional separator)
  (interactive (list (when current-prefix-arg (read-char "Separator: "))))
  (font-lock-mode 1)
  (let* ((separator (or separator ?\,))
         (n (count-matches (string separator) (pos-bol) (pos-eol)))
         (colors (cl-loop for i from 0 to 1.0 by (/ 2.0 n)
                          collect (apply #'color-rgb-to-hex 
                                         (color-hsl-to-rgb i 0.3 0.5)))))
    (cl-loop for i from 2 to n by 2 
             for c in colors
             for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
             do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))
(add-hook 'csv-mode-hook 'csv-highlight)
(add-hook 'csv-mode-hook 'csv-align-mode)
(add-hook 'csv-mode-hook '(lambda () (interactive) (toggle-truncate-lines nil)))

(use-package vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000)
  (setq-default vterm-buffer-name-string "vterm: %s")
  )

(when (file-exists-p "~/.emacs.d/mylisp/mymodeline.el")
  (load "~/.emacs.d/mylisp/mymodeline.el"))

(when (file-exists-p "~/.emacs.d/mylisp/myacewindow.el")
  (load "~/.emacs.d/mylisp/myacewindow.el"))


(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           ;; :right-divider-width 1
           :right-divider-width 20
           :scroll-bar-width 0
           :left-fringe-width 20
           :right-fringe-width 20))
  (spacious-padding-mode 1))

(use-package elfeed
  :config
  (setq ;;elfeed-search-feed-face ":foreground #ffffff :weight bold"
   elfeed-feeds (quote
		 (("https://hnrss.org/frontpage" hackernews)
		  ("https://www.reddit.com/r/emacs.rss" emacs reddit)
		  ("https://www.reddit.com/r/montreal.rss" montreal reddit)
		  ))))

;; When several buffers visit identically-named files
(require 'uniquify) (setq uniquify-buffer-name-style 'forward)

;; highlight lines
(use-package hl-line
  :ensure t
  :hook ((after-init . global-hl-line-mode)
         (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))))

;; Remeber last location in buffers
(use-package saveplace
  :init (save-place-mode))

;;;****************************** PYTHON ******************************

;; (use-package pyvenv
;;   :ensure t
;;   :config
;;   (pyvenv-mode 1)
;;   (pyvenv-activate "~/miniconda3/envs/qdgp/")
;;   )

(use-package conda
  :config
  (conda-env-activate "qdgp")
  )

;; (use-package ein
;;   :init
;;   (setq ein:worksheet-enable-undo t)
;;   (setq ein:output-area-inlined-images t)
;;   ;; :bind (:map ein:notebook-mode-map
;;   ;; 	      ("C-S-k" . ein:worksheet-delete-cell)
;;   ;; 	      ("C-S-a" . ein:worksheet-insert-cell-above)
;;   ;; 	      ("C-S-b" . ein:worksheet-insert-cell-below)
;;   ;; 	      )
;;   )

;; (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

;; This works well but doesn't provide autocomplete and other stuff:
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(python-mode . ("ruff-lsp"))))

(use-package flymake-ruff
  :straight (flymake-ruff :type git :host nil :repo "https://github.com/erickgnavar/flymake-ruff")
  :hook ((eglot-managed-mode . flymake-ruff-load))
  )

(use-package ruff-format
  :hook (eglot-managed-mode . ruff-format-on-save-mode))

;; This was originally use to not make double erros from Pyright/ruff
;; But it turns out that Pyright had it's on problem where it was sending double messages:
;; flymake-show-buffer-diagnostics:
;; 1  16 error    e-f-b    Pyright [reportUnusedImport]: Import "np" is not accessed
;; 1  16 note     e-f-b    Pyright: "np" is not accessed
(defun my-fix-eglot-diagnostics (diags)
  "Drop Pyright 'variable not accessed' notes from DIAGS."
  (list (seq-remove (lambda (d)
                      (and
		       (eq (flymake-diagnostic-type d) 'eglot-note)
                       (s-starts-with? "Pyright:" (flymake-diagnostic-text d))
                       ;; (s-ends-with? "is not accessed" (flymake-diagnostic-text d))
		       ))
                    (car diags))))
(advice-add 'eglot--report-to-flymake :filter-args #'my-fix-eglot-diagnostics)

(use-package eglot
  :preface
  :bind (:map eglot-mode-map
              ;; ("C-c C-d" . eldoc)
              ("C-c C-e" . eglot-rename)
              ("C-c C-o" . python-sort-imports)
              ("C-c C-f" . eglot-format-buffer))
  :hook ((python-ts-mode . eglot-ensure)
         (python-ts-mode . flyspell-prog-mode)
         ;; (python-ts-mode . superword-mode)
         (python-ts-mode . hs-minor-mode)
         ;; (python-ts-mode . (lambda () (set-fill-column 88)))
	 ;; (python-ts-mode . (lambda () (diminish 'hs-minor-mode)))
	 ;; (eglot-managed-mode . mp-eglot-eldoc)
	 )
  :config
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))
  :custom
  (fset #'jsonrpc-log--event #'ignore)
  (eglot-events-buffer-size 0)
  ;; (eglot-sync-connect nil)
  ;; (eglot-connect-timeout nil)
  (eglot-autoshutdown t)
  ;; :config
  ;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  ;; (add-to-list 'eglot-server-programs '(python-mode . ("ruff-lsp" "-stdio" "--linters" "pyflakes,mccabe,pycodestyle,pydocstyle,bandit,black,isort")))
  )

;; Had to download eglot-lsp-booster from here first:
;; https://github.com/blahgeek/emacs-lsp-booster?tab=readme-ov-file#obtain-or-build-emacs-lsp-booster
;; Then "mv emacs-lsp-booster ~/.local/bin/"
(use-package eglot-booster
  :ensure t
  :straight (:type git :host github :repo "joaotavora/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))

(use-package isortify
  :hook (python-ts-mode . isortify-mode))

;; (use-package company
;;   :ensure t
;;   :hook ((python-ts-mode . company-mode))
;;   :diminish
;;   :config
;;   (setq company-prescient-mode 1)
;;   (setq-default
;;    company-idle-delay 0
;;       company-minimum-prefix-length 1
;;       company-show-numbers t
;;       company-tooltip-limit 10
;;       company-tooltip-align-annotations t
;;       ;; invert the navigation direction if the the completion popup-isearch-match
;;       ;; is displayed on top (happens near the bottom of windows)
;;       company-tooltip-flip-when-above t)
;;   )

;; (with-eval-after-load 'company
;;   (define-key company-active-map [tab] 'company-complete-selection)
;;   (define-key company-active-map (kbd "TAB") 'company-complete-selection)
;;   ;; (define-key company-active-map (kbd "return") nil)
;; ;;  )
;; ;;(with-eval-after-load 'company
;;   (define-key company-active-map (kbd "<return>") nil)
;;   (define-key company-active-map (kbd "RET") nil)
;;   (define-key company-active-map (kbd "C-RET") #'company-complete-selection)
;;   (define-key company-active-map (kbd "C-<return>") #'company-complete-selection))

;; ;; (define-key company-active-map (kbd "<return>") nil)
;; ;; (define-key company-active-map (kbd "<return>") nil)
;; (add-hook 'after-init-hook 'global-company-mode)

;; ******************************  Debugging  ******************************
;; (use-package realgud
;;   :config
;;   (setq realgud:pdb-command-name "python -m pdb")
;;   )

;; (use-package realgud-ipdb
;;   :config
;; (setq realgud--ipdb-command-name "ipdb3")  
;;  )

;; settings: https://github.com/microsoft/debugpy/wiki/Debug-configuration-settings
(use-package dape
  :after eglot
  :config
  ;; (setq dape-buffer-window-arrangement 'right)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-buffer-window-arrangement 'left)
  ;; (setq dape-info-buffer-window-groups nil)  ;; only repl, remove for all windows
  (add-to-list 'dape-configs
	       `(debugpy
		 modes (python-ts-mode python-mode)
		 command "python"
		 command-args ("-m" "debugpy.adapter")
		 :type "executable"
		 :request "launch"
		 :cwd dape-cwd-fn
		 :program dape-buffer-default
		 :redirectOutput t  ; fixes double prints?
		 )))

;; (use-package jupyter)
;; (use-package zmq)
;; (add-to-list 'load-path "~/Downloads/jupyter-0.8.3")
;; (require 'jupyter)

;; (use-package code-cells
;;   :config
;;   (let ((map code-cells-mode-map))
;;     (define-key map (kbd "C-c <up>") 'code-cells-backward-cell)
;;     (define-key map (kbd "C-c <down>") 'code-cells-forward-cell)
;;     (define-key map (kbd "M-<up>") 'code-cells-move-cell-up)
;;     (define-key map (kbd "M-<down>") 'code-cells-move-cell-down)
;;     (define-key map (kbd "C-c C-c") 'code-cells-eval)
;;     ;; Overriding other minor mode bindings requires some insistence...
;;     (define-key map [remap jupyter-eval-line-or-region] 'code-cells-eval)))
;; (add-hook 'python-mode-hook 'code-cells-mode-maybe)

;; (defun gm/jupyter-eval-region (beg end)
;;   (jupyter-eval-region nil beg end))

;; (add-to-list 'code-cells-eval-region-commands '(jupyter-repl-interaction-mode . gm/jupyter-eval-region))

;; (defun gm/jupyter-eval-region (beg end)
;;   (jupyter-eval-region nil beg end))
;; (add-to-list 'code-cells-eval-region-commands '(jupyter-repl-interaction-mode . gm/jupyter-eval-region))
;; (defun gm/jupyter-eval-region (beg end)
;;   (jupyter-eval-region nil beg end))

(defun gm/jupyter-eval-region (beg end)
  (jupyter-eval-region nil beg end))

;; (add-to-list 'code-cells-eval-region-commands '(jupyter-repl-interaction-mode . gm/jupyter-eval-region))

;; (use-package jupyter :defer t :custom (jupyter-repl-echo-eval-p t))
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

;;;****************************** STYLE ******************************


;; (set-face-attribute 'default nil :font "Fira Code Retina 10")
;; (set-face-attribute 'default nil :font "Menlo 10")
;; (set-face-attribute 'default nil :font "Monaco 10")
;; (set-face-attribute 'default nil :font "BlexMono Nerd Font 10")
;; (set-face-attribute 'default nil :font "Iosevka Comfy Wide Medium 10")
(set-face-attribute 'default nil :font "Iosevka Comfy Wide")
;; (set-face-attribute 'default nil :font "Iosevka Comfy Wide Duo 10")
;; (set-face-attribute 'default nil :font "Iosevka Comfy Wide 10")
;; (set-face-attribute 'default nil :font "Iosevka Comfy Wide Motion Duo 10")
;; (set-face-attribute 'default nil :font "Inconsolata 11")
;; (set-face-attribute 'default nil :font "IBM 3270 12")
;; (set-face-attribute 'default nil :font "EnvyCodeR Nerd Font 11")
;; (set-face-attribute 'default nil :font "Iosevka Fixed SS04 12")
;; (set-face-attribute 'default nil :font "Iosevka Term SS05 11")
;; (set-face-attribute 'default nil :font "Latin Modern Mono 11")
;; (set-face-attribute 'default nil :font "CommitMono Nerd Font 10")

;; (set-frame-font "Menlo:pixelsize=13")
;; (set-frame-font "Inconsolata:pixelsize=15")

(use-package ef-themes
  :ensure t
  :bind
  ("<f5>" . ef-themes-toggle)
  :custom
  (ef-themes-to-toggle '(ef-trio-dark ef-maris-light))
  ;; (ef-themes-variable-pitch-ui t)
  (ef-themes-mixed-fonts t)
  (ef-themes-headings '((0 1.4) (1 1.3) (2 1.2) (3 1.1)))
  :init
  (load-theme (if (display-graphic-p) 'ef-maris-light 'ef-trio-dark) t))


;; (use-package modus-themes
;;   :config
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-italic-constructs nil
;; 	modus-themes-bold-constructs t
;; 	modus-themes-slanted-constructs nil)
;;   (setq modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
;;   (load-theme 'modus-operandi-tinted t)
;;   (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; use-package with package.el:
;; (use-package dashboard
;;   :config
;;   ;; Content is not centered by default. To center, set
;;   (setq dashboard-center-content t)
;;   ;; vertically center content
;;   (setq dashboard-vertically-center-content t)
;;   ;; (setq dashboard-startup-banner nil)
;;   ;; (setq dashboard-startup-banner 'nil)
;;   ;; To disable shortcut "jump" indicators for each section, set
;;   ;; (setq dashboard-show-shortcuts nil)
;;   ;; (setq dashboard-startup-banner 'nil)
;;   (setq dashboard-items '((recents   . 5)
;;                         (bookmarks . 5)
;;                         (projects  . 5)
;;                         ;; (agenda    . 5)
;;                         (registers . 5)))
;;   (dashboard-setup-startup-hook)
;;   )

(use-package delight)
(delight '((eldoc-mode nil "eldoc")
	   (hs-minor-mode nil "hideshow")
	   (flyspell-mode nil "flyspell")
	   (superword-mode nil "subword")
	   (isortify-mode nil "isortify")
	   (auto-revert-mode nil "autorevert")
	   (ruff-format-on-save-mode nil "ruff-format")
	   ))

;; (use-package doom-modeline
;;   :hook (after-init . doom-modeline-mode)
;;   :config
;;   ;; (setq doom-modeline-buffer-file-name-style 'auto)
;;   (setq doom-modeline-buffer-file-name-style 'truncate-nil)
;;   (setq doom-modeline-icon t)
;;   (setq doom-modeline-lsp-icon nil)
;;   (setq doom-modeline-time-icon nil)
;;   (setq doom-modeline-time-analogue-clock nil)
;;   (setq doom-modeline-position-line-format '("L%l"))
;;   ;; (setq doom-modeline-position-column-line-format '("%l:%c"))
;;   ;; (setq doom-modeline-position-column-line-format nil)
;;   ;; (setq doom-modeline-minor-modes t)
;;   (setq doom-modeline-minor-modes t)
;;   (setq doom-modeline-buffer-encoding nil)
;;   ;; (setq doom-modeline-display-misc-in-all-mode-lines t)
;;   (setq doom-modeline-env-enable-python nil)
;;   (setq doom-modeline-percent-position '(-3 "%p"))
;;   )

(with-eval-after-load 'time
  ;; (setq display-time-default-load-average nil)
  (setq display-time-default-load-average nil)
  (setq display-time-24hr-format t)
  ;; (setq display-time-format "%a %b %d %R")
  ;; (setq display-time-format "%F %R")  ;; date and time
  (setq display-time-format "%R")        ;; just time
  ;; (setq display-time-day-and-date t)
  )
(display-time)


;;;****************************** MY KEY BINDINGS ******************************

(define-key global-map [remap zap-to-char] #'zap-up-to-char)

(define-key dired-mode-map (kbd "C-c w") 'wdired-change-to-wdired-mode)

(define-key global-map (kbd "C-c C-q") 'restart-emacs)

;; C-w to kill a line
;; M-w to copy a line
(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  :config (whole-line-or-region-global-mode)
  )

(defun my-comment-copy-yank-line-or-region ()
  "Copy the current line or region, comment it out, and yank below."
  (interactive)
					; hack to make sure region begins at beg. of line
  (if (region-active-p) (if (< (point) (mark)) (exchange-point-and-mark)))
  (if (region-active-p)  
      (progn (exchange-point-and-mark)
	     (move-beginning-of-line 1)
	     (exchange-point-and-mark)
	     )
    )
  (let ((beg (if (region-active-p) (region-beginning)  (line-beginning-position)))
        (end (if (region-active-p) (region-end) (line-end-position))))
    (copy-region-as-kill beg end)
    (comment-region beg end)
    (move-end-of-line 1)
    (newline)
    (yank)))

(define-key global-map (kbd "C-M-;") #'my-comment-copy-yank-line-or-region)

(when (file-exists-p "~/.emacs.d/mylisp/myhydra.el")
  (load "~/.emacs.d/mylisp/myhydra.el"))

(when (file-exists-p "~/.emacs.d/mylisp/myavy.el")
  (load "~/.emacs.d/mylisp/myavy.el"))

(define-key global-map (kbd "<f8>") #'avy-goto-char-timer)

(defun my-defn-other-window ()
  "Open defn in other window, but also move to top"
  (interactive)
  ;;   (save-excursion
  ;;     (xref-find-definitions-other-window nil)
  ;;     ;; (move-cursor-to-top)
  ;; ))
  (let ((original-window (selected-window))
        (original-point (point)))
    (xref-find-definitions-other-window nil)
    (move-cursor-to-top)
    (select-window original-window)
    (goto-char original-point)))

(define-key global-map (kbd "C-x 4 .") #'my-defn-other-window)


;; (global-set-key (kbd "M-n") 'my-end-of-par)
;; (global-set-key (kbd "M-n") 'python-nav-end-of-block)

;; (defun my-python-mode-hook ()
;;   "Custom key bindings for Python mode."
;;   (define-key python-mode-map (kbd "M-n") 'python-nav-end-of-block))

;; (add-hook 'python-mode-hook 'my-python-mode-hook)

;; (eval-after-load 'python
;; '(define-key python-mode-map "M-n" 'python-nav-end-of-block))

;; (add-hook 'python-ts-mode-hook
;;           (lambda () (local-set-key (kbd "M-n") 'python-nav-end-of-block)))
;; (add-hook 'python-ts-mode-hook
;;           (lambda () (local-set-key (kbd "M-p") 'python-nav-backward-block)))

(add-hook 'python-ts-mode-hook
          (lambda () (local-set-key (kbd "M-e") 'python-nav-end-of-block)))
(add-hook 'python-ts-mode-hook
          (lambda () (local-set-key (kbd "M-p") 'python-nav-backward-block)))
(add-hook 'python-ts-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'python-nav-forward-block)))

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
	;; This would override `fill-column' if it's an integer.
	(emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; (global-set-key (kbd "M-n") '('set-mark-command 'my-end-of-par 'my-copy-comment-and-paste-region))

;; (global-set-key (kbd "C-S-a") 'back-to-indentation)
;; (global-set-key (kbd "C-a") 'back-to-indentation)
;; (global-set-key (kbd "C-a") 'beginning-of-line)  ; default binding

;; newline-without-break-of-line
(defun newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
(define-key global-map (kbd "<C-return>") #'newline-without-break-of-line)


(use-package expand-region
  :bind ("C-=" . er/expand-region))

(define-key global-map (kbd "C-x C-i") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(define-key global-map (kbd "M-[") #'backward-paragraph)
(define-key global-map (kbd "M-]") #'forward-paragraph)

(define-key global-map (kbd "C-S-s") #'isearch-forward-thing-at-point)

(defun move-cursor-to-top ()
  "Move the display so that the cursor is at the top."
  (interactive)
  (recenter 0))
(define-key global-map (kbd "C-c t") #'move-cursor-to-top)

(define-key global-map (kbd "C-x C-r") #'revert-buffer)

(defun scroll-up-one-line ()
  "Scroll the buffer up one line."
  (interactive)
  (scroll-up 1))
(define-key global-map (kbd "C-}") #'scroll-up-one-line)
(define-key global-map (kbd "C-<end>") #'scroll-up-one-line)


(defun scroll-down-one-line ()
  "Scroll the buffer up one line."
  (interactive)
  ;; (scroll-lock-mode)
  (scroll-down 1)
  ;; (scroll-lock-mode)
  )
(define-key global-map (kbd "C-{") #'scroll-down-one-line)
(define-key global-map (kbd "C-<home>") #'scroll-down-one-line)

(define-key global-map (kbd "M-m") #'duplicate-dwim)

(define-key global-map (kbd "<home>") #'beginning-of-buffer)
(define-key global-map (kbd "<end>") #'end-of-buffer)

;; commenting
;; original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(define-key global-map (kbd "M-;") #'comment-dwim-line)


;; since tab is used for company, bind ~ to fill in snippets
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-~") yas-maybe-expand)

(define-key global-map (kbd "C-S-f") #'consult-recent-file)

(defun my-recentf-open-other-window ()
  (interactive)
  (split-window-right)
  (windmove-right nil)
  ;; (counsel-recentf)
  (consult-recent-file)
  (windmove-left nil) (setq register-preview-delay 0.5
			    register-preview-function #'consult-register-format)
  )
(define-key global-map (kbd "C-x 4 C-S-f") #'my-recentf-open-other-window)


(defun my-kill-word ()
  (interactive)
  (backward-word nil)
  (kill-word nil))

(define-key global-map (kbd "C-S-d") #'my-kill-word)

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

(defvar +vertico-transform-functions nil)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
  (dolist (fun (ensure-list +vertico-transform-functions))
    (setq cand (funcall fun cand)))
  (cl-call-next-method cand prefix suffix index start))

(defun +vertico-highlight-directory (file)
  "If FILE ends with a slash, highlight it as a directory."
  (if (string-suffix-p "/" file)
      (propertize file 'face 'marginalia-file-priv-dir) ; or face 'dired-directory
    file))

;; function to highlight enabled modes similar to counsel-M-x
(defun +vertico-highlight-enabled-mode (cmd)
  "If MODE is enabled, highlight it as font-lock-constant-face."
  (let ((sym (intern cmd)))
    (if (or (eq sym major-mode)
            (and
             (memq sym minor-mode-list)
             (boundp sym)))
	;; (propertize cmd 'face 'font-lock-constant-face)
	(propertize cmd 'face 'font-lock-warning-face)
      cmd)))

;; add-to-list works if 'file isn't already in the alist
;; setq can be used but will overwrite all existing values
(add-to-list 'vertico-multiform-categories
             '(file
               ;; this is also defined in the wiki, uncomment if used
               ;; (vertico-sort-function . sort-directories-first)
               (+vertico-transform-functions . +vertico-highlight-directory)))
(add-to-list 'vertico-multiform-commands
             '(execute-extended-command 
               ;; reverse
               (+vertico-transform-functions . +vertico-highlight-enabled-mode)))

;; (desktop-save-mode 1)

(use-package key-chord)
(key-chord-mode 1)
(key-chord-define-global "pf" 'consult-project-buffer)
(key-chord-define-global "hh"     'hydra-zoom/body)
(key-chord-define-global ";;" 'comment-dwim-line)
(key-chord-define-global "\'\'" 'my-comment-copy-yank-line-or-region)
(key-chord-define-global "aa" 'back-to-indentation)
(key-chord-define-global "ee" 'move-end-of-line)
(key-chord-define-global "pq"     'avy-goto-char-timer)
(key-chord-define-global "ww" 'aw-show-dispatch-help)
(key-chord-define-global "pw" 'ace-window)
