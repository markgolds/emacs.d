;; -*- lexical-binding: t; eval: (local-set-key (kbd "C-c i") #'consult-outline); outline-regexp: ";;;"; -*-


;;;****************************** GENERAL STUFF ******************************
(setq warning-minimum-level :error)

;; no start-up screen
(setq inhibit-startup-message t)

;; Turn beep off
(setq visible-bell nil)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

; turon off externals
;; Remove scrollbars, menu bars, and toolbars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(set-fringe-mode '(12 . 0))  ; extra space on left, none on right

;; Remembering recently edited files, recentf-open
;; (recentf-mode 1)

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

(setq dired-listing-switches "-alFh")  ;; ls -alFh when opening dired

;; Setup straight --------------------------------------------------
;; (setq package-enable-at-startup nil)

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

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(setq holiday-bahai-holidays nil)
(setq holiday-islamic-holidays nil)

;; don't show asterix in *bold* in org mode, etc...
(setq org-hide-emphasis-markers t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Capture templates
(setq org-capture-templates
      '(("n"               ; key
         "Note"            ; name
         entry             ; type
         (file+headline "~/Dropbox/org/notes.org" "Notes")  ; target
         "* %? %(org-set-tags)  :note: \n:PROPERTIES:\n:Created: %U\n:Linked: %A\n:END:\n%i"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
        )
	("w"               ; key
         "Weights"         ; name
         table-line        ; type
         (file "~/Dropbox/org/habits/weights.org" )  ; target
	 "|%U|%^{weight}|%^{comment}|"
         :prepend t        ; properties
         :kill-buffer t    ; properties
        )

	("q"               ; key
	 "Quick note"      ; name
	 entry             ; type
	 (file "~/Dropbox/org/quicknotes.org")  ; target
	 "* %?\nEntered on %U\n" ;template
	 :prepend t        ; properties
	 :empty-lines 1    ; properties
	 :created t        ; properties
	 )
	("d"               ; key
	 "Todos"      ; name
	 entry             ; type
	 (file "~/Dropbox/org/todos.org")  ; target
	 "* %?\nEntered on %U\n" ;template
	 :prepend t        ; properties
	 :empty-lines 1    ; properties
	 :created t        ; properties
	 )

	("t"               ; key
	 "Time Log"      ; name
	 entry             ; type
	 (file "~/Dropbox/org/timelog.org")  ; target
	 "* %?\nEntered on %U\n" ;template
	 :prepend t        ; properties
	 :empty-lines 1    ; properties
	 :created t        ; properties
	 )
	("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org") "* \n%?\n" :clock-in t)
	("k" "Journal-night" entry (file+datetree "~/Dropbox/org/journal-night.org") "* \n%?\n" :clock-in t)
        ))

;; indentation for lists in org mode
(setq org-list-indent-offset 2)

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-agenda-files (list
			;; "~/Dropbox/org/references/articles.org"
			"~/Dropbox/org/quicknotes.org"
			"~/Dropbox/org/todos.org"
			;;"~/Dropbox/org/journal.org"
			"~/Dropbox/org/timelog.org"
			"~/Dropbox/org/birthdays.org"
			"~/Dropbox/org/Ped.org"
			     ))

(setq org-todo-keywords
       '((sequence "TODO(t)" "READ" "STUDY" "PAPERS" "CODE" "ANKI" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
  '(("TODO" . (:foreground "#ff39a3" :weight bold))
("READ" . (:foreground "#c4efcd" :weight bold))
;;("STUDY" . (:foreground "white" :background "#4d4d4d" :weight bold))
("STUDY" . (:foreground "white" :weight bold))
("PAPERS" . (:foreground "#b3c6e5" :weight bold))
("CODE" . (:foreground "#bc86e0" :weight bold))
("ANKI" . (:foreground "#e2ab58" :weight bold))
))

;; sort by todo state (for todo list) and then time (for daily logs)
(setq org-agenda-sorting-strategy 
      '((agenda todo-state-up time-up)))


;; Turn this off in org mode:
;;https://emacs.stackexchange.com/questions/73986/how-do-i-stop-org-babel-from-trying-to-edit-a-source-block-in-a-dedicated-buffer
;; electric-indent-mode
(add-hook 'org-mode-hook
          (lambda ()
            (electric-indent-local-mode -1)))

(setq org-image-actual-width nil)

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
;;;****************************** MAIN PACKAGES ******************************;;

;; (use-package markdown-mode)

(use-package company-statistics)
(add-hook 'after-init-hook 'company-statistics-mode)

(use-package diminish)

(use-package magit)
(global-set-key "\C-xg" 'magit-status)

(use-package rainbow-delimiters
  :diminish
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package yasnippet
  :diminish)
;; (require 'yasnippet-snippets)
(use-package yasnippet-snippets
  :diminish)
(yas-global-mode 1)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
	"~/.emacs.d/elpa/yasnippet-snippets-20240221.1621/snippets"
        ))

;god-mode
(use-package god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
(defun c/god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))
;end god-mode

(use-package drag-stuff
  :diminish
  :bind (
	 ("M-<up>" . 'drag-stuff-up)
	 ("M-<down>" . 'drag-stuff-down))
  :config (drag-stuff-global-mode 1)
  )

(use-package counsel
  :diminish
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  )

(use-package company-prescient
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  )

;; swiper is an ivy enhanced version of isearch.
(use-package swiper
  :bind (("M-s" . counsel-grep-or-swiper)))

(use-package ivy-hydra)

(use-package ivy
  ;; :bind (("TAB" . ivy-alt-done))
  :diminish
  :config
  (ivy-mode 1)
  (ivy-prescient-mode 1)
  (setq ivy-initial-inputs-alist nil)  ;; removes ‘^’ in things like counsel-M-x and other ivy/counsel prompts.
  )

;; The following line 
;; The default ‘^’ string means that if you type something immediately after this string only completion candidates that begin with what you typed are shown.  Most of the time, I’m searching for a command without knowing what it begins with though.

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1)) ;; this gets us descriptions in M-x.

(use-package all-the-icons
  :if (display-graphic-p))
;; M-x all-the-icons-install-fonts    ;; do this once after install
(use-package all-the-icons-dired)
(use-package dired-open)
(use-package peep-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)


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
;;;****************************** PYTHON ******************************

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1)
  ;; (pyvenv-activate "~/miniconda3/envs/alchemistry/")
  (pyvenv-activate "~/miniconda3/envs/qdgp/")
  )

;; (use-package conda
;;   :config
;;   (conda-env-activate "qdgp")
;;   )

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

;; (use-package poetry
;;   :ensure t
;;   :defer t
;;   :config
;;   ;; Checks for the correct virtualenv. Better strategy IMO because the default
;;   ;; one is quite slow.
;;   (setq poetry-tracking-strategy 'switch-buffer)
;;   :hook (python-mode . poetry-tracking-mode))

;; (require 'blacken)
;; (add-hook 'python-mode-hook 'blacken-mode)

;; (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

;; This works well but doesn't provide autocomplete and other stuff:
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(python-mode . ("ruff-lsp"))))


;; (use-package eglot
;;   :preface
;;   :defer t
;;   :bind (:map eglot-mode-map
;;               ;; ("C-c C-d" . eldoc)
;;               ("C-c C-e" . eglot-rename)
;;               ("C-c C-o" . python-sort-imports)
;;               ("C-c C-f" . eglot-format-buffer))
;;   :hook ((python-ts-mode . eglot-ensure)
;;          ;; (python-ts-mode . flyspell-prog-mode)
;;          ;; (python-ts-mode . superword-mode)
;;          (python-ts-mode . hs-minor-mode)
;;          ;; (python-ts-mode . (lambda () (set-fill-column 88)))
;; 	 ;; (python-ts-mode . (lambda () (diminish 'hs-minor-mode)))
;; 	 ;; (eglot-managed-mode . mp-eglot-eldoc)
;; 	 )
;;   :custom
;;   (fset #'jsonrpc--log-event #'ignore)
;;   (eglot-events-buffer-size 0)
;;   ;; :config
;;   ;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
;;   ;; (add-to-list 'eglot-server-programs '(python-mode . ("ruff-lsp" "-stdio" "--linters" "pyflakes,mccabe,pycodestyle,pydocstyle,bandit,black,isort")))
;;   )

;; (use-package eglot
;;   :preface
;;   :defer t
;;   :bind (:map eglot-mode-map
;;               ;; ("C-c C-d" . eldoc)
;;               ("C-c C-e" . eglot-rename)
;;               ("C-c C-o" . python-sort-imports)
;;               ("C-c C-f" . eglot-format-buffer))
;;   :hook ((python-mode . eglot-ensure)
;;          ;; (python-mode . flyspell-prog-mode)
;;          ;; (python-mode . superword-mode)
;;          (python-mode . hs-minor-mode)
;;          ;; (python-mode . (lambda () (set-fill-column 88))))
;; 	 )
;;   :config
;;   (setq-default eglot-workspace-configuration
;;                 '((:pylsp . (:configurationSources ["flake8"]
;;                              :plugins (
;;                                        :pycodestyle (:enabled :json-false)
;;                                        :mccabe (:enabled :json-false)
;;                                        :pyflakes (:enabled :json-false)
;;                                        ;; :flake8 (:enabled :json-false
;;                                                 ;; :maxLineLength 88)
;;                                        :ruff (:enabled t
;;                                               :lineLength 88)
;;                                        ;; :pydocstyle (:enabled t
;;                                                     ;; :convention "numpy")
;;                                        ;; :yapf (:enabled :json-false)
;;                                        ;; :autopep8 (:enabled :json-false)
;;                                        ;; :black (:enabled t
;;                                        ;;         :line_length 88
;;                                        ;;         :cache_config t)
;; 				       ))))))



(use-package flymake-ruff
  :straight (flymake-ruff :type git :host nil :repo "https://github.com/erickgnavar/flymake-ruff")
  :hook ((eglot-managed-mode . flymake-ruff-load))
  )

(use-package ruff-format
  :hook (eglot-managed-mode . ruff-format-on-save-mode))

;; (use-package flymake-ruff
;;   :ensure t
;;   :hook (python-ts-mode . flymake-ruff-load))

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
;; (advice-remove 'eglot--report-to-flymake #'my-fix-eglot-diagnostics)

(use-package eglot
  :preface
  ;; :defer t
  :bind (:map eglot-mode-map
              ;; ("C-c C-d" . eldoc)
              ("C-c C-e" . eglot-rename)
              ("C-c C-o" . python-sort-imports)
              ("C-c C-f" . eglot-format-buffer))
  :hook ((python-ts-mode . eglot-ensure)
         ;; (python-ts-mode . flyspell-prog-mode)
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

;; (use-package flymake-ruff
;;   :ensure t
;;   :hook ((python-mode . flymake-ruff-load)
;;   (python-ts-mode . flymake-ruff-load)
;; )
;;   )

;; (advice-add 'eglot--report-to-flymake :filter-args #'my-filter-eglot-diagnostics)


;; (add-hook 'python-mode-hook #'flymake-ruff-load)
;; (add-hook 'python-ts-mode-hook #'flymake-ruff-load)

(use-package isortify
  :hook (python-ts-mode . isortify-mode))

(use-package company
  :ensure t
  :hook ((python-ts-mode . company-mode))
  :diminish
  :config
  (setq company-prescient-mode 1)
  (setq-default
   company-idle-delay 0
      company-minimum-prefix-length 1
      company-show-numbers t
      company-tooltip-limit 10
      company-tooltip-align-annotations t
      ;; invert the navigation direction if the the completion popup-isearch-match
      ;; is displayed on top (happens near the bottom of windows)
      company-tooltip-flip-when-above t)
  )

(with-eval-after-load 'company
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection))

(add-hook 'after-init-hook 'global-company-mode)

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
  (setq dape-buffer-window-arrangement 'right)
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
		 )
	       ))
;; (use-package doom-themes
;;   :config
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;; 	doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   ;; (load-theme 'doom-material-dark t)
;;   (load-theme 'doom-tomorrow-night t)
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config)
;;   ;; (load-theme 'doom-sourcerer)
;;   ;; (load-theme 'doom-one)
;;   )

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))



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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   ;; (julia . t)
   (python . t)
   (jupyter . t)
   ))

;; (defun org-edit-src-code nil)


(defun insert-my-jupyter-block ()
  "Inserts a custom code block into the Org mode buffer."
  (interactive)
  ;; (insert "#+BEGIN_SRC jupyter-python :session py :results scalar :display plain\n")
  (insert "#+BEGIN_SRC jupyter-python\n")
  (insert "\n")
  (insert "#+END_SRC\n")
  (forward-line -2)
  (move-end-of-line 1))


;; Create a new jupyter block with C-c p
(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c p") 'insert-my-jupyter-block))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-<tab>") 'company-complete))

;; Make org headers smaller
(defun my-org-customize-font ()
  "Customize font size for Org mode lines starting with #+."
  (setq-local face-remapping-alist
              '((org-meta-line . (:height 0.6))
                (org-block-begin-line . (:height 0.6))
                (org-block-end-line . (:height 0.6)))))

(add-hook 'org-mode-hook 'my-org-customize-font)

;; (setq org-src-preserve-indentation nil)

;; (setq org-src-tab-acts-natively t)

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

(use-package elfeed
  :config
  (setq elfeed-search-feed-face ":foreground #ffffff :weight bold"
	elfeed-feeds (quote
		      (("https://hnrss.org/frontpage" hackernews)
		       ("https://www.reddit.com/r/emacs.rss" emacs reddit)
		      ))))  


;;;****************************** STYLE ******************************


;; (set-face-attribute 'default nil :font "Fira Code Retina 10")
(set-face-attribute 'default nil :font "Menlo 10")
;; (set-face-attribute 'default nil :font "Monaco 10")
;; (set-face-attribute 'default nil :font "Inconsolata 12")
;; (set-face-attribute 'default nil :font "IBM 3270 12")

;; (set-frame-font "Menlo:pixelsize=13")
;; (set-frame-font "Inconsolata:pixelsize=17")

(use-package ef-themes
  :ensure t
  :bind
  ("<f5>" . ef-themes-toggle)
  :custom
  (ef-themes-to-toggle '(ef-symbiosis ef-day))
  ;; (ef-themes-variable-pitch-ui t)
  (ef-themes-mixed-fonts t)
  (ef-themes-headings '((0 1.4) (1 1.3) (2 1.2) (3 1.1)))
  :init
  (load-theme (if (display-graphic-p) 'ef-day 'ef-symbiosis) t))

;;   (use-package ef-themes
;;    ;; :defer
;;    :straight (ef-themes :type git :host github :repo "protesilaos/ef-themes")
;;    :bind ("<f5>" . ef-themes-toggle)
;;    :config
   
;;    ;; (ef-themes-select 'ef-summer)
;;    (ef-themes-select 'ef-frost)
;;    (defun my-ef-themes-hl-todo-faces ()
;;   "Configure `hl-todo-keyword-faces' with Ef themes colors.
;; The exact color values are taken from the active Ef theme."
;;   (ef-themes-with-colors
;;     (setq hl-todo-keyword-faces
;;           `(("HOLD" . ,yellow)
;;             ("TODO" . ,red)
;;             ("NEXT" . ,blue)
;;             ("THEM" . ,magenta)
;;             ("PROG" . ,cyan-warmer)
;;             ("OKAY" . ,green-warmer)
;;             ("DONT" . ,yellow-warmer)
;;             ("FAIL" . ,red-warmer)
;;             ("BUG" . ,red-warmer)
;;             ("DONE" . ,green)
;;             ("NOTE" . ,blue-warmer)
;;             ("KLUDGE" . ,cyan)
;;             ("HACK" . ,cyan)
;;             ("TEMP" . ,red)
;;             ("FIXME" . ,red-warmer)
;;             ("XXX+" . ,red-warmer)
;;             ("REVIEW" . ,red)
;;             ("DEPRECATED" . ,yellow))))))

;; (add-hook 'ef-themes-post-load-hook #'my-ef-themes-hl-todo-faces)
;;    (setq ef-themes-to-toggle (list 'ef-frost 'ef-symbiosis )
;;    )


;; (load-theme 'ef-spring t)


;; (use-package modus-themes
;;   :config
;;   ;; Add all your customizations prior to loading the themes
;;   (setq
;;    modus-themes-italic-constructs t
;;    modus-themes-bold-constructs nil
;;    ;; modus-themes-slanted-constructs t
;;    )

;;   ;; (set-face-attribute 'default nil :family "Fira Code" :height 100)
;;   ;; (set-face-attribute 'italic nil :family "Hack")
;;   ;; (set-face-attribute 'bold nil :family "Hack")

;;   ;; Remove the border
;;   (setq modus-themes-common-palette-overrides
;; 	'((border-mode-line-active unspecified)
;;           (border-mode-line-inactive unspecified)))

;;   (setq modus-themes-to-toggle '(modus-vivendi-deuteranopia modus-operandi-deuteranopia))

;;   ;; (load-theme 'modus-vivendi-tritanopia t) ;; sort of cherry
;;   (load-theme 'modus-vivendi-deuteranopia t) ;; purplisha
;;   ;; (load-theme 'modus-vivendi t)  	;; also purplish, more black

;;   (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; use-package with package.el:
(use-package dashboard
  :config
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; vertically center content
  (setq dashboard-vertically-center-content t)
  ;; (setq dashboard-startup-banner nil)
  ;; (setq dashboard-startup-banner 'nil)
  ;; To disable shortcut "jump" indicators for each section, set
  ;; (setq dashboard-show-shortcuts nil)
  ;; (setq dashboard-startup-banner 'nil)
  (setq dashboard-items '((recents   . 5)
                        (bookmarks . 5)
                        (projects  . 5)
                        ;; (agenda    . 5)
                        (registers . 5)))
  (dashboard-setup-startup-hook)
  )
 
(use-package delight)
(delight '((eldoc-mode nil "eldoc")
	   (hs-minor-mode nil "hideshow")
	   (flyspell-mode nil "flyspell")
	   (superword-mode nil "subword")
	   (yas-minor-mode nil "yasnippet")
	   (isortify-mode nil "isortify")
	   (auto-revert-mode nil "autorevert")
	   (ruff-format-on-save-mode nil "ruff-format")
	   ))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  ;; (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-buffer-file-name-style 'truncate-nil)
  (setq doom-modeline-icon t)
  (setq doom-modeline-lsp-icon nil)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-time-analogue-clock nil)
  (setq doom-modeline-position-line-format '("L%l"))
  ;; (setq doom-modeline-position-column-line-format '("%l:%c"))
  ;; (setq doom-modeline-position-column-line-format nil)
  ;; (setq doom-modeline-minor-modes t)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-buffer-encoding nil)
  ;; (setq doom-modeline-display-misc-in-all-mode-lines t)
  (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-percent-position '(-3 "%p"))
  )

(with-eval-after-load 'time
  (setq display-time-default-load-average nil)
  ;; (setq display-time-format "%a %b %e %Y")
  (setq display-time-day-and-date t)
  )
(display-time)


;;;****************************** MY KEY BINDINGS ******************************

(defhydra hydra-zoom ()
  "
 Avy (pq)            ^Mark^             ^Navigation^                        ^Manips^                         ^Python^    
^^^^^^---------------------------------------------------------------------------------------------------------------------------   --------------------------------
C-y: yank           _r_: set reg       ^M-[^: back-paragraph           C-S-k: kill line                     _n_: next error        | C-x C-i: init.el               |
M-w: copy           _j_: jump reg      ^M-]^: forward-paragraph        C-S-m: copy-comment-paste            _p_: prev error        |     C-t: transpose char (typo) |
C-k: kill-move    ^M-h^: paragraph       _t_: cursor top               C-S-c: copy-line                   ^C-~^: yasnippet         |   C-S-t: transpose frames      |
C-t: kill-stay      ^ ^              _<end>_: scroll other up      C-<enter>: newline go                  ^M-n^: next indent lvl   |   C-S-f: find recent files     |
C-m: mark word      ^ ^             _<home>_: scroll other down          C-;: copy-comment-paste-region   ^M-p^: prev indent lvl   |   C-S-r: avy copy region       |
M-t: teleport       ^ ^                  _s_: search thing @ pt       C-S-d:  my-kill-word                ^M-e^: end of                C-M-u: up to bracket
  C: copy-line	    ^ ^			 ^ ^								    ^ ^		               C-M-k: kill sexp
  Y: yank-line
  z: zap region
C-e: jump to end of line
"
  ("g" text-scale-increase "zoom in")
  ("l" text-scale-decrease "zoom out")
  ("r" point-to-register :exit t)
  ("j" jump-to-register :exit t)
  ("t" move-cursor-to-top :exit t)
  ("n" flymake-goto-next-error)
  ("p" flymake-goto-prev-error)
  ("<home>" scroll-other-window-down)
  ("<end>" scroll-other-window)
  ("q" nil "quit")
  ("s" isearch-forward-thing-at-point :exit t)
  )

(global-set-key (kbd "C-c d") 'hydra-zoom/body)
(key-chord-define-global "hh"     'hydra-zoom/body)

(defun my-avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun my-avy-action-yank-whole-line (pt)
  (my-avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(defun my-avy-action-goto-end-of-line (pt)
  "Goto PT."
  (let ((frame (window-frame (selected-window))))
    (unless (equal frame (selected-frame))
      (select-frame-set-input-focus frame)
      (raise-frame frame))
    (goto-char pt)
    (move-end-of-line nil)
    ))


(use-package avy
  :bind (
	 ;("C-S-a" . 'avy-goto-char-timer)
	 ;; ("C-S-r" . 'avy-copy-region))  ; this will also paste to pt
	 ("C-S-r" . 'avy-kill-ring-save-region))
	 ;; ("C-S-a" . 'avy-goto-char-2)
  :config
  (setq avy-styles-alist '((avy-goto-char-timer . pre)))
  (setq avy-timeout-seconds 0.3)
  (setq avy-background t)	  
  (setf (alist-get ?\C-y avy-dispatch-alist) 'avy-action-yank 
      (alist-get ?\M-w avy-dispatch-alist) 'avy-action-copy 
      (alist-get ?\C-k avy-dispatch-alist) 'avy-action-kill-move 
      (alist-get ?\C-t avy-dispatch-alist) 'avy-action-kill-stay
      (alist-get ?\C-m avy-dispatch-alist) 'avy-action-mark
      (alist-get ?\M-t avy-dispatch-alist) 'avy-action-teleport
      (alist-get ?C avy-dispatch-alist) 'my-avy-action-copy-whole-line
      (alist-get ?Y avy-dispatch-alist) 'my-avy-action-yank-whole-line
      (alist-get ?\C-e avy-dispatch-alist) 'my-avy-action-goto-end-of-line
  )
  )

(key-chord-define-global "aa"     'avy-goto-char-timer)
(key-chord-define-global "pq"     'avy-goto-char-timer)


(use-package key-chord)
(key-chord-mode 1)


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
(global-set-key (kbd "C-x 4 .") 'my-defn-other-window)

(defun my-copy-and-comment-region ()
  "Copy the selected region of text and then comment it out."
  (interactive)
  (when (region-active-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (kill-ring-save beg end)
      (comment-region beg end)
      (newline-and-indent)
      (message "Region copied and commented out."))))

(defun my-copy-comment-and-paste-region ()
  "Copy the selected region of text, comment it out, and paste it below."
  (interactive)
  (when (region-active-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (kill-ring-save beg end)
      (comment-region beg end)
      (newline-and-indent)
      (yank)
      ;; (newline-and-indent)
      (message "Region copied, commented, and pasted."))))

(global-set-key (kbd "C-;") 'my-copy-comment-and-paste-region)

(defun my-end-of-par ()
  "Move point to the end of the current paragraph."
  (interactive)
  (end-of-paragraph-text)
  ;; (end-of-line)
  )

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

(defun my-test ()
  
     (interactive)
     (set-mark-command nil)
     (python-nav-end-of-block)
     (my-copy-comment-and-paste-region)
     (message "boom"))
;; (global-set-key (kbd "M-n") '('set-mark-command 'my-end-of-par 'my-copy-comment-and-paste-region))

;; (global-set-key (kbd "C-S-a") 'back-to-indentation)
(global-set-key (kbd "C-a") 'back-to-indentation)

;; newline-without-break-of-line
(defun newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

(use-package expand-region
    :bind ("C-=" . er/expand-region))

(global-set-key (kbd "C-x C-i") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

(global-set-key (kbd "C-S-s") 'isearch-forward-thing-at-point)

;define a kbd macro to copy a line
(defun my-copy-line () (interactive)
       (kill-ring-save (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-S-c") 'my-copy-line) 

(defun move-cursor-to-top ()
  "Move the display so that the cursor is at the top."
  (interactive)
  (recenter 0))
(global-set-key (kbd "C-c t") 'move-cursor-to-top)

;define a kbd macro to insert new line below and go to it
(defun my-insert-line () (interactive) (move-end-of-line 1) (newline))
(global-set-key (kbd "C-S-n") 'my-insert-line)

(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-S-<Backspace>") 'kill-whole-line) 

;C-x C-r to revert-buffer
(global-set-key [(control x) (control r)] 'revert-buffer)

(defun scroll-up-one-line ()
  "Scroll the buffer up one line."
  (interactive)
  (scroll-up 1))
(global-set-key (kbd "C-}") 'scroll-up-one-line)
;; (global-set-key (kbd "<end>") 'scroll-up-one-line)
(global-set-key (kbd "<end>") 'scroll-up-one-line)

(defun scroll-down-one-line ()
  "Scroll the buffer up one line."
  (interactive)
  ;; (scroll-lock-mode)
  (scroll-down 1)
  ;; (scroll-lock-mode)
  )
(global-set-key (kbd "C-{") 'scroll-down-one-line)
(global-set-key (kbd "<home>") 'scroll-down-one-line)

; copy current line, comment it out, and paste it to next line
(defun copy-comment-paste ()
  (interactive)
  (save-excursion
    (let ((current-line (thing-at-point 'line t)))
      (goto-char (line-beginning-position))
      (insert current-line)
      (comment-region (line-beginning-position 0)
                      (line-end-position 0)))))
(global-set-key (kbd "C-S-m") 'copy-comment-paste) 

;commenting
;; Original idea from
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
(global-set-key "\M-;" 'comment-dwim-line)
(key-chord-define-global ";;" 'comment-dwim-line)


;; since tab is used for company, bind ~ to fill in snippets
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-~") yas-maybe-expand)

(global-set-key (kbd "C-S-f") 'counsel-recentf)
(global-set-key (kbd "C-c f") 'counsel-recentf)

(defun my-recentf-open-other-window ()
  (interactive)
  (split-window-right)
  (windmove-right nil)
  (counsel-recentf)
  (windmove-left nil) (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
)
(global-set-key (kbd "C-x 4 C-S-f") 'my-recentf-open-other-window)

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  )

;; (straight-use-package 'eglot-booster)
;; (straight-use-package '(eglot-booster :type git :host github :repo "joaotavora/eglot-booster"))

;; (use-package eglot-booster
;; 	:after eglot
;; 	:config	(eglot-booster-mode))



;; (use-package corfu
;;   ;; Optional customizations
;;   :custom
;;   ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Enable Corfu only for certain modes.
;;   :hook ((prog-mode . corfu-mode)
;;          (shell-mode . corfu-mode)
;;          (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
;;   ;; be used globally (M-/).  See also the customization variable
;;   ;; `global-corfu-modes' to exclude certain modes.
;;   :init
;;   (global-corfu-mode))



;; Had to download eglot-lsp-booster from here first:
;; https://github.com/blahgeek/emacs-lsp-booster?tab=readme-ov-file#obtain-or-build-emacs-lsp-booster
;; Then "mv emacs-lsp-booster ~/.local/bin/"
;; (use-package eglot-booster
;;   :ensure t
;;   :straight (:type git :host github :repo "jdtsmith/eglot-booster")
;;   :after eglot
;;   :config
;;   (eglot-booster-mode))

;; (use-package eglot-booster
;;   :ensure t
;;   :straight (:type git :host github :repo "joaotavora/eglot-booster")
;;   :after eglot
;;   :config
;;   (eglot-booster-mode))

;; With use-package:
;; (use-package company-box
;;   :hook (company-mode . company-box-mode)
;;   ;; :config (setq company-box-doc-frame-parameters ((internal-border-width . 5)))
;;   :config
;;   ;; (setq company-box-doc-frame-parameters '((internal-border-width . 2) (left . 44) (right . -44)))
;;   (setq company-box-doc-frame-parameters '((left . -44) (right . -44)))
;;   ;; (setq company-box-doc-enable t)
;;   ;; (setq company-box-frame-top-margin 200)  ; this would move the main pop up box away from typing area
;;   )

;; (setq company-format-margin-function    #'company-vscode-dark-icons-margin)



;; Remove the function responsible for notes from the list
;; (setq flymake-diagnostic-functions
;;       (remove 'flymake-proc-legacy-flymake flymake-diagnostic-functions))


(defun my-kill-word ()
  (interactive)
  (backward-word nil)
  (kill-word nil))

;; (global-set-key (kbd "M-d") 'my-kill-word)
(global-set-key (kbd "C-S-d") 'my-kill-word)
;; (global-set-key (kbd "C-D") 'my-kill-word)

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))



(require 'uniquify) (setq uniquify-buffer-name-style 'forward)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)))

(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  :config (whole-line-or-region-global-mode)
  )

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
