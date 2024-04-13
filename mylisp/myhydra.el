;; -*- lexical-binding: t; outline-regexp: ";;;" -*-

(use-package hydra)
(defhydra hydra-zoom ()
  "
 Avy (pq)            ^Mark^             ^Navigation^                        ^Manips^                         ^Python^    
^^^^^^---------------------------------------------------------------------------------------------------------------------------   --------------------------------
C-y: yank           _r_: set reg       ^M-[^: back-paragraph           C-S-k: kill line                     _n_: next error        | C-x C-i: init.el               |
M-w: copy           _j_: jump reg      ^M-]^: forward-paragraph        C-S-m: my-copy-comment-paste            _p_: prev error        |     C-t: transpose char (typo) |
C-k: kill-move    ^M-h^: paragraph       _t_: cursor top               C-S-c: copy-line                   ^C-~^: yasnippet         |   C-S-t: transpose frames      |
C-t: kill-stay      ^ ^              _<end>_: scroll other up      C-<enter>: newline go                  ^M-n^: next indent lvl   |   C-S-f: find recent files     |
C-m: mark word      ^ ^             _<home>_: scroll other down          C-;: copy-comment-paste-region   ^M-p^: prev indent lvl   |   C-S-r: avy copy region       |
M-t: teleport       ^ ^                  _s_: search thing @ pt       C-S-d:  my-kill-word                ^M-e^: end of                C-M-u: up to bracket
  C: copy-line	    ^ ^			 ^ ^								    ^ ^		               C-M-k: kill sexp
  Y: yank-line
C-w: kill-line-go
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

(provide 'myhydra)
