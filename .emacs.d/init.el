;; Determine 'user-emacs-directory'
(when load-file-name
  (setq user-emacs-directory (expand-file-name
                              (file-name-directory load-file-name))))


(eval-when-compile (require 'cl))

(when load-file-name
  (setq user-emacs-directory (expand-file-name
                              (file-name-directory load-file-name))))

;; Turn off mouse interfaces
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Open my init.el
(defun open-my-init-file ()
  (interactive)
  (find-file user-init-file))

(defalias 'yes-or-no-p 'y-or-n-p)

;;;;
;; Packages
;;;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))

;; Load and activate emacs packages.
(package-initialize)

;; Download the Elpa archive description if needed.
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)
    (error "Please install use-package")))
(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; to update packages
(use-package package-utils
  :commands (package-utils-upgrade-by-name package-utils-upgrade-all))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This setting make buffer names to unique.
;; Adds directory name when same file names exist.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode.
(use-package recentf
  :config
  (progn
    (setq recentf-save-file (concat user-emacs-directory ".recentf"))
    (require 'recentf)
    (recentf-mode 1)
    (setq recentf-max-menu-items 40)))

;; projectile everywhere
(use-package projectile
  :config
  (projectile-global-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn off the menu bar at the top of each frame.
(menu-bar-mode -1)

;; Turn off the graphical tool bar at the top of each frame.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Color Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'wombat)

;; increase font size for better readability
(cond
 ((or (eq system-type 'mac) (eq system-type 'darwin))
  (set-face-attribute 'default nil :family "Ricty" :height 150))
 ((eq system-type 'gnu/linux) (set-face-attribute 'default nil :family "Ricty Diminished" :height 100))
 (t (error "unknown system")))


;; No cursor blinking.
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bel
(setq ring-bell-function 'ignore)

;; Turn off scroll bar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; show column
(column-number-mode 1)

;; Show trailing white spaces
(setq-default show-trailing-whitespace t)
(defun my/disable-trailing-mode-hook ()
  "Disable show trail whitespace"
  (setq show-trailing-whitespace nil))

;; anything.el
(use-package anything
  :config
  (require 'anything-config)
  (setq anything-enable-shortcuts 'prefix)
  (define-key anything-map (kbd "@") 'anything-select-with-prefix-shortcut)
  :bind
  ("C-x C-b" . anything-mini)
  ("C-x C-f" . anything-for-files)
  ("M-x" . anything-M-x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations relating to editing a buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lisp-frienfly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthsis
(show-paren-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; comments toggle
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; use 2 spaces for tabs
(defun die-tabs (n)
  (interactive "nSpaces count:")
  (set-variable 'tab-width n)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

(setq electric-indent-mode nil)


;; Ctrl-H to backspace
(global-set-key (kbd "C-h") 'backward-delete-char)

;; C-k delete a line and '\n' if cursor is on the head of line
(setq kill-whole-line t)

;; ensure newline on the end of file
(setq require-final-newline t)

(use-package flycheck)

;; snippet
(use-package yasnippet
  :config
  (define-key yas-keymap (kbd "C-i") nil)
  (yas-global-mode 1))

;; search and replace strings
(use-package anzu
  :config
  (global-anzu-mode 1)
  (setq anzu-mode-lighter ""
        anzu-deactivate-region t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC (hard to categorize other categories.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; On OS X, meta key is option.
;; Change meta key binding to left command.
(when (eq system-type 'darwin)
  (setq default-input-method "MacOSX")
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super))
(when (eq system-type 'gnu/linux)
  (setq x-super-keysym 'meta)
  (setq x-meta-keysym 'super))

;; shell integration
(use-package shell-pop
  :config
  (progn
    (custom-set-variables
     '(shell-pop-default-directory "~")
     '(shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
     '(shell-pop-term-shell (substring (shell-command-to-string "which zsh 2>/dev/null") 0 -1))))
  :bind ("C-c c" . shell-pop))

;; version control
(use-package magit)

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode t))

;; undo history
(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  (global-set-key (kbd "M-/") 'undo-tree-redo))

;; highlight when undo and yank...
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

;; Twitter
(use-package twittering-mode
  :commands (twit)
  :config
  (setq twittering-account-authorization 'authorized)
  (setq twittering-oauth-access-token-alist
        '(("oauth_token" . "3174259753-C075cEsx85kt2nmrOC494YfHqjJXiBE6i0nd12O")
          ("oauth_token_secret" . "TAxdcXcgTIkPG7h2H8crxmI6RmCvTf4WzN0IFhfBBwVrq")
          ("user_id" . "3174259753")
          ("screen_name" . "agatan_")
          ("x_auth_expires" . "0"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SKK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ddskk
  :config
  (require 'skk-vars)
  (autoload 'skk-mode "skk" nil t)
  (autoload 'skk-auto-fill-mode "skk" nil t)
  (autoload 'skk-isearch-mode-setup "skk-isearch" nil t)
  (autoload 'skk-isearch-mode-cleanup "skk-isearch" nil t)

  ;; Enterキーを押したときには確定する
  (setq skk-egg-like-newline t)
  ;; 辞書登録のとき， 余計な送り仮名を送らないようにする
  (setq skk-check-okurigana-on-touroku 'auto)
  ;; look コマンド を使った検索をする
  (setq skk-use-look t)
  ;; 半角数字
  (setq skk-number-style nil)
  ;; 句読点に. , を使う
  (setq skk-kutouten-type 'en)
  :bind
  ("C-x C-j" . skk-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :bind (("C-c a t" . org-agenda))
  :config
  (progn
    (setq org-directory "~/Dropbox/org/")
    (setq org-agenda-files (list org-directory))
    (setq org-src-fontify-natively t)))
(use-package open-junk-file
  :bind (("C-c j" . open-junk-file))
  :config
  (progn
    (setq open-junk-file-format "~/Dropbox/org/%Y-%m%d-memo.")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lnaguages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; Lisp
;;;;;;;;;;;;;;;;;;;;
(use-package paredit
  :defer t
  :config
  (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)

  (defun conditionally-enable-paredit-mode ()
    (if (eq this-command 'eval-expression)
        (paredit-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode))

(use-package eldoc
  :defer t
  :config
  (setq eldoc-delay 0.1
        eldoc-minor-mode-string ""))

(use-package rainbow-delimiters)

(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (require 'cl-lib)
  (require 'color)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 40))))

(defun my/rainbow-delimiters-mode-with-stronger-colors ()
  (rainbow-delimiters-mode 1)
  (rainbow-delimiters-using-stronger-colors))

(defun my/lisp-mode-defaults ()
  (paredit-mode 1)
  (my/rainbow-delimiters-mode-with-stronger-colors)
  (eldoc-mode 1))
(defun my/lisp-mode-hook ()
  (my/lisp-mode-defaults))

(add-hook 'emacs-lisp-mode-hook 'my/lisp-mode-hook)

;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.cljs$'" . clojure-mode))

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'my/lisp-mode-hook))

(use-package cider
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'my/lisp-mode-hook)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package cider-eval-sexp-fu)

(use-package clj-refactor
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c j"))


;;;;;;;;;;;;;;;;;;;;
;; common lisp
;;;;;;;;;;;;;;;;;;;;
(use-package auto-complete
  :config
  (require 'auto-complete-config)
  (ac-config-default))

(use-package ansi-color)

(when (executable-find "ros")
  (let ((slime-helper (expand-file-name "~/.roswell/lisp/quicklisp/slime-helper.el")))
    (unless (file-exists-p slime-helper)
      (shell-command "ros -Q -e '(ql:quickload :quicklisp-slime-helper)' -q"))
    (when (file-exists-p slime-helper)
      (load slime-helper)
      (setq inferior-lisp-program "ros -Q run")
      (slime-setup '(slime-fancy)))))

(defadvice slime-repl-emit (around slime-repl-ansi-colorize activate compile)
  (with-current-buffer (slime-output-buffer)
    (let ((start slime-output-start))
      (setq ad-return-value ad-do-it)
      (ansi-color-apply-on-region start slime-output-end))))

(add-to-list 'auto-mode-alist '("\\.ros$'" . slime-mode))
(add-hook 'slime-repl-mode-hook '(lambda () (my/lisp-mode-defaults)))
(add-hook 'slime-mode-hook #'(lambda () (my/lisp-mode-defaults)))

(use-package ac-slime
  :config
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac))


;;;;;;;;;;;;;;;;;;;;
;; c/c++
;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "k&r")
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 2)))

(add-to-list 'auto-mode-alist '("\\.hpp$'" . c++-mode))

;;;;;;;;;;;;;;;;;;;;
;; rust
;;;;;;;;;;;;;;;;;;;;
(use-package rust-mode
  :mode (("\\.rs$'" . rust-mode))
  :config
  (progn
    (use-package flycheck-rust)
    (use-package racer)
    (setq racer-cmd "~/rust/racer/target/release/racer")
    (setq racer-rust-src-path "~/rust/rust/src/")
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)))

;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;
(use-package haskell-mode
  :mode
  (("\\.hs\\'" . haskell-mode)
   ("\\.cabal\\'" . haskell-cabal-mode))
  :config
  (progn
    (use-package ghc)
    (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
    (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
    (add-hook 'haskell-mode-hook 'haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'font-lock-mode)
    (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)))

;;;;;;;;;;;;;;;;;;;;
;; OCaml
;;;;;;;;;;;;;;;;;;;;
(use-package tuareg
  :mode (("\\.ml" . tuareg-mode))
  :init
  (progn
    (add-hook 'tuareg-mode-hook
              '(lambda ()
                 (local-set-key "\C-c;" 'ocamlspot-query)
                 (local-set-key "\C-c:" 'ocamlspot-query-interface)
                 (local-set-key "\C-c'" 'ocamlspot-query-uses)
;                 (local-set-key "\C-c\C-t" 'ocamlspot-type)
                 (local-set-key "\C-c\C-i" 'ocamlspot-xtype)
                 (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
                 (local-set-key "\C-ct" 'caml-types-show-type)
                 (local-set-key "\C-cp" 'ocamlspot-pop-jump-stack)))
    (add-hook 'tuareg-mode-hook 'merlin-mode))
  :config
  (setq opam-share
        (substring
         (shell-command-to-string "opam config var share 2> /dev/null")
         0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
  (require 'utop)
  (require 'ocp-indent)
  (require 'ocamlspot)
  (require 'merlin))

;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; JavaScript
;;;;;;;;;;;;;;;;;;;;
(use-package js2-mode
  :mode (("\\.jsx$" . js2-jsx-mode))
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FINISHED!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "init.el loaded!!")

(add-hook 'after-init-hook
          (lambda ()
            (message "init time: %.3f sec"
                     (float-time (time-subtract after-init-time before-init-time)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (anything js2-mode tuareg ghc haskell-mode rust-mode ac-slime auto-complete clj-refactor cider-eval-sexp-fu cider clojure-mode rainbow-delimiters paredit open-junk-file ddskk twittering-mode volatile-highlights undo-tree git-gutter-fringe magit shell-pop anzu yasnippet flycheck projectile package-utils use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
