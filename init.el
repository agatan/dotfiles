;; -*- mode: emacs-lisp; coding: utf-8; indent-tabs-mode: nil -*-

(cd "~/")
(message (format "Startup time: %s" (format-time-string "%Y/%m/%d %H:%M:%S")))

;; Cask
(unless (require 'cask nil t)
  (require 'cask "~/.cask/cask"))
(cask-initialize)
(require 'pallet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(setq-default dibi-display-reordering nil)

;; 環境変数
(exec-path-from-shell-initialize)
(let ((envs '("PATH")))
  (exec-path-from-shell-copy-envs envs))

;; use-package
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))

;; eww (www browser on elisp)
(setq eww-search-prefix "https://www.google.co.jp/search?q=")

;; skk
(use-package skk
  :bind (("C-x j" . skk-mode))
  :config
  (progn
    (require 'skk-study)
    (setq default-input-method "japanese-skk")))

;; uniquify
(use-package uniquify
  :config
  (progn
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
    (setq uniquify-ignore-buffer-re "*[^*]+*")))

;; saveplace
(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-file (concat user-emacs-directory "places"))))

;; expand-region
(use-package expand-region
  :bind (("C-@" . er/expand-region)
         ("C-M-@" . er/contract-region))
  :config
  (transient-mark-mode t))

;; yasnippet
(use-package yasnippet
  :config
  (progn
    (setq yas-snippet-dirs
          '("~/.emacs.d/mySnippets"
            "~/.emacs.d/snippets"))
    (yas-global-mode 1)
    (define-key yas-minor-mode-map (kbd "C-x C-y") 'yas-insert-snippet)
    (define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
    (define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-erros-function #'flycheck-pos-tio-error-messages)))

;; Auto Complete
(use-package auto-complete
  :config
  (progn
    (require 'auto-complete-config)
    (ac-config-default)
    (add-to-list 'ac-sources 'ac-source-symbols)
    (add-to-list 'ac-sources 'ac-source-filename)))

;; helm
(use-package helm-config
  :config
  (progn
    (helm-mode 1)
    ;; ミニバッファでC-hをバックスペースに
    (define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)
    ;; TABで補完
    (define-key helm-read-file-map (kbd "<tab>") 'helm-execute-persistent-action)))

;; helm-themes
(use-package helm-themes
  :commands (helm-themes))

;; undo-tree
(use-package undo-tree
  :config
  (progn
    (global-undo-tree-mode t)
    (global-set-key (kbd "M-/") 'undo-tree-redo)))

;; smartparens
;; かっこを入力した時にその中身をハイライトしてくれるのがうざいので無効化
;(use-package smartparens-config
;  :config
;  (progn
;    (smartparens-global-mode t)
;    ;; `と'を自動補完対象から削除
;    (sp-pair "'" nil :actions :rem)
                                        ;    (sp-pair "`" nil :actions :rem)))
(electric-pair-mode t)

;; powerline
(use-package powerline
  :config
  (powerline-default-theme))

;; smooth-scroll
(use-package smooth-scroll
  :config
  (smooth-scroll-mode t))

;; yascroll
(use-package yascroll
  :config
  (global-yascroll-bar-mode 1))

;; popwin
(use-package popwin
  :config
  (progn
    (setq display-buffer-function 'popwin:display-buffer)
    (setq popwin:popup-window-position 'bottom)
    ;; popwinによる表示をするバッファ
    ;; Apropos
    (push '("*slime-apropos*") popwin:special-display-config)
    ;; Macroexpand
    (push '("*slime-macroexpansion*") popwin:special-display-config)
    ;; Help
    (push '("*slime-description*") popwin:special-display-config)
    ;; Compilation
    (push '("*slime-compilation*" :noselect t) popwin:special-display-config)
    ;; Cross-reference
    (push '("*slime-xref*") popwin:special-display-config)
    ;; Debugger
    (push '(sldb-mode :stick t) popwin:special-display-config)
    ;; REPL
    (push '(slime-repl-mode) popwin:special-display-config)
    ;; Connections
    (push '(slime-connection-list-mode) popwin:special-display-config)))

;; js2-mode
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)))

;; slime
(use-package slime
  :commands (slime)
  :config
  (progn
    (setq inferior-lisp-program "ccl")
    (slime-setup '(slime-repl slime-fancy slime-banner))
    (setq common-lisp-hyperspec-root
          "/usr/local/share/doc/hyperspec/HyperSpec/")
    (setq common-lisp-hyperspec-symbol-table
          (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
    (setq common-lisp-hyperspec-issuex-table
          (concat common-lisp-hyperspec-root "Data/Map_IssX.txt"))))
(use-package ac-slime
  :defer t
  :init
  (progn
    (add-hook 'slime-mode-hook 'set-up-slime-ac)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)))


;; emmet
(use-package emmet-mode
  :defer t
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))))
  :config
  (progn
    (define-key emmet-mode-keymap (kbd "C-j") nil)
    (setq emmet-move-cursor-between-quotes t)
    (define-key emmet-mode-keymap (kbd "C-c C-j") 'emmet-expand-line)))

;; web-mode
(use-package web-mode
  :mode (("\\.php\\'" . web-mode)
         ("\\.html?\\'" . web-mode)))

;; twitter
(use-package twittering-mode
  :commands (twittering-mode)
  :config
  (progn
    (setq twittering-account-authorization 'authorized)
    (setq twittering-oauth-access-token-alist
          '(("oauth_token" . "2266023746-F0kQaxHYXWt7q596SMotb2rPfYCXNsIKDFGCjBm")
            ("oauth_token_secret" . "qll8TjpFRcrTdRViB3h86DTVj8XPFviQBRG6c5BVa3x1K")
            ("user_id" . "2266023746")
            ("screen_name" . "_agatan")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; カラーテーマ
(load-theme 'ample t)


;; スタートアップ非表示
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; ツールバー非表示
(tool-bar-mode -1)
;; メニューバーを非表示
;; (menu-bar-mode -1)
;; スクロールバー非表示
(set-scroll-bar-mode nil)
(scroll-bar-mode -1)

;; 行番号
(global-linum-mode t)
(setq linum-format "%4d")

(column-number-mode 1)

;; 対応するカッコの表示
(show-paren-mode t)

;; 現在行の強調
;(global-hl-line-mode)

;; カーソルの位置が何文字目かを表示する
(column-number-mode t)

;; 行末の空白を強調
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")
;; ブラウザやシェルでは行末の空白強調を無効に
(defun ag-disable-show-trailing-hook ()
  "disable `show-trailing-whitespace`"
  (setq show-trailing-whitespace nil))
(defvar ag-disable-trailing-modes
  `(eshell-mode
    eww-mode
    tuareg-interactive-mode))
(mapc
 (lambda (mode) (add-hook (intern (concat (symbol-name mode) "-hook"))
                          'ag-disable-show-trailing-hook))
 ag-disable-trailing-modes)

;; 日本語フォント
(set-fontset-font
  nil 'japanese-jisx0208
  (font-spec :family "Hiragino Kaku Gothic ProN"))

;; 時間を表示
(display-time)

;; 画像ファイルを表示
(auto-image-file-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 日本語入力
(setq default-input-method "MacOSX")


;; cmdキーをmetaに
; (setq ns-command-modifier (quote meta))
; (setq ns-alternate-modifier (quote super))


;; ctrl-hをバックスペースに
(global-set-key "\C-h" 'backward-delete-char)

;; タブをスペースに
(setq-default indent-tabs-mode nil)
;; タブ幅
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(flycheck-display-erros-function (function flycheck-pos-tio-error-messages))
 '(haskell-tags-on-save t)
 '(org-agenda-files
   (quote
    ("/Users/nao/Dropbox/org/todo.org" "/Users/nao/Dropbox/org/memo.org")))
 '(tab-width 4))

;; 行の先頭でC-kを押したら行全体を削除
(setq kill-whole-line t)

;; 最終行を\nに
(setq require-final-newline t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @その他
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; バックアップファイルを作らない
(setq baskup-inhibited t)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'clojure-mode-hook 'cider-mode)

;; mini bufferに関数の引数を表示
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; C-x b時に *nrepl-connection* と *nrepl-server* を非表示
(setq nrepl-hide-special-buffers t)

;; REPLのbuffer名をproject名+port番号にする
(setq nrepl-buffer-name-show-port t)

;; autocomplete (cider-jack-inが必要)
(autoload 'ac-nrepl "ac-nrepl" nil t)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;
;; @OCaml
;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; load-path
(when (file-exists-p "~/.opam/4.01.0/lib/ocamlspot/")
  (setq load-path (cons "~/.opam/4.01.0/lib/ocamlspot/" load-path))

  (require 'ocamlspot)

                                        ; tuareg mode hook (use caml-mode-hook instead if you use caml-mode)
  (add-hook 'tuareg-mode-hook
            '(lambda ()
               (local-set-key "\C-c;" 'ocamlspot-query)
               (local-set-key "\C-c:" 'ocamlspot-query-interface)
               (local-set-key "\C-c'" 'ocamlspot-query-uses)
               (local-set-key "\C-c\C-t" 'ocamlspot-type)
               (local-set-key "\C-c\C-i" 'ocamlspot-xtype)
               (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
               (local-set-key "\C-ct" 'caml-types-show-type)
               (local-set-key "\C-cp" 'ocamlspot-pop-jump-stack)))

                                        ; set the path of the ocamlspot binary. If you did make opt, ocamlspot.opt is recommended.
  (setq ocamlspot-command "~/.opam/4.01.0/bin/ocamlspot.opt")

                                        ; Optional: You can also change overlay colors as follows:
                                        ;  (set-face-background 'ocamlspot-spot-face "#660000")
                                        ;  (set-face-background 'ocamlspot-tree-face "#006600")

  ;; utop
  (setq load-path (cons "~/.opam/4.01.0/build/utop.1.17/src/top/" load-path))
  (autoload 'utop "utop" "Toplevel for OCaml" t))

;;;;;;;;;;;;;;;;;;;
;; @haskell
;;;;;;;;;;;;;;;;;;;
(use-package haskell-mode
  :mode (("\\.hs\\'" . haskell-mode))
  :init
  (progn
    (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    (add-hook 'haskell-mode-hook 'font-lock-mode)
    (add-hook 'haskell-mode-hook 'imenu-add-menubar-index))
  :config
  (progn
    (require 'haskell-cabal)
    ;; ghc-mod
    (add-to-list 'exec-path (concat (getenv "HOME") "/.cabal/bin"))
    (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
      (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
      (add-to-list  'exec-path my-cabal-path))

    (autoload 'ghc-init "ghc" nil t)
    (autoload 'ghc-debug "ghc" nil t)

    (add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))

    (require 'company)
    (add-hook 'haskell-mode-hook 'company-mode)
    (add-to-list 'company-backends 'company-ghc)))


;;;;;;;;;;;;;;;;;;
;; @C lang
;;;;;;;;;;;;;;;;;;
(setq c-basic-offset 4)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(setq auto-mode-alist
      (cons '("\\.h$" . c++-mode) auto-mode-alist))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

;;;;;;;;;;;;;;;
;; @Sphinx
;;;;;;;;;;;;;;;
(use-package rst
  :mode (("\\.rst$" . rst-mode)
         ("\\.rest$" . rst-mode))
  :init
  (add-hook 'rst-mode-hook '(lambda() (setq indent-tabs-mode nil)))
  :config
  (setq frame-background-mode 'dark))


;;;;;;;;;;;;;;
;; @rust
;;;;;;;;;;;;;;
(when (file-exists-p "~/rust/racer/target/release/racer")
  (setq racer-rust-src-path "/Users/nao/rust/rust/src")
  (setq racer-cmd "/Users/nao/rust/racer/target/release/racer")
  (add-to-list 'load-path "/Users/nao/rust/racer/editors")
  (require 'racer)
  (add-hook 'rust-mode-hook 'company-mode))

;;;;;;;;;;;;;
;; @org
;;;;;;;;;;;;;
(require 'ox-latex)
(require 'ox-bibtex)

(setq org-latex-pdf-process
      '("platex %f"
        "platex %f"
        "bibtex %b"
        "platex %f"
        "platex %f"
        "dvipdfmx %b.dvi"))
(setq org-latex-with-hyperref nil)

(add-to-list 'org-latex-classes
             '("thesis"
               "\\documentclass{jarticle}
                [NO-PACKAGES]
                [NO-DEFAULT-PACKAGES]
                \\usepackage[dvipdfmx]{graphicx}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; open-junk-file
(use-package open-junk-file
  :bind (("\C-x a" . open-junk-file))
  :config
  (setq open-junk-file-format "~/Documents/org/%Y-%m%d-%H%M%S."))

(require 'org)
(setq org-directory "~/Dropbox/org")
(setq org-agenda-files (list org-directory))
(setq org-src-fontify-natively t)


;; capture templates
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline (expand-file-name "~/Dropbox/org/todo.org")
                                         "Inbox")
         "** TODO %?\n  %i\n  %a\n  %T")
        ("m" "memo" entry (file (expand-file-name "~/Dropbox/org/memo.org"))
         "* %?\n  %i\n  %a\n  %T")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
