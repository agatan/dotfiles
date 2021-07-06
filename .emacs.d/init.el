;;; init.el --- My init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; My init.el.


;;; Code:

;; this enables the following running command.
;;   emacs -q -l /path/to/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
	  (expand-file-name
	   (file-name-directory (or load-file-name byte-compile-current-file))))))


(prog1 "leaf installation: https://qiita.com/conao3/items/347d7e472afd0c58fbd7"
  (eval-and-compile
    (customize-set-variable
     'package-archives '(("org"   . "https://orgmode.org/elpa/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("gnu"   . "https://elpa.gnu.org/packages/")))
    (package-initialize)
    (unless (package-installed-p 'leaf)
      (package-refresh-contents)
      (package-install 'leaf))

    (leaf leaf-keywords
      :ensure t
      :init
      ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
      (leaf hydra :ensure t)
      (leaf el-get :ensure t)
      (leaf blackout :ensure t)

      :config
      ;; initialize leaf-keywords.el
      (leaf-keywords-init))))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
	     (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))


;; Start server for emacsclient
(leaf server
  :require t
  :when (memq window-system '(mac ns x))
  :commands (server-running-p server-start)
  :config
  (unless (server-running-p)
    (server-start)))


(leaf *Basis
  :config
  (defun open-my-init-file ()
    (interactive)
    (find-file user-init-file))
  (defalias 'yes-or-no-p 'y-or-n-p)

  (leaf better-defaults
    :ensure t
    :require t)

  (leaf exec-path-from-shell
    :ensure t
    :require t
    :config
    (when (executable-find "zsh")
      (custom-set-variables '(exec-path-from-shell-shell-name "zsh")))
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))

  ;; Stop dumping custom-variables to init.el
  (leaf cus-edit
    :doc "tools forr customizing Emacs and List packages"
    :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

  ;; restart-emacs command
  (leaf restart-emacs
    :ensure t)

  (recentf-mode t)

  (setq vc-follow-symlinks t)
  (setq gc-cons-threshold 12800000)
  (setq read-process-output-max (* 1024 1024)))


(leaf *Visual
  :config
  (setq visible-bell nil)
  (when (cl-every (lambda (n) (member n (font-family-list))) '("Hiragino Sans" "JetBrains Mono"))
    (create-fontset-from-ascii-font "JetBrains Mono:slant=normal" nil "monohiragino")
    (set-fontset-font "fontset-monohiragino" 'unicode (font-spec :family "Hiragino Sans") nil 'append)
    (add-to-list 'default-frame-alist '(font . "fontset-monohiragino")))

  (leaf window-divider
    :custom-face (window-divider . '((t (:foreground "#4a4a4a"))))
    :custom ((window-divider-default-right-width . 8)
             (window-divider-mode . t)))

  (leaf all-the-icons
    :ensure t)

  (leaf doom-themes
    :ensure t
    :custom ((doom-themes-enable-italic . t)
	     (doom-themes-enable-bold . t)
             (doom-theme . 'doom-vibrant))
    :hook ((after-init-hook . (lambda () (load-theme 'doom-vibrant t)))))
  (leaf doom-modeline
    :ensure t
    :custom ((doom-modeline-unicode-fallback . t))
    :hook ((after-init-hook . doom-modeline-mode)))

  ;; 非アクティブな window を暗くする
  (leaf dimmer
    :ensure t
    :disabled t
    :config
    (dimmer-configure-company-box)
    (dimmer-configure-hydra)
    (dimmer-configure-posframe)
    (dimmer-configure-which-key)
    (dimmer-mode t)))


(leaf *Navigation
  :config
  (leaf posframe :ensure t)

  (leaf consult
    :ensure t
    :custom ((consult-project-root-function . (lambda ()
                                                (when-let (project (project-current))
                                                  (project-root project))))))
  (leaf vertico
    :ensure t
    :custom ((vertico-cycle . t)
             (vertico-count . 20))
    :init
    (leaf orderless :ensure t)
    (leaf savehist :ensure t)
    (leaf marginalia :ensure t)
    (leaf affe :ensure t
      :bind (("C-x f" . affe-find))
      :custom ((affe-regexp-function . #'orderless-pattern-compiler)
               (affe-highlight-function . #'orderless--highlight)
               (affe-find-command . "fd --color=never --full-path --no-ignore --hidden")))
    (leaf consult-ghq :ensure t)
    :config
    (vertico-mode)
    (marginalia-mode)
    (savehist-mode)
    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion))))

    (defun my-consult-line (&optional at-point)
      "Consult-line uses things-at-point if set C-u prefix."
      (interactive "P")
      (if at-point
          (consult-line (thing-at-point 'symbol))
        (consult-line)))
    (global-set-key (kbd "C-s") 'my-consult-line)
    (global-set-key [remap goto-line] 'consult-goto-line)
    (global-set-key (kbd "C-x b") 'consult-buffer))

  (leaf ace-window
    :ensure t
    :bind (("C-M-o" . ace-window))
    :custom
    ((aw-keys . '(?j ?k ?l ?i ?o ?h ?y ?u ?p)))
    :custom-face
    ((aw-leading-char-face . '((t (:height 3.0 :foreground "red")))))))


(leaf *Keybindings
  :config
  (when (eq system-type 'darwin)
    (setq ns-command-modifier 'meta))

  (leaf *global-key-maps
    :bind
    (("C-h" . delete-backward-char)))

  (leaf which-key
    :ensure t
    :custom ((which-key-idle-time . 0.5)
             (which-key-mode . t))))

(leaf *Edit
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (leaf undo-tree
    :ensure t
    :custom ((global-undo-tree-mode . t)))

  (leaf smartparens :ensure t
    :require smartparens-config
    :bind ((:smartparens-mode-map
	    ;; basis
	    ("C-M-f" . sp-forward-sexp)
	    ("C-M-b" . sp-backward-sexp)
	    ("C-M-n" . sp-next-sexp)
	    ("C-M-p" . sp-previous-sexp)
	    ("C-M-a" . sp-beginning-of-sexp)
	    ("C-M-e" . sp-end-of-sexp)))
    :custom
    ((smartparens-global-mode . t)))
  (leaf yasnippet
    :ensure t
    :custom ((yas-global-mode . t))
    :init
    (leaf yasnippet-snippets
      :ensure t
      :after yasnippet))
  (leaf flycheck
    :ensure t
    :config
    (global-flycheck-mode)
    (leaf flycheck-posframe
      :ensure t
      :disabled t
      :after flycheck
      :custom-face
      (flycheck-posframe-face . '((t (:foreground "#4a4a4a" :background "#5d4a4f"))))
      :hook ((flycheck-mode-hook . flycheck-posframe-mode))))
  (leaf company
    :ensure t
    :custom
    ((company-selection-wrap-around . t)
     (company-idle-delay . 0.4)
     (company-minimum-prefix-length . 2)
     (company-dabbrev-downcase . nil)
     (global-company-mode . t))
    :bind
    (:company-active-map
     ("M-n" . nil)
     ("M-p" . nil)
     ("C-n" . company-select-next)
     ("C-p" . company-select-previous))
    :config
    (leaf company-box
      :ensure t
      :after company all-the-icons
      :hook ((company-mode-hook . company-box-mode))
      :custom ((company-box-icons-alist . 'company-box-icons-all-the-icons))))

  (leaf projectile
    :ensure t
    :custom ((projectile-project-search-path . '("~/repos/src/github.com/agatan" "~/repos/src/github.com/bw-company/"))
             (projectile-mode . t))
    :bind (("C-c p" . projectile-command-map))
    :init
    (leaf counsel-projectile
      :ensure t
      :after projectile
      :custom ((counsel-projectile-mode . t))))

  (leaf lsp-mode
    :ensure t
    :hook ((lsp-mode-hook . lsp-enable-which-key-integration))
    :custom ((lsp-completion-provider . :capf)
             (lsp-response-time . 0.5))
    :commands lsp
    :init
    (leaf *lsp-basis
      :config
      (leaf lsp-ui
        :ensure t
        :custom-face (lsp-ui-doc-background . '((t (:background "#224a4a"))))
        :after lsp-mode))
    (leaf *lsp-languages
      :config
      (leaf *python
        :custom ((python-shell-interpreter . "ipython")
                 (python-shell-interpreter-args . "-i --simple-prompt --InteractiveShell.display_page=True")))
      (leaf lsp-python-ms
        :ensure t
        :hook ((python-mode-hook . (lambda ()
                                     (require 'lsp-python-ms)
                                     (lsp)))))))

  (leaf *LanguageSupport
    :config
    (leaf dockerfile-mode
      :ensure t)

    (leaf markdown-mode
      :ensure t)

    (leaf csv-mode
      :ensure t)

    (leaf yaml-mode
      :ensure t)

    (leaf go-mode
      :ensure t
      :custom ((gofmt-command . "goimports"))
      :hook (go-mode-hook . (lambda ()
                              (add-hook 'before-save-hook 'gofmt-before-save)
                              (lsp))))

    (leaf protobuf-mode
      :ensure t)

    (leaf rustic
      :ensure t)

    (leaf *OCaml
      :config
      (leaf tuareg
        :ensure t
        :hook (tuareg-mode-hook . lsp)))))


(provide 'init)
;;; init.el ends here
