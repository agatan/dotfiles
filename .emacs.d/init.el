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

  (setq vc-follow-symlinks t))


(leaf *Visual
  :config
  (setq visible-bell nil)
  (when (every (lambda (n) (member n (font-family-list))) '("Hiragino Kaku Gothic ProN" "JetBrains Mono"))
    (create-fontset-from-ascii-font "JetBrains Mono:slant=normal" nil "monokakugo")
    (set-fontset-font "fontset-monokakugo" 'unicode (font-spec :family "Hiragino Kaku Gothic ProN") nil 'append)
    (add-to-list 'default-frame-alist '(font . "fontset-monokakugo")))

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
    :hook ((after-init-hook . doom-modeline-mode))))


(leaf *Navigation
  :config
  (leaf posframe :ensure t)
  (leaf ivy
    :ensure t
    :blackout ivy-mode
    :custom ((ivy-use-virtual-buffers . t)
	     (ivy-truncate-lines . nil)
	     (ivy-wrap . t)
	     (ivy-mode . t)
	     (counsel-mode . t))
    :custom-face
    (ivy-current-match . '((t (:background "#557b7b"))))
    :init
    (leaf *ivy-requirements
      :config
      (leaf swiper
	:ensure t
	:bind (([remap isearch-forward-regexp] . swiper)))
      (leaf counsel
	:ensure t
	:blackout counsel-mode
	:bind (("C-x C-r" . counsel-recentf))))
    (leaf ivy-rich
      :ensure t
      :after ivy
      :custom ((ivy-rich-mode . t))
      :init
      (leaf all-the-icons-ivy-rich
        :ensure t
        :after ivy-rich
        :custom ((all-the-icons-ivy-rich-mode . t))))
    (leaf ivy-posframe
      :after ivy
      :ensure t
      :custom-face (ivy-posframe . '((t (:background "#224b4b"))))
      :custom ((ivy-posframe-mode . t)
	       (ivy-posframe-height-alist . '((swiper . 30) (t . 40)))
	       (ivy-posframe-display-functions-alist
		. '((swiper . ivy-display-function-fallback)
		    (t . ivy-posframe-display-at-frame-center)))))))


(leaf *Keybindings
  :config
  (when (eq system-type 'darwin)
    (setq ns-command-modifier 'meta))

  (leaf *global-key-maps
    :bind
    (("C-h" . delete-backward-char)))

  (leaf which-key
    :ensure t
    :custom ((which-key-idle-delay . 0.5)
             (which-key-mode . t))
    :init
    (leaf which-key-posframe
      :ensure t
      :after which-key
      :custom-face (which-key-posframe . '((t (:background "#224b4b"))))
      :custom ((which-key-posframe-mode . t)))))


(leaf *Edit
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
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
  (leaf flycheck
    :ensure t
    :config
    (global-flycheck-mode)
    (leaf flycheck-posframe
      :ensure t
      :after flycheck
      :custom-face
      (flycheck-posframe-face . '((t (:foreground "#4a4a4a" :background "#5d4a4f"))))
      :hook ((flycheck-mode-hook . flycheck-posframe-mode))))
  (leaf company
    :ensure t
    :custom
    ((company-selection-wrap-around . t)
     (company-idle-delay . 0)
     (company-minimum-prefix-length . 2)
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
    :custom ((projectile-project-search-path . '("~/repos/src/github.com/agatan" "~/repos/src/github.com/wantedly/"))
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
    :commands lsp
    :init
    (leaf *lsp-basis
      :config
      (leaf lsp-ui
        :ensure t
        :after lsp-mode))
    (leaf *lsp-languages
      :config
      (leaf lsp-python-ms
        :ensure t
        :after lsp-mode
        :hook ((python-mode-hook . (lambda ()
                                     (require 'lsp-python-ms)
                                     (lsp)))))))

  (leaf *LanguageSupport
    :config
    (leaf dockerfile-mode
      :ensure t)

    (leaf markdown-mode
      :ensure t)

    (leaf go-mode
      :ensure t
      :custom ((gofmt-command . "goimports"))
      :hook (go-mode-hook . (lambda ()
                              (add-hook 'before-save-hook 'gofmt-before-save)
                              (lsp))))))


(provide 'init)
;;; init.el ends here
