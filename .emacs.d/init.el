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


(leaf *Basis
  :config
  (defun open-my-init-file ()
    (interactive)
    (find-file user-init-file))
  (defalias 'yes-or-no-p 'y-or-n-p)

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
    :init
    (leaf *ivy-requirements
      :config
      (leaf swiper
	:ensure t
	:bind (([remap isearch-forward] . swiper)))
      (leaf counsel
	:ensure t
	:blackout counsel-mode
	:bind (("C-x C-r" . counsel-recentf))))
    (leaf ivy-posframe
      :after ivy
      :ensure t
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
    (("C-h" . delete-backward-char))))


(leaf *Edit
  :config
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
     ("C-p" . company-select-previous))))

(provide 'init)
;;; init.el ends here
