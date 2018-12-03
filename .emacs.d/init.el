;;; Code:
(when load-file-name
  (setq user-emacs-directory (expand-file-name
                              (file-name-directory load-file-name))))

(defun open-my-init-file ()
  (interactive)
  (find-file user-init-file))

(defalias 'yes-or-no-p 'y-or-n-p)

;; install straight.el (copy from https://github.com/raxod502/straight.el)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(eval-when-compile
  (require 'use-package))

(use-package use-package-ensure-system-package)

(use-package better-defaults)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package jbeans-theme
  :config
  (load-theme 'jbeans t))

(global-linum-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t))

(use-package ido-grid-mode
  :config
  (ido-grid-mode t))

(use-package smex
  :bind (("M-x" . 'smex)))

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode t))

(use-package flx-ido
  :config
  (flx-ido-mode))

;; ido-ghq is only on github.
(straight-use-package '(ido-ghq :type git :host github :repo "uwabami/ido-ghq"))
(use-package ido-ghq
  :config
  (setq ido-ghq-short-list t)
  :ensure-system-package ghq)

(use-package iflipb
  :bind (("<C-tab>" . iflipb-next-buffer)
         ("<C-S-tab>" . iflipb-previous-buffer))
  :config
  (setq iflipb-wrap-around t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :init
  (setq company-selection-wrap-around t)
  :bind
  (:map company-active-map
	("M-n" . nil)
	("M-p" . nil)
	("C-n" . company-select-next)
	("C-p" . company-select-previous))
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode t)
  (setq company-quickhelp-delay 0.1))

(use-package projectile
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(defun my/recentf-find-file ()
  (interactive)
  (find-file (ido-completing-read "Find recent file: " recentf-list)))
(use-package recentf
  :config
  (recentf-mode))
(bind-key "C-x C-r" 'my/recentf-find-file)
(setq recentf-max-saved-items 200)

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package smartparens
  :config
  (smartparens-global-mode)
  (require 'smartparens-config))

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindinggs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For macos
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Python
(defun goto-begining-of-block (&optional n)
  "Move to the N previous block."
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (if (search-backward-regexp "\n[\t ]*\n[\t ]*\n" nil t n)
        (goto-char (match-end 0))
      (goto-char (point-min)))))

(defun goto-previous-block (&optional n)
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (search-backward-regexp "\n[\t ]*\n[\t ]*\n" nil t n)
    (goto-begining-of-block)))

(defun goto-end-of-block (&optional n)
  "Move to the N next block."
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (if (search-forward-regexp "\n[\t ]*\n[\t ]*\n" nil t n)
        (goto-char (match-beginning 0))
      (goto-char (point-max)))))

(defun goto-next-block (&optional n)
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (search-forward-regexp "\n[\t ]*\n[\t ]*\n" nil t 1)))

(defun elpy-shell-send-current-block ()
  "Send current block to Python shell."
  (interactive)
  (let ((pos (point)))
    (goto-begining-of-block)
    (push-mark nil t t)
    (goto-end-of-block)
    (elpy-shell-send-region-or-buffer)
    (pop-mark)
    (goto-char pos)))

(use-package elpy
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :bind (:map elpy-mode-map
              ("C-c c" . elpy-shell-send-current-block)
              ("C-c b n" . goto-next-block)
              ("C-c b p" . goto-previous-block))
  :config
  (elpy-enable)
  (elpy-mode))

(defun my/enable-project-root-venv ()
  (let* ((root (projectile-project-root))
	 (target (expand-file-name "venv" root)))
    (if (file-directory-p target)
	(progn
	  (message "venv enabled: %s" target)
	  (pyvenv-activate target)
          (setq python-shell-interpreter "ipython"
                python-shell-interpreter-args "-i --simple-prompt")))))
(when (require 'projectile nil t)
  (add-hook 'window-configuration-change-hook #'my/enable-project-root-venv))


;;; markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (gfm-mode)
  :init
  (setq markdown-command "multimarkdown"))
