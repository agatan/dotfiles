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
(use-package jbeans-theme ;; :disabled t
  :config
  (load-theme 'jbeans t))


(use-package doom-themes :disabled t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-vibrant t))


(use-package doom-modeline
  :custom
  (doom-modelin-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-vcs-max-length 24)
  :hook (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package beacon
  :custom
  (beacon-color "yellow")
  :config
  (beacon-mode 1))

(global-linum-mode)


(use-package elscreen
  :defer nil
  :bind (("C-<tab>" . elscreen-next)
         ("C-S-<tab>" . elscreen-previous))
  :config
  (setq elscreen-prefix-key (kbd "C-c w"))
  (setq elscreen-display-tab nil)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (elscreen-start)
  (defun elscreen-display-toggle ()
    (interactive)
    (setq elscreen-display-tab (not elscreen-display-tab))))

(use-package helm-elscreen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;; ido
;; (use-package ido
;;   :config
;;   (ido-mode t)
;;   (ido-everywhere t)
;;   (setq ido-enable-flex-matching t))

;; (use-package ido-grid-mode
;;   :config
;;   (ido-grid-mode t))

;; (use-package smex
;;   :bind (("M-x" . 'smex)))

;; (use-package ido-completing-read+
;;   :config
;;   (ido-ubiquitous-mode t))

;; (use-package flx-ido
;;   :config
;;   (flx-ido-mode))

;; ;; ido-ghq is only on github.
;; (straight-use-package '(ido-ghq :type git :host github :repo "uwabami/ido-ghq"))
;; (use-package ido-ghq
;;   :config
;;   (setq ido-ghq-short-list t)
;;   :ensure-system-package ghq)


;;; helm
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c o" . helm-occur))
  :init
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t))


(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package helm-ghq
  :defer t
  :commands 'helm-ghq)

(use-package helm-rg
  :defer t
  :commands 'helm-rg)
;;; << helm

(use-package shackle
  :config
  (shackle-mode)
  (setq helm-display-function 'pop-to-buffer)
  (setq shackle-rules
        '(
          ("\\*helm" :regexp t :align below :ratio 0.4)
          )))

(use-package iflipb
  :bind (("C-)" . iflipb-next-buffer)
         ("C-(" . iflipb-previous-buffer))
  :config
  (setq iflipb-wrap-around t))

(use-package all-the-icons)

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
  (push 'company-lsp company-backends)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode t)
  (setq company-quickhelp-delay 0.1))

(use-package projectile
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching t))

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

(use-package magit)
(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package bind-key)
(bind-key "C-h" 'delete-backward-char)
(winner-mode t)
(bind-key "C-z" 'winner-undo)
(bind-key "C-M-z" 'winner-redo)
;; For macos
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; lsp-mode
(use-package lsp-mode
  :custom
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental)
  (lsp-response-timeout 5)
  (lsp-enable-completion-at-point nil)
  (lsp-prefer-flymake 'flymake)
  (create-lockfiles nil)
  :hook
  (prog-major-mode . lsp-prog-major-mode-enable)
  (ruby-mode-hook . lsp)
  (python-mode-hook . lsp)
  :config
  (use-package lsp-ui
    :after lsp-mode
    :config
    (setq lsp-ui-doc-use-childframe t)
    (setq lsp-ui-doc-use-webkit t)
    (setq lsp-ui-flycheck-enable nil)
    :hook (lsp-mode . lsp-ui-mode)
    :bind
    (:map lsp-mode-map
          ("C-c C-r" . lsp-ui-peek-find-references)
          ("C-c C-d" . lsp-ui-peek-find-definitions)
          ("C-c i" . lsp-ui-peek-find-implementation)))

  (use-package company-lsp
    :after (lsp-mode company yasnippet)
    :defines company-backends
    :functions company-backend-with-yas
    :init (cl-pushnew (company-backend-with-yas 'company-lsp) company-backends)
    :custom
    (company-lsp-cache-candidates t))

  (use-package dap-mode
    :after lsp-mode
    :defer t
    :config
    (dap-mode t)
    (dap-ui-mode t)))


;;; Python
(use-package pipenv
  :hook
  (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))


;;; markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init
  (setq markdown-command "multimarkdown"))


;;; golang
(use-package company-go)
(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :init
  (add-hook 'go-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'gofmt-before-save)
                            (local-set-key (kbd "M-.") 'godef-jump)
                            (setq indent-tabs-mode nil)
                            (setq tab-width 4)
                            (setq c-basic-offset 4)
                            (set (make-local-variable 'company-backends) '(company-go))
                            (add-to-list 'company-backends 'company-go)))
  (setq gofmt-command "goimports"))


;; protobuf
(use-package protobuf-mode
  :mode (("\\.proto\\'" . protobuf-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :commands org-agenda
  :mode (("\\.org\\'" . org-mode))
  :init
  (setq org-directory "~/Dropbox/org/")
  (setq org-default-notes-file (concat org-directory "main.org"))
  (setq org-agenda-files (list org-directory))
  (defun my:org-goto-inbox ()
    (interactive)
    (find-file org-default-notes-file))
  :config
  (setq org-hide-leading-stars t)
  (setq org-startup-folded nil)
  ;; org-capture
  (setq org-todo-keywords '((sequence "TASK(t)" "WAIT(w)" "|" "DONE(d)" "ABORT(a)" "SOMEDAY(s)")))
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline org-default-notes-file "inbox")
           "** TASK %?\n   CREATED: %U\n")
          ("i" "Idea" entry (file+headline org-default-notes-file "idea")
           "** %?\n   CREATED: %U\n")))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
  ;; org-clock
  (setq org-log-done 'time)
  (setq org-clock-clocked-in-display 'frame-title)
  ;; org-agenda
  (setq org-agenda-custom-commands
        '(("x" "Unscheduled Tasks" tags-todo
           "-SCHEDULED>=\"<now>\"" nil)
          ("d" "Daily Tasks" agenda ""
           ((org-agenda-span 1)))))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-return-follows-link t)  ;; RET to follow link
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0900 1200 1300 1800) "......" "----------------"))
  (setq org-columns-default-format
        "%68ITEM(Task) %6Effort(Effort){:} %6CLOCKSUM(Clock){:}")
  (defadvice org-agenda-switch-to (after org-agenda-close)
    "Close a org-agenda window when RET is hit on the window."
    (progn (delete-other-windows)
           (recenter-top-bottom)))
  (ad-activate 'org-agenda-switch-to)
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c g" . org-clock-goto)
   ("C-c i" . my:org-goto-inbox)
   :map org-mode-map
   ("C-m" . org-return-indent)
   ("M-n" . org-forward-heading-same-level)
   ("M-p" . org-backward-heading-same-level)))
