;;; .emacs --- Custom emacs configuration of Murali
;;; Commentary:
;;;
;;; Code:

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

(straight-use-package 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-vibrant t)
(doom-themes-org-config)

;; Smart Mode Line
(straight-use-package 'smart-mode-line)
(setq sml/theme 'respectful)
(setq sml/no-confirm-load-theme t)
(sml/setup)

;; Undo Tree
(straight-use-package 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; For getting correct path
(straight-use-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(straight-use-package 'lsp-ivy)

;;Org Journal
(straight-use-package 'org-journal)
(require 'org-journal)
(setq org-journal-file-type 'yearly)

;;Ledger
(straight-use-package 'ledger-mode)
(require 'ledger-mode)
(setq ledger-post-auto-align t)

;; Flycheck
(straight-use-package 'flycheck)
(global-flycheck-mode)
;; LSP
(straight-use-package 'lsp-mode)
(require 'lsp-mode)
(setq gc-cons-threshold 100000000)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(straight-use-package 'treemacs)
(straight-use-package 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)

;; Ivy
(straight-use-package 'ivy)
(straight-use-package 'swiper)
(straight-use-package 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; Magit
(straight-use-package 'magit)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

;; Projectile
(straight-use-package 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Enable Evil
(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)

;; Enable evil org
(straight-use-package 'evil-org)
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
;; (require 'evil-org-agenda) Deactivating as follow mode does not work
(setq org-agenda-start-with-follow-mode t)

;; Which Key
(straight-use-package 'which-key)
(which-key-mode)

;; Helpful
(straight-use-package 'helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;; Evil Escape
(straight-use-package 'evil-escape)
(require 'evil-escape)
(evil-escape-mode +1)
(setq evil-escape-excluded-states '(normal visual evilified emacs motion)
      evil-escape-excluded-major-modes '(treemacs-mode)
      evil-escape-key-sequence "jj"
      evil-escape-delay 0.20)

;; Evil Magit
(straight-use-package 'evil-magit)
(require 'evil-magit)

;; General
(straight-use-package 'general)
(require 'general)

;; company-mode
(straight-use-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(general-define-key
 :keymaps '(global)
 :states  '(normal emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "SPC" 'counsel-M-x

 "a"  'org-agenda
 "c"  'org-capture
 "ff" 'counsel-find-file
 "fr" 'counsel-recentf
 "fs" 'save-buffer
 "fq" 'kill-buffer

 "gs" 'magit-status
 "gm" 'magit-dispatch

 "hf" #'helpful-callable
 "hv" #'helpful-variable
 "hk" #'helpful-key

 "p"  'projectile-command-map

 "wd" 'delete-window
 "wh" 'windmove-left
 "wj" 'windmove-down
 "wk" 'windmove-up
 "wl" 'windmove-right
 "wv" 'split-window-right
 "/"  #'projectile-grep
 )

;; Private Settings
(setq custom-file "~/.emacs-private.el")
(load custom-file)
;; Shared Settings
(setq custom-file "~/.emacs.d/.emacs-shared.el")
(load custom-file)

