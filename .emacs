;;; .emacs --- Custom emacs configuration of Murali
;;; Commentary:

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

;; Custom faces
(custom-set-faces
 '(org-scheduled-previously ((t (:foreground "#d74b4b"))))
 '(org-scheduled-today ((t (:foreground "#fffaf9" :italic t))))
 '(org-upcoming-deadline ((t (:foreground "#8595ff")))))

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
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(defun org-reset-check-on-repeat ()
  "When a repeating task is marked todo. Reset all the check boxes."
  (when (and (org-get-repeat) (member org-state org-done-keywords))
    (org-reset-checkbox-state-subtree)))
(add-hook 'org-after-todo-state-change-hook 'org-reset-check-on-repeat)

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
(setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
      evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
      evil-escape-key-sequence "jj"
      evil-escape-delay 0.20)

;; nlinum-relative --- Relative Line Numbers
(straight-use-package 'nlinum-relative)
(require 'nlinum-relative)
(nlinum-relative-setup-evil)
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
(setq nlinum-relative-redisplay-delay 0)
(setq nlinum-relative-current-symbol "")
(setq nlinum-relative-offset 0)
(global-nlinum-relative-mode)

;; Evil Magit
(straight-use-package 'evil-magit)
(require 'evil-magit)

;; General
(straight-use-package 'general)
(require 'general)

(general-define-key
 :keymaps '(global)
 :states  '(normal emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "SPC" 'counsel-M-x

 "ff" 'counsel-find-file
 "fr" 'counsel-recentf
 "fs" 'save-buffer
 "fq" 'kill-buffer

 "gg" 'magit-status
 "g/" 'magit-dispatch

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
 "/"  #'projectile-ripgrep
 )

;; Private Settings
(setq custom-file "~/.emacs-private.el")
(load custom-file)
;; Shared Settings
(setq custom-file "~/.emacs.d/.emacs-shared.el")
(load custom-file)

;; Wrap around
(set-default 'truncate-lines t)
;; show cursor position within line
(column-number-mode 1)
;; hide toolbar and scroll bar
(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))
;; No menu bar in command line mode
(menu-bar-mode -1)
;; (global-display-line-numbers-mode) ;; Not required if using nlinum-relative
(show-paren-mode 1)
;; Dont make backup files
(setq make-backup-files nil)
;; Show current cursor line
(global-hl-line-mode +1)
;; Set Font
(setq default-frame-alist '((font . "Fira Mono-12")))
