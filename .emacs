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

;; Add path
(setq user-emacs-directory "~/.emacs.d")
(add-to-list 'load-path (concat user-emacs-directory "/mine"))
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
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; Enable Evil
(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)
(evil-set-leader 'normal " ")
(evil-define-key 'normal 'global
  ;; ---- mimic my vim mappings ----- ;;
  (kbd "<leader>ff") 'counsel-find-file
  (kbd "<leader>fr") 'counsel-recentf
  (kbd "<leader>q") 'kill-buffer
  (kbd "<leader> SPC q") 'delete-window
  (kbd "<leader>w") 'save-buffer
  (kbd "<leader>v") 'split-window-right
  (kbd "M-h") 'windmove-left
  (kbd "M-j") 'windmove-down
  (kbd "M-k") 'windmove-up
  (kbd "M-l") 'windmove-right
  )


;; Enable evil org
(straight-use-package 'evil-org)
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)
;; -- adapted from evil-org-mode -- ;;
(defun evil-org-agenda-custom-keys ()
  "Set motion state keys for `org-agenda'."
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-define-key 'motion org-agenda-mode-map
    (kbd "M-h") 'windmove-left
    (kbd "M-j") 'windmove-down
    (kbd "M-k") 'windmove-up
    (kbd "M-l") 'windmove-right
    ))
(evil-org-agenda-custom-keys)
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

;; Private Settings
(setq custom-file "~/.emacs-custom.el")
     (load custom-file)
;; Custom config files
(require 'mine-org-late)

;; Personal Settings which require no packages
(set-default 'truncate-lines t)
;; Maximize Emacs on Startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; show cursor position within line
(column-number-mode 1)
;; hide toolbar and scroll bar
(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))
;; (global-display-line-numbers-mode) ;; Not required if using relative mode
(show-paren-mode 1)
(setq make-backup-files nil)
;; Follow symlinks
(setq vc-follow-symlinks t)
;; Show current cursor line
(global-hl-line-mode +1)
;; Set Font
(setq default-frame-alist '((font . "Fira Mono-12")))
;; ORG MODE
(global-set-key "\C-ca" 'org-agenda)
;; Org habit column
(setq org-habit-graph-column 80)
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))
(setq org-agenda-show-future-repeats nil)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-use-time-grid nil)
(setq org-agenda-custom-commands
      '(;; match those are not scheduled, are not DONE.
        ("iu" "unscheduled TOTO tasks" tags "-SCHEDULED={.+}/+TODO|+STARTED|+WAITING")
        ;; match those are not scheduled, are not DONE.
        ("iU" "Unscheduled tasks with no TODO" tags "-SCHEDULED={.+}-TODO={.+}")
	("O" "Old - No Recent Acitvity"
         ((tags "-TODO={DONE}"
                ((org-agenda-overriding-header "Tasks Not acted Upon Recently")
                 (org-agenda-skip-function '(+org/has-child-and-last-update-before 30)))))
         nil nil)
        ))
(setq org-agenda-span 7
      org-agenda-start-on-weekday nil
      ;org-agenda-start-day "-3d"
      )

