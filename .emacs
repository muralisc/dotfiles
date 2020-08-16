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

;; Theme 0
;; (straight-use-package 'doom-themes)
;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-one t)
;;   (doom-themes-visual-bell-config)
;; (doom-themes-org-config)
;; Theme 1
(straight-use-package 'zenburn-theme)
(load-theme 'zenburn t)

;; Helm
;; (straight-use-package 'helm)
;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
;; (global-set-key "\C-x\ \C-r" 'helm-recentf)
;; (global-set-key (kbd "M-x") #'helm-M-x)
;; (helm-mode 1)
;; (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
;; (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
;; (define-key helm-map (kbd "C-z") #'helm-select-action)
;; ;; Helm at bottom only
;; (add-to-list 'display-buffer-alist
;;                     `(,(rx bos "*helm" (* not-newline) "*" eos)
;;                          (display-buffer-in-side-window)
;;                          (inhibit-same-window . t)
;;                          (window-height . 0.4)))

;; Ivy
(straight-use-package 'ivy)
(straight-use-package 'swiper)
(straight-use-package 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
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
  (kbd "<leader>m") 'counsel-recentf
  (kbd "<leader>q") 'kill-buffer
  (kbd "<leader> SPC q") 'delete-window
  (kbd "<leader>w") 'save-buffer
  (kbd "<leader>v") 'split-window-right
  (kbd "M-h") 'windmove-left
  (kbd "M-j") 'windmove-down
  (kbd "M-k") 'windmove-up
  (kbd "M-l") 'windmove-right
  )
;; -- adapted from evil-org-mode -- ;;
(straight-use-package 'evil-org)
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
;; Use my custome mappings adapted from:
;; (require 'evil-org-agenda)
;; (evil-org-agenda-set-keys)
(defun evil-org-agenda-set-keys ()
  "Set motion state keys for `org-agenda'."
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-define-key 'motion org-agenda-mode-map
    (kbd " m") 'helm-recentf
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    "t" 'org-agenda-todo
    (kbd "M-h") 'windmove-left
    (kbd "M-j") 'windmove-down
    (kbd "M-k") 'windmove-up
    (kbd "M-l") 'windmove-right
    ))
(evil-org-agenda-set-keys)
(defun org-reset-check-on-repeat ()
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

;; Key Chord
(straight-use-package 'key-chord)
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map  "jj" 'evil-normal-state)
(setq key-chord-two-keys-delay 0.2)


;; Relative Line Numbers
(straight-use-package 'nlinum-relative)
(require 'nlinum-relative)
(nlinum-relative-setup-evil)
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
(setq nlinum-relative-redisplay-delay 0)
(setq nlinum-relative-current-symbol "")
(setq nlinum-relative-offset 0)
(global-nlinum-relative-mode)

(setq custom-file "~/.emacs-custom.el")
     (load custom-file)

;; Personal Settings which require no packages
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
(setq org-habit-graph-column 60)
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))
(setq org-agenda-show-future-repeats nil)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-use-time-grid nil)
(setq org-agenda-custom-commands
      `(;; match those are not scheduled, are not DONE.
        ("iu" "unscheduled TOTO tasks" tags "-SCHEDULED={.+}/+TODO|+STARTED|+WAITING")
        ;; match those are not scheduled, are not DONE.
        ("iU" "Unscheduled tasks with no TODO" tags "-SCHEDULED={.+}-TODO={.+}")
        ))
(setq org-agenda-span 7
      org-agenda-start-on-weekday nil
      ;org-agenda-start-day "-3d"
      )
