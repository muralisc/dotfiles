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

;; Theme 1
;; (straight-use-package 'dracula-theme)
;; (load-theme 'dracula t)
;; Theme 1
;; (straight-use-package 'zenburn-theme)
;; (load-theme 'zenburn t)
;; Theme 1
;; (straight-use-package 'solarized-theme)
;; (load-theme 'solarized-dark t)
;; (setq solarized-high-contrast-mode-line t)
;; Theme 1
;; (straight-use-package 'gotham-theme)
;; (load-theme 'gotham t)
;; Theme 1
(straight-use-package 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
(doom-themes-org-config)
;; Theme 1
;; (straight-use-package 'tangotango-theme)
;; (load-theme 'tangotango t)
;; Theme 1
;; (straight-use-package 'spacemacs-theme)
;; (load-theme 'spacemacs-dark t)

;; Helm
(straight-use-package 'helm)
;; Allow to use Tab completion in Helm

(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key "\C-x\ \C-r" 'helm-recentf)
(global-set-key (kbd "M-x") #'helm-M-x)
(helm-mode 1)
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") #'helm-select-action)

;; Enable Evil
(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)
(evil-set-leader 'normal " ")
;; https://github.com/Wilfred/.emacs.d/blob/gh-pages/init.el
(evil-define-key 'normal 'global
  ;; ---- * leader-??? * ---- ;;
  (kbd "<leader>m") 'helm-recentf
  (kbd "<leader>q") 'kill-buffer
  (kbd "<leader> SPC q") 'delete-window
  (kbd "<leader>w") 'save-buffer
  (kbd "<leader>v") 'split-window-right
  (kbd "M-h") 'windmove-left
  (kbd "M-j") 'windmove-down
  (kbd "M-k") 'windmove-up
  (kbd "M-l") 'windmove-right
  ;; Window Size
  (kbd "<up>") 'shrink-window
  (kbd "<down>") 'enlarge-window
  (kbd "<left>") 'shrink-window-horizontally
  (kbd "<right>") 'enlarge-window-horizontally
  )

;; Evil org
(straight-use-package 'evil-org)
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;; Which Key
(straight-use-package 'which-key)
(which-key-mode)

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
;; hide toolbar and scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)
;; (global-display-line-numbers-mode)
(show-paren-mode 1)
(setq make-backup-files nil)
;; Follow symlinks
(setq vc-follow-symlinks t) 
(global-hl-line-mode +1)
(setq default-frame-alist '((font . "Fira Mono-14")))
;; Org Mode
(global-set-key "\C-ca" 'org-agenda)
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))
(setq org-agenda-show-future-repeats nil)
(setq org-agenda-skip-scheduled-if-done t)
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
