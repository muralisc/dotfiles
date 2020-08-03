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
(straight-use-package 'helm)
(straight-use-package 'solarized-theme)
(straight-use-package 'dracula-theme)
(straight-use-package 'ace-window)

;; Helm settings
(setq helm-recentf-fuzzy-match t
      helm-buffers-fuzzy-matching t)

;; Personal Prefs
; (load-theme 'solarized-dark t)
(load-theme 'dracula t)
(global-linum-mode 1)
(show-paren-mode 1)
(setq vc-follow-symlinks t)  ;; Follow symlinks
(setq org-agenda-show-future-repeats nil)
(global-hl-line-mode +1)

(add-to-list 'default-frame-alist '(height . 68))
(add-to-list 'default-frame-alist '(width . 230))

(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/shared_folders/minimal/Pensieve/textfiles/journal/Done.org" "~/shared_folders/minimal/Pensieve/textfiles/journal/Today.org" "~/shared_folders/minimal/Pensieve/textfiles/journal/MurNote.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Org Mode
(global-set-key "\C-ca" 'org-agenda)
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))
(setq org-agenda-custom-commands 
      '(("r" "Routine" agenda "USE" ;; (1) (2) (3) (4)
         ((org-agenda-files '("~/shared_folders/minimal/Pensieve/textfiles/journal/Routine.org")))
	 )))

;; Access Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(setq org-agenda-skip-scheduled-if-done t)
(global-set-key (kbd "C-x C-r") #'helm-recentf)

;; Easy window access
(global-set-key (kbd "M-o") 'ace-window)
