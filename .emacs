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

;;(straight-use-package 'dracula-theme)
;; (load-theme 'dracula t)
;; (straight-use-package 'zenburn-theme)
;; (load-theme 'zenburn t)
;; (straight-use-package 'solarized-theme)
;; (load-theme 'solarized-dark t)
;; (setq solarized-high-contrast-mode-line t)
;; (straight-use-package 'gotham-theme)
;; (load-theme 'gotham t)
(straight-use-package 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
(doom-themes-org-config)
;; (straight-use-package 'tangotango-theme)
;; (load-theme 'tangotango t)

;; Helm
(straight-use-package 'helm)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key "\C-x\ \C-r" 'helm-recentf)
(global-set-key (kbd "M-x") #'helm-M-x)

;; Enable Evil
(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)
(evil-set-leader 'normal " ")
;; https://github.com/Wilfred/.emacs.d/blob/gh-pages/init.el
(evil-define-key 'normal 'global
  ;; ---- * leader-??? * ---- ;;
  (kbd "<leader>fr") 'helm-recentf
  (kbd "<leader>q") 'kill-buffer
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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/shared_folders/minimal/Pensieve/textfiles/journal/WorkNote.org" "~/shared_folders/minimal/Pensieve/textfiles/journal/Done.org" "~/shared_folders/minimal/Pensieve/textfiles/journal/Today.org" "~/shared_folders/minimal/Pensieve/textfiles/journal/MurNote.org")))
  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; Personal Settings which require no packages
;; hide toolbar and scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode)
(show-paren-mode 1)
(setq make-backup-files nil)
;; Follow symlinks
(setq vc-follow-symlinks t) 
(global-hl-line-mode +1)
(setq default-frame-alist '((font . "Fira Mono-13")))
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

