;;;;;;;;;;;;;;;;;;
;; Custom packages

(setq my-package-list '(zenburn-theme
                        helm
                        buffer-move
                        multi-term
                        neotree
                        ))

(require 'package)
(setq package-archives '(("melpa stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;; install
(dolist (i-package my-package-list)
  (unless (package-installed-p i-package)
    (package-install i-package)))


;;;;;;;;;
;; System

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/backup" t)))

;; use default shell PATH
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "GOPATH")

(global-set-key (kbd "<f9>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

;;;;;;;;;;;;;;;;
;; Look and feel

(load-theme 'atom-one-dark t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq-default cursor-type 'box)
(blink-cursor-mode 0)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

(global-set-key [f11] 'switch-full-screen)

;; highlighting
(global-linum-mode t)
(global-visual-line-mode t)
(show-paren-mode 1)
(delete-selection-mode t)

(if window-system (global-hl-line-mode t))
(set-face-foreground 'highlight nil)
(set-face-attribute hl-line-face nil :underline nil :box nil)

(add-hook 'term-mode-hook (lambda ()
                                    (setq-local global-hl-line-mode
                                                nil)
                                    (linum-mode -1)))

;(set-face-attribute 'isearch nil :box "white" :background nil)
;(set-face-attribute 'lazy-highlight nil :box "white" :background nil)
;(set-face-attribute 'isearch nil :box "white" :background nil)
;(set-face-attribute 'lazy-highlight nil :box "white" :background nil)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 101)

;; keyboard ergonomics
  
;; buffer movement shortcuts
(windmove-default-keybindings 'meta)
;(global-set-key (kbd "M-left")  'windmove-left)
;(global-set-key (kbd "M-right") 'windmove-right)
;(global-set-key (kbd "M-up")    'windmove-up)
;(global-set-key (kbd "M-down")  'windmove-down)

(require 'buffer-move)
(setq buffer-move-behavior 'move)
(global-set-key (kbd "<M-C-up>")     'buf-move-up)
(global-set-key (kbd "<M-C-down>")   'buf-move-down)
(global-set-key (kbd "<M-C-left>")   'buf-move-left)
(global-set-key (kbd "<M-C-right>")  'buf-move-right)

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)
(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

  
;;;;;;;;;;;;
;; Utilities

(require 'multi-term)
(setq multi-term-program "/bin/bash")

(require 'helm)
(require 'helm-config)
;(require 'helm-fzf)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(setq helm-split-window-in-side-p t)
(setq helm-autoresize-max-height 20)
(setq helm-autoresize-min-height 0)
(helm-autoresize-mode 1)

(helm-mode 1)

(setq helm-buffers-truncate-lines t)
(setq helm-buffer-max-length 20)

(require 'ido)
(ido-mode t)

(require 'neotree)
(global-set-key (kbd "C-c C-d") 'neotree-toggle)
(setq neo-theme 'arrow)
(setq auto-hscroll-mode nil)
(add-hook 'neo-after-create-hook
   #'(lambda (_)
       (with-current-buffer (get-buffer neo-buffer-name)
         (setq truncate-lines t)
         (setq word-wrap nil)
         (make-local-variable 'auto-hscroll-mode)
         (setq auto-hscroll-mode nil))))

;;;;;;;;;;;;;
;;; Go config

(add-to-list 'load-path (concat (getenv "GOPATH")  "/bin"))
(defun go-mode-setup ()
  "Go mode setup from google."
  (setq compile-command "go build -v && go test -v && go vet && golint && errcheck")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'before-save-hook 'go-remove-unused-imports)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports))
(add-hook 'go-mode-hook 'go-mode-setup)

;; Autocompletion
(ac-config-default)
(require 'auto-complete-config)
(require 'go-autocomplete)

;; Linter
(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake"))
                                        ;(require 'go-flymake)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

  
;; Golint (M-x golint)
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
;(require 'golint)

;; Project explorer
;(require 'project-explorer)
;(global-set-key (kbd "M-e") 'project-explorer-toggle)


;;;;;;;;;;;;;;;;;;;;;;;
;; Custom set variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck neotree multi-term buffer-move zenburn-theme project-explorer helm go-eldoc go-autocomplete exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
