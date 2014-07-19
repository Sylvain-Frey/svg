;; install custom packages
(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(package-initialize)


(defvar custom-packages
  '(auto-indent-mode 
    better-defaults 
    bs-ext 
    nrepl clojure-mode cljdoc clojure-test-mode cider 
    haskell-mode 
    ido-ubiquitous idomenu 
    minimap 
    paredit 
    pkg-info 
    epl 
    dash 
    popup 
    pretty-mode-plus 
    rainbow-delimiters 
    scala-mode 
    smex 
    sml-modeline
    sublime-themes))

(dolist (p custom-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes (quote ("cc7ca8a9325f5b68b49b6beba8b27eb23bb80d7af6f9a3cfd9cd5457e0291269" default)))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))

(defun switch-full-screen ()
      (interactive)
      (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(add-hook 'clojure-mode-hook 'typed-clojure-mode)

(setq-default cursor-type 'bar)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/backup" t)))

(global-set-key [f11] 'switch-full-screen)
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "<C-tab>") 'other-window)

(global-linum-mode t)
(global-visual-line-mode t)
(show-paren-mode 1)
(delete-selection-mode t)

;; cider customisation
(setq nrepl-hide-special-buffers t)
(setq cider-auto-select-error-buffer nil)
(setq cider-stacktrace-fill-column 80)
(setq cider-show-error-buffer nil)
(setq cider-repl-print-length 100)
(setq cider-prompt-save-file-on-load nil)


(load-theme 'wilson t)
