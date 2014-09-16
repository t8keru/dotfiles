;; --------------------------------------------------
;; @ Cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(set-face-attribute 'default nil
		    :family "Inconsolata" ;; font
		    :height 120)    ;; font size

(setq make-backup-files nil)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message "")

(tool-bar-mode 0)

(keyboard-translate ?\C-h ?\C-?)

(setq default-tab-width 4)
(global-linum-mode t)

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 3) ;; keyboard scroll one line at a time
;; Delay updates to give Emacs a chance for other changes
(setq linum-delay t)
;; scrolling to always be a line at a time
(setq scroll-conservatively 10000)

; server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))

(defun my-kill-emacs ()
  (interactive)
  (when (y-or-n-p "really kill emacs")
    (save-buffers-kill-terminal)))

(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

;; --------------------------------------------------
;; @ auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; --------------------------------------------------
;; @ Helm
(when (require 'helm-config nil t)
  (helm-mode 1)

  (define-key global-map (kbd "C-x C-f") 'helm-for-files)
  (define-key global-map (kbd "M-x")     'helm-M-x)
;;  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list)

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

  ;; Disable helm in some functions
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern)))))))

(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

;; Emulate `kill-line' in helm minibuffer
(setq helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  "Emulate `kill-line' in helm minibuffer"
  (kill-new (buffer-substring (point) (field-end))))

;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)


;; --------------------------------------------------
;; @ powerline
(require 'powerline)
(powerline-default-theme)
;; (set-face-attribute 'mode-line nil
;;                     :foreground "#fff"
;;                     :background "#660066"
;;                     :box nil)

;; (set-face-attribute 'powerline-active1 nil
;;                     :foreground "#fff"
;;                     :background "#6633Z99"
;;                     :inherit 'mode-line)

;; (set-face-attribute 'powerline-active2 nil
;;                     :foreground "#000"
;;                     :background "#ffffff"
;;                     :inherit 'mode-line)

;; --------------------------------------------------
;; @ rainbow delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
(custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d")))))

;; --------------------------------------------------
;; @ color theme
;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-snow)

;; --------------------------------------------------
;; @ helm theme
;; ???

;; --------------------------------------------------
;; @ linum relative
(require 'linum-relative)
(linum-on)

;; --------------------------------------------------
;; @ Golang
(defvar my/helm-go-source
  '((name . "Helm Go")
    (candidates . (lambda ()
                    (cons "builtin" (go-packages))))
    (action . (("Show document" . godoc)
               ("Import package" . my/helm-go-import-add)))))

(defun my/helm-go-import-add (candidate)
  (dolist (package (helm-marked-candidates))
    (go-import-add current-prefix-arg package)))

(defun my/helm-go ()
  (interactive)
  (helm :sources '(my/helm-go-source) :buffer "*helm go*"))

(eval-after-load "go-mode"
  '(progn
     (require 'go-autocomplete)
     (add-hook 'go-mode-hook 'go-eldoc-setup)

     ;; key bindings
     (define-key go-mode-map (kbd "M-.") 'godef-jump)
     (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)))

;; --------------------------------------------------
;; @ Haskell
(require 'haskell-mode)
(require 'haskell-cabal)

;; (autoload 'haskell-mode "haskell-mode")
;; (autoload 'haskell-cabal "haskell-cabal")
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))
(setq haskell-program-name "/usr/local/bin/ghci")

;; ghc-mod
(add-to-list 'exec-path (concat (getenv "HOME") "/.cabal/bin"))
(add-to-list 'load-path "~/.cabal/share/x86_64-freebsd-ghc-7.8.3/ghc-mod-5.0.1.1")
(autoload 'ghc-init "ghc")
(ghc-init)
(add-to-list 'ac-sources 'ac-source-ghc-mod)
