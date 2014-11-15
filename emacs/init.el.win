;; --------------------------------------------------
;; @ Package
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'cl)

(defvar installing-package-list
  '(
    f s
    expand-region
    htmlize
    idle-highlight-mode
    multiple-cursors
    nyan-mode
    prodigy
    rainbow-delimiters
;;    use-package
;;    projectile
;;    pallet
;;    smex
    
    popwin
    popup
    browse-kill-ring
    powerline
    color-theme
    hl-line+
    
    auto-complete

    helm
    helm-git-grep
    helm-ls-git
    helm-themes
    helm-gtags
    helm-pydoc
    helm-go-package
    
    git-gutter+
    magit

    yasnippet

    flycheck
    pylint
    jedi
    epc
    deferred

    ruby-mode
    scala-mode
    markdown-mode
    scss-mode
;;    haskell-mode
    google-c-style
    yaml-mode
    
    go-mode
    go-autocomplete
    go-eldoc
    go-direx
    go-errcheck
    

    ))

(let ((not-installed (loop for x in installing-package-list
			   when (not (package-installed-p x))
			   collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

;; --------------------------------------------------
;; @ lang
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)

;; --------------------------------------------------
;; @ font
(set-face-attribute 'default nil
		    :family "Inconsolata" ;; font
		    :height 110)    ;; font size

(create-fontset-from-ascii-font
 "-outline-メイリオ-normal-r-normal-normal-12-*-*-*-*-*-iso8859-1"
 nil "メイリオ")

(create-fontset-from-ascii-font
 "-outline-メイリオ-normal-r-normal-normal-14-*-*-*-*-*-iso8859-1"
 nil "メイリオ")

(set-fontset-font "fontset-メイリオ"
 'japanese-jisx0208
 '("メイリオ*" . "jisx0208-sjis"))
(set-fontset-font "fontset-メイリオ"
 'katakana-jisx0201
 '("メイリオ*" . "jisx0201-katakana"))

;; backup off
(setq make-backup-files nil)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
;; (setq initial-scratch-message "")
;; tool bar disable
(tool-bar-mode 0)
;; C-h trans Back
(keyboard-translate ?\C-h ?\C-?)
;; tab width size
(setq tab-width 4)
;; line number
(global-linum-mode t)
;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)
;; keyboard scroll one line at a time
(setq scroll-step 3)
;; Delay updates to give Emacs a chance for other changes
(setq linum-delay t)
;; scrolling to always be a line at a time
(setq scroll-conservatively 10000)
;; 
(show-paren-mode t)
(display-time)
;; confirm kill
(defun my-kill-emacs ()
  (interactive)
  (when (y-or-n-p "really kill emacs")
    (save-buffers-kill-terminal)))
(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

;; --------------------------------------------------
;; hl-line+
(when (require 'hl-line+)
  (toggle-hl-line-when-idle))
;; --------------------------------------------------
;; @ auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)

(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(set-face-background 'ac-candidate-face "#e1e8ed")
(set-face-underline 'ac-candidate-face "#aabb99")
(set-face-background 'ac-selection-face "#ff6699")
(set-face-background 'popup-summary-face "#e1e8ed")
(set-face-foreground 'popup-summary-face "#555555")  ;; 候補のサマリー部分
(set-face-background 'popup-tip-face "#20b39f")  ;; ドキュメント部分
(set-face-foreground 'popup-tip-face "#ffffff")

;; --------------------------------------------------
;; @ Helm
(when (require 'helm-config nil t)
  (helm-mode 1)

  (define-key global-map (kbd "C-x C-f") 'helm-for-files)
  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x f") 'helm-find-files)
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
(when (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

;; --------------------------------------------------
;; @ powerline
(when (require 'powerline)
  (powerline-default-theme)
  (defun arrow-right-xpm (color1 color2)
    "Return an XPM right arrow string representing."
    (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"  color1 color2))

  (defun arrow-left-xpm (color1 color2)
    "Return an XPM right arrow string representing."
    (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"  color2 color1))


  (defconst color1 "#FF6699")
  (defconst color3 "#CDC0B0")
  (defconst color2 "#FF0066")
  (defconst color4 "#CDC0B0")

  (defvar arrow-right-1 (create-image (arrow-right-xpm color1 color2) 'xpm t :ascent 'center))
  (defvar arrow-right-2 (create-image (arrow-right-xpm color2 "None") 'xpm t :ascent 'center))
  (defvar arrow-left-1  (create-image (arrow-left-xpm color2 color1) 'xpm t :ascent 'center))
  (defvar arrow-left-2  (create-image (arrow-left-xpm "None" color2) 'xpm t :ascent 'center))

  (setq-default mode-line-format
		(list  '(:eval (concat (propertize " %b " 'face 'mode-line-color-1)
				       (propertize " " 'display arrow-right-1)))
		       '(:eval (concat (propertize " %m " 'face 'mode-line-color-2)
				       (propertize " " 'display arrow-right-2)))

		       ;; Justify right by filling with spaces to right fringe - 16
		       ;; (16 should be computed rahter than hardcoded)
		       '(:eval (propertize " " 'display '((space :align-to (- right-fringe 17)))))

		       '(:eval (concat (propertize " " 'display arrow-left-2)
				       (propertize " %p " 'face 'mode-line-color-2)))
		       '(:eval (concat (propertize " " 'display arrow-left-1)
				       (propertize "%4l:%2c  " 'face 'mode-line-color-1)))
		       )) 

  (make-face 'mode-line-color-1)
  (set-face-attribute 'mode-line-color-1 nil
		      :foreground "#fff"
		      :background color1)

  (make-face 'mode-line-color-2)
  (set-face-attribute 'mode-line-color-2 nil
		      :foreground "#fff"
		      :background color2)

  (set-face-attribute 'mode-line nil
		      :foreground "#fff"
		      :background color3
		      :box nil)
  (set-face-attribute 'mode-line-inactive nil
		      :foreground "#fff"
		      :background color4)
  )

;; --------------------------------------------------
;; @ rainbow-delimiters
(when (require 'rainbow-delimiters)
  (global-rainbow-delimiters-mode t)
  (custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d"))))))

;; --------------------------------------------------
;; @ color-theme
(when (require 'color-theme)
  (color-theme-initialize)
  (color-theme-standard))

;; --------------------------------------------------
;; @ linum-relative
(when (require 'linum-relative)
  (linum-on))

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
;; @ Python
(require 'python)
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(define-key python-mode-map (kbd "<C-tab>") 'jedi:complete)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(defun tnoda/turn-on-flycheck-mode ()
  (flycheck-mode 1))
(add-hook 'python-mode-hook 'tnoda/turn-on-flycheck-mode)

;; --------------------------------------------------
;; @ server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))
