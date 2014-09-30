;; --------------------------------------------------
;; @ common lisp
;; (require 'cl-lib)

;; --------------------------------------------------
;; @ site-lisp
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;; --------------------------------------------------
;; @ Cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(set-face-attribute 'default nil
		    :family "Inconsolata" ;; font
		    :height 100)    ;; font size


;; --------------------------------------------------
;; @ hl-line+
(when (require 'hl-line+)
  (toggle-hl-line-when-idle))

;; --------------------------------------------------
;; @ auto-complete
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
  (defvar helm-delete-minibuffer-contents-from-point t)
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
(defvar helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  "Emulate `kill-line' in helm minibuffer"
  (kill-new (buffer-substring (point) (field-end))))

;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

;; --------------------------------------------------
;; @ eshell
(eval-after-load "eshell-mode"
  '(progn
     (require 'pcomplete)
     (add-to-list 'ac-modes 'eshell-mode)
     (ac-define-source pcomplete
       '((candidates . pcomplete-completions)))
     (setq ac-sources
           '(ac-source-pcomplete
             ac-source-filename
             ac-source-files-in-current-dir
             ac-source-words-in-buffer
             ac-source-dictionary))
     (define-key eshell-mode-map (kbd "C-i") 'auto-complete)
     (define-key eshell-mode-map [(tab)] 'auto-complete)
     (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
     (define-key eshell-mode-map (kbd "M-n") 'helm-esh-pcomplete)
     ))

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


  (defconst global-color1 "#FF6699")
  (defconst global-color3 "#CDC0B0")
  (defconst global-color2 "#FF0066")
  (defconst global-color4 "#CDC0B0")

  (defvar arrow-right-1 (create-image (arrow-right-xpm global-color1 global-color2) 'xpm t :ascent 'center))
  (defvar arrow-right-2 (create-image (arrow-right-xpm global-color2 "None") 'xpm t :ascent 'center))
  (defvar arrow-left-1  (create-image (arrow-left-xpm global-color2 global-color1) 'xpm t :ascent 'center))
  (defvar arrow-left-2  (create-image (arrow-left-xpm "None" global-color2) 'xpm t :ascent 'center))

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
              :background global-color1)

  (make-face 'mode-line-color-2)
  (set-face-attribute 'mode-line-color-2 nil
              :foreground "#fff"
              :background global-color2)

  (set-face-attribute 'mode-line nil
              :foreground "#fff"
              :background global-color3
              :box nil)
  (set-face-attribute 'mode-line-inactive nil
              :foreground "#fff"
              :background global-color4)
  )

;; --------------------------------------------------
;; @ rainbow-delimiters
(when (require 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
  ;; (global-rainbow-delimiters-mode t)
  ;; (custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d"))))))

;; --------------------------------------------------
;; @ color-theme
(when (require 'color-theme)
  (color-theme-initialize)
  (color-theme-charcoal-black))

;; --------------------------------------------------
;; @ git-gutter+
(global-git-gutter+-mode t)

;; --------------------------------------------------
;; @ linum-relative
;; (when (require 'linum-relative)
;;   (linum-on))
(defvar linum-format "%5d | ")

;; --------------------------------------------------
;; @ flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (eval-after-load 'flycheck
;;   '(custom-set-variables
;;    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; --------------------------------------------------
;; @ Golang
;; (defvar my/helm-go-source
;;   '((name . "Helm Go")
;;     (candidates . (lambda ()
;;                     (cons "builtin" (go-packages))))
;;     (action . (("Show document" . godoc)
;;                ("Import package" . my/helm-go-import-add)))))

;; (defun my/helm-go-import-add (candidate)
;;   (dolist (package (helm-marked-candidates))
;;     (go-import-add current-prefix-arg package)))

;; (defun my/helm-go ()
;;   (interactive)
;;   (helm :sources '(my/helm-go-source) :buffer "*helm go*"))

(eval-after-load "go-mode"
  '(progn
     (require 'go-autocomplete)
     (add-hook 'go-mode-hook 'go-eldoc-setup)
     (add-hook 'before-save-hook 'gofmt-before-save)
     ;; key bindings
     (define-key go-mode-map (kbd "M-.") 'godef-jump)
     (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)
     ;; display
     (setq tab-width 4)
     (setq indent-tabs-mode 1)
     ))



;; --------------------------------------------------
;; @ Python
(eval-after-load 'python-mode
     '(progn
        (require 'epc)
        (require 'python)

        (define-key python-mode-map (kbd "<C-tab>") 'jedi:complete)
        (setq python-indent-guess-indent-offset 4)

        (flycheck-mode 1)

        ))

(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(setq jedi:complete-on-dot t)
(setq jedi:key-show-doc (kbd "C-c D"))

;; --------------------------------------------------
;; @ Haskell
(autoload 'haskell-mode "haskell-mode")
(autoload 'haskell-cabal "haskell-cabal")
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))
(defvar haskell-program-name "C:/Program Files/Haskell Platform/2014.2.0.0/bin/ghci.exe")

;; ghc-mod
(add-to-list 'exec-path "C:/Users/midiu_000/AppData/Roaming/cabal/bin")
;; (add-to-list 'load-path "~/.cabal/share/ghc-mod-3.1.3")
(autoload 'ghc-init "ghc")
(add-hook 'haskell-mode-hook
  (lambda () (ghc-init)))
(add-to-list 'ac-sources 'ac-source-ghc-mod)

(ac-define-source ghc-mod
  '((depends ghc)
    (candidates . (ghc-select-completion-symbol))
    (symbol . "s")
    (cache)))

(defun my-ac-haskell-mode ()
  (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-dictionary ac-source-ghc-mod)))
(add-hook 'haskell-mode-hook 'my-ac-haskell-mode)

(defun my-haskell-ac-init ()
  (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
    (auto-complete-mode t)
    (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-dictionary ac-source-ghc-mod))))

(add-hook 'find-file-hook 'my-haskell-ac-init)

;; --------------------------------------------------
;; @ server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))

;; --------------------------------------------------
;; @ lang
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
;; (setq buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)

;; --------------------------------------------------
;; @ font
(prin1 (font-family-list))
(set-face-attribute 'default nil
                    :family "Inconsolata" ;; font
                    :height 100)    ;; font size

(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Migu 1M"))

(setq face-font-rescale-alist
      '(("Migu 1M" . 1.2)))

;; backup off
(setq make-backup-files nil)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
;; (setq initial-scratch-message "")

;; tool bar disable
(tool-bar-mode 0)

;; C-h trans Back
(keyboard-translate ?\C-h ?\C-?)

;; line number
(global-linum-mode t)

;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

;; keyboard scroll one line at a time
(setq scroll-step 3)

;; scrolling to always be a line at a time
(setq scroll-conservatively 10000)

;; highlight parentheses.
(show-paren-mode t)

;; highlight trail space.
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; confirm kill
(defun my-kill-emacs ()
  (interactive)
  (when (y-or-n-p "really kill emacs")
    (save-buffers-kill-terminal)))
(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

(global-set-key (kbd "RET") 'newline-and-indent)

(setq indent-tabs-mode nil)

(blink-cursor-mode nil)

(setq tab-width 4)

(tool-bar-mode 0)

(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))

(global-whitespace-mode 1)

(defvar my/bg-color "#232323")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)
