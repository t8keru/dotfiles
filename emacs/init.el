;; --------------------------------------------------
;; @ common lisp
;; (require 'cl)

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
(set-face-foreground 'popup-summary-face "#555555") ;; 候補のサマリー部分
(set-face-background 'popup-tip-face "#20b39f") ;; ドキュメント部分
(set-face-foreground 'popup-tip-face "#ffffff")

;; --------------------------------------------------
;; @ Helm
(when (require 'helm-config nil t)
  (helm-mode 1)
  (require 'helm-files)

  (define-key global-map (kbd "C-x C-f") 'helm-for-files)
  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "C-x f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i") 'helm-imenu)
  (define-key global-map (kbd "C-x b") 'helm-buffers-list)
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
                      (concat ".*" input-pattern))))))

  ;; List files in git repos
  ;; From @ http://d.hatena.ne.jp/syohex/20120718/1342620627 >>>
  (defun helm-c-sources-git-project-for (pwd)
    (loop for elt in
          '(("Modified files" . "--modified")
            ("Untracked files" . "--others --exclude-standard")
            ("All controlled files in this project" . nil))
          for title  = (format "%s (%s)" (car elt) pwd)
          for option = (cdr elt)
          for cmd    = (format "git ls-files %s" (or option ""))
          collect
          `((name . ,title)
            (init . (lambda ()
                      (unless (and (not ,option) (helm-candidate-buffer))
                        (with-current-buffer (helm-candidate-buffer 'global)
                          (call-process-shell-command ,cmd nil t nil)))))
            (candidates-in-buffer)
            (type . file))))

  (defun helm-git-project-topdir ()
    (file-name-as-directory
     (replace-regexp-in-string
      "\n" ""
      (shell-command-to-string "git rev-parse --show-toplevel"))))

  (defun helm-git-project ()
    (interactive)
    (let ((topdir (helm-git-project-topdir)))
      (unless (file-directory-p topdir)
        (error "I'm not in Git Repository!!"))
      (let* ((default-directory topdir)
             (sources (helm-c-sources-git-project-for default-directory)))
        (helm-other-buffer sources
                           (format "*helm git project in %s*" default-directory)))))
  (define-key global-map (kbd "C-;") 'helm-git-project)
  ;; <<<

  )

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
;; @expand-region
(when (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

;; --------------------------------------------------
;; @ powerline
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(defun powerline-my-theme ()
  "Setup the my mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (powerline-buffer-size nil 'l)
                                     (powerline-raw mode-line-mule-info nil 'l)
                                     (powerline-raw
                                      (shorten-directory default-directory 15)
                                      nil 'l)
                                     (powerline-buffer-id nil 'r)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-my-theme)

;; --------------------------------------------------
;; @ rainbow-delimiters
(when (require 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
;; (custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d"))))))

;; --------------------------------------------------
;; @ git-gutter+
(global-git-gutter+-mode t)

;; --------------------------------------------------
;; @ flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; --------------------------------------------------
;; @ Go
(eval-after-load "go-mode"
  '(progn
     (require 'go-autocomplete)
     (add-hook 'go-mode-hook 'go-eldoc-setup)
     (add-hook 'before-save-hook 'gofmt-before-save)
     ;; key bindings
     (define-key go-mode-map (kbd "M-.") 'godef-jump)
     (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)
     ;; display
     (custom-set-variables
      '(tab-width 4)
      '(indent-tabs-mode 1)
      )
     ))

;; --------------------------------------------------
;; @ Python
(require 'python)
(require 'jedi)

(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook 'jedi:setup)

(defun my/turn-on-flycheck-mode ()
  (setq jedi:complete-on-dot t)
  (define-key python-mode-map (kbd "<C-tab>") 'jedi:complete)
  (setq jedi:key-show-doc (kbd "C-c D"))
  (setq python-indent-guess-indent-offset 4)
  (flycheck-mode 1))

(add-hook 'python-mode-hook 'my/turn-on-flycheck-mode)

;; --------------------------------------------------
;; @ Haskell
(autoload 'haskell-mode "haskell-mode")
(autoload 'haskell-cabal "haskell-cabal")

(add-to-list 'exec-path "~/.cabal/bin")
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(setq haskell-program-name "/usr/local/bin/ghci")

(autoload 'ghc-init "ghc")
(add-hook 'haskell-mode-hook
          (lambda ()
            (ghc-init)
            (custom-set-variables
             '(haskell-indent-offset 2))
            (haskell-indent-mode t)
            ))

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
;; @javascript
;; (require 'stylus-mode)
;; (require 'jade-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

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
;; backup off
(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
;; (setq initial-scratch-message "")
;; C-h trans Back
(keyboard-translate ?\C-h ?\C-?)
;; ;; line number
;; (defvar linum-format "%5d | ")
(global-linum-mode t)
;; (when (require 'linum-relative)
;;   (custom-set-variables
;;    '(linum-relative-format "%5s | ")
;;    '(linum-relative-current-symbol "->")
;;    )
;;   (linum-on)
;;   )
;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)
;; keyboard scroll one line at a time
(setq scroll-step 3)
;; scrolling to always be a line at a time
(setq scroll-conservatively 10000)
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

(when (require 'whitespace)
  (setq whitespace-style '(face ; faceで可視化
                           trailing ; 行末
                           tabs ; タブ
                           spaces ; スペース
                           empty ; 先頭/末尾の空行
                           space-mark ; 表示のマッピング
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
  (global-whitespace-mode 1)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("0e7da2c7c64fb5d4764250ffa4b8b33c0946577108d1d6444f1020d0dabba784" "57f8801351e8b7677923c9fe547f7e19f38c99b80d68c34da6fa9b94dc6d3297" default)))
 '(indent-tabs-mode nil)
 '(linum-relative-current-symbol "->")
 '(linum-relative-format "%5s | ")
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 100 :width normal)))))

;; --------------------------------------------------
;; @ color-theme
(load-theme 'monokai)

;; --------------------------------------------------
;; @ server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))
;;; init.el ends here
