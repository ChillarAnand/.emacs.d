;;; init.el --- Emacs Config
;;

;;; Code:

(require 'cl)
(require 'package)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; basic config


;; set package-user-dir to be relative to ~/.emacs.d/
(defvar root-dir (file-name-directory load-file-name)
  "The root dir of the Emacs")
(setq package-user-dir (expand-file-name "elpa" root-dir))
(setq package-vendor-dir (expand-file-name "vendor" root-dir))
(setq recent-files-dir (expand-file-name "recentf" root-dir))
(load-file (expand-file-name ".private.el" root-dir))

;; Always load newest byte code
(setq load-prefer-newer t)

;; turn on debug
;; (toggle-debug-on-error)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; dont prompt while creating new buffer
(setq confirm-nonexistent-file-or-buffer nil)

;; kill process buffer without confirmation
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; confirm before killing emacs
(setq confirm-kill-emacs
      (lambda (&rest _)
        (message "Quit in 3 sec (`C-g' or other action cancels)")
        (sit-for 3)))

;; always split vertically
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; always kill line with whitespace
(setq kill-whole-line t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ui config

;; maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; remove scroll bar
(scroll-bar-mode -1)

;; disable tool bar
(tool-bar-mode -1)

;; disable menu bar
(menu-bar-mode -1)

;; disable blinking
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; set fond & colors
(set-default-font "Ubuntu Mono 13")
(set-background-color "#f1f1f1")
(add-to-list 'default-frame-alist '(background-color . "#f1f1f1"))

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)

;; show size in mode line
;; (size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; set frame title
(setq frame-title-format
      '("" invocation-name
        " Avil Page - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages


;; dired config
(require 'dired)

;; auto revert dired buffers
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; auto select target as split
(setq dired-dwim-target t)

(setq delete-by-moving-to-trash t)
(setq dired-no-confirm t)
(define-key dired-mode-map "u" 'dired-up-directory)
(setq dired-deletion-confirmer '(lambda (x) t))

;; unzip zipped file dired
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

;; re use dired buffers
;; (diredp-toggle-find-file-reuse-dir 1)

;; hide unnecessary files
(setq-default dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")

;; (defadvice dired-delete-entry (before force-clean-up-buffers (file) activate)
;;   (kill-buffer (get-file-buffer file)))


;; save recent files
(require 'recentf)
(setq
 ;; recentf-save-file (expand-file-name "recentf" recent-files-dir)
 recentf-max-saved-items 500
 recentf-max-menu-items 15
 ;; disable recentf-cleanup on Emacs start, because it can cause
 ;; problems with remote files
 recentf-auto-cleanup 'never)
(recentf-mode +1)

;; save point positions across sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-forget-unreadable-files nil)
(setq save-place-file (concat user-emacs-directory "saveplace.el"))

;; save history
;; (psession-mode 1) 
(savehist-mode 1)
(setq history-length 1000)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
;; (setq savehist-file "~/.emacs.d/savehist")


;; enable semantic mode
(semantic-mode 1)


;; add melpa to archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-refresh-contents)
(package-initialize)

;; dont check signatures
(setq package-check-signature nil)


;; install use package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;; general packages
(use-package dired+)

(use-package session)
(add-hook 'after-init-hook 'session-initialize)


;; programming mode packages

(use-package projectile)

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))


(use-package vimish-fold
  :config
  (vimish-fold-global-mode-enable-in-buffers)
  (add-hook 'markdown-mode-hook #'vimish-fold-mode))


(use-package smartparens
  :config
  (sp-pair "`" "`" :wrap "C-`")
  (sp-pair "%" "%" :wrap "C-%")
  (sp-pair "<" ">" :wrap "C->")
  (defun strict-smartparens ()
    (turn-on-smartparens-strict-mode))
  (add-hook 'prog-mode-hook 'strict-smartparens))


(use-package electric-operator
  :config
  (add-hook 'python-mode-hook #'electric-operator-mode))


(use-package real-auto-save
  :config
  (add-hook 'prog-mode-hook 'real-auto-save-mode)
  (setq real-auto-save-interval 4))


;; python mode
;; (add-to-list 'load-path "~/projects/lisp/elpy") 
;; (load "elpy" nil t)
;; (elpy-enable)
(use-package elpy
  :config
  (elpy-enable)
  (setq python-indent-offset 4)
  (setq elpy-test-runner 'elpy-test-pytest-runner)
  (setq elpy-rpc-timeout nil)
  (setq elpy-rgrep-file-pattern   "*.py *.html")
  ;; (setq elpy-rpc-python-command "python3")
  (append grep-find-ignored-files "flycheck_*")

  ;; activate exp
  (pyvenv-workon "exp")
  (elpy-rpc-restart)
  
  (defun my/send-region-or-buffer (&optional arg)
    (interactive "P")
    (elpy-shell-send-region-or-buffer arg)
    (with-current-buffer (process-buffer (elpy-shell-get-or-create-process))
      (set-window-point (get-buffer-window (current-buffer))
                        (point-max))))
  (define-key elpy-mode-map (kbd "C-c C-c") 'my/send-region-or-buffer)
  (define-key elpy-mode-map (kbd "M-.") 'pop-tag-mark)

  (defun company-yasnippet-or-completion ()
    "Solve company yasnippet conflicts."
    (interactive)
    (let ((yas-fallback-behavior
           (apply 'company-complete-common nil)))
      (yas-expand)))

  (add-hook 'company-mode-hook
            (lambda ()
              (substitute-key-definition
               'company-complete-common
               'company-yasnippet-or-completion
               company-active-map))))


(use-package salt-mode)


(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-c m e") 'mc/edit-lines)
  (global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))


(use-package company
  :config
  (global-company-mode 1)

  (setq company-idle-delay 0)
  (setq company-tooltip-limit 5)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-flip-when-above t)

  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))


(use-package header2)


(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))

  (setq-default indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-js-indent-offset 0)

  (setq web-mode-script-padding 0)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-pairing nil)

  (set (make-local-variable 'company-backends) '(company-css))

  (bind-key "C-c C-i" 'web-mode-buffer-indent)
  (bind-key "C-c C-l" 'web-mode-fold-or-unfold))


(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))


(use-package nyan-mode
  :init
  (nyan-mode))


(use-package magit
  :config
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq magit-last-seen-setup-instructions "1.4.0"))


(use-package sx
  :config
  (require 'sx-load))


(use-package edit-server
  :init
  (when (require 'edit-server nil t)
    (setq edit-server-new-frame nil)
    (edit-server-start)))


(use-package multi-term
  :config
  (setq multi-term-program "/bin/zsh")
  (bind-key "C-c C-t" 'multi-term)
  (bind-key "C-c C-n" 'multi-term-next)
  (bind-key "C-c C-p" 'multi-term-prev))


(use-package free-keys)


(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (rich-minority-mode 1)
  (setq rm-whitelist t)
  (sml/setup)
  (sml/apply-theme 'light))

;; (setq mode-line-format
;;       '("%e" mode-line-front-space mode-line-mule-info mode-line-client
;;         mode-line-modified mode-line-remote mode-line-frame-identification
;;         mode-line-buffer-identification
;;         (vc-mode vc-mode)
;;         sml/pre-modes-separator mode-line-modes
;;         sml/pos-id-separator mode-line-position))

(use-package impatient-mode)


(use-package highlight-symbol
  :init
  (progn
    (global-set-key [f3] 'highlight-symbol-next)
    (global-set-key [(shift f3)] 'highlight-symbol-prev)
    (global-set-key [(control f3)] 'highlight-symbol)
    (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
    (highlight-symbol-mode 1)
    (highlight-symbol-nav-mode 1)))


(use-package comment-dwim-2
  :init
  (global-set-key (kbd "M-;") 'comment-dwim-2))


(use-package openwith
  :init
  (openwith-mode t)
  (setq large-file-warning-threshold 500000000)
  (setq openwith-associations
        (list (list (openwith-make-extension-regexp '("pdf"))
                    "evince" '(file))
              (list (openwith-make-extension-regexp '("flac" "mp3" "wav"))
                    "vlc" '(file))
              (list (openwith-make-extension-regexp
                     '("avi" "flv" "mov" "mp4" "mkv" "mpeg" "mpg" "ogg" "wmv"))
                    "vlc" '(file))
              (list (openwith-make-extension-regexp '("bmp" "jpeg" "jpg" "png"))
                    "ristretto" '(file))
              (list (openwith-make-extension-regexp '("doc" "docx" "odt"))
                    "libreoffice" '("--writer" file))
              (list (openwith-make-extension-regexp '("ods" "xls" "xlsx"))
                    "libreoffice" '("--calc" file))
              (list (openwith-make-extension-regexp '("odp" "pps" "ppt" "pptx"))
                    "libreoffice" '("--impress" file))
              )))


(use-package easy-kill
  :config
  (bind-key "M-w" 'easy-kill))

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (show-paren-mode +1))

(use-package helm-chrome)
(use-package helm-swoop)
(use-package helm-descbinds)
(use-package helm-projectile)
(use-package helm-ag)
(use-package helm-dired-recent-dirs)
(use-package helm-github-stars
  :config
  (setq helm-github-stars-username "chillaranand"))

(use-package helm
  :config

  (require 'helm-eshell)

  (defvar helm-source-emacs-commands
    (helm-build-sync-source "Emacs commands"
      :candidates (lambda ()
                    (let ((cmds))
                      (mapatoms
                       (lambda (elt) (when (commandp elt) (push elt cmds))))
                      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "A simple helm source for Emacs commands.")

  (defvar helm-source-emacs-commands-history
    (helm-build-sync-source "Emacs commands history"
      :candidates (lambda ()
                    (let ((cmds))
                      (dolist (elem extended-command-history)
                        (push (intern elem) cmds))
                      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "Emacs commands history")

  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-dired-recent-dirs
                                    helm-source-emacs-commands-history
                                    helm-source-emacs-commands
                                    helm-chrome-source
                                    hgs/helm-c-source-stars
                                    hgs/helm-c-source-repos
                                    helm-source-buffer-not-found
                                    hgs/helm-c-source-search))

  (setq  helm-ff-newfile-prompt-p              nil
         helm-echo-input-in-header-line        t
         helm-M-x-always-save-history          t
         helm-split-window-in-side-p           t
         helm-buffers-fuzzy-matching           t
         helm-move-to-line-cycle-in-source     t
         helm-ff-search-library-in-sexp        t
         helm-ff-file-name-history-use-recentf t))


;; swiper for search
(use-package swiper-helm
  :config
  (ivy-mode 1)
  ;; make swiper to use helm display
  (setq swiper-helm-display-function 'helm-default-display-buffer)
  (setq ivy-use-virtual-buffers t)
  (global-set-key "\C-s" 'swiper-helm)
  (global-set-key "\C-r" 'swiper-helm)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key [f8] 'ivy-resume))


(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1))


(use-package google-translate
  :config
  (setq  google-translate-default-source-language "en")
  (setq  google-translate-default-target-language "kn")
  (require 'google-translate-default-ui))


(use-package ace-link
  :config
  (ace-link-setup-default "f"))


(use-package sotlisp)


(use-package benchmark-init
  :config
  (benchmark-init/activate))


(use-package markdown-mode)


(use-package writegood-mode)
(use-package writeroom-mode
  :config
  (add-hook 'markdown-mode-hook 'cap))

(use-package auto-capitalize
  :config
  (autoload 'auto-capitalize-mode "auto-capitalize"
    "Toggle `auto-capitalize' minor mode in this buffer." t)
  (autoload 'turn-on-auto-capitalize-mode "auto-capitalize"
    "Turn on `auto-capitalize' minor mode in this buffer." t)
  (autoload 'enable-auto-capitalize-mode "auto-capitalize"
    "Enable `auto-capitalize' minor mode in this buffer." t)
  (defun cap ()
    (auto-capitalize-mode))
  (add-hook 'org-mode-hook 'cap)
  (add-hook 'markdown-mode-hook 'cap)
  (defvar auto-capitalize-words
    '("I" "Python" "Emacs" "You")))


(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


(use-package key-chord
  :config
  ;; (setq key-chord-one-keys-delay 0.5)
  ;; (setq key-chord-two-keys-delay 0.5)
  )


(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))


(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))


(use-package bm
  :config
  (global-set-key (kbd "<f5>") 'bm-toggle)
  (global-set-key (kbd "<f7>") 'bm-next)
  (global-set-key (kbd "<f6>") 'bm-previous))

(use-package hl-line
  :config
  (global-hl-line-mode 1))


(use-package flycheck-pos-tip
  :config
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))


(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))


;; sql config
(require 'sql)
(use-package sqlup-mode
  :config
  (add-hook 'sql-mode-hook 'sqlup-mode))

(use-package sql-indent
  :config
  (eval-after-load "sql"
    '(load-library "sql-indent")))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(sql-set-product "mysql")
(setq sql-port 3306)
(setq sql-connection-alist
      '(
        (pool-server
         (sql-server sql-server-address)
         (sql-user sql-server-user)
         (sql-password sql-server-password)
         (sql-database sql-server-database)
         (sql-port sql-port))
        (pool-local
         (sql-server sql-local-server)
         (sql-user sql-local-user)
         (sql-password sql-local-password)
         (sql-database sql-local-database)
         (sql-port sql-port))))

(defun sql-connect-preset (name)
  "Connect to a predefined SQL connection listed in `sql-connection-alist'"
  (eval `(let ,(cdr (assoc name sql-connection-alist))
           (flet ((sql-get-login (&rest what)))
             (sql-product-interactive sql-product)))))

(defun sql-pool-server ()
  (interactive)
  (sql-connect-preset 'pool-server))

(defun sql-pool-local ()
  (interactive)
  (sql-connect-preset 'pool-local))

(defun mysql-send-paragraph ()
  (interactive)
  (sql-send-paragraph)
  (with-current-buffer (process-buffer (get-process "SQL"))
    (set-window-point (get-buffer-window (current-buffer))
                      (point-max))))

(defun sql-add-newline-first (output)
  "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'"
  (remove-hook 'comint-preoutput-filter-functions
               'sql-add-newline-first)
  (concat "\n" output))

(defun my-sql-save-history-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename 
               (concat "~/.emacs.d/sql/"
                       (symbol-name (symbol-value rval))
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       (format "SQL history will not be saved because %s is nil"
               (symbol-name rval))))))

(add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)

(defun sql-add-newline-first (output)
  "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'"
  (message "fooo")
  (concat "\n" output))

(defun sqli-add-hooks ()
  "Add hooks to `sql-interactive-mode-hook'."
  (add-hook 'comint-preoutput-filter-functions
            'sql-add-newline-first))

(add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)


;; slides
(load-file "~/.emacs.d/vendor/htmlize.el")
(use-package htmlize)
(require 'org)
(use-package ox-reveal
  :config
  (setq org-reveal-root "file:///home/anand/.emacs.d/vendor/reveal.js"))

(use-package ob-translate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities

(defun my-edit-file-as-sudo/root ()
  "Find file as root"
  (interactive)
  (let*
      ((sudo (/= (call-process "sudo" nil nil "-n true") 0))
       (file-name
        (if (tramp-tramp-file-p buffer-file-name)
            (with-parsed-tramp-file-name buffer-file-name parsed
              (tramp-make-tramp-file-name
               (if sudo "sudo" "su")
               "root"
               parsed-host
               parsed-localname
               (let ((tramp-postfix-host-format "|")
                     (tramp-prefix-format))
                 (tramp-make-tramp-file-name
                  parsed-method
                  parsed-user
                  parsed-host
                  ""
                  parsed-hop))))
          (concat (if sudo
                      "/sudo::"
                    "/su::")
                  buffer-file-name))))
    (find-alternate-file file-name)))


;; remote connection
(defun connect-to-server ()
  (interactive)
  (dired (format  "/ssh:%s@%s:/" server-user server-host)))


(defun is-line-empty-p ()
  "Return t if current line is empty."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun is-inside-parens-p ()
  "Return t if point is inside parens."
  (> (car (syntax-ppss)) 0))

(defun my-lispy-kill ()
  "Clean up whitespace along with lispy kill."
  (interactive)
  (if (is-line-empty-p)
      (lispy-kill)
    (if (is-inside-parens-p)
        (lispy-kill)
      (lispy-kill)
      (kill-line))))

;; remote connection
;; (defun connect-to-server ()
;;   (interactive)
;;   (dired (format  "/%s@%s:/" server-user server-host)))


;; recenter when hyperlink is clicked
(defun my-recenter-on-find-function (orig &rest args)
  (let ((result (apply orig args)))
    (when result
      (recenter 0))
    result))
(advice-add 'help-button-action :around #'my-recenter-on-find-function)


(defun delete-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))


(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


(defun swap-numbers-symbols ()
  "Swap symbols to digits."
  (interactive)
  (define-key key-translation-map (kbd "!") (kbd "1"))
  (define-key key-translation-map (kbd "@") (kbd "2"))
  (define-key key-translation-map (kbd "#") (kbd "3"))
  (define-key key-translation-map (kbd "$") (kbd "4"))
  (define-key key-translation-map (kbd "%") (kbd "5"))
  (define-key key-translation-map (kbd "^") (kbd "6"))
  (define-key key-translation-map (kbd "&") (kbd "7"))
  (define-key key-translation-map (kbd "*") (kbd "8"))
  (define-key key-translation-map (kbd "(") (kbd "9"))
  (define-key key-translation-map (kbd ")") (kbd "0"))

  (define-key key-translation-map (kbd "1") (kbd "!"))
  (define-key key-translation-map (kbd "2") (kbd "@"))
  (define-key key-translation-map (kbd "3") (kbd "#"))
  (define-key key-translation-map (kbd "4") (kbd "$"))
  (define-key key-translation-map (kbd "5") (kbd "%"))
  (define-key key-translation-map (kbd "6") (kbd "^"))
  (define-key key-translation-map (kbd "7") (kbd "&"))
  (define-key key-translation-map (kbd "8") (kbd "*"))
  (define-key key-translation-map (kbd "9") (kbd "("))
  (define-key key-translation-map (kbd "0") (kbd ")")))

(defun swap-numbers-parens ()
  "( -> 9 and ) -> 0"
  (interactive)
  (define-key key-translation-map (kbd "(") (kbd "9"))
  (define-key key-translation-map (kbd ")") (kbd "0"))
  (define-key key-translation-map (kbd "9") (kbd "("))
  (define-key key-translation-map (kbd "0") (kbd ")")))
(swap-numbers-parens)


(defun current-dired ()
  (interactive)
  (dired "."))


(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))


(defun launch-separate-emacs-under-x ()
  (interactive)
  (call-process "sh" nil nil nil "-c" "emacs &"))


(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (add-hook 'kill-emacs-hook
            (if (display-graphic-p)
                #'launch-separate-emacs-under-x
              #'launch-separate-emacs-in-terminal)
            t)
  (kill-emacs))


(defun get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))


(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))


(defun smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'smart-move-beginning-of-line)

(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (comment-region l r)
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))


(defun upgrade-all-packages ()
  "Upgrade all packages, no questions asked."
  (interactive)
  (save-window-excursion
    (package-list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute 'no-query)))


(defun start-space-to-ctrl ()
  "Active space2cctl."
  (interactive)
  (async-shell-command "s2cctl start"))

(defun stop-space-to-ctrl ()
  "Active space2cctl."
  (interactive)
  (async-shell-command "s2cctl stop"))

(start-space-to-ctrl)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; key bindings

(bind-keys*
 ("<f12>" . menu-bar-mode)

 ("C-+" .  text-scale-increase)
 ("C--" .  text-scale-decrease)
 ("C-," .  avy-goto-char)
 ("C-^" .  top-join-line)

 ("C-x C-1" . delete-other-windows)
 ("C-x C-3" . split-window-right)
 ("C-x C-a" . beginning-of-buffer)
 ("C-x C-b" . switch-to-previous-buffer)
 ("C-x C-d" . duplicate-current-line-or-region)
 ("C-x C-h" . mark-whole-buffer)
 ("C-x C-i" . delete-other-windows)
 ("C-x C-k" . kill-this-buffer)
 ("C-x C-m" . helm-M-x)
 ("C-x C-z" . end-of-buffer)
 ("C-x r l" . helm-bookmarks)

 ("C-c C-f" . helm-projectile-find-file)
 ("C-c C-g" . beginning-of-buffer)
 ("C-c C-k" . delete-other-windows)
 ("C-c C-v" . eval-buffer)

 ("M-h" . backward-kill-word)
 ("M-o" . other-window)
 ("M-x" . helm-M-x)
 ("M-y" . helm-show-kill-ring)
 ("M-z" . zop-up-to-char)
 ("M-Z" . zop-to-char)
 ("M-?" . mark-paragraph)
 ("M-/" . hippie-expand))


;; lisp mode
(define-key lispy-mode-map (kbd "C-;") #'comment-or-uncomment-sexp)
(define-key lispy-mode-map (kbd "C-k") #'my-lispy-kill)


;; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

(global-set-key [remap kill-whole-line] 'delete-whole-line)


;;(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; shell history.
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

;; use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))

(substitute-key-definition 'find-tag 'helm-etags-select global-map)
(setq projectile-completion-system 'helm)
(helm-descbinds-mode)
(helm-mode 1)


;; enable Helm version of Projectile with replacment commands
(helm-projectile-on)

;; better defaults for comint mode
(eval-after-load 'comint
  '(progn
     (define-key comint-mode-map (kbd "<up>") #'comint-previous-matching-input-from-input)
     (define-key comint-mode-map (kbd "<down>") #'comint-next-matching-input-from-input)))


(require 'key-chord)
(key-chord-mode +1)
(key-chord-define-global "dd" 'delete-whole-line)
(key-chord-define-global "df" 'describe-function)
(key-chord-define-global "dk" 'describe-key)
(key-chord-define-global "dv" 'describe-variable)
(key-chord-define-global "hr" 'helm-resume)
(key-chord-define-global "fj" 'helm-mini)
(key-chord-define-global "jb" 'switch-to-previous-buffer)
(key-chord-define-global "jc" 'avy-goto-char)
(key-chord-define-global "jd" 'helm-dired-recent-dirs-view)
(key-chord-define-global "jl" 'avy-goto-line)
(key-chord-define-global "js" 'helm-semantic-or-imenu)
(key-chord-define-global "kf" 'bury-buffer)
(key-chord-define-global "kw" 'delete-window)
(key-chord-define-global "md" 'current-dired)
(key-chord-define-global "mg" 'magit-status)
(key-chord-define-global "mx" 'helm-M-x)
(key-chord-define-global "ps" 'helm-projectile-switch-project)
(key-chord-define-global "pf" 'helm-projectile-find-file)
(key-chord-define-global "pg" 'helm-projectile-grep)
(key-chord-define-global "sm" 'set-mark-command)


(message "Successfully loaded config... ")


(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
