;;; init.el --- Emacs Config
;;

;;; Code:

(require 'cl)
(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; basic config

;; Always load newest byte code
(setq load-prefer-newer t)

;; turn on debug
(toggle-debug-on-error)

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

;; set package-user-dir to be relative to Prelude install path
(defvar root-dir (file-name-directory load-file-name)
  "The root dir of the Emacs")
(setq package-user-dir (expand-file-name "elpa" root-dir))
(setq package-vendor-dir (expand-file-name "vendor" root-dir))
(load-file (expand-file-name ".private.el" root-dir))


;; confirm before killing emacs
(setq confirm-kill-emacs
      (lambda (&rest args)
        (interactive)
        (null (read-event "Quitting in 3 seconds. Hit any key to stop."
                          nil 3))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ui config

;; maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; remove scroll bar
(scroll-bar-mode -1)

;; colors
(set-background-color "#f1f1f1")
(add-to-list 'default-frame-alist '(background-color . "#f1f1f1"))
(set-default-font "Ubuntu Mono 13")

;; disable tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
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

;; add melpa to archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; add melpa to archives
(defun install-packages (switch)
  "Refresh package contents if -i switch is passed."
  (message "-i was passed. refreshing package contents")
  (package-refresh-contents)
  ;; install use package
  (unless (package-installed-p 'use-package)
    (package-install 'use-package)))
(add-to-list 'command-switch-alist '("-i" . install-packages))

;; dont check signatures
(package-initialize)
(setq package-check-signature nil)

(require 'use-package)
(setq use-package-always-ensure t)


(use-package projectile)

(use-package save-sexp)

(use-package smartparens
  :config
  (sp-pair "`" "`" :wrap "C-`")
  (sp-pair "%" "%" :wrap "C-%")
  (sp-pair "<" ">" :wrap "C->")
  (defun strict-smartparens ()
    (turn-on-smartparens-strict-mode))
  (add-hook 'prog-mode-hook 'strict-smartparens))


(use-package elpy
  :config
  (elpy-enable)
  (setq python-indent-offset 4)
  (setq elpy-test-runner 'elpy-test-pytest-runner)
  (setq elpy-rpc-timeout nil)
  ;; (setq elpy-rpc-python-command "python3")
  (append grep-find-ignored-files "flycheck_*")

  (defun my/send-region-or-buffer (&optional arg)
    (interactive "P")
    (elpy-shell-send-region-or-buffer arg)
    (with-current-buffer (process-buffer (elpy-shell-get-or-create-process))
      (set-window-point (get-buffer-window (current-buffer))
                        (point-max))))
  (define-key elpy-mode-map (kbd "C-<right>") 'elpy-nav-forward-indent)
  (define-key elpy-mode-map (kbd "C-<left>") 'elpy-nav-backward-indent)
  (define-key elpy-mode-map (kbd "C-c C-c") 'my/send-region-or-buffer))


(use-package real-auto-save
  :config
  (add-hook 'prog-mode-hook 'real-auto-save-mode)
  (setq real-auto-save-interval 4))


(use-package multiple-cursors
  :init
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


(use-package header2
  :init
  (add-hook 'emacs-lisp-mode-hook 'auto-make-header))


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

  (bind-key "C-c C-i" 'web-mode-buffer-indent))


(use-package company-quickhelp
  :init
  (company-quickhelp-mode 1))


(use-package nyan-mode
  :init
  (nyan-mode))


(use-package magit
  :init
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq magit-last-seen-setup-instructions "1.4.0"))


(use-package sx
  :init
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
  (sml/setup)
  (sml/apply-theme 'light))


(use-package impatient-mode)

(use-package pony-mode
  :ensure t
  :config
  (setq pony-server-host "127.0.0.1")
  (add-hook 'python-mode-hook 'pony-mode))


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


;; (use-package slime
;;   :init
;;   (progn
;;     (setq inferior-lisp-program "/usr/bin/sbcl")
;;     (setq slime-contribs '(slime-fancy))
;;     (add-to-list 'slime-contribs 'slime-repl)))


(use-package ace-link
  :config
  (ace-link-setup-default))


(use-package writegood-mode)
(use-package writeroom-mode)
(use-package sotlisp)


;; (require 'emmet-mode)
;; (add-hook 'sgml-mode-hook 'emmet-mode)
;; (add-hook 'html-mode-hook 'emmet-mode)
;; (add-hook 'css-mode-hook  'emmet-mode)

;; (use-package zencoding-mode)
;; (add-hook 'html-mode-hook 'zencoding-mode)


(use-package benchmark-init
  :config
  (benchmark-init/activate))

(use-package auto-capitalize
  :config
  (autoload 'auto-capitalize-mode "auto-capitalize"
    "Toggle `auto-capitalize' minor mode in this buffer." t)
  (autoload 'turn-on-auto-capitalize-mode "auto-capitalize"
    "Turn on `auto-capitalize' minor mode in this buffer." t)
  (autoload 'enable-auto-capitalize-mode "auto-capitalize"
    "Enable `auto-capitalize' minor mode in this buffer." t))

(require 'markdown-mode)


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

;; (use-package ws-butler
;;   :config
;;   (ws-butler-global-mode))


(require 'dired)
(setq delete-by-moving-to-trash t)
(setq dired-no-confirm t)
(define-key dired-mode-map "u" 'dired-up-directory)
(defun delete-current-item ()
  (interactive)
  (dired-flag-file-deletion 1)
  (dired-do-flagged-delete))
(define-key dired-mode-map  [delete] 'delete-current-item)
(setq dired-deletion-confirmer '(lambda (x) t))

(defadvice dired-delete-entry (before force-clean-up-buffers (file) activate)
  (kill-buffer (get-file-buffer file)))

(define-key dired-mode-map "u" 'dired-up-directory)
;; unzip zipped file dired
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(use-package dired+
  :config
  (defun dir ()
    (interactive)
    (dired-sort-other "ll"))
  ;; (add-hook 'dired-mode-hook 'dir)
  )


(use-package flycheck-pos-tip
  :config
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))

(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))


(require 'sql)
(load-file (expand-file-name "mysql.el" package-vendor-dir))
(require 'mysql)
(use-package sqlup-mode
  :config
  (add-hook 'sql-mode-hook 'sqlup-mode))
(progn
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
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)))

  (define-key sql-mode-map (kbd "C-c C-c") 'mysql-send-paragraph))





;; (load-file "~/projects/lisp/real-auto-save/real-auto-save.el")
;; (add-hook 'prog-mode-hook 'real-auto-save-mode)
;; (setq real-auto-save-interval 4)


;; (use-package phi-search
;;   :init
;;   (global-set-key (kbd "C-s") 'phi-search))

;; (use-package circe
;;   :init
;;   (setq circe-network-options
;;         `(("Freenode"
;;            :nick "chillaranand"
;;            :channels
;;            ("#emacs" "#emacs-circe" "#emacs-elpy"
;;             "#python-india" "#python-dev"
;;             "#dgplug")
;;            :nickserv-password ,freenode-password)))
;;   (setq circe-reduce-lurker-spam t))


;; (use-package wakatime-mode
;;   :config
;;   (setq wakatime-python-bin "/usr/local/bin/wakatime")
;;   (global-wakatime-mode))


;; (require 'company)
;; (require 'company-web-html)
;; (add-to-list 'company-backends 'company-web-html)

;; (define-key web-mode-map (kbd "C-'") 'company-web-html)
;; (add-hook 'web-mode-hook (lambda ()
;;                            (set (make-local-variable 'company-backends) '(company-web-html company-files))
;;                            (company-mode t)))


;; (use-package elisp-slime-nav
;;   :init
;;   (progn
;;     (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;       (add-hook hook 'turn-on-elisp-slime-nav-mode))
;;     (global-set-key (kbd "C-c C-d") 'elisp-slime-nav-describe-elisp-thing-at-point)))



;; (load-file "~/.emacs.d/vendor/sql-completion.el")
;; (require 'sql-completion)
;; (setq sql-interactive-mode-hook
;;       (lambda ()
;;         (define-key sql-interactive-mode-map "\t" 'comint-dynamic-complete)
;;         (sql-mysql-completion-init)))




;; (use-package soundcloud
;;   :config
;;   (require 'emms-setup)
;;   (emms-standard)
;;   (emms-default-players))

;; (use-package elpy)
;; (add-to-list 'load-path "~/projects/lisp/elpy")
;; (load "elpy" nil t)
;; (elpy-enable)


;; (use-package electric-case
;;   :config
;;   (defun electric-case-python-init ()

;;     (electric-case-mode 1)
;;     (setq electric-case-max-iteration 2)

;;     (setq electric-case-criteria
;;           (lambda (b e)
;;             (let ((proper (electric-case--possible-properties b e))
;;                   (key (key-description (this-single-command-keys))))
;;               (cond
;;                ((member 'font-lock-variable-name-face proper)
;;                 ;; #ifdef A_MACRO  /  int variable_name;
;;                 (if (member '(cpp-macro) (python-guess-basic-syntax)) 'usnake 'snake))
;;                ((member 'font-lock-string-face proper) nil)
;;                ((member 'font-lock-comment-face proper) nil)
;;                ((member 'font-lock-keyword-face proper) nil)
;;                ((member 'font-lock-function-name-face proper) 'snake)
;;                ((member 'font-lock-type-face proper) 'snake)
;;                (electric-case-convert-calls 'snake)
;;                (t nil)))))

;;     (defadvice electric-case-trigger (around electric-case-c-try-semi activate)
;;       (when (and electric-case-mode
;;                  (eq major-mode 'python-mode)))))

;;   (add-hook 'python-mode-hook 'electric-case-python-init)
;;   (setq electric-case-convert-calls t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities


;; recenter when hyperlink is clicked
(defun my-recenter-on-find-function (orig &rest args)
  (let ((result (apply orig args)))
    (when result
      (recenter 0))
    result))
(advice-add 'help-button-action :around #'my-recenter-on-find-function)


;; suppress file save messages
;; (defun my-auto-save-wrapper (save-fn &rest args)
;;   (apply save-fn '(t)))
;; (advice-add 'save-buffer :around #'my-auto-save-wrapper)


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
  "Bind symbols to digits."
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
(start-space-to-ctrl)

(defun stop-space-to-ctrl ()
  "Active space2cctl."
  (interactive)
  (async-shell-command "s2cctl stop"))




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

;; Start proced in a similar manner to dired
(unless (eq system-type 'darwin)
  (global-set-key (kbd "C-x p") 'proced))

(define-key emacs-lisp-mode-map (kbd "C-M-;")
  #'comment-or-uncomment-sexp)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

(global-set-key [remap kill-whole-line] 'delete-whole-line)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

(unless (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f11>") 'prelude-fullscreen))

;;(global-set-key (kbd "C-h") 'paredit-backward-delete)
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
