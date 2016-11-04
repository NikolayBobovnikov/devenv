;;; package --- Summary
;;; Commentary: 
;;; Code:

(require 'package)
  (push '("gnu" . "http://elpa.gnu.org/packages/")
        package-archives)
      			       
;Apparently needed for the package auto-complete (why?)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)


(package-initialize)
(setq url-http-attempt-keepalives nil)
(when (not package-archive-contents)
    (package-refresh-contents))


;; Define install-package for easier installing of packages.
;; copied from github.com/purcell/emacs.d
(defun install-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (message "%s" package)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (install-package package min-version t)))))

;(package-refresh-contents)

(defvar my-packages
  '(;;;; Misc
    ;exec-path-from-shell
    ;undo-tree
    ;bind-key
    ;nameframe
    ;avy
    ;link-hint
    ;swiper
    ;
    ;;;;; Mode-line
    ;diminish
    ;smart-mode-line
    ;
    ;;;;; UI
    indent-guide
    ;yascroll
    ;highlight-symbol
    ;smooth-scroll
    nlinum
    ;
    ;;;;; ido, ~M-x~
    flx-ido
    ido-ubiquitous
    smex
    idomenu
    ido-vertical-mode
    ;
    ;;;;; Window and frame management
    ;buffer-move
    ;window-number
    ;fullframe
    ;
    ;;;;; Interactive Search
    ;anzu
    ;
    ;;;;; Completion
    company
    ;company-emoji
    ;
    ;;;;; Linting
    flycheck
					;
    ;;;;; C++
    cmake-ide
    rtags
    key-chord
    smartparens ;https://github.com/Fuco1/smartparens
    vimish-fold
    irony
    company-irony
    ycmd
    company-ycmd
    clang-format
    srefactor
    cmake-font-lock
    yasnippet

    ;;;;; Dired
    ;dired+
    ;
    ;;;;; Ack & Ag
    ;ag
    ;
    ;;;;; Git
    ;magit
    ;git-blame
    ;gitignore-mode
    ;gitconfig-mode
    ;git-messenger
    ;git-gutter
    ;
    ;;;;; Projectile
    projectile
    flx
    project-explorer
    neotree
    nameframe-projectile
    ;
    ;;;;; Perspective
    ;perspective
    ;nameframe-perspective
    ;
    ;;;;; Evil (Vim)
    evil
    evil-anzu
    evil-surround
    evil-leader
    evil-matchit
    evil-nerd-commenter
    evil-search-highlight-persist
    evil-vimish-fold
    ;
    ;;;;; Ledger
    ;ledger-mode
    ;flycheck-ledger
    ;
    ;;;;; Language specific
    ;;;;;;; Python
    ;virtualenvwrapper
    ;anaconda-mode
    ;company-anaconda
    ;nose
    ;
    ;;;;;;; YAML
    ;yaml-mode
    ;
    ;;;;;;; HTML, CSS
    ;web-mode
    ;
    ;;;;;;; Markdown
    ;markdown-mode
    ;
    ;;;;;;; Javascript
    ;json-mode
    ;js2-mode
    ;
    ;;;;;;; Lisp
    ;paredit
    ;rainbow-delimiters
    ;highlight-parentheses
    ;paren-face
    ;
    ;;;;;;; Clojure
    ;cider
    ;
    ;;;;;;; Misc
    ;haskell-mode
    ;ghc
    ;flycheck-haskell
    ;purescript-mode
    ;elm-mode
    ;mu4e-alert
    ;
    ;;;;;;; Org
    ;htmlize
    ;org-journal
    )
  "My packages!")

;; loop over my-packages and install them
(defun install-my-packages ()
  ;my interactive install package function
  (interactive)
  (mapc 'install-package my-packages))

(install-my-packages)

(require 'indent-guide)

;; don't show toolbar
(tool-bar-mode -1)

;; highlight matching parentheses
(show-paren-mode 1)

;; set default font size
(set-face-attribute 'default nil :height 150)


;; Options for tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4) ; or any other preferred value
    (defvaralias 'c-basic-offset 'tab-width)
    (defvaralias 'cperl-indent-level 'tab-width)
                                             

(defconst modi/linum-mode-hooks '(emacs-lisp-mode-hook
                                  c-mode-hook
                                  python-mode-hook
                                  matlab-mode-hook
                                  sh-mode-hook
                                  web-mode-hook
                                  html-mode-hook
                                  css-mode-hook
                                  makefile-gmake-mode-hook
                                  tcl-mode-hook)
  "List of hooks of major modes in which a linum mode should be enabled.")
(when global-linum-mode
      (global-nlinum-mode -1))
(dolist (hook modi/linum-mode-hooks)
      (add-hook hook #'nlinum-mode))


;; show column number in mode-line
(column-number-mode)

(setq inhibit-splash-screen t)

(setq-default indicate-empty-lines t)

; nerdcommenter
(evilnc-default-hotkeys)


;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)
;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
;(ac-set-trigger-key "TAB")
;(ac-set-trigger-key "<tab>")


;; place semicolon at the end of string
(defun maio/electric-semicolon ()
  (interactive)
  (end-of-line)
  (when (not (looking-back ";"))
    (insert ";"))
  (newline)
  (indent-according-to-mode))

;(global-set-key  ";" 'maio/electric-semicolon)
;(eval-after-load 'c-mode '(define-key c-mode-map (kbd ";") 'maio/electric-semicolon))
;(eval-after-load 'c++-mode '(define-key c++-mode-map (kbd ";") 'maio/electric-semicolon))


;; flycheck
;;(package-refresh-contents)
;;(package-install 'flycheck)
(global-flycheck-mode)

;;reload local dir variables when saved
(add-hook 'emacs-lisp-mode-hook
	  (defun enable-autoreload-for-dir-locals ()
	    (when (and (buffer-file-name)
		       (equal dir-locals-file
			      (file-name-nondirectory (buffer-file-name))))
	      (add-hook (make-variable-buffer-local 'after-save-hook)
			                        'my-reload-dir-locals-for-all-buffer-in-this-directory))))

; themes - add directories to emacs custom theme load path
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized/")
;;(load-theme 'solarized t)
(load-theme 'tsdh-dark)
(set-face-attribute 'region nil :background "#666")

;;; start with file tree view
;(neotree)


;; revert buffers when files are updated
(global-auto-revert-mode t)

;;;;;;; UI ;;;;;;;;
; from enberg on #emacs
(setq compilation-finish-function
  (lambda (buf str)
    (if (null (string-match ".*exited abnormally.*" str))
        ;;no errors, make the compilation window go away in a few seconds
        (progn
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!")))))



; semantic refactoring
(require 'srefactor)
(require 'srefactor-lisp)

;; OPTIONAL: ADD IT ONLY IF YOU USE C/C++. 
(semantic-mode 1) ;; -> this is optional for Lisp

; evil mode
(require 'evil)
(evil-mode 1)

;; esc quits
(defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
	(setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; set tab 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;(setq-default
; tab-width 4
; make-backup-files nil
; indent-tabs-mode nil
; ;show-trailing-whitespace t
; visible-bell nil)

; Ctrl-C Ctrl-V
;(cua-mode t)
;(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;(transient-mark-mode 1) ;; No region when it is not highlighted
;(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour


(setq show-paren-delay 0.01)
(show-paren-mode 1)

(require 'smartparens-config)
(add-hook 'c++-mode-hook #'smartparens-mode)
(add-hook 'c-mode-hook #'smartparens-mode)
(add-hook 'lisp-mode-hook #'smartparens-mode)


;; folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

(require 'vimish-fold)
;(vimish-fold-global-mode 1)

(require 'evil-vimish-fold)
(evil-vimish-fold-mode 1)

; enable hideshow minor mode in all programming modes
(setq evil-fold-list (remove-if (lambda (e) (eq (caar e) 'hs-minor-mode)) evil-fold-list))
(add-to-list 'evil-fold-list '((hs-minor-mode) ...)) 

; buffer navigation
(require 'ido)
(ido-mode t)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)


;enable company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; cmake-ide for C++
(require 'rtags)
(cmake-ide-setup)

;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
(defun setup-flycheck-rtags ()
  (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

;refactoring and completion with rtags
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;; use rtags flycheck mode -- clang warnings shown inline
(require 'flycheck-rtags)
;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'setup-flycheck-rtags)

    
;; completion with irony mode 
;(add-hook 'c++-mode-hook 'irony-mode)
;(add-hook 'c-mode-hook 'irony-mode)
;(add-hook 'objc-mode-hook 'irony-mode)
;(eval-after-load 'company  '(add-to-list 'company-backends 'company-irony))
;;; replace the `completion-at-point' and `complete-symbol' bindings in
;;; irony-mode's buffers by irony-mode's function
;(defun my-irony-mode-hook ()
;  (define-key irony-mode-map [remap completion-at-point]
;    'irony-completion-at-point-async)
;  (define-key irony-mode-map [remap complete-symbol]
;    'irony-completion-at-point-async))
;(add-hook 'irony-mode-hook 'my-irony-mode-hook)
;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;(push 'company-irony company-backends)


;; mapping for commands

;;load a file named key-chord.el from some directory in the load-path (e.g. "~/.emacs.d")
(require 'key-chord)
(key-chord-mode 1)

(local-set-key [tab] 'tab-to-tab-stop)
(local-set-key (kbd "TAB") 'tab-to-tab-stop)

;; goto definition
(global-set-key (kbd "<f2>") 'rtags-find-symbol-at-point)
(global-set-key (kbd "<f3>") 'ff-find-other-file)

;; rename
;;(global-set-key (kbd "n") 'nil)
(global-set-key (kbd "C-S-r") 'rtags-rename-symbol)

;; find all references
(key-chord-define-global "fr" 'rtags-find-all-references-at-point)

;; goto next reference
(key-chord-define-global "f,"  'rtags-next-match)

;; goto prev reference
(key-chord-define-global "f." 'rtags-next-match)

;; folding
(global-set-key (kbd "C-S-c") 'hs-toggle-hiding)
(global-set-key (kbd "M-0") 'hs-hide-all)
(global-set-key (kbd "M-9") 'hs-show-all)

;; build
(global-set-key (kbd "<f7>") 'cmake-ide-compile)

(key-chord-define-global ";;" 'maio/electric-semicolon)

; srefactor
(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)



;;TODO: autopair


(provide '.emacs)
;;; .emacs ends here
