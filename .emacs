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
    ;flx-ido
    ;ido-ubiquitous
    ;smex
    ;idomenu
    ;ido-vertical-mode
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
    clang-format
    srefactor
    
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
    ;projectile
    ;flx
    ;project-explorer
    ;nameframe-projectile
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
  (interactive)
  (mapc 'install-package my-packages))

(install-my-packages)

(require 'indent-guide)

;; don't show toolbar
(tool-bar-mode -1)

;; highlight matching parentheses
(show-paren-mode 1)

;; show line numbers (use nlinum-mode; linum-mode is slow)
(global-nlinum-mode)

;; show column number in mode-line
(column-number-mode)

(setq inhibit-splash-screen t)

(setq-default indicate-empty-lines t)








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


;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; configure irony mode ================================================
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(eval-after-load 'company  '(add-to-list 'company-backends 'company-irony))


;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; end configure irony mode ==============================================

; semantic refactoring
(require 'srefactor)
(require 'srefactor-lisp)

;; OPTIONAL: ADD IT ONLY IF YOU USE C/C++. 
(semantic-mode 1) ;; -> this is optional for Lisp

(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)

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


; highlight-parentheses with autopair mode:
;(add-hook 'highlight-parentheses-mode-hook
;	  '(lambda ()
;	     (setq autopair-handle-action-fns
;		   (aend
;		    (if autopair-handle-action-fns
;			autopair-handle-action-fns
;		      '(autopair-default-handle-action))
;		    '((lambda (action pair pos-before)
;			(hl-paren-color-update)))))))


;Enables highlight-parentheses-mode on all buers:
;(define-globalized-minor-mode global-highlight-parentheses-mode
; highlight-parentheses-mode
;  (lambda ()
;    (highlight-parentheses-mode t)))
;(global-highlight-parentheses-mode t)

(setq show-paren-delay 0.01)
(show-paren-mode 1)

(require 'smartparens-config)
(add-hook 'c++-mode-hook #'smartparens-mode)
(add-hook 'c-mode-hook #'smartparens-mode)


;; folding
(require 'vimish-fold)
(vimish-fold-global-mode 1)

(require 'evil-vimish-fold)
(evil-vimish-fold-mode 1)


(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)

;; cmake-ide for C++
(require 'rtags)
(cmake-ide-setup)

;; mapping for commands

;;load a file named key-chord.el from some directory in the load-path (e.g. "~/.emacs.d")
(require 'key-chord)
(key-chord-mode 1)


;; goto definition
(key-chord-define-global "ff" 'rtags-find-symbol-at-point)

;; rename
;;(global-set-key (kbd "n") 'nil)
(key-chord-define-global "rr" 'rtags-rename-symbol)

;; find all references
(key-chord-define-global "fr" 'rtags-find-all-references-at-point)

;; goto next reference
(key-chord-define-global "nn" 'rtags-next-match)

;; goto prev reference
(key-chord-define-global "pp" 'rtags-next-match)

;; folding
;;(global-set-key (kbd "M-0") 'vimish-fold-toggle)
;;(key-chord-define-global "za" 'vimish-fold)




;;TODO: autopair


(provide '.emacs)
;;; .emacs ends here
