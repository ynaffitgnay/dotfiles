; from the template file /lusr/share/udb/pub/dotfiles/emacs
;
; This is just to give you some idea of the things you can set
; in your .emacs file.  If you want to use any of these commands
; remove the ";" from in front of the line.

;; add this path to the emacs load path for different libraries
(add-to-list 'load-path "~/.emacs.d/lisp/")
  
; enable MELPA packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(dolist (package '(dtrt-indent auto-complete markdown-mode))
  (unless (package-installed-p package)
    (package-install package))
     (require package))

;; To change the font size under X.
; (set-default-font "9x15")

;; Turn off toolbar
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)))

;; Don't ask if it's okay to follow symlinks (just follow them)
(setq vc-follow-symlinks t)

;; scroll one line at a time (less "jumpy" than defaults)    
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Set your term type to vt100
; (load "term/vt100")

;; When in text (or related mode) break the lines at 80 chars
(setq text-mode-hook 'turn-on-auto-fill)

;; When writing comments, break lines at 80 chars
(setq comment-auto-fill-only-comments t)

;;; Only break lines for comments in the specified programming modes
;(defun comment-auto-fill ()
;  (setq-local comment-auto-fill-only-comments t)
;  (auto-fill-mode 1))
;;; C-mode
;(add-hook 'c-mode-common-hook 'comment-auto-fill)

;; To add line numbers in the margin
(global-linum-mode t)

;; Add column numbers
(setq column-number-mode t)

;; Set fill-column to 80
(setq-default fill-column 80)
(setq fill-column 80)

;; Add fill-column-indicator package
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (if (and
           (not (string-match "^\*.*\*$" (buffer-name)))
           (not (eq major-mode 'dired-mode)))
          (fci-mode 1))))
  (global-fci-mode 1)

;; use dtrt-indent.el
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; To use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; set default tab-width (this is for viewing and not for editing, but fuck it...)
;; when default is set, tab will always be 2. even for ones that don't have it.
;; good for if you want to untabify a file
(setq-default tab-width 2)
(setq tab-width 2)
(setq indent-line-function 'insert-tab)

;; make this consistent
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; change the indentation level for c
;(setq-default c-basic-offset 2)

;;make emacs show c code properly (not indent braces per gnu)
(setq c-default-style "linux")
;;make it gnu
;(setq c-default-style "gnu")

;; Use c-mode for CUDA
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c-mode))

;;[tab] inserts 2 spaces (there are better ways to do this per mode,
;; set the tab stops to use (default is 8 spaces apart)
(setq tab-stop-list (number-sequence 2 120 2))

;; reindents the line only if point is to the left of the first non-whitespace character on the line.
;; Otherwise it inserts some whitespace.
(setq c-tab-always-indent nil)
 
;; auto-complete settings
(require 'auto-complete)
(global-auto-complete-mode t)
;; bind ret to original and tab to finish completion
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)
; use tab as autocomplete trigger key

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wheatgrass)))
 '(package-selected-packages
   (quote
    (markdown-mode dtrt-indent auto-complete auto-compile)))
 '(safe-local-variable-values (quote ((auto-fill-mode . 1) (auto-fill-mode)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; make backups
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )
