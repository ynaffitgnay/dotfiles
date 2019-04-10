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

;; activate installed packages
(package-initialize)

;; make sure to have a downloaded archive description.
(unless package-archive-contents
    (package-refresh-contents))

;; Define list of important packages
(setq reqd-packages '(dtrt-indent async diff magit auto-complete hydra
          			      markdown-mode))

;; Remove packages that require the newest version of emacs from the list
(when (< emacs-major-version 25) (dolist (pkg '(magit nil))
  (setq reqd-packages (delq pkg reqd-packages))))

;; Ensure that all of the listed packages installed.
;; Otherwise, refresh the package archive to install the correct version.
(setq n 0)                                  ; set n as 0
(dolist (pkg reqd-packages)                 ; for each pkg in list
  (unless
    (package-installed-p pkg)               ; pkg is installed
      (setq n (+ n 1))))                    ; increment n

(when (> n 0)                               ; if n > 0, 
  (package-refresh-contents))               ; refresh packages

;; make sure that the proper packages are installed
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Ensure the packages in the list are installed
(mapcar 'ensure-package-installed reqd-packages)
 
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

;; Asynchronously run dired commands for copying, renaming, and symlinking
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

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
;(setq fill-column 80)

;; Add fill-column-indicator package
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (if (and
           (not (string-match "^\*.*\*$" (buffer-name)))
           (not (eq major-mode 'dired-mode)))
          (fci-mode 1))))
  (global-fci-mode 1)

;; To use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)
;(setq indent-tabs-mode nil)

;; use dtrt-indent.el 
(require 'dtrt-indent)
;; turn dtrt-indent on globally
(setq-default dtrt-indent-mode 1)
;(electric-indent-mode -1)

;; set default tab-width (this is for viewing and not for editing, but fuck it...)
;; when default is set, tab will always be 2. even for ones that don't have it.
;; good for if you want to untabify a file
(setq-default tab-width 2)
;(setq tab-width 2)
(setq indent-line-function 'insert-tab)

;; make the indentation levels consistent
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;;make emacs show c code properly (not indent braces per gnu)
(setq c-default-style "linux")
;;make it gnu
;(setq c-default-style "gnu")

;; Use c-mode for CUDA
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c-mode))

;;[tab] inserts 2 spaces (there are better ways to do this per mode,
;; set the tab stops to use (default is 8 spaces apart)
(setq tab-stop-list (number-sequence 2 120 2))

;; reindents the line only if point is to the left of the first non-whitespace 
;; character on the line.
;; Otherwise it inserts some whitespace.
(setq c-tab-always-indent nil)
 
;; auto-complete settings
(require 'auto-complete)
(global-auto-complete-mode t)
;; bind ret to original and tab to finish completion
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)
; use tab as autocomplete trigger key

;; Define a hydra binding to use arrow keys to jump between windows
(defhydra hydra-other-window
  (global-map "C-x"
              :color red)
  "other window"
  ("<down>" other-window "↓")
  ("<up>" (lambda () (interactive) (other-window -1)) "↑"))

;; Define a hydra binding to use arrow keys to jump between buffers
(defhydra hydra-other-buffer
  (global-map "C-x"
              :color red)
  "other buffer"
  ("<right>" next-buffer "→")
  ("<left>" previous-buffer "←"))
  
;; Define function to get diff between two buffers
(require 'diff)
(require 'async)
(defun diff-buffers-without-temp-files (buffer1 buffer2 &optional switches)
  "Run diff program on BUFFER1 and BUFFER2.
Make the comparison without the creation of temporary files.

When called interactively with a prefix argument, prompt
interactively for diff switches.  Otherwise, the switches
specified in the variable `diff-switches' are passed to the diff command."
  (interactive
   (list (read-buffer "buffer1: " (current-buffer))
         (read-buffer "buffer2: " (current-buffer))
         (diff-switches)))
  (or switches (setq switches diff-switches))
  (unless (listp switches) (setq switches (list switches)))
  (let ((buffers (list buffer1 buffer2))
        (buf (get-buffer-create "*diff-buffers*"))
        fifos res)
    (dotimes (_ 2) (push (make-temp-name "/tmp/pipe") fifos))
    (setq fifos (nreverse fifos))
    (with-current-buffer buf (erase-buffer))
    (unwind-protect
        (progn
          (dotimes (i 2)
            (let ((cmd (format "cat > %s" (nth i fifos))))
              (call-process "mkfifo" nil nil nil (nth i fifos))
              (async-start
               `(lambda ()
                  (with-temp-buffer
                    (insert ,(with-current-buffer (nth i buffers) (buffer-string)))
                    (call-process-region
                     1 (point-max) shell-file-name nil nil nil
                     shell-command-switch ,cmd))))))
          (setq res (apply #'call-process diff-command nil buf nil (car fifos) (cadr fifos) switches))
          (if (zerop res)
              (message "Buffers have same content")
            (display-buffer buf)
            (with-current-buffer buf (diff-mode))
            (message "Buffer contents are different"))
          res)
      ;; Clean up.
      (dolist (x fifos)
        (and (file-exists-p x) (delete-file x))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wheatgrass)))
 '(package-selected-packages
   (quote
    (markdown-mode dtrt-indent auto-complete auto-compile)))
 '(safe-local-variable-values
   (quote
    ((c-file-offsets
      (block-close . 0)
      (brace-list-close . 0)
      (brace-list-entry . 0)
      (brace-list-intro . +)
      (case-label . 0)
      (class-close . 0)
      (defun-block-intro . +)
      (defun-close . 0)
      (defun-open . 0)
      (else-clause . 0)
      (inclass . +)
      (label . 0)
      (statement . 0)
      (statement-block-intro . +)
      (statement-case-intro . +)
      (statement-cont . +)
      (substatement . +)
      (topmost-intro . 0))
     (auto-fill-mode . 1)
     (auto-fill-mode))))
 '(verilog-align-ifelse t)
 '(verilog-auto-indent-on-newline nil)
 '(verilog-indent-begin-after-if nil)
 '(verilog-indent-level 2)
 '(verilog-indent-level-behavioral 2)
 '(verilog-indent-level-declaration 2)
 '(verilog-indent-level-directive 0)
 '(verilog-indent-level-module 2))
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
