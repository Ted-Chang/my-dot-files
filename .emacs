;; Copyright (C) 2012 Ted Chang. All rights reserved
;; E-mail: zgk_1208@yahoo.com.cn
;; 
;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.

(message "* --[ Loading my Emacs init file ]--")

;; uptimes
(setq emacs-load-start-time (current-time))

;; backup~ file settings
(setq make-backup-files t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; add a new load path
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(defvar missing-package-list nil
  "List of packages that `try-require' can't find.")

;; attempt to load a feature/library, failing silently
(defun try-require(feature)
  "Attempt to load a library or module. Return true if the library
given as argument is successfully loaded. If not, instead of an error,
just add the package to a list of missing packages."
  (condition-case err
      ;; protected from
      (progn
	(message "checking for library `%s' ..." feature)
	(if (stringp feature)
	    (load-library feature)
	  (require feature))
	(message "checking for library `%s' ... found" feature))
    ;; error handler
    (file-error	; condition
     (progn
       (message "checking for library `%s' ... missing" feature)
       (add-to-list 'missing-package-list feature 'append))
     nil)))

;; UI
;; turn off the menu mode
;(menu-bar-mode -1)

;; turn off the toobar mode
(tool-bar-mode -1)

;; turn off the scroll bar
(scroll-bar-mode -1)

;; turn on syntax coloring
(global-font-lock-mode t)

;; show line number
(global-linum-mode t)
(message "UI initialization... Done")

;; font settings
;; use `C-x C-+' `C-x C--' to resize (zoom) the buffer text
;;(set-face-attribute 'default nil :font "Courier New-10")

;; maximizing emacs on startup in Windows
(defun maximize-frame()
  "Maximize the active frame in Windows"
  (interactive)
  ; Send a `WM_SYSCOMMAND' message to the active frame with the
  ; `SC_MAXIMIZE' parameter
  (when (eq system-type 'windows-nt)
    (w32-send-sys-command 61488)))
(add-hook 'window-setup-hook 'maximize-frame t)

;; tabbar configuration
(require 'tabbar)
(tabbar-mode)
(setq tabbar-buffer-groups-function
      (lambda ()
	(list "All Buffers")))
(setq tabbar-buffer-list-function
      (lambda ()
	(remove-if
	 (lambda (buffer)
	   (find (aref(buffer-name buffer) 0) " *"))
	 (buffer-list))))
(defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
  `(defun ,name (arg)
     (interactive "P")
     ,do-always
     (if (equal nil arg)
	 ,on-no-prefix
       ,on-prefix)))
(defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
(defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
(global-set-key (kbd "<C-tab>") 'shk-tabbar-next)
(global-set-key (kbd "<C-S-iso-lefttab>") 'shk-tabbar-prev)

;; move through camelCaseWords
(global-subword-mode 1)

;; turn on paren match highlighting
(show-paren-mode 1)
(setq show-paren-ring-bell-on-mismatch t)

;; mode line
;; show the cursor's column position and line position
(setq column-number-mode t)
(setq line-number-mode t)
(setq size-indication-mode t)

;; color-theme
;; set the color theme at the very beginning
(when (try-require 'color-theme)
  (color-theme-initialize)
  (color-theme-dark-blue2))

;; minibuffer
;; enable switching between buffers using substring
(iswitchb-mode t)
;; minibuffer completion incremental feedback
(icomplete-mode t)
;; do not consider case significant in completion
(setq read-buffer-completion-ignore-case t)
(auto-insert-mode t)
(message "The Minibuffer... Done")

;; don't add newline to end of buffer when scrolling
(setq next-line-add-newlines nil)

;; buffer menu for buffer switch
(global-set-key (kbd "C-x C-b") 'buffer-menu)
;; kill buffer
;; replace the original kill buffer with my kill buffer
(defun my-kill-this-buffer()
  "Kill the buffer without confirmation (if not modified)"
  (interactive)
  (kill-buffer nil))
(global-set-key (kbd "C-x k") 'my-kill-this-buffer)
(message "Using multiple buffer... Done")

;; goto the specified line
(global-set-key (kbd "M-g") 'goto-line)

;; eleminate long "yes" or "no" prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; stop creating those #auto-save# files
(setq auto-save-default nil)

;; recent file
;; keep a list of recently opened files
(recentf-mode 1)
(setq recentf-max-saved-items 300
      recentf-exclude '("/tmp/" "/ssh:"))
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(defun ido-recentf-open()
  "use `ido-completeing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file ...")
    (message "Aborting")))

;; replace highlighted text with what you type
(delete-selection-mode 1)

;; winner-mode
;; use `MODIFIER-{left,right,up,down}' to move between windows
;; default MODIFIER is shift
;; use `C-x-{left,right}' to redo or undo window layout
(when (fboundp 'winner-mode)
  (winner-mode)
  (windmove-default-keybindings))

;; shell
;; use `C-u M-x shell' to start another shell
;; make our shell support ANSI color
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(defun clear-shell()
  (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)))
(global-set-key (kbd "C-c z") 'shell)

;; scrolling
;; scroll line by line
(setq scroll-step 1)

;; CC-mode configuration http://cc-mode.sourceforge.net
;; use `M-i' to insert a tab
(require 'cc-mode)
(defun my-c-mode-common-hook()
  (setq c-basic-offset 8
	tab-width 8
	indent-tabs-mode t)
  (setq abbrev-mode t)
  ;; auto indent the new line
  (local-set-key (kbd "RET") 'newline-and-indent)
)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; text-mode
(defun my-text-mode-hook()
  "Turn on filling mode in text mode"
  (turn-on-auto-fill))
(add-hook 'text-mode-hook 'my-text-mode-hook)

;; cua-seletion-mode
(cua-selection-mode t)

;; where to save the bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks.txt")
;; each command that sets a bookmark will also save your bookmarks
(setq bookmark-save-flag 1)

;; Speedbar configuration
;; use sr-speedbar-toggle to open the speedbar
(try-require 'sr-speedbar)

;; auto-complet configuration
(require 'auto-complete-config)
(global-auto-complete-mode t)
(ac-config-default)	; Use default configuration for auto complete
(setq ac-auto-start t)
(setq ac-dwim t)	; To get pop-ups with docs even if a word is uniquely complete
(setq ac-use-menu-map t)

;; Visual white space configuration
(require 'whitespace)
;; Use whitespace-mode to show/hide the white spaces

;; Display time mode
(display-time-mode t)

;; yanking
;; auto-indent pasted code
(defadvice yank (after indent-region activate)
  (if (member major-mode
	      '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
				latex-mode python-mode))
      (indent-region (region-beginning) (region-end) nil)))
(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
	      '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
				latex-mode python-mode))
      (indent-region (region-beginning) (region-end) nil)))
(message "Yanking... Done")

;; search and replacement
(defun isearch-occur ()
  "Invoke `occur' from within isearch"
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
;; query replace
(global-set-key (kbd "M-#") 'query-replace-regexp)
(message "Search and replacement... Done")

;; Automatically reload files was modified by external program
(global-auto-revert-mode t)
;; and display a message to notify the user
(defun inform-revert-modified-file (&optional p1 p2)
  "my custom function"
  (let ((revert-buffer-function nil))
    (revert-buffer p1 p2)
    (message "File `%s' automatically reverted" buffer-file-name)))
(setq revert-buffer-function 'inform-revert-modified-file)

;; CEDET configuration
;; I use the CEDET come with the emacs instead of the stand-alone one
(require 'cedet)
(global-ede-mode t)		; Enable the project management system
(semantic-mode t)
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
				  global-semanticdb-minor-mode
				  global-semantic-idle-summary-mode
				  global-semantic-mru-bookmark-mode))
(defun my-cedet-hook()
  ;; Jump to the definition
  (local-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c s") 'semantic-ia-show-summary)
  (local-set-key (kbd "C-c p") 'semantic-analyze-proto-impl-toggle)
  )
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(message "Editing programs... Done")

;; full name of this user
(setq user-full-name "Ted Chang")

;; full mailing address of this user
;; (used in MAIL envelope FORM, and to select the default personality ID)
(setq user-mail-address
      (concat (rot13-string "zgk_1208") "@" "yahoo.com.cn"))

;; Debugging
(if missing-package-list
    (progn (message "Packages not found: %S" missing-package-list)))