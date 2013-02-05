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

;; Environment
(defvar running-ms-windows
  (eq system-type 'windows-nt))

(defvar running-ms-windows
  (string-match "windows" (prin1-to-string system-type)))

(defvar running-gnu-linux
  (string-match "linux" (prin1-to-string system-type)))

;; OS type --- are we running GNU Linux?
(defmacro GNULinux (&rest body)
  (list 'if (string-match "linux" (prin1-to-string system-type))
	(cons 'progn body)))

(defmacro Windows (&rest body)
  (list 'if (string-match "windows" (prin1-to-string system-type))
	(cons 'progn body)))

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
(if (functionp 'menu-bar-mode) (menu-bar-mode -1))

;; turn off the toobar mode
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))

;; turn off the scroll bar
(if (functionp 'scroll-bar-mode) (scroll-bar-mode -1))

;; turn on syntax coloring
(global-font-lock-mode t)

;; show line number
(when (try-require 'linum)
  (global-linum-mode t))
(message "UI initialization... Done")

;; font settings
;; use `C-x C-+' `C-x C--' to resize (zoom) the buffer text
;;(set-face-attribute 'default nil :font "Courier New-10")

;; quiet, no dingding!
(setq visible-bell t)

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
;; On my laptop <C-S-tab> not work in Fedora 16
(cond
 (running-gnu-linux
  (global-set-key (kbd "<C-S-iso-lefttab>") 'shk-tabbar-prev))
 (running-ms-windows
  (global-set-key (kbd "<C-S-tab>") 'shk-tabbar-prev)))

;; interactively do things
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; make tooltips appear in the echo area
(when (try-require 'tooltip-mode)
  (tooltip-mode 1)
  (setq tooltip-use-echo-area t))

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
(global-set-key (kbd "<f10>") 'rename-buffer)
;;(global-set-key (kbd "C-x C-b") 'buffer-menu)
;; `buffer-menu' moves point in the window which list your buffers
;; `electric-buffer-list' pops up a buffer describing the set of buffers
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

;; kill buffer
;; replace the original kill buffer with my kill buffer
(defun my-kill-this-buffer()
  "Kill the buffer without confirmation (if not modified)"
  (interactive)
  (kill-buffer nil))
(global-set-key (kbd "C-x k") 'my-kill-this-buffer)
(message "Using multiple buffer... Done")

;; goto the specified line
;; `M-g M-p' and `M-g M-n' is useful in compile mode
(global-set-key (kbd "M-g M-g") 'goto-line)

;; eleminate long "yes" or "no" prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; stop creating those #auto-save# files
(setq auto-save-default nil)

;; move (shift) a line of text up or down like you would do in Eclipse
;; pressing `M-up' (or `M-down')
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
	start
	end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))
(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))
(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))
(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)

;; Behave like VI's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a new line."
  (interactive "P")
  (next-line 1)
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode))
)
;; Behave like VI's O command
(defun open-prev-line (arg)
  "Open a new line before the current line."
  (interactive "P")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode))
)
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-prev-line)

;; redo the most recent undo
(when (try-require 'redo+)
  (global-set-key (kbd "C-?") 'redo))

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
;; cycle through all windows on current frame
(global-set-key (kbd "<f6>") 'other-window)
;; enlarge or shrink windows more easily than with `C-x {' and the like
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)

;; shell
;; use `C-u M-x shell' to start another shell
;; use `C-c C-c' to interrupt the shell or its current subjob
;; make our shell support ANSI color
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
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
;; set indent-tabs-mode to nil if you want spaces instead of tabs
(require 'cc-mode)
(defun my-c-mode-hook()
  (c-set-offset 'substatement-open 0)
  (setq c-basic-offset 8
	tab-width 8
	indent-tabs-mode t)
  (setq abbrev-mode t)
  ;; auto indent the new line
  (local-set-key (kbd "RET") 'newline-and-indent)
)
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

;; text-mode
(defun my-text-mode-hook()
  "Turn on filling mode in text mode"
  ;;(turn-on-auto-fill)
)
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
(when (try-require 'auto-complete-config)
  (global-auto-complete-mode t)
  (ac-config-default)	; Use default configuration for auto complete
  (setq ac-dwim t)	; To get pop-ups with docs even if a word is uniquely complete
  (setq ac-use-menu-map t)
  (setq ac-auto-start 4)
)

;; Visual white space configuration
(require 'whitespace)
;; Use whitespace-mode to show/hide the white spaces

;; Display time mode
(display-time-mode t)

;; When the mark is active, the *region is highlighted*
(transient-mark-mode t)

;; Change cutting behavior:
;; "Many times you'll do a kill-line command with the only intention of
;; getting the contents of the line into the killring. Here's an idea stolen
;; from Slickedit, if you press copy or cut when no region is active, you'll
;; copy or cut the current line."
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy the current line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))
(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill the current line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))
(message "Killing and Moving Text... Done")

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
;; interactively insert items from kill ring
(when (try-require 'browse-kill-ring)
  ;; string separating entries in the `separated' style
  (setq browse-kill-ring-separator
	"\n---separator------------------------------")
  ;; temporarily highlight the inserted `kill-ring' entry
  (setq browse-kill-ring-highlight-inserted-item t)
  ;; face in which to highlight the `browse-kill-ring-separator'
  (defface separator-face '((t (:foreground "Blueviolet" :weight bold))) nil)
  (setq browse-kill-ring-separator-face 'separator-face)
  ;; use `M-y' to invoke `browse-kill-ring'
  (browse-kill-ring-default-keybindings))
(message "Yanking... Done")

;; using cursor color to indicate some modes (read-only, insert and
;; overwrite modes)
(setq my-set-cursor-color-color "")
(setq my-set-cursor-color-buffer "")
(defun my-set-cursor-color-according-to-mode ()
  "Change cursor color according to some minor modes."
  (let ((color
	 (if buffer-read-only "yellow"
	   (if overwrite-mode "red"
	     "#15FF00"))))	; insert mode
    (unless (and (string= color my-set-cursor-color-color)
		 (string= (buffer-name) my-set-cursor-color-buffer))
      (set-cursor-color (setq my-set-cursor-color-color color))
      (setq my-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'my-set-cursor-color-according-to-mode)

;; search and replacement
;; isearch-occur is replaced with `M-s o' starting with Emacs 23
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

;; Dired configurations
;; Try to guess a default target directory
(setq dired-dwim-target t)
;; Enable the use of the command `dired-find-alternate-file'
;; without confirmation
(put 'dired-find-alternate-file 'disabled nil)
;; Copy recursively without asking
(setq dired-recursive-copies 'always)
;; Make dired use the same buffer for viewing directory, instead
;; of spawning may
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "<RET>")
	      'dired-find-alternate-file)	; was dired-advertised-find-file
	    (define-key dired-mode-map (kbd "^")
	      (lambda () (interactive) (find-alternate-file ".."))))
	    ; was dired-up-directory
)
;; Openning files in external apps
(defun open-in-external-app ()
  "Open the current file or dired marked files in external app.
Works in Microsoft Windows and Linux."
  (interactive)
  (let (doIt
	(myFileList
	 (cond
	  ((string-equal major-mode "dired-mode") (dired-get-marked-files))
	  (t (list (buffer-file-name))) ) ) ) 
    (setq doIt (if (<= (length myFileList) 5)
		   t
		 (y-or-n-p "Open more than 5 files?")))
    (when doIt
      (cond
       (running-ms-windows
	(mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList)
	)
       (running-gnu-linux
	(mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList)))))
)
(message "Dired... Done")

;; Cscope configuration
(when (try-require 'xcscope)
  ;; By default, xcscope.el does automatic indexing by use of a Bash 
  ;; script(cscope-indexer). As Windows lacks Bash support, so disable it.
  ;; Usage:
  ;; >PROJ=/home/project
  ;; >find "$PROJ/include" "$PROJ/module1" "$PROJ/module2" \
  ;; > -name "*.[ch]*" -print > "$PROJ/cscope.files"
  ;; >cd "$PROJ"
  ;; >cscope -b -q -k
  ;; Or
  ;; >cscope -b -k -R -I incdir -s srcdir
  ;; cscope command: cscope -b -k -R
  (setq cscope-do-not-update-database t))
(message "cscope... Done")

;; Show the function name at mode line
(which-function-mode t)

(when (try-require 'quick-jump)
  (quick-jump-default-keybinding))

;; offer save of `*scratch*' buffer on exit
(save-excursion
  (set-buffer "*scratch*")
  (setq buffer-file-name "~/*scratch*"))

;; full name of this user
(setq user-full-name "Ted Chang")

;; full mailing address of this user
;; (used in MAIL envelope FORM, and to select the default personality ID)
(setq user-mail-address
      (concat (rot13-string "zgk_1208") "@" "yahoo.com.cn"))

;; Debugging
(if missing-package-list
    (progn (message "Packages not found: %S" missing-package-list)))
