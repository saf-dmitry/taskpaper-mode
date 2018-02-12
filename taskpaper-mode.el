;;; taskpaper-mode.el --- Major mode for working with TaskPaper files

;; Copyright 2016-2017 Dmitry Safronov

;; Author: Dmitry Safronov <saf.dmitry@gmail.com>
;; Maintainer: Dmitry Safronov <saf.dmitry@gmail.com>
;; URL: <https://github.com/saf-dmitry/taskpaper-mode>
;; Keywords: outlines, notetaking, task management, taskpaper

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TaskPaper mode is a major mode for working with files in TaskPaper
;; format. The format was invented by Jesse Grosjean and named after his
;; TaskPaper macOS app <https://www.taskpaper.com>, which is a system
;; for organizing your outlines and tasks in a text file.
;;
;; TaskPaper mode is implemented on top of Outline mode. Visibility
;; cycling and structure editing help to work with the outline
;; structure. Special commands also provided for tags manipulation,
;; sorting, querying, refiling, and archiving of items.

;;; Code:

;;;; Loaded modules

(require 'outline)
(require 'font-lock)
(require 'easymenu)
(require 'calendar)
(require 'parse-time)
(require 'cal-iso)

;;;; Variables

(defconst taskpaper-mode-version "0.4"
  "TaskPaper mode version number.")

(defvar taskpaper-mode-map (make-keymap)
  "Keymap for TaskPaper mode.")

(defvar taskpaper-mode-syntax-table
  (let ((tab (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?@ "\'" tab)
    tab)
  "Syntax table for TaskPaper mode.")

(defvar taskpaper-read-date-history nil
  "History list for date prompt.")

(defvar taskpaper-query-history nil
  "History list for query prompt.")

;;;; Custom variables

(defgroup taskpaper nil
  "Major mode for editing and querying files in TaskPaper format."
  :prefix "taskpaper-"
  :group 'wp
  :group 'text
  :group 'applications)

(defcustom taskpaper-faces-easy-properties :foreground
  "The property changes by easy faces.
The value can be :foreground or :background. A color string for
specific tags will then be interpreted as either foreground or
background color. For more details see custom variable
`taskpaper-tag-faces'."
  :group 'taskpaper
  :type '(choice (const :foreground)
                 (const :background)))

(defcustom taskpaper-tag-faces nil
  "Faces for specific tags.
This is a list of cons cells, with tag names in the car and faces
in the cdr. The tag name can basically contain uppercase and
lowercase letters, digits, hyphens, underscores, and dots. The
face can be a symbol corresponding to a name of an existing face,
a color (in which case it will be interpreted as either
foreground or background color according to the variable
`taskpaper-faces-easy-properties' and the rest is inherited from
the face `taskpaper-tag-face') or a property list of attributes,
like (:foreground \"blue\" :weight bold)."
  :group 'taskpaper
  :type '(repeat
          (cons (string :tag "Tag name")
                (choice :tag "Face"
                        (string :tag "Color")
                        (sexp :tag "Face")))))

(defcustom taskpaper-tag-alist nil
  "List of tags for fast selection.
The value of this variable is an alist, the car of each entry
must be a tag as a string, the cdr may be a character that is
used to select that tag through the fast-selection interface."
  :group 'taskpaper
  :type '(repeat
          (cons (string :tag "Tag name")
                (character :tag "Access char"))))

(defcustom taskpaper-tags-exclude-from-inheritance nil
  "List of tags that should never be inherited."
  :group 'taskpaper
  :type '(repeat (string :tag "Tag name")))

(defcustom taskpaper-tags-to-remove-when-done nil
  "List of tags to remove when tagging with \"@done\"."
  :group 'taskpaper
  :type '(repeat (string :tag "Tag name")))

(defcustom taskpaper-complete-save-date t
  "Non-nil means, include date when tagging with \"@done\"."
  :group 'taskpaper
  :type 'boolean)

(defcustom taskpaper-read-date-popup-calendar t
  "Non-nil means, pop up a calendar when prompting for a date."
  :group 'taskpaper
  :type 'boolean)

(defcustom taskpaper-read-date-display-live t
  "Non-nil means, display the date prompt interpretation live."
  :group 'taskpaper
  :type 'boolean)

(defcustom taskpaper-startup-folded nil
  "Non-nil means switch to OVERVIEW when entering TaskPaper mode."
  :group 'taskpaper
  :type 'boolean)

(defcustom taskpaper-startup-with-inline-images nil
  "Non-nil means show inline images when entering TaskPaper mode."
  :group 'taskpaper
  :type 'boolean)

(defcustom taskpaper-max-image-size nil
  "Maximum width and height for displayed inline images.
This variable may be nil or a cons cell (MAX-WIDTH . MAX-HEIGHT),
where MAX-WIDTH and MAX-HEIGHT are maximum image width and height
in pixels. When nil, use the actual size. Otherwise, use
ImageMagick to resize larger images. This requires Emacs to be
built with ImageMagick support."
  :group 'taskpaper
  :type '(choice
          (const :tag "Actual size" nil)
          (cons (choice (sexp :tag "Maximum width")
                        (const :tag "No maximum width" nil))
                (choice (sexp :tag "Maximum height")
                        (const :tag "No maximum height" nil)))))

(defcustom taskpaper-after-sorting-items-hook nil
  "Hook run after sorting of items.
When children are sorted, the cursor is in the parent line when
this hook gets called."
  :group 'taskpaper
  :type 'hook)

(defcustom taskpaper-archive-location "%s_archive.taskpaper::"
  "The location where subtrees should be archived.

The value of this variable is a string, consisting of two parts,
separated by a double-colon. The first part is a filename and the
second part is a heading.

When the filename is omitted, archiving happens in the same file.
A %s formatter in the filename will be replaced by the current
file name (without the directory part and file extension).

The archived items will be filed as subtrees of the specified
item. When the heading is omitted, the subtrees are simply filed
away at the end of the file, as top-level items. Also in the
heading you can use %s to represent the file name, this can be
useful when using the same archive for a number of different
files."
  :group 'taskpaper
  :type 'string)

(defcustom taskpaper-archive-save-context nil
  "Non-nil means, add context information when archiving."
  :group 'taskpaper
  :type 'boolean)

(defcustom taskpaper-archive-hook nil
  "Hook run after successfully archiving a subtree.
Hook functions are called with point on the subtree in the
original location. At this stage, the subtree has been added to
the archive location, but not yet deleted from the original
location."
  :group 'taskpaper
  :type 'hook)

(defcustom taskpaper-reverse-note-order nil
  "Non-nil means, store new notes at the beginning of item."
  :group 'taskpaper
  :type 'boolean)

(defcustom taskpaper-file-apps
  '((directory . emacs)
    (remote . emacs)
    (auto-mode . emacs))
  "External applications for opening file links in a document.
The entries in this list are cons cells where the car identifies
files and the cdr the corresponding command.

Possible values for the file identifier are:
 \"string\"   Matches files with this extension
 directory  Matches directories
 remote     Matches remote files
 auto-mode  Matches files that are matched by any entry in `auto-mode-alist'
 system     The system command to open files
 t          Default for files not matched by any of the other options

Possible values for the command are:
 \"string\"   A command to be executed by a shell;
            a %s formatter will be replaced by the file name
 emacs      The file will be visited by the current Emacs process
 default    Use the default application for this file type
 system     Use the system command for opening files
 mailcap    Use command specified in the mailcaps
 function   A Lisp function, which will be called with one argument:
            the file path

See also variable `taskpaper-open-non-existing-files'."
  :group 'taskpaper
  :type '(repeat
          (cons
           (choice :value ""
                   (string :tag "Extension")
                   (const :tag "System command to open files" system)
                   (const :tag "Default for unrecognized files" t)
                   (const :tag "Remote file" remote)
                   (const :tag "Links to a directory" directory)
                   (const :tag "Any files that have Emacs modes" auto-mode))
           (choice :value ""
                   (const :tag "Visit with Emacs" emacs)
                   (const :tag "Use default" default)
                   (const :tag "Use the system command" system)
                   (string :tag "Command")
                   (sexp :tag "Lisp form")))))

(defcustom taskpaper-open-non-existing-files nil
  "Non-nil means, `taskpaper-open-file' will open non-existing files.
When nil, an error will be generated. This variable applies only
to external applications because they might choke on non-existing
files. If the link is to a file that will be opened in Emacs, the
variable is ignored."
  :group 'taskpaper
  :type 'boolean)

(defcustom taskpaper-custom-queries nil
  "List of custom queries for fast selection.
The value of this variable is a list, the first element is a
character that is used to select that tag through the
fast-selection interface, the second element is a short
description string, and the last must be a query string. If the
first element is a string, it will be used as block separator."
  :group 'taskpaper
  :type '(repeat
          (choice (list (character :tag "Access char")
                        (string :tag "Description")
                        (string :tag "Query string"))
                  (string :tag "Block separator"))))

(defcustom taskpaper-pretty-task-marks t
  "Non-nil means, enable the composition display of task marks.
This does not change the underlying buffer content, but it
overlays the UTF-8 character for display purposes only."
  :group 'taskpaper
  :type 'boolean)

(defcustom taskpaper-bullet ?\u2014
  "Display character for task mark."
  :group 'taskpaper
  :type 'character)

(defcustom taskpaper-bullet-done ?\u2014
  "Display character for done task mark."
  :group 'taskpaper
  :type 'character)

(defcustom taskpaper-mode-hook nil
  "Hook run when entering `taskpaper-mode'."
  :group 'taskpaper
  :type 'hook)

;;;; Common definitions

(defun taskpaper-mode-version ()
  "Show TaskPaper mode version."
  (interactive)
  (message "TaskPaper mode version %s" taskpaper-mode-version))

(defun taskpaper-mode-manual ()
  "Browse Taskpaper mode user's manual."
  (interactive)
  (browse-url
   "https://github.com/saf-dmitry/taskpaper-mode/blob/master/README.md"))

;; NOTE: Added in Emacs v23.2
(unless (fboundp 'string-prefix-p)
  (defun string-prefix-p (prefix string &optional ignore-case)
    "Return non-nil if PREFIX is a prefix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
    (let ((prefix-length (length prefix)))
      (if (> prefix-length (length string)) nil
        (eq t (compare-strings prefix 0 prefix-length string 0
                               prefix-length ignore-case))))))

;; NOTE: Added in Emacs v24.4
(unless (fboundp 'string-suffix-p)
  (defun string-suffix-p (suffix string  &optional ignore-case)
    "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
    (let ((start-pos (- (length string) (length suffix))))
      (and (>= start-pos 0)
           (eq t (compare-strings suffix nil nil string
                                  start-pos nil ignore-case))))))

(defun taskpaper-chomp (str)
  "Chomp leading and trailing whitespaces from STR."
  (setq str (replace-regexp-in-string "\\`[ \t\n\r]*" "" str)
        str (replace-regexp-in-string "[ \t\n\r]*\\'" "" str))
  str)

(defun taskpaper-overlay-display (ovl text &optional face evap)
  "Make overlay OVL display TEXT with face FACE.
EVAP non-nil means, set the `evaporate' property to t."
  (overlay-put ovl 'display text)
  (when face (overlay-put ovl 'face face))
  (when evap (overlay-put ovl 'evaporate t)))

(defun taskpaper-new-marker (&optional pos)
  "Return a new marker.
Marker is at point, or at POS if non-nil."
  (let ((marker (copy-marker (or pos (point)) t))) marker))

(defsubst taskpaper-get-at-bol (property)
  "Get text property PROPERTY at the beginning of line."
  (get-text-property (point-at-bol) property))

(defun taskpaper-release-buffers (blist)
  "Release all buffers in BLIST.
When a buffer is unmodified, it is just killed. When modified, it
is saved after user confirmation and then killed."
  (let (file)
    (dolist (buf blist)
      (setq file (buffer-file-name buf))
      (when (and (buffer-modified-p buf) file
                 (y-or-n-p (format "Save file %s? " file)))
        (with-current-buffer buf (save-buffer)))
      (kill-buffer buf))))

(defun taskpaper-find-base-buffer-visiting (file)
  "Return the base buffer visiting FILE.
Like `find-buffer-visiting' but always returns the base buffer
and not an indirect buffer."
  (let ((buf (or (get-file-buffer file)
                 (find-buffer-visiting file))))
    (if buf (or (buffer-base-buffer buf) buf) nil)))

(defsubst taskpaper-check-invisible ()
  "Return non-nil if point is in an invisible region."
  (if (and
       (or (not (boundp 'visible-mode)) (not visible-mode))
       (or (get-char-property (point) 'invisible)
           (get-char-property (max (point-min) (1- (point)))
                              'invisible)))
      (user-error "Unfold subtree before editing")))

(defun taskpaper-self-insert-command (N)
  "Modified version of `self-insert-command'."
  (interactive "p")
  (taskpaper-check-invisible) (self-insert-command N))

(defun taskpaper-delete-char (N)
  "Modified version of `delete-char'."
  (interactive "p")
  (taskpaper-check-invisible) (delete-char N))

(defun taskpaper-delete-forward-char (N)
  "Modified version of `delete-forward-char'."
  (interactive "p")
  (taskpaper-check-invisible) (delete-forward-char N))

(defun taskpaper-delete-backward-char (N)
  "Modified version of `delete-backward-char'."
  (interactive "p")
  (with-no-warnings
    (taskpaper-check-invisible) (delete-backward-char N)))

(defun taskpaper-remap (map &rest commands)
  "In keymap MAP, remap the functions given in COMMANDS.
COMMANDS is a list of alternating OLDDEF NEWDEF command names."
  (let (new old)
    (while commands
      (setq old (pop commands) new (pop commands))
      (if (fboundp 'command-remapping)
          (define-key map (vector 'remap old) new)
        (substitute-key-definition old new map global-map)))))

(taskpaper-remap
 taskpaper-mode-map
 #'self-insert-command #'taskpaper-self-insert-command
 #'delete-char #'taskpaper-delete-char
 #'delete-forward-char #'taskpaper-delete-forward-char
 #'delete-backward-char #'taskpaper-delete-backward-char)

(defun taskpaper-in-regexp-p (regexp)
  "Return non-nil if point is in a match for REGEXP.
Set the match data. Only the current line is checked."
  (catch 'exit
    (let ((pos (point)) (eol (line-end-position 1)))
      (save-excursion
        (beginning-of-line)
        (while (re-search-forward regexp eol t)
          (if (and (<= (match-beginning 0) pos)
                   (>= (match-end 0) pos))
              (throw 'exit (cons (match-beginning 0)
                                 (match-end 0)))))))))

(defsubst taskpaper-set-local (var value)
  "Make VAR local in the current buffer and set it to VALUE."
  (set (make-local-variable var) value))

(defsubst taskpaper-uniquify (list)
  "Non-destructively remove duplicate elements from LIST."
  (let ((res (copy-sequence list))) (delete-dups res)))

(defsubst taskpaper-sort (list)
  "Non-destructively sort elements of LIST as strings."
  (let ((res (copy-sequence list))) (sort res 'string-lessp)))

(defconst taskpaper-nonsticky-props
  '(face mouse-face keymap help-echo display invisible intangible))

(defsubst taskpaper-rear-nonsticky-at (pos)
  "Add nonsticky text properties at POS."
  (add-text-properties
   (1- pos) pos
   (list 'rear-nonsticky taskpaper-nonsticky-props)))

(defun taskpaper-remove-flyspell-overlays-in (begin end)
  "Remove Flyspell overlays in region between BEGIN and END."
  (and (bound-and-true-p flyspell-mode)
       (fboundp 'flyspell-delete-region-overlays)
       (flyspell-delete-region-overlays begin end)))

(defun taskpaper-file-remote-p (file)
  "Test whether FILE specifies a location on a remote system."
  (cond
   ((fboundp 'file-remote-p)
    (file-remote-p file))
   ((fboundp 'tramp-handle-file-remote-p)
    (tramp-handle-file-remote-p file))
   ((and (boundp 'ange-ftp-name-format)
         (string-match (car ange-ftp-name-format) file))
    t)))

;;;; Re-usable regexps

(defconst taskpaper-tag-name-char-regexp
  (concat
   "[-a-zA-Z0-9._\u00B7\u0300-\u036F\u203F-\u2040"
   "\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D"
   "\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF"
   "\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]")
  "Regular expression for tag name character.")

(defconst taskpaper-tag-name-regexp
  (concat taskpaper-tag-name-char-regexp "+")
  "Regular expression for tag name.")

(defconst taskpaper-tag-value-regexp
  "\\(?:\\\\(\\|\\\\)\\|[^()\n]\\)*"
  "Regular expression for tag value.")

(defconst taskpaper-tag-regexp
  (format "\\(?:^\\|[ \t]\\)\\(@\\(%s\\)\\(?:(\\(%s\\))\\)?\\)"
          taskpaper-tag-name-regexp
          taskpaper-tag-value-regexp)
  "Regular expression for tag.
Group 1 matches the whole tag expression.
Group 2 matches the tag name.
Group 3 matches the tag value, if any.")

(defconst taskpaper-consecutive-tags-regexp
  (format "\\(?:%s[ \t]*\\)+" taskpaper-tag-regexp)
  "Regular expression for consecutive tags.")

(defconst taskpaper-uri-schemes-browser
  '("aaa" "about" "acap" "apt" "attachment" "bzr" "bzr+ssh" "chrome" "cid"
    "content" "crid" "cvs" "data" "dav" "dict" "dns" "doi" "dtn" "fax" "fax"
    "feed" "finger" "fish" "ftp" "geo" "git" "go" "gopher" "h323" "http"
    "https" "im" "imap" "info" "ipp" "irc" "irc6" "ircs" "iris.beep" "jar"
    "ldap" "ldaps" "magnet" "mid" "mms" "mmsh" "modem" "modem" "mtqp" "mupdate"
    "news" "nfs" "nntp" "opaquelocktoken" "pop" "pres" "prospero" "prospero"
    "resource" "rmi" "rsync" "rtsp" "rtspu" "service" "sftp" "sip" "sips" "smb"
    "sms" "snews" "snmp" "soap.beep" "soap.beeps" "ssh" "svn" "svn+ssh" "tag"
    "tel" "telnet" "tftp" "tip" "tn3270" "udp" "urn" "uuid" "vemmi" "wais"
    "webcal" "xmlrpc.beep" "xmlrpc.beeps" "xmpp" "xri" "z39.50r" "z39.50s")
  "URI schemes for URI, which should be opened in WWW browser.")

(defvar taskpaper-uri-browser-regexp
  (concat
   "\\<\\("
   "\\(?:"
   (regexp-opt taskpaper-uri-schemes-browser) ":"
   "\\(?:/\\{1,3\\}\\|[a-z0-9%.]\\)"
   "\\|"
   "www[:digit:]\\{0,3\\}[.]"
   "\\|"
   "\\(?:[-a-z0-9_]+[.]\\)+[a-z]\\{2,4\\}/"
   "\\)"
   "\\(?:[^[:space:]()<>]+\\|(\\(?:[^[:space:]()<>]+\\|([^[:space:]()<>]+)\\)*)\\)+"
   "\\(?:(\\(?:[^[:space:]()<>]+\\|([^[:space:]()<>]+)\\)*)\\|[^][:space:]`!()[{};:'\".,<>?«»“”‘’]\\)"
   "\\)")
  "Regular expression for URI, which should be opened in WWW browser.")

(defconst taskpaper-email-regexp
  "\\(\\(?:\\<mailto:\\)?[-a-zA-Z0-9=._+%]+@\\(?:[-a-zA-Z0-9_]+[.]\\)+[a-zA-Z]\\{2,4\\}\\)"
  "Regular expression for mail URI.")

(defconst taskpaper-file-path-regexp
  "\\(?:^\\|[ \t]\\)\\(\\(?:file:\\|[.]\\{0,2\\}/\\|~/\\)\\(?:\\\\ \\|[^ \0\n]\\)*\\)"
  "Regular expression for file URI.")

;;;; Font Lock regexps

(defconst taskpaper-task-regexp
  "^[ \t]*\\([-+*]\\)[ ]+\\([^\n]*\\)$"
  "Regular expression for task.
Group 1 matches the task mark.
Group 2 matches the task name.")

(defconst taskpaper-project-regexp
  (format
   "^[ \t]*\\([^-+* \t\n][^\n]*\\)\\(:\\)[ \t]*\\(%s\\)?[ \t]*$"
   taskpaper-consecutive-tags-regexp)
  "Regular expression for project.
Group 1 matches the project name.
Group 2 matches the project mark.
Group 3 matches trailing tags, if any.")

(defconst taskpaper-note-regexp
  "^[ \t]*\\(.*\\S-.*\\)$"
  "Regular expression for note.")

(defconst taskpaper-done-tag-regexp
  (format
   "\\(?:^\\|[ \t]\\)@done\\(?:(%s)\\)?\\(?:[ \t]\\|$\\)"
   taskpaper-tag-value-regexp)
  "Regular expression for \"@done\" tag.")

;;;; Faces

(defgroup taskpaper-faces nil
  "Faces used in TaskPaper mode."
  :group 'taskpaper
  :group 'faces)

(defface taskpaper-project-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for project name."
  :group 'taskpaper-faces)

(defface taskpaper-project-mark-face
  '((t :inherit taskpaper-project-name-face))
  "Face for project mark."
  :group 'taskpaper-faces)

(defface taskpaper-task-face
  '((t :inherit default))
  "Face for task."
  :group 'taskpaper-faces)

(defface taskpaper-item-marked-as-done-face
  '((t :strike-through t :inherit font-lock-comment-face))
  "Face for items marked as complete."
  :group 'taskpaper-faces)

(defface taskpaper-task-undone-mark-face
  '((t :inherit taskpaper-task-face))
  "Face for undone task mark."
  :group 'taskpaper-faces)

(defface taskpaper-task-done-mark-face
  '((t :inherit font-lock-comment-face))
  "Face for done task mark."
  :group 'taskpaper-faces)

(defface taskpaper-note-face
  '((t :inherit font-lock-comment-face))
  "Face for note."
  :group 'taskpaper-faces)

(defface taskpaper-tag-face
  '((t :inherit font-lock-comment-face))
  "Face for tag."
  :group 'taskpaper-faces)

(defface taskpaper-link-face
  '((t :inherit link))
  "Face for link."
  :group 'taskpaper-faces)

(defface taskpaper-missing-link-face
  '((t :foreground "red" :inherit link))
  "Face for file link to non-existing files."
  :group 'taskpaper-faces)

(defface taskpaper-query-error-face
  '((t :inherit error))
  "Face for malformed query string."
  :group 'taskpaper-faces)

(defface taskpaper-fast-select-key-face
  '((t :weight bold :inherit default))
  "Face for key in fast selection dialogs."
  :group 'taskpaper-faces)

(defface taskpaper-query-secondary-text-face
  '((t :inherit font-lock-comment-face))
  "Face for secondary text in query string."
  :group 'taskpaper-faces)

;;;; Font Lock

(defun taskpaper-file-path-unescape (path)
  "Remove file URL scheme and unescape spaces in PATH."
  (when (stringp path)
    (setq path (replace-regexp-in-string "\\`file:" "" path)
          path (replace-regexp-in-string "\\\\ " " " path)))
  path)

(defun taskpaper-font-lock-done-tasks (limit)
  "Fontify tasks marked as done."
  (while (re-search-forward taskpaper-task-regexp limit t)
    (let ((item (buffer-substring (match-beginning 0) (match-end 0))))
      (when (string-match-p taskpaper-done-tag-regexp item)
        (add-text-properties
         (match-beginning 1) (match-end 1)
         (list 'face 'taskpaper-task-done-mark-face))
        (add-text-properties
         (match-beginning 2) (match-end 2)
         (list 'face 'taskpaper-item-marked-as-done-face))))
    t))

(defun taskpaper-font-lock-done-projects (limit)
  "Fontify tasks marked as done."
  (while (re-search-forward taskpaper-project-regexp limit t)
    (let ((item (buffer-substring (match-beginning 0) (match-end 0))))
      (when (string-match-p taskpaper-done-tag-regexp item)
        (add-text-properties
         (match-beginning 0) (match-end 0)
         (list 'face 'taskpaper-item-marked-as-done-face))))
    t))

(defvar taskpaper-mouse-map-link
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'taskpaper-open-link-at-point)
    map)
  "Mouse events for links.")

(defun taskpaper-activate-email-links (limit)
  "Add text properties to email links."
  (when (re-search-forward taskpaper-email-regexp limit t)
    (taskpaper-remove-flyspell-overlays-in
     (match-beginning 1) (match-end 1))
    (add-text-properties
     (match-beginning 1) (match-end 1)
     (list 'mouse-face 'highlight
           'keymap taskpaper-mouse-map-link
           'help-echo "Follow Link"))
	(taskpaper-rear-nonsticky-at (match-end 1))
    t))

(defun taskpaper-activate-uri-links (limit)
  "Add text properties to URI links."
  (when (re-search-forward taskpaper-uri-browser-regexp limit t)
    (taskpaper-remove-flyspell-overlays-in
     (match-beginning 1) (match-end 1))
    (add-text-properties
     (match-beginning 1) (match-end 1)
     (list 'mouse-face 'highlight
           'keymap taskpaper-mouse-map-link
           'help-echo "Follow Link"))
	(taskpaper-rear-nonsticky-at (match-end 1))
    t))

(defun taskpaper-activate-file-links (limit)
  "Add text properties to file links.
In case of local files check to see if the file exists and
highlight accordingly."
  (when (re-search-forward taskpaper-file-path-regexp limit t)
    (taskpaper-remove-flyspell-overlays-in
     (match-beginning 1) (match-end 1))
    (add-text-properties
     (match-beginning 1) (match-end 1)
     (list 'mouse-face 'highlight
           'keymap taskpaper-mouse-map-link
           'help-echo "Follow Link"))
    (let* ((path (match-string-no-properties 1))
           (path (taskpaper-file-path-unescape path)))
      (if (not (taskpaper-file-remote-p path))
          (if (condition-case nil (file-exists-p path) (error nil))
              (add-text-properties
               (match-beginning 1) (match-end 1)
               (list 'face 'taskpaper-link-face))
            (add-text-properties
             (match-beginning 1) (match-end 1)
             (list 'face 'taskpaper-missing-link-face)))
        (add-text-properties
         (match-beginning 1) (match-end 1)
         (list 'face 'taskpaper-link-face))))
    (taskpaper-rear-nonsticky-at (match-end 1))
    t))

(defun taskpaper-face-from-face-or-color (inherit face-or-color)
  "Create a face list that inherits INHERIT, but set the color.
When FACE-OR-COLOR is not a string, just return it."
  (if (stringp face-or-color)
      (list :inherit inherit
            taskpaper-faces-easy-properties face-or-color)
    face-or-color))

(defun taskpaper-get-tag-face (tag)
  "Get the right face for TAG.
If TAG is a number, get the corresponding match group."
  (let ((tag (if (wholenump tag) (match-string tag) tag)))
    (or (taskpaper-face-from-face-or-color
         'taskpaper-tag-face (cdr (assoc tag taskpaper-tag-faces)))
        'taskpaper-tag-face)))

(defvar taskpaper-mouse-map-tag
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'taskpaper-search-tag-at-point)
    map)
  "Mouse events for tags.")

(defun taskpaper-activate-tags (limit)
  "Add text properties to tags."
  (when (re-search-forward taskpaper-tag-regexp limit t)
    (taskpaper-remove-flyspell-overlays-in
     (match-beginning 1) (match-end 1))
    (add-text-properties
     (match-beginning 1) (match-end 1)
     (list 'face (taskpaper-get-tag-face 2)
           'mouse-face 'highlight
           'keymap taskpaper-mouse-map-tag
           'help-echo "Filter on Tag"))
    (taskpaper-rear-nonsticky-at (match-end 1))
    t))

(defvar taskpaper-mouse-map-mark
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'taskpaper-item-toggle-done)
    map)
  "Mouse events for task marks.")

(defun taskpaper-activate-task-marks (limit)
  "Add text properties to task marks."
  (when (re-search-forward taskpaper-task-regexp limit t)
    (let ((item (buffer-substring (match-beginning 0) (match-end 0))))
      (if (string-match-p taskpaper-done-tag-regexp item)
          (when taskpaper-bullet-done
            (put-text-property (match-beginning 1) (match-end 1)
                               'display (char-to-string taskpaper-bullet-done)))
        (when taskpaper-bullet
          (put-text-property (match-beginning 1) (match-end 1)
                             'display (char-to-string taskpaper-bullet)))))
    (add-text-properties (match-beginning 1) (match-end 1)
                         (list 'mouse-face 'highlight
                               'keymap taskpaper-mouse-map-mark
                               'help-echo "Toggle Done"
                               'cursor 0))
    (taskpaper-rear-nonsticky-at (match-end 1))
    t))

(defvar taskpaper-font-lock-keywords nil)
(defun taskpaper-set-font-lock-defaults ()
  "Set font lock defaults for the current buffer."
  (let ((font-lock-keywords
         (list
          (cons taskpaper-task-regexp
                '((1 'taskpaper-task-undone-mark-face)
                  (2 'taskpaper-task-face)))
          (cons taskpaper-project-regexp
                '((1 'taskpaper-project-name-face)
                  (2 'taskpaper-project-mark-face)))
          (cons taskpaper-note-regexp
                '((1 'taskpaper-note-face)))
          (cons 'taskpaper-activate-email-links
                '((1 'taskpaper-link-face t t)))
          (cons 'taskpaper-activate-uri-links
                '((1 'taskpaper-link-face t t)))
          '(taskpaper-activate-file-links)
          '(taskpaper-activate-tags)
          '(taskpaper-font-lock-done-tasks)
          '(taskpaper-font-lock-done-projects)
          (when taskpaper-pretty-task-marks
            '(taskpaper-activate-task-marks)))))
    (setq taskpaper-font-lock-keywords (delq nil font-lock-keywords))
    (taskpaper-set-local
     'font-lock-defaults
     '(taskpaper-font-lock-keywords t nil nil backward-paragraph))
    (kill-local-variable 'font-lock-keywords)
    nil))

(defun taskpaper-unfontify-region (begin end)
  "Remove fontification in region between BEGIN and END."
  (font-lock-default-unfontify-region begin end)
  (let* ((buffer-undo-list t)
         (inhibit-read-only t) (inhibit-point-motion-hooks t)
         (inhibit-modification-hooks t)
         deactivate-mark buffer-file-name buffer-file-truename)
    (decompose-region begin end)
    (remove-text-properties begin end
                            (list 'display t 'mouse-face t 'keymap t))))

;;;; Files

(defun taskpaper-save-all-taskpaper-buffers ()
  "Save all TaskPaper mode buffers without user confirmation."
  (interactive)
  (save-some-buffers t (lambda () (derived-mode-p 'taskpaper-mode))))

(defconst taskpaper-file-apps-defaults-gnu
  '((remote . emacs)
    (system . mailcap)
    (t      . mailcap))
  "Default file applications on a UNIX or GNU/Linux system.")

(defconst taskpaper-file-apps-defaults-macos
  '((remote . emacs)
    (system . "open %s")
    (t      . "open %s"))
  "Default file applications on a macOS system.")

(defconst taskpaper-file-apps-defaults-windowsnt
  (list '(remote . emacs)
        (cons 'system (lambda (file _path)
                        (with-no-warnings (w32-shell-execute "open" file))))
        (cons t (lambda (file _path)
                  (with-no-warnings (w32-shell-execute "open" file)))))
  "Default file applications on a Windows NT system.")

(defun taskpaper-default-apps ()
  "Return the default applications for this operating system."
  (cond ((eq system-type 'darwin) taskpaper-file-apps-defaults-macos)
        ((eq system-type 'windows-nt) taskpaper-file-apps-defaults-windowsnt)
        (t taskpaper-file-apps-defaults-gnu)))

(defun taskpaper-apps-regexp-alist (list &optional add-auto-mode)
  "Convert file extensions to regular expressions in the cars of LIST.
When ADD-AUTO-MODE is non-nil, make all matches in
`auto-mode-alist' point to the symbol 'emacs, indicating that the
file should be visited in Emacs."
  (append
   (delq nil
         (mapcar (lambda (x)
                   (cond ((not (stringp (car x))) nil)
                         ((string-match "\\W" (car x)) x)
                         (t (cons (concat "\\." (car x) "\\'") (cdr x)))))
                 list))
   (when add-auto-mode
     (mapcar (lambda (x) (cons (car x) 'emacs)) auto-mode-alist))))

(declare-function mailcap-parse-mailcaps "mailcap" (&optional path force))
(declare-function mailcap-extension-to-mime "mailcap" (extn))
(declare-function mailcap-mime-info "mailcap" (string &optional request no-decode))
(defun taskpaper-open-file (path &optional in-emacs)
  "Open the file at PATH.
With optional argument IN-EMACS, visit the file in Emacs."
  (let* ((file (if (equal path "")
                   buffer-file-name
                 (substitute-in-file-name (expand-file-name path))))
         (apps (append taskpaper-file-apps (taskpaper-default-apps)))
         (remp (and (assq 'remote apps) (taskpaper-file-remote-p file)))
         (dirp (file-directory-p file))
         (amap (assq 'auto-mode apps))
         (dfile (downcase file))
         (ext (and
               (string-match ".*?\\.\\([a-zA-Z0-9]+\\(\\.gz\\)?\\)$" dfile)
               (match-string 1 dfile)))
         cmd)
    ;; Set open command
    (cond
     (in-emacs (setq cmd 'emacs))
     (t (setq cmd (or (and remp (cdr (assoc 'remote apps)))
                      (and dirp (cdr (assoc 'directory apps)))
                      (assoc-default dfile
                                     (taskpaper-apps-regexp-alist apps amap)
                                     'string-match)
                      (cdr (assoc ext apps))
                      (cdr (assoc t apps))))))
    (cond
     ((eq cmd 'system) (setq cmd (cdr (assoc 'system apps))))
     ((eq cmd 'default) (setq cmd (cdr (assoc t apps))))
     ((eq cmd 'mailcap)
      (require 'mailcap)
      (mailcap-parse-mailcaps)
      (let* ((mime-type (mailcap-extension-to-mime (or ext "")))
             (command (mailcap-mime-info mime-type)))
        (if (stringp command) (setq cmd command) (setq cmd 'emacs)))))
    ;; Check if file exists
    (if (and (not (eq cmd 'emacs))
             (not (file-exists-p file))
             (not taskpaper-open-non-existing-files))
        (user-error "No such file: %s" file))
    ;; Open file
    (cond
     ((and (stringp cmd) (not (string-match-p "^\\s-*$" cmd)))
      ;; Open using external command
      (while (string-match "['\"]%s['\"]" cmd)
        (setq cmd (replace-match "%s" t t cmd)))
      (while (string-match "%s" cmd)
        (setq cmd (replace-match
                   (save-match-data
                     (shell-quote-argument (convert-standard-filename file)))
                   t t cmd)))
      (save-window-excursion (start-process-shell-command cmd nil cmd))
      (message "Running %s" cmd))
     ((or (stringp cmd) (eq cmd 'emacs))
      ;; Open in Emacs
      (find-file-other-window file))
     ((functionp cmd)
      ;; Evaluate Lisp function
      (let ((file (convert-standard-filename file)))
        (save-match-data (funcall cmd file))))
     (t
      ;; Open in Emacs
      (find-file-other-window file)))))

;;;; Links

(defun taskpaper-file-path-complete (&optional arg)
  "Read file path using completion.
Return absolute or relative path to the file as string. If ARG is
non-nil, force absolute path."
  (let ((file (read-file-name "File: "))
        (pwd  (file-name-as-directory (expand-file-name ".")))
        (pwd1 (file-name-as-directory
               (abbreviate-file-name (expand-file-name ".")))))
    (cond
     (arg (abbreviate-file-name (expand-file-name file)))
     ((string-match (concat "\\`" (regexp-quote pwd1) "\\(.+\\)") file)
      (match-string 1 file))
     ((string-match (concat "\\`" (regexp-quote pwd) "\\(.+\\)")
                    (expand-file-name file))
      (match-string 1 (expand-file-name file)))
     (t file))))

(defun taskpaper-insert-file-link-at-point (&optional arg)
  "Insert a file link at point using completion.
The path to the file is inserted relative to the directory of the
current TaskPaper file, if the linked file is in the current
directory or in a subdirectory of it, or if the path is written
relative to the current directory using \"../\". Otherwise an
absolute path is used, if possible with \"~/\" for your home
directory. An absolute path can be forced with a
\\[universal-argument] prefix argument."
  (interactive "P")
  (let* ((path (taskpaper-file-path-complete arg))
         (path (replace-regexp-in-string " " "\\\\ " path)))
    (insert (concat "file:" path))))

(defun taskpaper-open-link-at-point ()
  "Open link at point."
  (interactive)
  (cond
   ((taskpaper-in-regexp-p taskpaper-uri-browser-regexp)
    (let ((uri (match-string-no-properties 1)))
      (when (string-match "\\`www" uri) (setq uri (concat "http://" uri)))
      (browse-url uri)))
   ((taskpaper-in-regexp-p taskpaper-file-path-regexp)
    (let* ((path (match-string-no-properties 1))
           (path (taskpaper-file-path-unescape path)))
      (taskpaper-open-file path)))
   ((taskpaper-in-regexp-p taskpaper-email-regexp)
    (let* ((address (match-string-no-properties 1))
           (address (replace-regexp-in-string "\\`mailto:" "" address)))
      (compose-mail address)))
   (t (user-error "No link at point"))))

;;;; Inline images

(defvar taskpaper-inline-image-overlays nil
  "List of inline image overlays.")
(make-variable-buffer-local 'taskpaper-inline-image-overlays)

(defun taskpaper-remove-inline-images ()
  "Remove inline images in the buffer."
  (interactive)
  (mapc #'delete-overlay taskpaper-inline-image-overlays)
  (setq taskpaper-inline-image-overlays nil))

(defun taskpaper-display-inline-images ()
  "Display inline images in the buffer.
Add inline image overlays to local image links in the buffer."
  (interactive)
  (unless (display-graphic-p) (error "Images cannot be displayed"))
  (taskpaper-remove-inline-images)
  (when (fboundp 'clear-image-cache) (clear-image-cache))
  (save-excursion
    (save-restriction
      (widen) (goto-char (point-min))
      (while (re-search-forward taskpaper-file-path-regexp nil t)
        (let* ((begin (match-beginning 1)) (end (match-end 1))
               (path (match-string-no-properties 1))
               (path (taskpaper-file-path-unescape path))
               (path (substitute-in-file-name (expand-file-name path)))
               (image (if (and taskpaper-max-image-size
                               (image-type-available-p 'imagemagick))
                          (create-image
                           path 'imagemagick nil
                           :max-width  (car taskpaper-max-image-size)
                           :max-height (cdr taskpaper-max-image-size))
                        (create-image path))))
          (when (and (file-exists-p path) image)
            (let ((ov (make-overlay begin end)))
              (overlay-put ov 'display image)
              (overlay-put ov 'face 'default)
              (push ov taskpaper-inline-image-overlays))))))))

(defun taskpaper-toggle-inline-images ()
  "Toggle displaying of inline images in the buffer."
  (interactive)
  (if taskpaper-inline-image-overlays
      (progn
        (taskpaper-remove-inline-images)
        (when (called-interactively-p 'interactive)
          (message "Inline image display turned off")))
    (taskpaper-display-inline-images)
    (when (called-interactively-p 'interactive)
      (message "Inline image display turned on"))))

;;;; Outline API and navigation

(defalias 'taskpaper-outline-up-level 'outline-up-heading
  "Move to the visible parent item.
With argument, move up ARG levels. If INVISIBLE-OK is non-nil,
also consider invisible items.")

(defalias 'taskpaper-outline-next-item 'outline-next-heading
  "Move to the next (possibly invisible) item.")

(defalias 'taskpaper-outline-previous-item 'outline-previous-heading
  "Move to the previous (possibly invisible) item.")

(defun taskpaper-outline-forward-same-level (arg)
  "Move forward to the ARG'th item at same level.
Call `outline-forward-same-level', but provide a better error
message."
  (interactive "p")
  (condition-case nil
      (outline-forward-same-level arg)
    (error (user-error "No following same-level item"))))

(defun taskpaper-outline-backward-same-level (arg)
  "Move backward to the ARG'th item at same level.
Call `outline-backward-same-level', but provide a better error
message."
  (interactive "p")
  (condition-case nil
      (outline-backward-same-level arg)
    (error (user-error "No previous same-level item"))))

(defsubst taskpaper-outline-up-level-safe ()
  "Move to the (possibly invisible) ancestor item.
This version will not throw an error. Also, this version is much
faster than `outline-up-heading', relying directly on leading
tabs."
  (when (ignore-errors (outline-back-to-heading t))
    (let ((level-up (1- (funcall outline-level))))
      (and (> level-up 0)
           (re-search-backward
            (format "^[\t]\\{0,%d\\}[^\t\n]" (1- level-up)) nil t)))))

(defun taskpaper-outline-forward-same-level-safe ()
  "Move to the next (possibly invisible) sibling.
This version will not throw an error."
  (condition-case nil
      (progn (outline-forward-same-level 1) (not (eobp)))
    (error nil)))

(defun taskpaper-outline-backward-same-level-safe ()
  "Move to the preceeding (possibly invisible) sibling.
This version will not throw an error."
  (condition-case nil
      (progn (outline-backward-same-level 1) (not (bobp)))
    (error nil)))

(defun taskpaper-outline-next-item-safe ()
  "Move to the next (possibly invisible) item.
This version will not throw an error."
  (condition-case nil
      (progn (outline-next-heading) (not (eobp)))
    (error nil)))

(defun taskpaper-outline-previous-item-safe ()
  "Move to the previous (possibly invisible) item.
This version will not throw an error."
  (condition-case nil
      (progn (outline-previous-heading) (not (bobp)))
    (error nil)))

(defun taskpaper-map-tree (func)
  "Call FUNC for every (possibly invisible) item of the current subtree."
  (outline-back-to-heading t)
  (let ((level (save-match-data (funcall outline-level))))
    (save-excursion
      (funcall func)
      (while
          (and (progn
                 (outline-next-heading)
                 (> (save-match-data (funcall outline-level)) level))
               (not (eobp)))
        (funcall func)))))

(defun taskpaper-map-region (func begin end)
  "Call FUNC for every (possibly invisible) item between BEGIN and END."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char begin)
    (if (outline-on-heading-p t) (funcall func))
    (while
        (and (progn
               (outline-next-heading)
               (< (point) end))
             (not (eobp)))
      (funcall func))))

;;;; Folding

(eval-and-compile
  (defalias 'taskpaper-outline-show-all
    (if (fboundp 'outline-show-all) 'outline-show-all 'show-all)
    "Show all items in the buffer.")
  (defalias 'taskpaper-outline-show-item
    (if (fboundp 'outline-show-entry) 'outline-show-entry 'show-entry)
    "Show the current item.")
  (defalias 'taskpaper-outline-show-children
    (if (fboundp 'outline-show-children) 'outline-show-children 'show-children)
    "Show all direct subitems of the current item.")
  (defalias 'taskpaper-outline-show-subtree
    (if (fboundp 'outline-show-subtree) 'outline-show-subtree 'show-subtree)
    "Show all subitems of the current item.")
  (defalias 'taskpaper-outline-hide-subtree
    (if (fboundp 'outline-hide-subtree) 'outline-hide-subtree 'hide-subtree)
    "Hide all subitems of the current item.")
  (defalias 'taskpaper-outline-hide-sublevels
    (if (fboundp 'outline-hide-sublevels) 'outline-hide-sublevels 'hide-sublevels))
  "Hide everything but the top-level items in the buffer.")

(defun taskpaper-outline-show-context ()
  "Show the current item and all its ancestors."
  (let (outline-view-change-hook)
    (save-excursion
      (outline-back-to-heading t) (taskpaper-outline-show-item)
      (while (taskpaper-outline-up-level-safe)
        (outline-flag-region
         (max (point-min) (1- (point)))
         (save-excursion (outline-end-of-heading) (point))
         nil)))))

(defun taskpaper-outline-hide-other ()
  "Hide everything except the current item, its ancestors and top-level items.
Essentially a slightly modified version of `outline-hide-other'."
  (interactive)
  (taskpaper-outline-hide-sublevels 1)
  (save-excursion (taskpaper-outline-show-context))
  (save-excursion (taskpaper-outline-show-children))
  (recenter-top-bottom))

(defun taskpaper-next-line ()
  "Forward line, but move over invisible line ends.
Essentially a much simplified version of `next-line'."
  (interactive)
  (beginning-of-line 2)
  (while (and (not (eobp)) (get-char-property (1- (point)) 'invisible))
    (beginning-of-line 2)))

(defvar taskpaper-cycle-global-status 1)
(make-variable-buffer-local 'taskpaper-cycle-global-status)

(defun taskpaper-cycle (&optional arg)
  "Perform visibility cycling.
When point is at the beginning of the buffer, or when called with
a \\[universal-argument] prefix argument, rotate the entire
buffer. When point is on an item, rotate the current subtree."
  (interactive "P")
  (cond
   (arg
    (save-excursion
      (goto-char (point-min))
      (taskpaper-cycle nil)))
   (t
    (cond
     ((bobp)
      ;; Global cycling
      (cond
       ((and (eq last-command this-command)
             (eq taskpaper-cycle-global-status 2))
        ;; Move from overview to all
        (taskpaper-outline-show-all)
        (message "SHOW ALL")
        (setq taskpaper-cycle-global-status 1))
       (t
        ;; Default to overview
        (taskpaper-outline-hide-sublevels 1)
        (message "OVERVIEW")
        (setq taskpaper-cycle-global-status 2))))
     ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
      ;; Local cycling
      (outline-back-to-heading)
      (let ((goal-column 0) eoh eol eos)
        ;; Determine boundaries
        (save-excursion
          (outline-back-to-heading)
          (save-excursion (taskpaper-next-line) (setq eol (point)))
          (outline-end-of-heading) (setq eoh (point))
          (outline-end-of-subtree) (setq eos (point)))
        (cond
         ((= eos eoh)
          ;; Leaf item
          (message "LEAF ITEM"))
         ((>= eol eos)
          ;; Entire subtree is hidden in one line
          (taskpaper-outline-show-children)
          (message "CHILDREN")
          (setq this-command 'taskpaper-cycle-children))
         ((eq last-command 'taskpaper-cycle-children)
          ;; Show everything
          (taskpaper-outline-show-subtree)
          (message "SUBTREE"))
         (t
          ;; Hide the subtree (default action)
          (taskpaper-outline-hide-subtree)
          (message "FOLDED")))))
     (t
      ;; Not at an item
      (outline-back-to-heading))))))

(defun taskpaper-set-startup-visibility ()
  "Set startup visibility."
  (if taskpaper-startup-folded
      (taskpaper-outline-hide-sublevels 1)
    (taskpaper-outline-show-all)))

;;;; Miscellaneous outline functions

(defun taskpaper-narrow-to-subtree ()
  "Narrow buffer to the current subtree."
  (interactive)
  (let (begin end)
    (save-excursion
      (save-match-data
        (outline-back-to-heading) (setq begin (point))
        (outline-end-of-subtree) (setq end (point))
        (narrow-to-region begin end)))))

(defalias 'taskpaper-mark-subtree 'outline-mark-subtree
  "Mark the current subtree.
Put point at the start of the current subtree, and mark at the
end.")

(defun taskpaper-outline-copy-visible (begin end)
  "Save all visible items between BEGIN and END to the kill ring."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end) (goto-char (point-min))
      (let ((buffer (current-buffer)) start end)
        (with-temp-buffer
          (let ((temp-buffer (current-buffer)))
            (with-current-buffer buffer
              ;; Starting on an item
              (when (outline-on-heading-p)
                (outline-back-to-heading)
                (setq start (point)
                      end (progn (outline-end-of-heading) (point)))
                (with-current-buffer temp-buffer
                  (insert-buffer-substring buffer start end)
                  (insert "\n")))
              (while (outline-next-heading)
                (unless (outline-invisible-p)
                  (setq start (point)
                        end (progn (outline-end-of-heading) (point)))
                  (with-current-buffer temp-buffer
                    (insert-buffer-substring buffer start end)
                    (insert "\n"))))))
          (kill-new (buffer-string)))))))

;;;; Promotion and demotion

(defun taskpaper-outline-promote ()
  "Promote the current (possibly invisible) item."
  (interactive)
  (outline-back-to-heading t)
  (let ((level (save-match-data (funcall outline-level))))
    (if (= level 1)
        (user-error "Already at top level"))
    (let ((indent (make-string (- level 2) ?\t)))
      (replace-match indent nil t nil 1))))

(defun taskpaper-outline-demote ()
  "Demote the current (possibly invisible) item."
  (interactive)
  (outline-back-to-heading t)
  (let* ((level (save-match-data (funcall outline-level)))
         (indent (make-string level ?\t)))
    (replace-match indent nil t nil 1)))

(defun taskpaper-outline-promote-subtree ()
  "Promote the current (possibly invisible) subtree."
  (interactive)
  (taskpaper-map-tree 'taskpaper-outline-promote))

(defun taskpaper-outline-demote-subtree ()
  "Demote the current (possibly invisible) subtree."
  (interactive)
  (taskpaper-map-tree 'taskpaper-outline-demote))

;;;; Vertical tree movement

(defalias 'taskpaper-outline-move-subtree-up 'outline-move-subtree-up
  "Move the current subtree up past ARG items of the same level.")

(defalias 'taskpaper-outline-move-subtree-down 'outline-move-subtree-down
  "Move the current subtree down past ARG items of the same level.")

;;;; Auto-formatting

(defun taskpaper-new-item-same-level ()
  "Insert new item of the same level."
  (interactive)
  (let (level indent)
    (save-excursion
      (setq level
            (condition-case nil
                (progn
                  (outline-back-to-heading)
                  (save-match-data (funcall outline-level)))
              (error 1))
            indent (make-string (1- level) ?\t)))
    (cond ((bolp) (newline))
          (t (newline) (insert indent)))))

(defun taskpaper-new-task-same-level ()
  "Insert new task at same level."
  (interactive)
  (taskpaper-new-item-same-level) (insert "- "))

;;;; Item parsing and formatting

(defun taskpaper-remove-type-formatting (item)
  "Remove type formatting from ITEM."
  (unless (stringp item) (error "Argument should be a string"))
  ;; Remove trailing whitespaces
  (setq item (replace-regexp-in-string "[ \t]+$" "" item))
  (save-match-data
    ;; Remove type formatting
    (cond ((string-match
            "^\\([ \t]*\\)[-+*][ ]+\\([^\n]*\\)$" item)
           ;; Task
           (setq item
                 (concat (match-string-no-properties 1 item)
                         (match-string-no-properties 2 item))))
          ((string-match
            (format "^\\([ \t]*\\)\\([^\n]*\\):[ \t]*\\(%s\\)?[ \t]*$"
                    taskpaper-consecutive-tags-regexp) item)
           ;; Project
           (setq item
                 (concat (match-string-no-properties 1 item)
                         (match-string-no-properties 2 item)
                         (match-string-no-properties 3 item))))
          (t
           ;; Note or blank
           item)))
  item)

(defun taskpaper-remove-indentation (item)
  "Remove indentation from ITEM."
  (unless (stringp item) (error "Argument should be a string"))
  (save-match-data
    (cond ((string-match "^[ \t]*\\([^\n]*\\)$" item)
           (setq item (match-string-no-properties 1 item)))
          (t item)))
  item)

(defun taskpaper-remove-trailing-tags (item)
  "Remove trailing tags from ITEM."
  (unless (stringp item) (error "Argument should be a string"))
  (save-match-data
    (cond ((string-match
            (format "^\\([^\n]*?\\)[ \t]*\\(?:%s\\)?[ \t]*$"
                    taskpaper-consecutive-tags-regexp) item)
           (setq item (match-string-no-properties 1 item)))
          (t item)))
  item)

(defun taskpaper-item-type ()
  "Return type of item at point as string.
Type can be \"project\", \"task\", \"note\", or \"blank\"."
  (let ((item (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (setq item (taskpaper-remove-indentation item))
    (setq item (taskpaper-remove-trailing-tags item))
    (cond ((string-match-p "^[ \t]*$" item) "blank")
          ((string-match-p "^[-+*] " item) "task")
          ((string-match-p ":$" item) "project")
          (t "note"))))

(defun taskpaper-item-text ()
  "Return text of item at point."
  (let ((item (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (setq item (taskpaper-remove-indentation item))))

(defun taskpaper-item-format (type)
  "Format item at point as TYPE.
Valid values are 'project, 'task, or 'note."
  (unless (member type (list 'project 'task 'note))
    (error "Invalid item type: %s" type))
  (let* ((beg (line-beginning-position)) (end (line-end-position))
         (item (buffer-substring-no-properties beg end)))
    (setq item (taskpaper-remove-type-formatting item))
    (save-match-data
      (cond ((equal type 'task)
             (string-match "^\\([ \t]*\\)\\([^\n]*\\)$" item)
             (setq item (concat
                         (match-string-no-properties 1 item) "- "
                         (match-string-no-properties 2 item))))
            ((equal type 'project)
             (string-match (format "^\\([^\n]*?\\)[ \t]*\\(%s\\)?[ \t]*$"
                                   taskpaper-consecutive-tags-regexp) item)
             (setq item (concat
                         (match-string-no-properties 1 item) ":"
                         (match-string-no-properties 2 item))))
            (t item)))
    (delete-region beg end) (insert item)))

(defun taskpaper-item-format-as-project ()
  "Format item at point as project."
  (interactive)
  (taskpaper-item-format 'project))

(defun taskpaper-item-format-as-task ()
  "Format item at point as task."
  (interactive)
  (taskpaper-item-format 'task))

(defun taskpaper-item-format-as-note ()
  "Format item at point as note."
  (interactive)
  (taskpaper-item-format 'note))

;;;: Attribute utilities

(defconst taskpaper-special-attributes '("type" "text")
  "The special item attributes.
These are implicit attributes that are not associated with
tags.")

(defun taskpaper-tag-name-p (name)
  "Return non-nil when NAME is a valid tag name."
  (when (stringp name)
    (string-match-p (format "\\`%s\\'" taskpaper-tag-name-regexp) name)))

(defun taskpaper-tag-value-escape (value)
  "Escape parentheses in tag VALUE."
  (when (stringp value)
    (setq value (replace-regexp-in-string "[\n]+" " " value)
          value (replace-regexp-in-string "(" "\\\\(" value)
          value (replace-regexp-in-string ")" "\\\\)" value)))
  value)

(defun taskpaper-tag-value-unescape (value)
  "Unescape parentheses in tag VALUE."
  (when (stringp value)
    (setq value (replace-regexp-in-string "\\\\(" "(" value)
          value (replace-regexp-in-string "\\\\)" ")" value)))
  value)

(defun taskpaper-item-get-special-attributes ()
  "Get special attrbutes for item at point.
Return alist (NAME . VALUE) where NAME is the attribute name and
VALUE is the attribute value, as strings."
  (let (attrs name value)
    (setq name "type" value (taskpaper-item-type))
    (push (cons name value) attrs)
    (setq name "text" value (taskpaper-item-text))
    (push (cons name value) attrs)
    attrs))

(defun taskpaper-item-get-explicit-attributes ()
  "Get explicit attrbutes for item at point.
Return alist (NAME . VALUE) where NAME is the attribute name and
VALUE is the attribute value, as strings."
  (let (attrs name value)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (re-search-forward taskpaper-tag-regexp (line-end-position) t)
        (setq name (match-string-no-properties 2)
              value (match-string-no-properties 3)
              value (taskpaper-tag-value-unescape value))
        (unless (member name taskpaper-special-attributes)
          (push (cons name value) attrs))))
    (nreverse attrs)))

(defun taskpaper-remove-uninherited-attributes (attrs)
  "Remove attributes excluded from inheritance from alist ATTRS."
  (let ((exattrs taskpaper-tags-exclude-from-inheritance) excluded)
    (when exattrs
      (dolist (attr attrs)
        (when (not (member (car attr) exattrs)) (push attr excluded)))
      (setq attrs (nreverse excluded))))
  attrs)

;;;; Attribute caching

(defvar taskpaper-attribute-cache (make-hash-table :size 10000)
  "Attribute cache.")
(make-variable-buffer-local 'taskpaper-attribute-cache)

(defun taskpaper-attribute-cache-clear ()
  "Clear attribute cache."
  (clrhash taskpaper-attribute-cache))

(defun taskpaper-attribute-cache-put (key attrs)
  "Push attribute list ATTRS into attribute cache, under KEY."
  (puthash key attrs taskpaper-attribute-cache))

(defun taskpaper-attribute-cache-get (key)
  "Retrieve attribute list for KEY from attribute cache."
  (gethash key taskpaper-attribute-cache))

(defun taskpaper-attribute-cache-build ()
  "Build attribute cache."
  (taskpaper-attribute-cache-clear)
  (message "Caching...")
  (save-excursion
    (goto-char (point-min))
    (let ((re (concat "^" outline-regexp)))
      (while (let (case-fold-search)
               (re-search-forward re nil t))
        (let ((key (point-at-bol))
              (attrs (taskpaper-item-get-attributes t)))
          (taskpaper-attribute-cache-put key attrs)))))
  (message "Caching...done"))

;;;; Attribute API

(defun taskpaper-item-get-attributes (&optional inherit)
  "Get attrbutes for item at point.
Return read-only alist (NAME . VALUE) where NAME is the attribute
name and VALUE is the attribute value, as strings. If INHERIT is
non-nil also check higher levels of the hierarchy."
  (let* ((key (point-at-bol))
         (chattrs (taskpaper-attribute-cache-get key))
         spattrs exattrs ihattrs attrs)
    (cond
     (chattrs
      (setq attrs chattrs))
     (t
      (setq spattrs (taskpaper-item-get-special-attributes))
      (setq exattrs (taskpaper-item-get-explicit-attributes))
      (when inherit
        (save-excursion
          (while (taskpaper-outline-up-level-safe)
            (setq ihattrs
                  (append
                   (taskpaper-remove-uninherited-attributes
                    (taskpaper-item-get-explicit-attributes))
                   ihattrs)))))
      (setq attrs (append spattrs exattrs ihattrs))))
    (taskpaper-uniquify attrs)))

(defun taskpaper-item-get-attribute (name &optional inherit)
  "Get value of attribute NAME for item at point.
Return the value as a string or nil if the attribute does not
exist or has no value. If the item has multiple attributes with
the same name, the first one will be evaluated. If INHERIT is
non-nil also check higher levels of the hierarchy."
  (unless (taskpaper-tag-name-p name)
    (user-error "Invalid attribute name: %s" name))
  (cdr (assoc name (taskpaper-item-get-attributes inherit))))

(defun taskpaper-item-has-attribute (name &optional value inherit)
  "Return non-nil if item at point has attribute NAME.
With optional argument VALUE, match only attributes with that
value. If INHERIT is non-nil also check higher levels of the
hierarchy."
  (if (member name taskpaper-special-attributes) t
    (unless (taskpaper-tag-name-p name)
      (user-error "Invalid attribute name: %s" name))
    (let ((attr (assoc name (taskpaper-item-get-attributes inherit))))
      (if value (equal value (cdr attr)) attr))))

(defun taskpaper-item-remove-attribute (name &optional value)
  "Remove all non-special attributes NAME from item at point.
With optional argument VALUE, match only attributes with that
value."
  (unless (taskpaper-tag-name-p name)
    (user-error "Invalid attribute name: %s" name))
  (when (member name taskpaper-special-attributes)
    (user-error "Special attribute cannot be removed: %s" name))
  (goto-char (line-beginning-position))
  (while (re-search-forward taskpaper-tag-regexp (line-end-position) t)
    (cond
     ((and value
           (equal (match-string 2) name)
           (equal (match-string 3) (taskpaper-tag-value-escape value)))
      (delete-region (match-beginning 0) (match-end 0)))
     ((and (not value)
           (equal (match-string 2) name))
      (delete-region (match-beginning 0) (match-end 0))))))

(defun taskpaper-item-set-attribute (name &optional value add)
  "Set non-special attribute NAME for item at point.
Any existing attribute NAME will be removed unless ADD is
non-nil. With optional argument VALUE, set attribute to that
value."
  (unless (taskpaper-tag-name-p name)
    (user-error "Invalid attribute name: %s" name))
  (when (member name taskpaper-special-attributes)
    (user-error "Special attribute cannot be set: %s" name))
  (unless add (taskpaper-item-remove-attribute name))
  (goto-char (line-end-position))
  (delete-horizontal-space) (unless (bolp) (insert " "))
  (if value
      (progn
        (setq value (taskpaper-tag-value-escape value))
        (insert (format "@%s(%s)" name value)))
    (insert (format "@%s" name))))

(defun taskpaper-string-get-attributes (str)
  "Get attrbutes for item string STR.
Like `taskpaper-item-get-attributes' but uses argument string
instead of item at point."
  (with-temp-buffer
    (erase-buffer) (insert str) (goto-char (point-min))
    (taskpaper-item-get-attributes)))

(defun taskpaper-string-get-attribute (str name)
  "Get value of attribute NAME for item string STR.
Like `taskpaper-item-get-attribute' but uses argument string
instead of item at point."
  (with-temp-buffer
    (erase-buffer) (insert str) (goto-char (point-min))
    (taskpaper-item-get-attribute name)))

(defun taskpaper-string-has-attribute (str name &optional value)
  "Return non-nil if item string STR has attribute NAME.
Like `taskpaper-item-has-attribute' but uses argument string
instead of item at point."
  (with-temp-buffer
    (erase-buffer) (insert str) (goto-char (point-min))
    (taskpaper-item-has-attribute name value)))

(defun taskpaper-string-remove-attribute (str name &optional value)
  "Remove all non-special attributes NAME from item string STR.
Like `taskpaper-item-remove-attribute' but uses argument string
instead of item at point. Return new string."
  (with-temp-buffer
    (erase-buffer) (insert str) (goto-char (point-min))
    (taskpaper-item-remove-attribute name value)
    (buffer-string)))

(defun taskpaper-string-set-attribute (str name &optional value add)
  "Set non-special attribute NAME for item string STR.
Like `taskpaper-item-set-attribute' but uses argument string
instead of item at point. Return new string."
  (with-temp-buffer
    (erase-buffer) (insert str) (goto-char (point-min))
    (taskpaper-item-set-attribute name value add)
    (buffer-string)))

(defun taskpaper-tag-value-to-list (value)
  "Convert tag value VALUE to a list.
Treat the tag value string as a comma-separated list of values
and return the values as a list of strings."
  (split-string value ", *" nil))

;;;; Date and time

(defvar taskpaper-time-was-given nil)
(make-variable-buffer-local 'taskpaper-time-was-given)

(defun taskpaper-parse-time-string (time-string)
  "Parse the time string TIME-STRING.
Return list (SEC MIN HOUR DAY MON YEAR DOW DST TZ)."
  (setq taskpaper-time-was-given nil)
  (let ((mod-re (regexp-opt '("this" "next" "last")))
        (nowdecode (decode-time (current-time)))
        (case-fold-search t)
        dir deltan deltaw mod tl year month day hour minute second wday wday1)
    ;; Strip duration offset as the very last part of `time-string', if any,
    ;; parse and postpone interpreting it until the rest of the parsing is done
    (when (string-match
           (concat
            " *\\([-+]\\) ?\\([0-9]+\\) ?"
            "\\([hdwmy]\\|hours?\\|days?\\|weeks?\\|months?\\|years?\\|"
            (mapconcat 'car parse-time-weekdays "\\|") "\\) *\\'")
           time-string)
      (setq dir (string-to-char (match-string 1 time-string))
            deltan (string-to-number (match-string 2 time-string))
            deltaw (match-string 3 time-string))
      (setq time-string (replace-match "" t t time-string)))
    ;; Strip relative dates and times as the very first part of `time-string',
    ;; if any, and parse it
    (cond
     ;; Years
     ((string-match (concat "\\` *\\(" mod-re "\\) year\\>") time-string)
      (setq mod (match-string 1 time-string)
            year (nth 5 nowdecode)
            month 1
            day 1
            hour 0
            minute 0
            second 0)
      (cond ((equal mod "next") (setq year (1+ year)))
            ((equal mod "last") (setq year (1- year))))
      (setq time-string (replace-match "" t t time-string)))
     ;; Months
     ((string-match (concat "\\` *\\(" mod-re "\\) month\\>") time-string)
      (setq mod (match-string 1 time-string)
            year (nth 5 nowdecode)
            month (nth 4 nowdecode)
            day 1
            hour 0
            minute 0
            second 0)
      (cond ((equal mod "next") (setq month (1+ month)))
            ((equal mod "last") (setq month (1- month))))
      (setq time-string (replace-match "" t t time-string)))
     ;; Month names with optional day
     ((string-match
       (concat "\\` *\\(" mod-re "\\) "
               "\\(" (mapconcat 'car parse-time-months "\\|") "\\)"
               "\\(?: \\([0-3]?[0-9]\\)\\)?\\(?:[ ]\\|\\'\\)")
       time-string)
      (setq mod (match-string 1 time-string)
            year (nth 5 nowdecode)
            month (cdr (assoc (downcase (match-string 2 time-string)) parse-time-months))
            day (if (match-end 3) (string-to-number (match-string 3 time-string)) 1)
            hour 0
            minute 0
            second 0)
      (cond ((equal mod "next") (setq year (1+ year)))
            ((equal mod "last") (setq year (1- year))))
      (setq time-string (replace-match "" t t time-string)))
     ;; Weeks
     ((string-match (concat "\\` *\\(" mod-re "\\) week\\>") time-string)
      (setq mod (match-string 1 time-string)
            wday 1 wday1 (nth 6 nowdecode)
            year (nth 5 nowdecode)
            month (nth 4 nowdecode)
            day (nth 3 nowdecode)
            hour 0
            minute 0
            second 0)
      (and (= wday1 0) (setq wday1 7)) (setq day (+ day (- wday wday1)))
      (cond ((equal mod "next") (setq day (+ day 7)))
            ((equal mod "last") (setq day (- day 7))))
      (setq time-string (replace-match "" t t time-string)))
     ;; Weekdays
     ((string-match
       (concat "\\` *\\(" mod-re "\\) "
               "\\(" (mapconcat 'car parse-time-weekdays "\\|") "\\)\\>")
       time-string)
      (setq mod (match-string 1 time-string)
            wday (cdr (assoc (downcase (match-string 2 time-string)) parse-time-weekdays))
            wday1 (nth 6 nowdecode)
            year (nth 5 nowdecode)
            month (nth 4 nowdecode)
            day (nth 3 nowdecode)
            hour 0
            minute 0
            second 0)
      (and (= wday 0) (setq wday 7)) (and (= wday1 0) (setq wday1 7))
      (setq day (+ day (- wday wday1)))
      (cond ((equal mod "next") (setq day (+ day 7)))
            ((equal mod "last") (setq day (- day 7))))
      (setq time-string (replace-match "" t t time-string)))
     ;; Days
     ((string-match "\\` *\\(today\\|tomorrow\\|yesterday\\)\\>" time-string)
      (setq mod (match-string 1 time-string)
            year (nth 5 nowdecode)
            month (nth 4 nowdecode)
            day (nth 3 nowdecode)
            hour 0
            minute 0
            second 0)
      (cond ((equal mod "tomorrow") (setq day (1+ day)))
            ((equal mod "yesterday") (setq day (1- day))))
      (setq time-string (replace-match "" t t time-string)))
     ;; Time
     ((string-match "\\` *now *\\'" time-string)
      (setq year (nth 5 nowdecode)
            month (nth 4 nowdecode)
            day (nth 3 nowdecode)
            hour (nth 2 nowdecode)
            minute (nth 1 nowdecode)
            second (nth 0 nowdecode)
            taskpaper-time-was-given t)
      (setq time-string (replace-match "" t t time-string))))
    ;; Help matching incomplete ISO 8601 date representations, like "2018-08" or "2018-8-2"
    (when (string-match
           (concat
            "\\` *\\([0-9]\\{4\\}\\)-\\([0-1]?[0-9]\\)\\(?:-\\([0-3]?[0-9]\\)\\)?"
            "\\([^-0-9]\\|\\'\\)")
           time-string)
      (let ((year (string-to-number (match-string 1 time-string)))
            (month (string-to-number (match-string 2 time-string)))
            (day (if (match-end 3) (string-to-number (match-string 3 time-string)) 1)))
        (setq time-string
              (replace-match (format "%04d-%02d-%02d\\4" year month day) t nil time-string))))
    ;; Help matching ISO date representation with year omitted, like "--08-02"
    (when (string-match
           "\\` *--\\([0-1][0-9]\\)-\\([0-3][0-9]\\)\\([^-0-9]\\|\\'\\)"
           time-string)
      (let ((year (nth 5 nowdecode))
            (month (string-to-number (match-string 1 time-string)))
            (day (string-to-number (match-string 2 time-string))))
        (setq time-string
              (replace-match (format "%04d-%02d-%02d\\3" year month day) t nil time-string))))
    ;; Help matching ISO 8601 week date representation, like "2018-W02-5" or "2018-W02"
    (when (string-match
           (concat
            "\\` *\\([0-9]\\{4\\}\\)-W\\([0-9]\\{1,2\\}\\)\\(?:-\\([1-7]\\)\\)?"
            "\\([^-0-9]\\|\\'\\)")
           time-string)
      (let* ((iso-year (string-to-number (match-string 1 time-string)))
             (iso-week (string-to-number (match-string 2 time-string)))
             (iso-wday (if (match-end 3) (string-to-number (match-string 3 time-string)) 1))
             (iso-wday (if (= iso-wday 7) 0 iso-wday))
             (iso-date (calendar-gregorian-from-absolute
                        (calendar-iso-to-absolute (list iso-week iso-wday iso-year))))
             (month (nth 0 iso-date)) (day (nth 1 iso-date)) (year (nth 2 iso-date)))
        (setq time-string
              (replace-match (format "%04d-%02d-%02d\\4" year month day) t nil time-string))))
    ;; Help matching am/pm times
    (when (string-match
           "\\([0-2]?[0-9]\\)\\(?::\\([0-5][0-9]\\)\\)? ?\\(am\\|pm\\)\\>"
           time-string)
      (let ((hour (string-to-number (match-string 1 time-string)))
            (minute (if (match-end 2) (string-to-number (match-string 2 time-string)) 0))
            (pm (equal ?p (string-to-char (downcase (match-string 3 time-string))))))
        (and (not pm) (= hour 12) (setq hour 0)) (and pm (< hour 12) (setq hour (+ 12 hour)))
        (setq time-string
              (replace-match (format "%02d:%02d" hour minute) t t time-string))))
    ;; Parse the rest of `time-string' using `parse-time-string' and expand date and time
    (setq tl (condition-case nil (parse-time-string time-string) (error nil))
          year   (or (nth 5 tl) year   (nth 5 nowdecode))
          month  (or (nth 4 tl) month  (nth 4 nowdecode))
          day    (or (nth 3 tl) day    (nth 3 nowdecode))
          hour   (or (nth 2 tl) hour   (nth 2 nowdecode))
          minute (or (nth 1 tl) minute (nth 1 nowdecode))
          second (or (nth 0 tl) second (nth 0 nowdecode)))
    ;; Set `taskpaper-time-was-given' flag
    (when (nth 2 tl) (setq taskpaper-time-was-given t))
    ;; Weekday was given but no day
    (when (and (nth 6 tl) (not (nth 3 tl)))
      (setq wday (nth 6 tl)
            wday1 (nth 6 (decode-time (encode-time 0 0 0 day month year))))
      ;; Re-calculate weekday according to ISO 8601 week day definition
      (and (= wday 0) (setq wday 7)) (and (= wday1 0) (setq wday1 7))
      (setq day (+ day (- wday wday1))))
    ;; Evaluate duration offset
    (when (and dir deltan deltaw)
      (let ((wday (cdr (assoc (downcase deltaw) parse-time-weekdays)))
            (wday1 (nth 6 (decode-time (encode-time 0 0 0 day month year))))
            delta)
        (if wday
            (progn (setq delta (mod (+ 7 (- wday wday1)) 7))
                   (and (= delta 0) (setq delta 7))
                   (when (= dir ?-)
                     (progn (setq delta (- delta 7))
                            (and (= delta 0) (setq delta -7))))
                   (when (> deltan 1)
                     (setq delta (+ delta (* (1- deltan) (if (= dir ?-) -7 7)))))
                   (setq deltan delta deltaw "d"))
          (setq deltan (* deltan (if (= dir ?-) -1 1)))))
      (cond ((member deltaw '("h" "hour" "hours"))
             (setq hour (+ hour deltan) taskpaper-time-was-given t))
            ((member deltaw '("d" "day" "days"))
             (setq day (+ day deltan)))
            ((member deltaw '("w" "week" "weeks"))
             (setq day (+ day (* 7 deltan))))
            ((member deltaw '("m" "month" "months"))
             (setq month (+ month deltan)))
            ((member deltaw '("y" "year" "years"))
             (setq year (+ year deltan)))))
    ;; Get rid of out-of-range values for seconds, minutes, hours, days, and months
    (decode-time (apply 'encode-time (list second minute hour day month year)))))

(defun taskpaper-time-string-to-seconds (s)
  "Convert a timestamp string S to a float number of seconds.
Return the float number of seconds since the beginning of the
epoch."
  (float-time (apply 'encode-time (taskpaper-parse-time-string s))))

(defun taskpaper-2ft (s)
  "Convert S to a float number of seconds.
If S is already a number of seconds, just return it. If it is a
string, parse it as a time string and convert to float time. If S
is nil, return 0."
  (cond
   ((numberp s) s)
   ((stringp s)
    (condition-case nil
        (taskpaper-time-string-to-seconds s) (error 0.)))
   (t 0.)))

;;;; Interaction with calendar

(defun taskpaper-goto-calendar (&optional arg)
  "Go to the calendar at the current date.
If point is on a tag with value, interpret the value as time
string and go to the corresponding date instead. A prefix ARG can
be used to force the current date."
  (interactive "P")
  (let ((calendar-move-hook nil)
        (calendar-view-holidays-initially-flag nil)
        (calendar-view-diary-initially-flag nil)
        value time date)
    (cond
     ((taskpaper-in-regexp-p taskpaper-tag-regexp)
      (setq value (match-string-no-properties 3)
            value (taskpaper-tag-value-unescape value))
      (when value
        (setq time (taskpaper-parse-time-string value)
              date (list (nth 4 time) (nth 3 time) (nth 5 time)))))
     (t (setq date nil)))
    (calendar)
    (if (and date (not arg)) (calendar-goto-date date) (calendar-goto-today))))

(defun taskpaper-show-in-calendar ()
  "Show date at point in calendar.
If point is on a tag with value, interpret the value as time
string and show the corresponding date."
  (interactive)
  (let ((win (selected-window)))
    (taskpaper-goto-calendar) (select-window win)))

(defun taskpaper-get-date-from-calendar ()
  "Return a list (month day year) of date at point in calendar."
  (with-current-buffer "*Calendar*"
    (save-match-data (calendar-cursor-to-date))))

(defun taskpaper-date-from-calendar ()
  "Insert time stamp corresponding to cursor date in *Calendar* buffer."
  (interactive)
  (let* ((date (taskpaper-get-date-from-calendar))
         (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
    (insert-before-markers (format-time-string "%Y-%m-%d" time))))

;;;; Date prompt

(defvar taskpaper-calendar-selected-date nil
  "Temporary storage for date selected from calendar.
Date is stored as internal time representation.")

(defun taskpaper-eval-in-calendar (form)
  "Eval FORM in the calendar window.
Return to the current window."
  (let ((sf (selected-frame))
        (sw (selected-window)))
    (select-window (get-buffer-window "*Calendar*" t))
    (eval form)
    (select-window sw) (select-frame-set-input-focus sf)))

(defvar taskpaper-read-date-minibuffer-local-map
  (let* ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "!"
      (lambda () (interactive)
        (taskpaper-eval-in-calendar '(diary-mark-entries))
        (message nil)))
    (define-key map (kbd "C-.")
      (lambda () (interactive)
        (taskpaper-eval-in-calendar '(calendar-goto-today))))
    (define-key map ">"
      (lambda () (interactive)
        (taskpaper-eval-in-calendar '(calendar-scroll-left 1))))
    (define-key map "<"
      (lambda () (interactive)
        (taskpaper-eval-in-calendar '(calendar-scroll-right 1))))
    map)
  "Keymap for minibuffer commands when using `taskpaper-read-date'.")

(defun taskpaper-calendar-select ()
  "Return to `taskpaper-read-date' with the date currently selected."
  (interactive)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
           (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
      (setq taskpaper-calendar-selected-date time))
    (when (active-minibuffer-window) (exit-minibuffer))))

(defvar taskpaper-read-date-overlay nil)
(defun taskpaper-read-date-display ()
  "Display the date prompt interpretation live in minibuffer."
  (when taskpaper-read-date-display-live
    (when taskpaper-read-date-overlay
      (delete-overlay taskpaper-read-date-overlay))
    (when (minibufferp (current-buffer))
      (save-excursion
        (end-of-line 1)
        (and (< (- (point-max) (point)) 3)
             (not (equal
                   (buffer-substring (max (point-min) (- (point) 3)) (point))
                   "   "))
             (insert "   ")))
      (let* ((str (buffer-substring (point-at-bol) (point-max)))
             (time (taskpaper-parse-time-string str))
             (fmt (if taskpaper-time-was-given "%Y-%m-%d %H:%M" "%Y-%m-%d"))
             (txt (format-time-string fmt (apply 'encode-time time))))
        (setq taskpaper-read-date-overlay
              (make-overlay (1- (point-at-eol)) (point-at-eol)))
        (taskpaper-overlay-display
         taskpaper-read-date-overlay txt 'secondary-selection)))))

(defun taskpaper-read-date (&optional prompt to-time)
  "Prompt the user for a date using PROMPT.
Return formatted date as string. If optional argument TO-TIME is
non-nil return the date converted to an internal time."
  (let ((mouse-autoselect-window nil)
        (calendar-move-hook nil)
        (calendar-view-diary-initially-flag nil)
        (calendar-view-holidays-initially-flag nil)
        (prompt (if prompt prompt "Date & time: ")) text)
    (save-excursion
      (save-window-excursion
        (when taskpaper-read-date-popup-calendar
          ;; Open calendar
          (calendar) (calendar-goto-today))
        (let (;; Save calendar keymap
              (old-map (copy-keymap calendar-mode-map))
              ;; Set keymap to control calendar from minibuffer
              (minibuffer-local-map
               (copy-keymap taskpaper-read-date-minibuffer-local-map)))
          (unwind-protect
              (progn
                ;; Set temporary calendar keymap
                (define-key calendar-mode-map (kbd "RET")
                  'taskpaper-calendar-select)
                (define-key calendar-mode-map [mouse-1]
                  'taskpaper-calendar-select)
                ;; Reset `taskpaper-calendar-selected-date'
                (setq taskpaper-calendar-selected-date nil)
                ;; Activate live preview
                (add-hook 'post-command-hook
                          'taskpaper-read-date-display)
                ;; Read date
                (setq text (read-string prompt nil
                                        taskpaper-read-date-history)))
            ;; Deactivate live preview
            (remove-hook 'post-command-hook
                         'taskpaper-read-date-display)
            ;; Restore calendar keymap
            (setq calendar-mode-map old-map)
            ;; Remove live preview overlay
            (when taskpaper-read-date-overlay
              (delete-overlay taskpaper-read-date-overlay)
              (setq taskpaper-read-date-overlay nil))))))
    ;; Convert and format date
    (let* ((date (taskpaper-parse-time-string text))
           (time (or taskpaper-calendar-selected-date
                     (apply 'encode-time date)))
           (fmt (if (and taskpaper-time-was-given
                         (not taskpaper-calendar-selected-date))
                    "%Y-%m-%d %H:%M" "%Y-%m-%d")))
      ;; Return the selected date
      (if to-time time (format-time-string fmt time)))))

(defun taskpaper-read-date-insert-timestamp ()
  "Prompt the user for a date and insert the timestamp at point."
  (interactive)
  (insert (format "%s" (taskpaper-read-date))))

;;;; Tags

(defun taskpaper-get-buffer-tags (&optional point)
  "Get a list of all tag names in the current buffer, for completion.
If POINT is inside a tag, ignore the tag."
  (let (tag tags)
    (save-excursion
      (save-restriction
        (save-match-data
          (widen) (goto-char (point-min))
          (while (re-search-forward taskpaper-tag-regexp nil t)
            (unless (and point
                         (<= (match-beginning 0) point)
                         (>= (match-end 0) point))
              (setq tag (match-string-no-properties 2))
              (push tag tags)))))
      (taskpaper-sort (taskpaper-uniquify tags)))))

(defun taskpaper-complete-tag-at-point (&optional attrs)
  "Complete tag name or query attribute at point.
Complete tag name or query attribute using completions from
ATTRS. If ATTRS is not given, use tag names from the current
buffer instead."
  (interactive "*")
  (setq attrs (or attrs (taskpaper-get-buffer-tags (point))))
  (let ((completion-ignore-case t)
        (re (format "@%s*" taskpaper-tag-name-char-regexp))
        (pattern
         (if (taskpaper-in-regexp-p taskpaper-tag-name-regexp)
             (match-string-no-properties 0) ""))
        (completion-buffer-name "*Completions*")
        (end (point)) completion)
    ;; Close completion window, if any
    (let ((window (get-buffer-window completion-buffer-name)))
      (when window (delete-window window)))
    ;; Check if there is something to complete
    (unless (taskpaper-in-regexp-p re)
      (user-error "Nothing to complete"))
    ;; Try completion
    (setq completion (try-completion pattern attrs))
    (cond
     ((eq completion t)
      ;; Completion done
      (message "Sole completion"))
     ((null completion)
      ;; No completion found
      (user-error "No match for %s" pattern))
     ((not (string-equal pattern completion))
      ;; Expand the current word to max match
      (delete-region (- end (length pattern)) end) (insert completion))
     (t
      ;; List possible completions
      (when completion-auto-help
        (with-output-to-temp-buffer completion-buffer-name
          (display-completion-list
           (all-completions pattern attrs) pattern)))
      (set-window-dedicated-p
       (get-buffer-window completion-buffer-name) 'soft)))))

(defun taskpaper-fast-tag-selection ()
  "Fast tag selection with single keys."
  (unless taskpaper-tag-alist (error "No predefined tags"))
  (save-excursion
    (save-window-excursion
      (switch-to-buffer-other-window
       (get-buffer-create "*TaskPaper tags*"))
      (erase-buffer)
      (setq show-trailing-whitespace nil)
      (let* ((table taskpaper-tag-alist)
             (maxlen
              (apply
               'max (mapcar
                     (lambda (x)
                       (if (stringp (car x)) (string-width (car x)) 0))
                     table)))
             (fwidth (+ maxlen 4))
             (ncol (/ (window-width) fwidth))
             cnt tbl c e tg)
        ;; Insert selection dialog
        (insert "\n")
        (setq tbl table cnt 0)
        (while (setq e (pop tbl))
          (setq tg (car e) c (cdr e))
          (if (and c tg)
              (insert
               (propertize (char-to-string c)
                           'face 'taskpaper-fast-select-key-face)
               " " tg (make-string (- fwidth 2 (length tg)) ?\ )))
          (when (= (setq cnt (1+ cnt)) ncol)
            (insert "\n") (setq cnt 0)))
        (insert "\n\n") (goto-char (point-min)) (fit-window-to-buffer)
        ;; Select tag
        (setq c (read-char-exclusive "Press key for tag:"))
        (if (setq e (rassoc c table) tg (car e))
            (prog1 tg (kill-buffer))
          (kill-buffer) (setq quit-flag t))))))

(defun taskpaper-item-set-tag-fast-select ()
  "Set the tag for the item at point using fast tag selection."
  (interactive)
  (let ((re (format "\\`\\(%s\\)\\(?:(\\(%s\\))\\)?\\'"
                    taskpaper-tag-name-regexp
                    taskpaper-tag-value-regexp))
        (tag (taskpaper-fast-tag-selection))
        name value)
    (unless (string-match re tag) (error "Invalid tag specifier: %s" tag))
    (setq name (match-string-no-properties 1 tag)
          value (match-string-no-properties 2 tag))
    ;; Expand tag value
    (cond ((equal value "%t")
           (setq value
                 (format-time-string "%Y-%m-%d" (current-time))))
          ((equal value "%T")
           (setq value
                 (format-time-string "%Y-%m-%d %H:%M" (current-time))))
          ((equal value "%^T")
           (setq value (taskpaper-read-date)))
          (t value))
    (taskpaper-item-set-attribute name value)))

(defun taskpaper-remove-tag-at-point ()
  "Remove tag at point."
  (interactive)
  (if (taskpaper-in-regexp-p taskpaper-tag-regexp)
      (delete-region (match-beginning 0) (match-end 0))
    (user-error "No tag at point.")))

(defun taskpaper-item-toggle-done ()
  "Toggle done state of item at point."
  (interactive)
  (when (or (equal (taskpaper-item-get-attribute "type") "task")
            (equal (taskpaper-item-get-attribute "type") "project"))
    (if (taskpaper-item-has-attribute "done")
        (taskpaper-item-remove-attribute "done")
      ;; Remove extra tags
      (mapc (lambda (tag) (taskpaper-item-remove-attribute tag))
            taskpaper-tags-to-remove-when-done)
      ;; Mark as complete
      (taskpaper-item-set-attribute
       "done"
       (when taskpaper-complete-save-date
         (format-time-string "%Y-%m-%d" (current-time)))))))

;;;; Relation functions

(defun taskpaper-num= (a b)
  "Return t if two arg numbers are equal.
String args are converted to numbers before test."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (cond ((numberp a) a)
                       ((stringp a) (string-to-number a))
                       (t 0.))
               b (cond ((numberp b) b)
                       ((stringp b) (string-to-number b))
                       (t 0.)))
         (= a b))))

(defun taskpaper-num< (a b)
  "Return t if first arg number is less than second.
String args are converted to numbers before test."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (cond ((numberp a) a)
                       ((stringp a) (string-to-number a))
                       (t 0.))
               b (cond ((numberp b) b)
                       ((stringp b) (string-to-number b))
                       (t 0.)))
         (< a b))))

(defun taskpaper-num<= (a b)
  "Return t if first arg number is less than or equal to second.
String args are converted to numbers before test."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (cond ((numberp a) a)
                       ((stringp a) (string-to-number a))
                       (t 0.))
               b (cond ((numberp b) b)
                       ((stringp b) (string-to-number b))
                       (t 0.)))
         (<= a b))))

(defun taskpaper-num> (a b)
  "Return t if first arg number is greater than second.
String args are converted to numbers before test."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (cond ((numberp a) a)
                       ((stringp a) (string-to-number a))
                       (t 0.))
               b (cond ((numberp b) b)
                       ((stringp b) (string-to-number b))
                       (t 0.)))
         (> a b))))

(defun taskpaper-num>= (a b)
  "Return t if first arg number is greater than or equal to second.
String args are converted to numbers before test."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (cond ((numberp a) a)
                       ((stringp a) (string-to-number a))
                       (t 0.))
               b (cond ((numberp b) b)
                       ((stringp b) (string-to-number b))
                       (t 0.)))
         (>= a b))))

(defun taskpaper-num<> (a b)
  "Return t if two arg numbers are not equal.
String args are converted to numbers before test."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (cond ((numberp a) a)
                       ((stringp a) (string-to-number a))
                       (t 0.))
               b (cond ((numberp b) b)
                       ((stringp b) (string-to-number b))
                       (t 0.)))
         (not (= a b)))))

(defun taskpaper-string= (a b)
  "Return t if two arg strings have identical contents.
Case is significant."
  (cond ((not (and a b)) nil)
        (t (string= a b))))

(defun taskpaper-string< (a b)
  "Return t if first arg string is less than second in lexicographic order.
Case is significant."
  (cond ((not (and a b)) nil)
        (t (string< a b))))

(defun taskpaper-string<= (a b)
  "Return t if first arg string is less than or equal to second in lexicographic order.
Case is significant."
  (cond ((not (and a b))
         nil)
        (t
         (or (string< a b)
             (string= a b)))))

(defun taskpaper-string> (a b)
  "Return t if first arg string is greater than second in lexicographic order.
Case is significant."
  (cond ((not (and a b))
         nil)
        (t
         (and (not (string< a b))
              (not (string= a b))))))

(defun taskpaper-string>= (a b)
  "Return t if first arg string is greater than or equal to second in lexicographic order.
Case is significant."
  (cond ((not (and a b)) nil)
        (t (not (string< a b)))))

(defun taskpaper-string<> (a b)
  "Return t if two arg string are not equal.
Case is significant."
  (cond ((not (and a b)) nil)
        (t (not (string= a b)))))

(defun taskpaper-string-match-p (a b)
  "Return t if first arg string matches second arg regexp.
Case is significant."
  (cond ((not (and a b))
         nil)
        (t
         (let ((case-fold-search nil))
           (string-match-p b a)))))

(defun taskpaper-string-contain-p (a b)
  "Return t if first arg string contains second.
Case is significant."
  (cond ((not (and a b))
         nil)
        (t
         (let ((case-fold-search nil))
           (setq b (regexp-quote b))
           (string-match-p b a)))))

(defun taskpaper-string-prefix-p (a b)
  "Return t if second arg string is a prefix of first.
Case is significant."
  (cond ((not (and a b)) nil)
        (t (string-prefix-p b a))))

(defun taskpaper-string-suffix-p (a b)
  "Return t if second arg string is a suffix of first.
Case is significant."
  (cond ((not (and a b)) nil)
        (t (string-suffix-p b a))))

(defun taskpaper-istring= (a b)
  "Return t if two strings have identical contents.
Case is ignored."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (downcase a) b (downcase b))
         (string= a b))))

(defun taskpaper-istring< (a b)
  "Return t if first arg string is less than second in lexicographic order.
Case is ignored."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (downcase a) b (downcase b))
         (string< a b))))

(defun taskpaper-istring<= (a b)
  "Return t if first arg string is less than or equal to second in lexicographic order.
Case is ignored."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (downcase a) b (downcase b))
         (or (string= a b) (string< a b)))))

(defun taskpaper-istring> (a b)
  "Return t if first arg string is greater than second in lexicographic order.
Case is ignored."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (downcase a) b (downcase b))
         (and (not (string= a b))
              (not (string< a b))))))

(defun taskpaper-istring>= (a b)
  "Return t if first arg string is greater than or equal to second in lexicographic order.
Case is ignored."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (downcase a) b (downcase b))
         (not (string< a b)))))

(defun taskpaper-istring<> (a b)
  "Return t if two arg string are not equal.
Case is ignored."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (downcase a) b (downcase b))
         (not (string= a b)))))

(defun taskpaper-istring-match-p (a b)
  "Return t if first arg string matches second arg regexp.
Case is ignored."
  (cond ((not (and a b))
         nil)
        (t
         (let ((case-fold-search nil))
           (setq a (downcase a) b (downcase b))
           (string-match-p b a)))))

(defun taskpaper-istring-contain-p (a b)
  "Return t if first arg string contains second.
Case is ignored."
  (cond ((not (and a b))
         nil)
        (t
         (let ((case-fold-search nil))
           (setq a (downcase a) b (downcase b))
           (setq b (regexp-quote b))
           (string-match-p b a)))))

(defun taskpaper-istring-prefix-p (a b)
  "Return t if second arg string is a prefix of first.
Case is ignored."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (downcase a) b (downcase b))
         (string-prefix-p b a))))

(defun taskpaper-istring-suffix-p (a b)
  "Return t if second arg string is a suffix of first.
Case is ignored."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (downcase a) b (downcase b))
         (string-suffix-p b a))))

(defun taskpaper-time= (a b)
  "Return t if two arg time strings are equal.
Time string are converted to a float number of seconds before
numeric comparison. If any argument is a float number, it will be
treated as the float number of seconds since the beginning of the
epoch."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (taskpaper-2ft a) b (taskpaper-2ft b))
         (and (> a 0) (> b 0) (= a b)))))

(defun taskpaper-time< (a b)
  "Return t if first arg time string is less than second.
Time string are converted to a float number of seconds before
numeric comparison. If any argument is a float number, it will be
treated as the float number of seconds since the beginning of the
epoch."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (taskpaper-2ft a) b (taskpaper-2ft b))
         (and (> a 0) (> b 0) (< a b)))))

(defun taskpaper-time<= (a b)
  "Return t if first arg time string is less than or equal to second.
Time string are converted to a float number of seconds before
numeric comparison. If any argument is a float number, it will be
treated as the float number of seconds since the beginning of the
epoch."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (taskpaper-2ft a) b (taskpaper-2ft b))
         (and (> a 0) (> b 0) (<= a b)))))

(defun taskpaper-time> (a b)
  "Return t if first arg time string is greater than second.
Time string are converted to a float number of seconds before
numeric comparison. If any argument is a float number, it will be
treated as the float number of seconds since the beginning of the
epoch."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (taskpaper-2ft a) b (taskpaper-2ft b))
         (and (> a 0) (> b 0) (> a b)))))

(defun taskpaper-time>= (a b)
  "Return t if first arg time string is greater than or equal to second.
Time string are converted to a float number of seconds before
numeric comparison. If any argument is a float number, it will be
treated as the float number of seconds since the beginning of the
epoch."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (taskpaper-2ft a) b (taskpaper-2ft b))
         (and (> a 0) (> b 0) (>= a b)))))

(defun taskpaper-time<> (a b)
  "Return t if two arg time strings are not equal.
Time string are converted to a float number of seconds before
numeric comparison. If any argument is a float number, it will be
treated as the float number of seconds since the beginning of the
epoch."
  (cond ((not (and a b))
         nil)
        (t
         (setq a (taskpaper-2ft a) b (taskpaper-2ft b))
         (and (> a 0) (> b 0) (taskpaper-num<> a b)))))

;;;; Filtering

(defvar taskpaper-occur-highlights nil
  "List of overlays used for occur matches.")
(make-variable-buffer-local 'taskpaper-occur-highlights)

(defun taskpaper-occur-add-highlights (begin end)
  "Highlight from BEGIN to END."
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'face 'secondary-selection)
    (push overlay taskpaper-occur-highlights)))

(defun taskpaper-occur-remove-highlights (&optional _begin _end)
  "Remove the occur highlights from the buffer."
  (interactive)
  (mapc 'delete-overlay taskpaper-occur-highlights)
  (setq taskpaper-occur-highlights nil))

(defun taskpaper-occur (&optional regexp)
  "Make a sparse tree showing items matching REGEXP.
Return the number of matches."
  (interactive)
  (setq regexp (or regexp (read-regexp "Regexp: ")))
  (when (equal regexp "") (user-error "Regexp cannot be empty"))
  (taskpaper-occur-remove-highlights)
  (outline-flag-region (point-min) (point-max) t)
  (goto-char (point-min))
  (let ((cnt 0))
    (while (re-search-forward regexp nil t)
      (setq cnt (1+ cnt))
      (taskpaper-occur-add-highlights
       (match-beginning 0) (match-end 0))
      (taskpaper-outline-show-context))
    (add-hook 'before-change-functions
              'taskpaper-occur-remove-highlights
              nil 'local)
    (when (called-interactively-p 'any) (message "%d match(es)" cnt))
    cnt))

(defun taskpaper-match-sparse-tree (matcher)
  "Create a sparse tree according to MATCHER.
MATCHER is a Lisp form to be evaluated at an outline item and
returns non-nil if the item matches."
  (taskpaper-occur-remove-highlights)
  (outline-flag-region (point-min) (point-max) t)
  (let ((re (concat "^" outline-regexp)))
    (goto-char (point-min))
    (save-excursion
      (while (let (case-fold-search)
               (re-search-forward re nil t))
        (when (let ((case-fold-search t))
                (save-excursion (eval matcher)))
          (taskpaper-outline-show-context))))))

(defun taskpaper-search-tag-at-point ()
  "Create a sparse tree showing items tagging with tag at point.
If point is on the tag name, match only the tag name, otherwise
match the tag-value combination."
  (interactive)
  (if (taskpaper-in-regexp-p taskpaper-tag-regexp)
      (let* ((name (match-string-no-properties 2))
             (value (match-string-no-properties 3))
             (value (taskpaper-tag-value-unescape value)))
        (if (taskpaper-in-regexp-p name)
            (taskpaper-match-sparse-tree
             `(taskpaper-item-has-attribute ,name))
          (taskpaper-match-sparse-tree
           `(taskpaper-item-has-attribute ,name ,value))))
    (user-error "No tag at point")))

;;;; Sorting

(defun taskpaper-sort-items-generic
  (getkey-func compare-func &optional with-case reverse)
  "Sort items on a certain level.
When point is at the beginning of the buffer, sort the top-level
items. Else, the children of the item at point are sorted.

The GETKEY-FUNC specifies a function to be called with point at
the beginning of the item. It must return either a string or a
number that should serve as the sorting key for that item. The
COMPARE-FUNC specifies a function to compare the sorting keys; it
is called with two arguments, the sorting keys, and should return
non-nil if the first key should sort before the second key.

Comparing items ignores case by default. However, with an
optional argument WITH-CASE, the sorting considers case as well.
The optional argument REVERSE will reverse the sort order.

When sorting is done, call `taskpaper-after-sorting-items-hook'."
  (if (buffer-narrowed-p)
      (widen))
  (let ((case-func (if with-case 'identity 'downcase))
        begin end)
    ;; Set sorting boundaries
    (cond
     ((bobp)
      ;; Sort top-level items
      (setq begin (point))
      (goto-char (point-max))
      ;; Add newline, if nessessary
      (unless (bolp) (end-of-line) (newline))
      (setq end (point-max))
      (goto-char begin)
      (outline-show-all)
      (or (outline-on-heading-p)
          (outline-next-heading)))
     (t
      ;; Sort children of the item at point
      (setq begin (point))
      (outline-end-of-subtree)
      (if (eq (char-after) ?\n) (forward-char 1)
        ;; Add newline, if nessessary
        (unless (bolp) (end-of-line) (newline)))
      (setq end (point))
      (goto-char begin)
      (outline-show-subtree)
      (outline-next-heading)))
    ;; Check sorting boundaries
    (when (>= begin end)
      (goto-char begin)
      (user-error "Nothing to sort"))
    ;; Sort items
    (message "Sorting items...")
    (save-restriction
      (narrow-to-region begin end)
      (let ((case-fold-search nil) tmp)
        ;; Call `sort-subr' function
        (sort-subr
         ;; REVERSE arg
         reverse
         ;; NEXTRECFUN arg
         (lambda nil
           (if (re-search-forward "^[\t]*[^\t\n]" nil t)
               (goto-char (match-beginning 0))
             (goto-char (point-max))))
         ;; ENDRECFUN arg
         (lambda nil
           (save-match-data
             (condition-case nil
                 (outline-forward-same-level 1)
               (error (goto-char (point-max))))))
         ;; STARTKEYFUN arg
         (lambda nil
           (progn
             (setq tmp (funcall getkey-func))
             (if (stringp tmp) (setq tmp (funcall case-func tmp)))
             tmp))
         ;; ENDKEYFUN arg
         nil
         ;; PREDICATE arg
         compare-func))))
  (run-hooks 'taskpaper-after-sorting-items-hook)
  (message "Sorting items...done"))

(defun taskpaper-sort-alpha (&optional reverse)
  "Sort items on a certain level alphabetically.
The optional argument REVERSE will reverse the sort order."
  (interactive "P")
  (taskpaper-sort-items-generic
   '(lambda nil (taskpaper-remove-type-formatting
                 (taskpaper-item-get-attribute "text")))
   'taskpaper-string< nil reverse))

(defconst taskpaper-sort-precedence-type
  '(("project" . 3)("task" . 2)("note" . 1))
  "Order of sorting precedence for item types.
Items with the higher precedence will be sorted before items with
the lower one.")

(defun taskpaper-sort-by-type (&optional reverse)
  "Sort items on a certain level by type.
The optional argument REVERSE will reverse the sort order."
  (interactive "P")
  (taskpaper-sort-items-generic
   '(lambda nil
      (let ((type (taskpaper-item-get-attribute "type")))
        (cdr (assoc type taskpaper-sort-precedence-type))))
   '> nil reverse))

;;;; Outline path

(defun taskpaper-item-get-outline-path (&optional self)
  "Return the outline path to the item at point.
An outline path is a list of ancestors for the current item, in
reverse order, as a list of strings. When SELF is non-nil, the
path also includes the current item."
  (let (item olpath)
    (save-excursion
      (outline-back-to-heading t)
      (when self
        (setq item (taskpaper-item-get-attribute "text"))
        (push item olpath))
      (while (taskpaper-outline-up-level-safe)
        (setq item (taskpaper-item-get-attribute "text"))
        (push item olpath)))
    olpath))

(defun taskpaper-format-olpath-prep (entry)
  "Format the outline path entry ENTRY for display."
  (setq entry (taskpaper-remove-type-formatting entry)
        entry (taskpaper-remove-trailing-tags entry)
        entry (replace-regexp-in-string "/" "." entry))
  entry)

(defun taskpaper-format-outline-path (olpath &optional prefix separator)
  "Format the outline path OLPATH for display.
PREFIX is a prefix to be included in the returned string.
SEPARATOR is inserted between the different entries of the path,
the default is \"/\"."
  (setq olpath (delq nil olpath) separator (or separator "/"))
  (concat prefix (mapconcat #'taskpaper-format-olpath-prep olpath separator)))

;;;; Goto interface

(defun taskpaper-goto-get-targets (&optional excluded-entries)
  "Produce a table with possible outline targets.
Return a list of (OLPATH POS) elements where OLPATH is the
formatted outline path as string and POS is the corresponding
buffer position. Single entries in OLPATH should be separated
with a slash. EXCLUDED-ENTRIES is a list of OLPATH elements,
which will be excluded from the results."
  (let ((re (concat "^" outline-regexp)) target targets)
    (save-excursion
      (save-restriction
        (widen) (goto-char (point-min))
        (message "Get targets...")
        (while (re-search-forward re nil t)
          (setq target (taskpaper-format-outline-path
                        (taskpaper-item-get-outline-path t)))
          (unless (or (not target) (member target excluded-entries))
            (push (list target (point-at-bol)) targets)))))
    (message "Get targets...done")
    (nreverse targets)))

(defun taskpaper-goto-get-location (&optional prompt no-exclude)
  "Prompt the user for a location, using PROMPT.
Return a list (OLPATH POS) where OLPATH is the formatted outline
path as string and POS is the corresponding buffer position.
Single entries in OLPATH should be separated with a slash. When
NO-EXCLUDE is set, do not exclude entries in the current
subtree."
  (let ((prompt (if prompt prompt "Path: "))
        excluded-entries targets target)
    (when (and (outline-on-heading-p) (not no-exclude))
      ;; Exclude the subtree at point
      (taskpaper-map-tree
       (lambda ()
         (setq excluded-entries
               (append excluded-entries
                       (list (taskpaper-format-outline-path
                              (taskpaper-item-get-outline-path t))))))))
    ;; Set possible targets
    (setq targets (taskpaper-goto-get-targets excluded-entries))
    (unless targets (user-error "No targets"))
    (let ((partial-completion-mode nil) (completion-ignore-case t))
      ;; Select outline path
      (setq target (completing-read prompt targets nil t))
      ;; Find the associated outline path and buffer position
      (assoc target targets))))

(defun taskpaper-goto ()
  "Go to a selected location."
  (interactive)
  (let* ((loc (taskpaper-goto-get-location "Goto: " t))
         (pos (nth 1 loc)))
    (widen) (goto-char pos) (back-to-indentation)
    (save-excursion (taskpaper-outline-show-context))))

;;;; Copying, cutting, and pasting of trees

(defun taskpaper-clone-subtree ()
  "Duplicate the current subtree."
  (interactive)
  (let (begin end)
    (save-excursion
      (save-match-data
        ;; Mark the current subtree
        (outline-back-to-heading)
        (setq begin (point))
        (outline-end-of-subtree)
        (if (eq (char-after) ?\n) (forward-char 1)
          ;; Add newline, if nessessary
          (unless (bolp) (end-of-line) (newline)))
        (setq end (point))
        ;; Paste duplicate
        (insert-before-markers
         (buffer-substring begin end))))))

(defun taskpaper-copy-subtree (&optional cut)
  "Copy the current subtree into the kill ring.
If CUT is non-nil, actually cut the subtree."
  (interactive)
  (let (begin end)
    (save-excursion
      (save-match-data
        ;; Mark the current subtree
        (outline-back-to-heading) (setq begin (point))
        (outline-end-of-subtree)
        (if (eq (char-after) ?\n) (forward-char 1)
          ;; Add newline, if nessessary
          (unless (bolp) (end-of-line) (newline)))
        (setq end (point))
        ;; Cut/copy region into the kill ring
        (if cut
            (kill-region begin end)
          (copy-region-as-kill begin end))))))

(defun taskpaper-cut-subtree ()
  "Cut the current subtree and put it into the kill ring."
  (interactive)
  (taskpaper-copy-subtree 'cut))

(defun taskpaper-kill-is-subtree-p (&optional txt)
  "Check if the current kill is a valid subtree.
Return nil if the first item level is not the largest item level
in the tree. So this will actually accept a set of subtrees as
well. If optional TXT string is given, check it instead of the
current kill."
  (save-match-data
    (let* ((kill (or txt (and kill-ring (current-kill 0)) ""))
           (start-level
            (and kill
                 (string-match
                  "\\`\\(?:[\t\n]*?\n\\)?\\([\t]*[^\t\n]\\)"
                  kill)
                 (- (match-end 1) (match-beginning 1))))
           (start (1+ (or (match-beginning 1) -1))))
      (if (not start-level) nil
        (catch 'exit
          (while (setq start
                       (string-match "^\\([\t]*[^\t\n]\\)" kill (1+ start)))
            (when (< (- (match-end 1) (match-beginning 1)) start-level)
              (throw 'exit nil)))
          t)))))

(defun taskpaper-paste-subtree (&optional level text remove)
  "Paste the current kill as a subtree, with modification of level.
If point is on a (possibly invisible) item, paste as child of the
current item. You can force a different level by using a numeric
prefix argument. If optional TEXT is given, use this text instead
of the current kill. Place point at the beginning of pasted
subtree. When REMOVE is non-nil, remove the subtree from the kill
ring."
  (interactive "P")
  (setq text (or text (and kill-ring (current-kill 0))))
  (unless text (user-error "Nothing to paste"))
  (unless (taskpaper-kill-is-subtree-p text)
    (user-error "The text is not a (set of) tree(s)"))
  (let* ((old-level (if (string-match "^\\([\t]*[^\t\n]\\)" text)
                        (- (match-end 1) (match-beginning 1))
                      1))
         (force-level (when level (prefix-numeric-value level)))
         (current-level (if (outline-on-heading-p t)
                            (save-match-data (funcall outline-level))
                          0))
         (new-level (or force-level (1+ current-level) 1))
         (shift (- new-level old-level))
         (delta (if (> shift 0) -1 1))
         (func (if (> shift 0)
                   'taskpaper-outline-demote
                 'taskpaper-outline-promote))
         begin end)
    ;; Paste the subtree and mark it
    (beginning-of-line 2)
    (unless (bolp) (end-of-line) (newline))
    (setq begin (point))
    (insert-before-markers text)
    ;; Add newline, if nessessary
    (unless (string-match-p "\n\\'" text) (newline))
    (setq end (point))
    ;; Adjust outline level
    (unless (= shift 0)
      (save-restriction
        (narrow-to-region begin end)
        (while (not (= shift 0))
          (taskpaper-map-region func (point-min) (point-max))
          (setq shift (+ delta shift)))
        (goto-char (point-min))
        (setq end (point-max))))
    ;; Place point at the beginning of the subtree
    (goto-char begin)
    (when (called-interactively-p 'any)
      (message "Clipboard pasted as level %d subtree." new-level)))
  (when remove (setq kill-ring (cdr kill-ring))))

;;;; Refiling

(defun taskpaper-refile-subtree (&optional arg rfloc)
  "Move the subtree at point to another (possibly invisible) location.
The subtree is filed below the target location as a subitem.
Depending on `taskpaper-reverse-note-order', it will be either
the first or last subitem. If ARG is non-nil, just copy the
subtree. RFLOC can be a refile location in form (OLPATH POS)
obtained in a different way."
  (interactive)
  (let* ((loc (or rfloc (taskpaper-goto-get-location nil arg)))
         (path (nth 0 loc)) (pos (nth 1 loc)) level)
    ;; Check the target position
    (if (and (not arg) pos
             (>= pos (point))
             (< pos (save-excursion (outline-end-of-subtree) (point))))
        (error "Cannot refile to item inside the current subtree"))
    ;; Copy the subtree
    (taskpaper-copy-subtree)
    ;; Move to the target position and paste the subtree
    (save-excursion
      (widen) (goto-char pos) (outline-back-to-heading t)
      (setq level (save-match-data (funcall outline-level)))
      (unless taskpaper-reverse-note-order (outline-end-of-subtree))
      (taskpaper-paste-subtree (1+ level)))
    ;; Cut the subtree from the original location
    (when (not arg) (taskpaper-cut-subtree))
    (when (called-interactively-p 'any)
      (message "Subtree refiled to %s." path))))

(defun taskpaper-refile-subtree-copy ()
  "Copy the subtree at point to another location.
Copying works like refiling, except that the subtree is not
deleted from the original location."
  (interactive)
  (taskpaper-refile-subtree t))

;;;; Archiving

(defun taskpaper-extract-archive-file (&optional location)
  "Extract and expand the file name from archive LOCATION.
Return file name for archive file. If LOCATION is not given, the
value of `taskpaper-archive-location' is used."
  (setq location (or location taskpaper-archive-location))
  (if (string-match "\\(.*\\)::\\(.*\\)" location)
      (if (= (match-beginning 1) (match-end 1))
          (buffer-file-name (buffer-base-buffer))
        (expand-file-name
         (format (match-string-no-properties 1 location)
                 (file-name-sans-extension
                  (file-name-nondirectory
                   (buffer-file-name (buffer-base-buffer)))))))))

(defun taskpaper-extract-archive-heading (&optional location)
  "Extract the heading from archive LOCATION.
If LOCATION is not given, the value of
`taskpaper-archive-location' is used."
  (setq location (or location taskpaper-archive-location))
  (if (string-match "\\(.*\\)::\\(.*\\)" location)
      (format (match-string-no-properties 2 location)
              (file-name-sans-extension
               (file-name-nondirectory
                (buffer-file-name (buffer-base-buffer)))))))

(defun taskpaper-archive-get-project ()
  "Get project hierarchy for the item at point."
  (let (project projects)
    (save-excursion
      (outline-back-to-heading t)
      (while (taskpaper-outline-up-level-safe)
        (when (equal "project" (taskpaper-item-get-attribute "type"))
          (setq project (taskpaper-item-get-attribute "text"))
          (push project projects))))
    (if projects (taskpaper-format-outline-path projects) nil)))

(defun taskpaper-archive-subtree ()
  "Move the current subtree to the archive location.
The archive can be a certain top-level heading in the current
file, or in a different file. For details see the variable
`taskpaper-archive-location'. The subtree is filed below the
archive heading as a subitem. Depending on
`taskpaper-reverse-note-order', it will be either the first or
last subitem."
  (interactive)
  (let ((this-buffer (current-buffer))
        (file (abbreviate-file-name
               (or (buffer-file-name (buffer-base-buffer))
                   (error "No file associated to buffer"))))
        afile heading buffer project level)
    ;; Extract archive heading and file name
    (setq afile (taskpaper-extract-archive-file
                 taskpaper-archive-location)
          heading (taskpaper-extract-archive-heading
                   taskpaper-archive-location))
    (unless afile (error "Invalid `taskpaper-archive-location'"))
    ;; When an archive file is specified, visit it
    ;; and set this buffer as archive buffer;
    ;; otherwise fall back to the current buffer
    (if (> (length afile) 0)
        (setq buffer (or (find-buffer-visiting afile)
                         (find-file-noselect afile)))
      (setq buffer (current-buffer)))
    (unless buffer (error "Cannot access file: %s" afile))
    ;; Archive subtree
    (save-excursion
      ;; Get context information
      (setq project (taskpaper-archive-get-project))
      ;; Copy subtree
      (taskpaper-copy-subtree)
      ;; Go to the archive buffer
      (set-buffer buffer)
      ;; Enforce TaskPaper mode for the archive buffer
      (when (not (derived-mode-p 'taskpaper-mode))
        (call-interactively 'taskpaper-mode))
      ;; Show everything
      (widen) (goto-char (point-min)) (taskpaper-outline-show-all)
      ;; Go to the archive location and paste the subtree
      (cond
       ((and (stringp heading) (> (length heading) 0))
        ;; Archive heading specified
        (if (re-search-forward
             (format "^%s" (regexp-quote heading)) nil t)
            ;; Archive heading found
            (goto-char (match-end 0))
          ;; No archive heading found, insert it at EOB
          (goto-char (point-max)) (delete-blank-lines)
          (unless (bolp) (end-of-line) (newline))
          (insert heading "\n"))
        ;; File the subtree under the archive heading
        (outline-back-to-heading)
        (setq level (save-match-data (funcall outline-level)))
        (unless taskpaper-reverse-note-order (outline-end-of-subtree))
        (taskpaper-paste-subtree (1+ level)))
       (t
        ;; No archive heading specified, go to EOB
        (goto-char (point-max)) (delete-blank-lines)
        (unless (bolp) (end-of-line) (newline))
        ;; Paste the subtree at EOB
        (taskpaper-paste-subtree)))
      ;; Add the context information
      (and taskpaper-archive-save-context project
           (taskpaper-item-set-attribute "project" project))
      ;; Save the buffer, if it is not the current buffer
      (when (not (eq this-buffer buffer)) (save-buffer)))
    ;; Run hooks
    (run-hooks 'taskpaper-archive-hook)
    ;; Bind `this-command' to avoid `kill-region' changes it,
    ;; which may lead to duplication of subtrees
    ;; NOTE: Do not bind `this-command' with `let' because
    ;; that would restore the old value in case of error
    (let (old-this-command this-command)
      (setq this-command t)
      ;; Cut the subtree from the original location
      (taskpaper-cut-subtree)
      (setq this-command old-this-command)))
  (when (called-interactively-p 'any) (message "Subtree archived.")))

;;;; Quick entry API

;;;###autoload
(defun taskpaper-add-entry (&optional text location file)
  "Add entry TEXT to LOCATION in FILE.

When FILE is specified, visit it and set this buffer as target
buffer, otherwise fall back to the current buffer.

Prompt user for entry TEXT and add it as child of the top-level
LOCATION item. The entry is filed below the target location as a
subitem. Depending on `taskpaper-reverse-note-order', it will be
either the first or last subitem. When the location is omitted,
the item is simply filed at the end of the file, as top-level
item."
  (interactive)
  (let ((text (or text (read-string "Entry: ")))
        (this-buffer (current-buffer)) buffer level)
    ;; Check the entry text
    (unless (taskpaper-kill-is-subtree-p text)
      (user-error "The text is not a (set of) tree(s)"))
    ;; Select buffer
    (if (and (stringp file) (> (length file) 0))
        (setq buffer (or (find-buffer-visiting file)
                         (find-file-noselect file)))
      (setq buffer (current-buffer)))
    (unless buffer (error "Cannot access file: %s" file))
    ;; Go to the target buffer
    (with-current-buffer buffer
      ;; Enforce TaskPaper mode
      (when (not (derived-mode-p 'taskpaper-mode)) (taskpaper-mode))
      ;; Show everything
      (widen) (goto-char (point-min)) (taskpaper-outline-show-all)
      ;; Go to the target location and paste the entry
      (cond
       ((and (stringp location) (> (length location) 0))
        ;; Target location specified
        (if (re-search-forward
             (format "^%s" (regexp-quote location)) nil t)
            ;; Location found
            (goto-char (match-end 0))
          ;; No location found, insert it at EOB
          (goto-char (point-max)) (delete-blank-lines)
          (unless (bolp) (end-of-line) (newline))
          (insert location "\n"))
        ;; File the entry under the target location
        (outline-back-to-heading)
        (setq level (save-match-data (funcall outline-level)))
        (unless taskpaper-reverse-note-order (outline-end-of-subtree))
        (taskpaper-paste-subtree (1+ level) text))
       (t
        ;; No location specified, go to EOB
        (goto-char (point-max)) (delete-blank-lines)
        (unless (bolp) (end-of-line) (newline))
        ;; Paste the entry at EOB
        (taskpaper-paste-subtree nil text)))
      ;; Save the buffer, if it is not the current buffer
      (when (not (eq this-buffer buffer)) (save-buffer))))
  (when (called-interactively-p 'any) (message "Entry added.")))

;;;; Querying

(defun taskpaper-query-op-to-func (op mod)
  "Convert operator OP and modifier MOD into function."
  (setq op
        (cond
         ((equal op "<" )
          '(taskpaper-time<
            taskpaper-num<
            taskpaper-string<
            taskpaper-istring<))
         ((equal op ">" )
          '(taskpaper-time>
            taskpaper-num>
            taskpaper-string>
            taskpaper-istring>))
         ((equal op "=" )
          '(taskpaper-time=
            taskpaper-num=
            taskpaper-string=
            taskpaper-istring=))
         ((equal op "<=")
          '(taskpaper-time<=
            taskpaper-num<=
            taskpaper-string<=
            taskpaper-istring<=))
         ((equal op ">=")
          '(taskpaper-time>=
            taskpaper-num>=
            taskpaper-string>=
            taskpaper-istring>=))
         ((equal op "!=")
          '(taskpaper-time<>
            taskpaper<>
            taskpaper-string<>
            taskpaper-istring<>))
         ((equal op "contains")
          '(taskpaper-istring-contain-p
            taskpaper-istring-contain-p
            taskpaper-string-contain-p
            taskpaper-istring-contain-p))
         ((equal op "beginswith")
          '(taskpaper-istring-prefix-p
            taskpaper-istring-prefix-p
            taskpaper-string-prefix-p
            taskpaper-istring-prefix-p))
         ((equal op "endswith")
          '(taskpaper-istring-suffix-p
            taskpaper-istring-suffix-p
            taskpaper-string-suffix-p
            taskpaper-istring-suffix-p))
         ((equal op "matches")
          '(taskpaper-istring-match-p
            taskpaper-istring-match-p
            taskpaper-string-match-p
            taskpaper-istring-match-p))
         (t (error "Invalid relation operator: %s" op))))
  (cond ((equal mod "d") (nth 0 op))
        ((equal mod "n") (nth 1 op))
        ((equal mod "s") (nth 2 op))
        ((equal mod "i") (nth 3 op))
        (t (error "Invalid relaton modifier: %s" mod))))

(defconst taskpaper-query-attribute-regexp
  (format "\\(@%s+\\)" taskpaper-tag-name-char-regexp)
  "Regular expression for attribute.")

(defconst taskpaper-query-operator-regexp
  "\\([<>!]=\\|[<>=]\\)"
  "Regular expression for non-alphanumeric relation operator.")

(defconst taskpaper-query-modifier-regexp
  "\\(\\[[isnd]\\]\\)"
  "Regular expression for relation modifier.")

(defconst taskpaper-query-quoted-string-regexp
  "\\(\"\\(?:\\\\\"\\|[^\"]\\)*\"\\)"
  "Regular expression for double-quoted string.")

(defconst taskpaper-query-open-regexp
  "\\((\\)"
  "Regular expression for opening parenthesis.")

(defconst taskpaper-query-close-regexp
  "\\()\\)"
  "Regular expression for closing parenthesis.")

(defconst taskpaper-query-word-regexp
  "\\([^][@<>=!()\" \t\n\r]+\\)"
  "Regular expression for word.")

(defconst taskpaper-query-whitespace-regexp
  "[ \t\n\r]*"
  "Regular expression for whitespace.")

(defconst taskpaper-query-word-operator
  '("and" "or" "not"
    "contains" "beginswith" "endswith" "matches")
  "Valid query word operators.")

(defconst taskpaper-query-non-word-operator
  '("=" "<" ">" "<=" ">=" "!=")
  "Valid query non-word operators.")

(defconst taskpaper-query-word-shortcut
  '("project" "task" "note")
  "Valid query type shortcuts.")

(defconst taskpaper-query-relation-operator
  '("=" "<" ">" "<=" ">=" "!="
    "contains" "beginswith" "endswith" "matches")
  "Valid query relation operators.")

(defconst taskpaper-query-relation-modifier
  '("[i]" "[s]" "[n]" "[d]")
  "Valid query relation modifiers.")

(defconst taskpaper-query-boolean-not
  '("not")
  "Valid Boolean NOT operators.")

(defconst taskpaper-query-boolean-binary
  '("and" "or")
  "Valid Boolean binary operators.")

(defconst taskpaper-query-open-close
  '("(" ")")
  "Opening and closing parentheses.")

(defun taskpaper-query-word-operator-p (str)
  "Return non-nil if STR is a valid word operator."
  (member str taskpaper-query-word-operator))

(defun taskpaper-query-type-shortcut-p (str)
  "Return non-nil if STR is a valid type shortcut."
  (member str taskpaper-query-word-shortcut))

(defun taskpaper-query-attribute-p (str)
  "Return non-nil if STR is a valid attribute."
  (let ((re (concat
             "\\`" taskpaper-query-attribute-regexp "\\'")))
    (string-match-p re str)))

(defun taskpaper-query-relation-operator-p (str)
  "Return non-nil if STR is a valid relation operator."
  (member str taskpaper-query-relation-operator))

(defun taskpaper-query-relation-modifier-p (str)
  "Return non-nil if STR is a valid relation modifier."
  (member str taskpaper-query-relation-modifier))

(defun taskpaper-query-open-p (str)
  "Return non-nil if STR is the opening parenthesis."
  (equal str "("))

(defun taskpaper-query-close-p (str)
  "Return non-nil if STR is the closing parenthesis."
  (equal str ")"))

(defun taskpaper-query-boolean-not-p (str)
  "Return non-nil if STR is a valid Boolean NOT operator."
  (member str taskpaper-query-boolean-not))

(defun taskpaper-query-boolean-binary-p (str)
  "Return non-nil if STR is a valid Boolean binary operator."
  (member str taskpaper-query-boolean-binary))

(defun taskpaper-query-search-term-p (str)
  "Return non-nil if STR is a valid search term."
  (and (not (taskpaper-query-word-operator-p str))
       (not (taskpaper-query-attribute-p str))
       (not (taskpaper-query-relation-operator-p str))
       (not (taskpaper-query-relation-modifier-p str))
       (not (taskpaper-query-open-p str))
       (not (taskpaper-query-close-p str))))

(defun taskpaper-query-read-tokenize (str)
  "Read query string STR into tokens.
Return list of substrings. Each substring is a run of valid
characters repsesenting different types ot tokens."
  (let ((depth 0) tokens val st)
    (while (> (length str) 0)
      ;; Trim leading whitespaces
      (setq str
            (replace-regexp-in-string
             (concat "\\`" taskpaper-query-whitespace-regexp)
             "" str))
      (unless (= (length str) 0)
        (cond
         ((equal (string-to-char str) ?@)
          ;; Read attribute
          (if (string-match
               (concat "\\`" taskpaper-query-attribute-regexp) str)
              (progn
                (setq val (match-string-no-properties 1 str))
                (setq str (replace-match "" nil nil str))
                (when st (push st tokens) (setq st nil))
                (push val tokens))
            (error "Error while reading attribute")))
         ((member (string-to-char str) '(?< ?> ?= ?!))
          ;; Read non-word relation operator
          (if (string-match
               (concat "\\`" taskpaper-query-operator-regexp) str)
              (progn
                (setq val (match-string-no-properties 1 str))
                (setq str (replace-match "" nil nil str))
                (when st (push st tokens) (setq st nil))
                (push val tokens))
            (error "Error while reading relation operator")))
         ((equal (string-to-char str) ?\[)
          ;; Read relation modifier
          (if (string-match
               (concat "\\`" taskpaper-query-modifier-regexp) str)
              (progn
                (setq val (match-string-no-properties 1 str))
                (setq str (replace-match "" nil nil str))
                (when st (push st tokens) (setq st nil))
                (push val tokens))
            (error "Error while reading relation modifier")))
         ((equal (string-to-char str) ?\()
          ;; Read opening parenthesis
          (if (string-match
               (concat "\\`" taskpaper-query-open-regexp) str)
              (progn
                (setq val (match-string-no-properties 1 str))
                (setq str (replace-match "" nil nil str))
                (when st (push st tokens) (setq st nil))
                (setq depth (1+ depth))
                (push val tokens))
            (error "Error while reading opening parenthesis")))
         ((equal (string-to-char str) ?\))
          ;; Read closing parenthesis
          (if (string-match
               (concat "\\`" taskpaper-query-close-regexp) str)
              (progn
                (setq val (match-string-no-properties 1 str))
                (setq str (replace-match "" nil nil str))
                (when st (push st tokens) (setq st nil))
                (if (= depth 0)
                    (error "Unbalanced closing parenthesis")
                  (setq depth (1- depth)))
                (push val tokens))
            (error "Error while reading closing parenthesis")))
         ((equal (string-to-char str) ?\")
          ;; Read quoted string
          (if (string-match
               (concat "\\`" taskpaper-query-quoted-string-regexp) str)
              (progn
                (setq val (match-string-no-properties 1 str))
                (setq str (replace-match "" nil nil str))
                (when st (push st tokens) (setq st nil))
                (push val tokens))
            (error "Error while reading quoted string")))
         (t
          ;; Read word
          (if (string-match
               (concat "\\`" taskpaper-query-word-regexp) str)
              (progn
                (setq val (match-string-no-properties 1 str))
                (setq str (replace-match "" nil nil str))
                (cond ((or (taskpaper-query-word-operator-p val)
                           (taskpaper-query-type-shortcut-p val))
                       (when st (push st tokens) (setq st nil))
                       (push val tokens))
                      (t
                       (setq st (if st (concat st " " val) val)))))
            (error "Error while reading word"))))))
    (when st (push st tokens))
    (when (> depth 0) (error "Unbalanced opening parenthesis"))
    (nreverse tokens)))

(defun taskpaper-query-expand-type-shortcuts (tokens)
  "Expand type shortcuts in TOKENS."
  (let (token prev next expanded)
    (while tokens
      (setq token (pop tokens) next (nth 0 tokens))
      (cond ((and (taskpaper-query-type-shortcut-p token)
                  (not (taskpaper-query-relation-operator-p prev))
                  (not (taskpaper-query-relation-modifier-p prev)))
             (setq prev nil)
             (push "@type" expanded) (push "=" expanded)
             (push token expanded)
             (and next (not (taskpaper-query-boolean-binary-p next))
                  (push "and" expanded)))
            (t
             (setq prev token)
             (push token expanded))))
    (nreverse expanded)))

(defun taskpaper-query-parse-predicate (tokens)
  "Parse next predicate expression in token list TOKENS.
Return a cons of the constructed Lisp form implementing the
matcher and the rest of the token list."
  (let (attr op mod val form)
    ;; Get predicate arguments
    (let ((token (nth 0 tokens)))
      (when (and token (taskpaper-query-attribute-p token))
        (setq attr (substring token 1)) (pop tokens)))
    (let ((token (nth 0 tokens)))
      (when (and token (taskpaper-query-relation-operator-p token))
        (setq op token) (pop tokens)))
    (let ((token (nth 0 tokens)))
      (when (and token (taskpaper-query-relation-modifier-p token))
        (setq mod (substring token 1 -1)) (pop tokens)))
    (let ((token (nth 0 tokens)))
      (when (and token (taskpaper-query-search-term-p token))
        (setq val
              (if (equal (string-to-char token) ?\")
                  (substring token 1 -1)
                token))
        (pop tokens)))
    ;; Provide default values
    (setq attr (or attr "text") op (or op "contains") mod (or mod "i"))
    ;; Convert operator to function
    (setq op (taskpaper-query-op-to-func op mod))
    ;; Unescape double quotes in search term
    (when val (setq val (replace-regexp-in-string "\\\\\"" "\"" val)))
    ;; Convert time string to speedup matching
    (and (equal mod "d") val (setq val (taskpaper-2ft val)))
    ;; Build Lisp form
    (cond
     ((not val)
      (setq form `(taskpaper-item-has-attribute ,attr nil t)))
     (t
      (setq form
            `(,op (taskpaper-item-get-attribute ,attr t) ,val))))
    (cons form tokens)))

(defun taskpaper-query-parse-boolean-unary (tokens)
  "Parse next unary Boolean expression in token list TOKENS.
Return a cons of the constructed Lisp form implementing the
matcher and the rest of the token list."
  (let (not temp right form)
    ;; Get operator
    (when (and tokens (taskpaper-query-boolean-not-p (nth 0 tokens)))
      (setq not t) (pop tokens))
    ;; Get the right side
    (cond
     ((taskpaper-query-open-p (nth 0 tokens))
      (setq temp (taskpaper-query-parse-parentheses tokens)))
     (t
      (setq temp (taskpaper-query-parse-predicate tokens))))
    (setq right (car temp) tokens (cdr temp))
    ;; Build Lisp form
    (cond
     ((and not right)
      (setq form `(not ,right)))
     (right
      (setq form right))
     (t
      (error "Invalid Boolean unary expression")))
    (cons form tokens)))

(defconst taskpaper-query-precedence-boolean
  '(("and" . 0)("or" . 1))
  "Order of precedence for binary Boolean operators.
Operators with lower precedence bind more strongly.")

(defun taskpaper-query-parse-boolean-binary (tokens prec &optional left)
  "Parse next binary Boolean expression in token list TOKENS.
Return a cons of the constructed Lisp form implementing the
matcher and the rest of the token list. PREC is the current
precedence for Boolean operators. LEFT is a Lisp form
representing the left side of the Boolean expression. This
function implements the top-down operator-precedence recursive
parsing algorithm known as Pratt's algorithm. See also variable
`taskpaper-query-precedence-boolean'."
  (let (temp bool cprec right form)
    ;; Get the left side
    (when (and tokens (not left))
      (cond
       ((taskpaper-query-open-p (nth 0 tokens))
        (setq temp (taskpaper-query-parse-parentheses tokens)))
       (t
        (setq temp (taskpaper-query-parse-boolean-unary tokens))))
      (setq left (car temp) tokens (cdr temp)))
    ;; Get operator
    (let ((token (nth 0 tokens)))
      (when (taskpaper-query-boolean-binary-p token)
        (setq bool token) (pop tokens)))
    ;; Get the right side
    (when (and tokens bool left)
      (cond
       ((taskpaper-query-open-p (nth 0 tokens))
        (setq temp (taskpaper-query-parse-parentheses tokens)))
       (t
        ;; Get the current precedence
        (setq cprec (cdr (assoc bool taskpaper-query-precedence-boolean)))
        (setq temp
              (if (> cprec prec)
                  (taskpaper-query-parse-boolean-binary tokens cprec)
                (taskpaper-query-parse-boolean-unary tokens)))))
      (setq right (car temp) tokens (cdr temp)))
    ;; Build Lisp form
    (when bool
      (setq bool (cond ((equal bool "or") 'or) ((equal bool "and") 'and))))
    (cond
     ((and bool left right)
      (setq form `(,bool ,left ,right)))
     ((and left (not bool))
      (setq form left))
     (t
      (error "Invalid Boolean binary expression")))
    (cons form tokens)))

(defun taskpaper-query-parse-parentheses (tokens)
  "Parse next parenthetical expression in token list TOKENS.
Return a cons of the constructed Lisp form implementing the
matcher and the rest of the token list."
  (let (temp left)
    (if (taskpaper-query-open-p (nth 0 tokens))
        (pop tokens)
      (error "Opening parenthesis expected"))
    (while (and tokens (not (taskpaper-query-close-p (nth 0 tokens))))
      ;; Parse Boolean binary expression
      (setq temp (taskpaper-query-parse-boolean-binary tokens 0 left)
            left (car temp) tokens (cdr temp))
      (when (and (not (taskpaper-query-close-p (nth 0 tokens)))
                 (not (taskpaper-query-boolean-binary-p (nth 0 tokens))))
        (error "Boolean binary operator or closing parenthesis expected")))
    (if (taskpaper-query-close-p (nth 0 tokens))
        (pop tokens)
      (error "Closing parenthesis expected"))
    (cons left tokens)))

(defun taskpaper-query-parse (tokens)
  "Parse token list TOKENS.
Return constructed Lisp form implementing the matcher."
  (let (temp left)
    (while tokens
      (setq temp (taskpaper-query-parse-boolean-binary tokens 0 left)
            left (car temp) tokens (cdr temp))
      (when (and tokens
                 (not (taskpaper-query-boolean-binary-p (nth 0 tokens))))
        (error "Boolean binary operator expected")))
    left))

(defun taskpaper-query-matcher (str)
  "Parse query string STR.
Return constructed Lisp form implementing the matcher. The
matcher is to be evaluated at an outline item and returns non-nil
if the item matches the selection string STR."
  (let (tokens)
    ;; Tokenize query string and expand shortcuts
    (setq tokens (taskpaper-query-read-tokenize str))
    (setq tokens (taskpaper-query-expand-type-shortcuts tokens))
    ;; Parse token list and construct matcher
    (if tokens (taskpaper-query-parse tokens) t)))

(defun taskpaper-query-selection ()
  "Fast selection for queries with single keys."
  (unless taskpaper-custom-queries (error "No predefined queries"))
  (save-excursion
    (save-window-excursion
      (switch-to-buffer-other-window
       (get-buffer-create "*TaskPaper queries*"))
      (erase-buffer)
      (toggle-truncate-lines 1)
      (setq show-trailing-whitespace nil)
      (let* ((table taskpaper-custom-queries) tbl c e desc qs)
        ;; Insert selection dialog
        (insert "\n")
        (setq tbl table)
        (while (setq e (pop tbl))
          (cond
           ((and (stringp (nth 0 e)) (eq (nth 0 e) ""))
            (insert "\n"))
           ((and (stringp (nth 0 e)) (not (eq (nth 0 e) "")))
            (insert (format "\n%s\n\n" (nth 0 e))))
           (t (setq c (nth 0 e) desc (nth 1 e) qs (nth 2 e))
              (when (and c desc qs)
                (insert (format
                         "%s %-20s %s\n"
                         (propertize (char-to-string c)
                                     'face 'taskpaper-fast-select-key-face)
                         (concat desc "  ") qs))))))
        (insert "\n") (goto-char (point-min)) (fit-window-to-buffer)
        ;; Select query
        (setq c (read-char-exclusive "Press key for query:"))
        (if (setq e (assoc c table) qs (nth 2 e))
            (prog1 qs (kill-buffer))
          (kill-buffer) (setq quit-flag t))))))

(defun taskpaper-query-fast-select ()
  "Query buffer using fast query selection."
  (interactive)
  (taskpaper-query (taskpaper-query-selection)))

(defun taskpaper-query-fontify-query ()
  "Fontify query in minibuffer."
  (save-excursion
    (let ((case-fold-search nil))
      ;; Fontify word operators
      (goto-char (point-min))
      (while (re-search-forward
              (regexp-opt taskpaper-query-word-operator 'words) nil t)
        (set-text-properties
         (match-beginning 0) (match-end 0)
         (list 'face 'taskpaper-query-secondary-text-face)))
      ;; Fontify non-word operators and modifiers
      (goto-char (point-min))
      (while (re-search-forward
              (regexp-opt (append taskpaper-query-non-word-operator
                                  taskpaper-query-relation-modifier
                                  taskpaper-query-open-close))
              nil t)
        (set-text-properties
         (match-beginning 0) (match-end 0)
         (list 'face 'taskpaper-query-secondary-text-face)))
      ;; Fontify double-quoted strings as the last step
      (goto-char (point-min))
      (while (re-search-forward
              taskpaper-query-quoted-string-regexp nil t)
        (set-text-properties
         (match-beginning 1) (match-end 1) (list 'face 'default))))))

(defun taskpaper-read-query-propertize (&optional _begin _end _length)
  "Propertize query string live in minibuffer.
Incrementally read query string, validate it and propertize
accordingly. The function should be called from minibuffer as
part of `after-change-functions' hook."
  (when (minibufferp (current-buffer))
    (condition-case nil
        (progn
          (remove-text-properties (point-at-bol) (point-max) (list 'face))
          (taskpaper-query-fontify-query)
          (taskpaper-query-matcher (minibuffer-contents-no-properties)))
      (error
       (set-text-properties
        (point-at-bol) (point-max)
        (list 'face 'taskpaper-query-error-face))))))

(defun taskpaper-query-read-query (&optional prompt)
  "Prompt user for search query.
Validate input and provide tab completion for attributes in
minibuffer. Return query string. PROMPT can overwrite the default
prompt."
  (let ((attrs (append (taskpaper-get-buffer-tags)
                       taskpaper-special-attributes))
        (map (make-sparse-keymap))
        (prompt (or prompt "Query: ")) str)
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "TAB")
      (lambda () (interactive) (taskpaper-complete-tag-at-point attrs)))
    (define-key map (kbd "C-c C-c")
      (lambda () (interactive) (delete-minibuffer-contents)))
    (let ((minibuffer-local-map (copy-keymap map))
          (minibuffer-message-timeout 0.5))
      (unwind-protect
          (progn
            ;; Add hooks
            (add-hook 'after-change-functions
                      'taskpaper-read-query-propertize)
            ;; Read query string
            (setq str (read-string prompt nil taskpaper-query-history)))
        ;; Remove hooks
        (remove-hook 'after-change-functions
                     'taskpaper-read-query-propertize))
      str)))

(defun taskpaper-query (&optional str)
  "Create a sparse tree according to query string STR."
  (interactive)
  (setq str (or str (taskpaper-query-read-query)))
  (message "Querying...")
  (let ((matcher (taskpaper-query-matcher str)))
    (if matcher (taskpaper-match-sparse-tree matcher)))
  (message "Querying...done"))

(defun taskpaper-iquery-query (&optional _begin _end _length)
  "Evaluate querying in main window.
The function should be called from minibuffer as part of
`after-change-functions' hook."
  (when (and (minibufferp (current-buffer))
             (minibuffer-selected-window))
    (let ((str (minibuffer-contents-no-properties)))
      ;; Select the main window
      (with-selected-window (minibuffer-selected-window)
        ;; Evaluate query
        (condition-case nil
            (let ((matcher (taskpaper-query-matcher str)))
              (when matcher (taskpaper-match-sparse-tree matcher)))
          (error nil))))))

(defun taskpaper-iquery (&optional prompt)
  "Create a sparse tree according to query string.
Query results are updated incrementally as you type, showing
items, that matches. PROMPT can overwrite the default prompt."
  (interactive)
  (let ((map (make-sparse-keymap))
        (prompt (or prompt "I-query: "))
        (attrs (append (taskpaper-get-buffer-tags)
                       taskpaper-special-attributes))
        (win (get-buffer-window (current-buffer))) str)
    ;; Build attribute cache
    (taskpaper-attribute-cache-build)
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "TAB")
      (lambda () (interactive) (taskpaper-complete-tag-at-point attrs)))
    (define-key map (kbd "C-c C-c")
      (lambda () (interactive) (delete-minibuffer-contents)))
    (let ((minibuffer-local-map (copy-keymap map))
          (minibuffer-message-timeout 0.5))
      (unwind-protect
          (progn
            ;; Add hooks
            (add-hook 'after-change-functions
                      'taskpaper-read-query-propertize 'append)
            (add-hook 'after-change-functions
                      'taskpaper-iquery-query 'append)
            ;; Read query string
            (read-string prompt nil taskpaper-query-history))
        ;; Remove hooks
        (remove-hook 'after-change-functions
                     'taskpaper-read-query-propertize)
        (remove-hook 'after-change-functions
                     'taskpaper-iquery-query)
        ;; Clear attribute cache
        (taskpaper-attribute-cache-clear)))))

;;;; Ispell and Flyspell functions

(defun taskpaper-ispell-setup ()
  "Ispell setup for TaskPaper-mode."
  (add-to-list 'ispell-skip-region-alist (list taskpaper-tag-regexp))
  (add-to-list 'ispell-skip-region-alist (list taskpaper-uri-browser-regexp))
  (add-to-list 'ispell-skip-region-alist (list taskpaper-email-regexp))
  (add-to-list 'ispell-skip-region-alist (list taskpaper-file-path-regexp)))

(defun taskpaper-mode-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate'."
  (and (not (taskpaper-in-regexp-p taskpaper-tag-regexp))
       (not (taskpaper-in-regexp-p taskpaper-uri-browser-regexp))
       (not (taskpaper-in-regexp-p taskpaper-email-regexp))
       (not (taskpaper-in-regexp-p taskpaper-file-path-regexp))))
(put 'taskpaper-mode 'flyspell-mode-predicate 'taskpaper-mode-flyspell-verify)

;;;; Miscellaneous

(defun taskpaper-tab ()
  "Demote item at point or indent line."
  (interactive)
  (cond ((outline-on-heading-p)
         (call-interactively #'taskpaper-outline-demote))
        (t (call-interactively #'indent-for-tab-command))))

(defun taskpaper-shifttab ()
  "Promote item at point."
  (interactive)
  (when (outline-on-heading-p)
    (call-interactively #'taskpaper-outline-promote)))

;;;; Major mode definition

;;;###autoload
(define-derived-mode taskpaper-mode outline-mode "TaskPaper"
  "Major mode for editing and querying files in TaskPaper format.
TaskPaper mode is implemented on top of Outline mode. Turning on
TaskPaper mode runs the normal hook `text-mode-hook', and then
`outline-mode-hook' and `taskpaper-mode-hook'."
  (kill-all-local-variables)
  ;; Disable Outline mode menus
  (define-key taskpaper-mode-map [menu-bar headings] 'undefined)
  (define-key taskpaper-mode-map [menu-bar hide] 'undefined)
  (define-key taskpaper-mode-map [menu-bar show] 'undefined)
  ;; General settings
  (setq major-mode 'taskpaper-mode)
  (setq mode-name "TaskPaper")
  (use-local-map taskpaper-mode-map)
  ;; Invisibility spec
  (taskpaper-set-local 'line-move-ignore-invisible t)
  (add-to-invisibility-spec '(outline . t))
  ;; Outline settings
  ;; NOTE: Group 1 in `outline-regexp' is used by `replace-match'
  ;; in `taskpaper-promote' and `taskpaper-demote' functions.
  (taskpaper-set-local 'outline-regexp "\\([\t]*\\)[^\t\n]")
  (taskpaper-set-local 'outline-heading-end-regexp "\n")
  (taskpaper-set-local 'outline-blank-line t)
  ;; Paragraph filling
  (taskpaper-set-local 'paragraph-start
                       (concat "\f\\|[ \t]*$\\|\\(?:" outline-regexp "\\)"))
  (taskpaper-set-local 'paragraph-separate "[ \t\f]*$")
  (taskpaper-set-local 'auto-fill-inhibit-regexp outline-regexp)
  (taskpaper-set-local 'adaptive-fill-regexp
                       "[ \t]*\\(\\(?:[-+*]+\\|[0-9]+\\.\\)[ \t]*\\)*")
  ;; Font lock settings
  (taskpaper-set-font-lock-defaults)
  (taskpaper-set-local 'font-lock-unfontify-region-function 'taskpaper-unfontify-region)
  ;; Indentation settings
  (taskpaper-set-local 'indent-tabs-mode t)
  (taskpaper-set-local 'indent-line-function 'indent-to-left-margin)
  ;; Syntax table settings
  (set-syntax-table taskpaper-mode-syntax-table)
  ;; Imenu settings
  (setq imenu-generic-expression (list (list nil taskpaper-project-regexp 1)))
  ;; Isearch settings
  (setq-local outline-isearch-open-invisible-function
              (lambda (&rest _) (taskpaper-outline-show-context)))
  ;; Misc. settings
  (taskpaper-ispell-setup)
  (setq-local require-final-newline mode-require-final-newline)
  ;; Startup settings
  (taskpaper-set-startup-visibility)
  (when taskpaper-startup-with-inline-images (taskpaper-display-inline-images))
  ;; Hooks
  (add-hook 'change-major-mode-hook 'taskpaper-outline-show-all nil t)
  (run-hooks 'taskpaper-mode-hook))

(add-to-list 'auto-mode-alist '("\\.taskpaper\\'" . taskpaper-mode))

;;;; Key bindings

(define-key taskpaper-mode-map (kbd "TAB") 'taskpaper-tab)
(define-key taskpaper-mode-map (kbd "<tab>") 'taskpaper-tab)
(define-key taskpaper-mode-map (kbd "S-<tab>") 'taskpaper-shifttab)
(define-key taskpaper-mode-map (kbd "<backtab>") 'taskpaper-shifttab)
(define-key taskpaper-mode-map (kbd "<S-iso-lefttab>") 'taskpaper-shifttab)
(define-key taskpaper-mode-map (kbd "C-<tab>") 'taskpaper-cycle)
(define-key taskpaper-mode-map (kbd "C-<up>") 'taskpaper-outline-backward-same-level)
(define-key taskpaper-mode-map (kbd "C-<down>") 'taskpaper-outline-forward-same-level)
(define-key taskpaper-mode-map (kbd "RET") 'taskpaper-new-item-same-level)
(define-key taskpaper-mode-map (kbd "<return>") 'taskpaper-new-item-same-level)
(define-key taskpaper-mode-map (kbd "M-<up>") 'taskpaper-outline-move-subtree-up)
(define-key taskpaper-mode-map (kbd "M-<down>") 'taskpaper-outline-move-subtree-down)
(define-key taskpaper-mode-map (kbd "M-<left>") 'taskpaper-outline-promote-subtree)
(define-key taskpaper-mode-map (kbd "M-<right>") 'taskpaper-outline-demote-subtree)
(define-key taskpaper-mode-map (kbd "M-RET") 'taskpaper-new-task-same-level)
(define-key taskpaper-mode-map (kbd "M-<return>") 'taskpaper-new-task-same-level)
(define-key taskpaper-mode-map (kbd "C-M-i") 'taskpaper-complete-tag-at-point)
(define-key taskpaper-mode-map (kbd "M-<tab>") 'taskpaper-complete-tag-at-point)
(define-key taskpaper-mode-map (kbd "ESC TAB") 'taskpaper-complete-tag-at-point)
(define-key taskpaper-mode-map (kbd "S-<up>") 'taskpaper-outline-up-level)
(define-key taskpaper-mode-map (kbd "ESC ESC") 'taskpaper-outline-show-all)

(define-key taskpaper-mode-map (kbd "C-c SPC") 'taskpaper-show-in-calendar)
(define-key taskpaper-mode-map (kbd "C-c #") 'taskpaper-narrow-to-subtree)
(define-key taskpaper-mode-map (kbd "C-c *") 'taskpaper-outline-hide-other)
(define-key taskpaper-mode-map (kbd "C-c >") 'taskpaper-goto-calendar)
(define-key taskpaper-mode-map (kbd "C-c <") 'taskpaper-date-from-calendar)
(define-key taskpaper-mode-map (kbd "C-c .") 'taskpaper-read-date-insert-timestamp)
(define-key taskpaper-mode-map (kbd "C-c @") 'taskpaper-item-set-tag-fast-select)
(define-key taskpaper-mode-map (kbd "C-c /") 'taskpaper-occur)
(define-key taskpaper-mode-map (kbd "C-c ?") 'taskpaper-query-fast-select)

(define-key taskpaper-mode-map (kbd "C-c C-a") 'taskpaper-outline-show-all)
(define-key taskpaper-mode-map (kbd "C-c C-c") 'taskpaper-occur-remove-highlights)
(define-key taskpaper-mode-map (kbd "C-c C-d") 'taskpaper-item-toggle-done)
(define-key taskpaper-mode-map (kbd "C-c C-j") 'taskpaper-goto)
(define-key taskpaper-mode-map (kbd "C-c C-l") 'taskpaper-insert-file-link-at-point)
(define-key taskpaper-mode-map (kbd "C-c C-m") 'taskpaper-mark-subtree)
(define-key taskpaper-mode-map (kbd "C-c C-o") 'taskpaper-open-link-at-point)
(define-key taskpaper-mode-map (kbd "C-c C-i") 'taskpaper-iquery)
(define-key taskpaper-mode-map (kbd "C-c C-q") 'taskpaper-query)
(define-key taskpaper-mode-map (kbd "C-c C-r") 'taskpaper-remove-tag-at-point)
(define-key taskpaper-mode-map (kbd "C-c C-t") 'taskpaper-search-tag-at-point)
(define-key taskpaper-mode-map (kbd "C-c C-w") 'taskpaper-refile-subtree)
(define-key taskpaper-mode-map (kbd "C-c M-w") 'taskpaper-refile-subtree-copy)

(define-key taskpaper-mode-map (kbd "C-c C-f p") 'taskpaper-item-format-as-project)
(define-key taskpaper-mode-map (kbd "C-c C-f t") 'taskpaper-item-format-as-task)
(define-key taskpaper-mode-map (kbd "C-c C-f n") 'taskpaper-item-format-as-note)

(define-key taskpaper-mode-map (kbd "C-c C-s a") 'taskpaper-sort-alpha)
(define-key taskpaper-mode-map (kbd "C-c C-s t") 'taskpaper-sort-by-type)

(define-key taskpaper-mode-map (kbd "C-c C-x a") 'taskpaper-archive-subtree)
(define-key taskpaper-mode-map (kbd "C-c C-x v") 'taskpaper-outline-copy-visible)

(define-key taskpaper-mode-map (kbd "C-c C-x C-c") 'taskpaper-clone-subtree)
(define-key taskpaper-mode-map (kbd "C-c C-x C-w") 'taskpaper-cut-subtree)
(define-key taskpaper-mode-map (kbd "C-c C-x M-w") 'taskpaper-copy-subtree)
(define-key taskpaper-mode-map (kbd "C-c C-x C-y") 'taskpaper-paste-subtree)
(define-key taskpaper-mode-map (kbd "C-c C-x C-v") 'taskpaper-toggle-inline-images)

;;;; Menu

(easy-menu-define taskpaper-mode-menu taskpaper-mode-map
  "Menu for TaskPaper mode."
  '("TaskPaper"
    ("Item"
     ["Format as Project" taskpaper-item-format-as-project
      :active (outline-on-heading-p)]
     ["Format as Task" taskpaper-item-format-as-task
      :active (outline-on-heading-p)]
     ["Format as Note" taskpaper-item-format-as-note
      :active (outline-on-heading-p)])
    ("Visibility"
     ["Cycle Visibility" taskpaper-cycle
      :active (outline-on-heading-p)]
     ["Cycle Visibility (Global)" (taskpaper-cycle t)]
     ["Hide Other" taskpaper-outline-hide-other
      :active (outline-on-heading-p)]
     ["Show All" taskpaper-outline-show-all])
    ("Navigation"
     ["Up Level" taskpaper-outline-up-level
      :active (outline-on-heading-p)]
     ["Forward Same Level" taskpaper-outline-forward-same-level
      :active (outline-on-heading-p)]
     ["Backward Same Level" taskpaper-outline-backward-same-level
      :active (outline-on-heading-p)]
     "--"
     ["Go To..." taskpaper-goto])
    ("Structure Editing"
     ["Promote Item" taskpaper-outline-promote
      :active (outline-on-heading-p)]
     ["Demote Item" taskpaper-outline-demote
      :active (outline-on-heading-p)]
     "--"
     ["Promote Subtree" taskpaper-outline-promote-subtree
      :active (outline-on-heading-p)]
     ["Demote Subtree" taskpaper-outline-demote-subtree
      :active (outline-on-heading-p)]
     "--"
     ["Move Subtree Up" taskpaper-outline-move-subtree-up
      :active (outline-on-heading-p)]
     ["Move Substree Down" taskpaper-outline-move-subtree-down
      :active (outline-on-heading-p)]
     "--"
     ["Copy Subtree" taskpaper-copy-subtree
      :active (outline-on-heading-p)]
     ["Cut Subtree" taskpaper-cut-subtree
      :active (outline-on-heading-p)]
     ["Paste Subtree" taskpaper-paste-subtree
      :active (and kill-ring (current-kill 0))]
     ["Duplicate Subtree" taskpaper-clone-subtree
      :active (outline-on-heading-p)]
     "--"
     ["Mark Subtree" taskpaper-mark-subtree
      :active (outline-on-heading-p)]
     ["Narrow to Subtree" taskpaper-narrow-to-subtree
      :active (outline-on-heading-p)]
     "--"
     ["Sort Children Alphabetically" taskpaper-sort-alpha
      :active (outline-on-heading-p)]
     ["Sort Children by Type" taskpaper-sort-by-type
      :active (outline-on-heading-p)]
     "--"
     ["Refile Subtree..." taskpaper-refile-subtree
      :active (outline-on-heading-p)]
     ["Refile Subtree (Copy)..." taskpaper-refile-subtree-copy
      :active (outline-on-heading-p)]
     "--"
     ["Archive Subtree" taskpaper-archive-subtree
      :active (outline-on-heading-p)]
     "--"
     ["Copy Visible Items" taskpaper-outline-copy-visible
      :active (region-active-p)])
    ("Tag"
     ["Complete Tag" taskpaper-complete-tag-at-point
      :active (taskpaper-in-regexp-p (format "@%s*" taskpaper-tag-name-char-regexp))]
     ["Select Tag..." taskpaper-item-set-tag-fast-select]
     ["Remove Tag" taskpaper-remove-tag-at-point
      :active (taskpaper-in-regexp-p taskpaper-tag-regexp)]
     "--"
     ["Toggle Done" taskpaper-item-toggle-done
      :active (outline-on-heading-p)])
    ("Date & Time"
     ["Show Date in Calendar" taskpaper-show-in-calendar
      :active (taskpaper-in-regexp-p taskpaper-tag-regexp)]
     ["Access Calendar" taskpaper-goto-calendar]
     ["Insert Date from Calendar" taskpaper-date-from-calendar
      :active (get-buffer "*Calendar*")]
     ["Insert Date..." taskpaper-read-date-insert-timestamp])
    ("Links & Images"
     ["Insert File Link..." taskpaper-insert-file-link-at-point]
     ["Show Inline Images" taskpaper-toggle-inline-images
      :style toggle
      :selected taskpaper-inline-image-overlays])
    ("Search"
     ["Start Incremental Search..." taskpaper-iquery]
     ["Start Non-incremental Search..." taskpaper-query]
     ["Select Search Query..." taskpaper-query-fast-select]
     "--"
     ["Filter by Regexp..." taskpaper-occur]
     ["Remove Highlights" taskpaper-occur-remove-highlights
      :active taskpaper-occur-highlights])
    ("Agenda View"
     ["Create Agenda View..." taskpaper-agenda-search]
     ["Select Agenda View..." taskpaper-agenda-select])
    "--"
    ("Documentation"
     ["Show Version" taskpaper-mode-version]
     ["Browse Manual" taskpaper-mode-manual])
    "--"
    ["Customize..." (customize-browse 'taskpaper)]))

;;;; Agenda view

(defcustom taskpaper-agenda-files nil
  "List of files to be used for agenda display.
If an entry is a directory, all files in that directory that are
matched by `taskpaper-agenda-file-regexp' will be part of the
file list."
  :group 'taskpaper
  :type 'list)

(defcustom taskpaper-agenda-file-regexp "^[^.].*\\.taskpaper\\'"
  "Regular expression to match files for `taskpaper-agenda-files'.
If any element in the list in that variable contains a directory
instead of a normal file, all files in that directory that are
matched by this regular expression will be included."
  :group 'taskpaper
  :type 'regexp)

(defcustom taskpaper-agenda-skip-unavailable-files nil
  "If non-nil skip unavailable files in `taskpaper-agenda-files'."
  :group 'taskpaper
  :type 'boolean)

(defcustom taskpaper-agenda-sorting-predicate nil
  "Predicate function for sorting items in Agenda buffer.
If non-nil, it is called with two arguments, the items to
compare, and should return non-nil if the first item should sort
before the second one."
  :group 'taskpaper
  :type 'symbol)

(defcustom taskpaper-agenda-start-with-follow-mode nil
  "The initial value of follow mode in a newly created agenda window."
  :group 'taskpaper
  :type 'boolean)

(defcustom taskpaper-agenda-after-show-hook nil
  "Normal hook run after an item has been shown from the agenda.
Point is in the buffer where the item originated."
  :group 'taskpaper
  :type 'hook)

(defcustom taskpaper-agenda-window-setup 'reorganize-frame
  "How the agenda buffer should be displayed.
Possible values for this option are:

 current-window    Show agenda in the current window, keeping other windows
 other-window      Show agenda in other window
 only-window       Show agenda in the current window, deleting other windows
 other-frame       Show agenda in other frame
 reorganize-frame  Show only the current window and the agenda window"
  :group 'taskpaper
  :type '(choice
          (const current-window)
          (const other-window)
          (const only-window)
          (const other-frame)
          (const reorganize-frame)))

(defcustom taskpaper-agenda-restore-windows-after-quit nil
  "Non-nil means, restore window configuration upon exiting agenda.
Before the window configuration is changed for displaying the
agenda, the current status is recorded. When the agenda is exited
and this option is set, the old state is restored. If
`taskpaper-agenda-window-setup' is `other-frame', the value of
this option will be ignored."
  :group 'taskpaper
  :type 'boolean)

(defconst taskpaper-agenda-buffer-name "*TaskPaper Agenda*")

(defvar taskpaper-agenda-pre-window-conf nil)
(defvar taskpaper-agenda-pre-follow-window-conf nil)

(defvar taskpaper-agenda-matcher-form nil
  "Recent matcher form for agenda re-building.")
(make-variable-buffer-local 'taskpaper-agenda-matcher-form)

(defvar taskpaper-agenda-new-buffers nil
  "Buffers created to visit agenda files.")

(defvar taskpaper-agenda-follow-mode
  taskpaper-agenda-start-with-follow-mode)

(defun taskpaper-agenda-get-file-buffer (file)
  "Get an agenda buffer visiting FILE.
If the buffer needs to be created, add it to the list of buffers
which might be released later."
  (let ((buf (taskpaper-find-base-buffer-visiting file)))
    (if buf buf
      (setq buf (find-file-noselect file))
      (when buf (push buf taskpaper-agenda-new-buffers))
      buf)))

(defun taskpaper-agenda-set-mode-name ()
  "Set mode name to indicate all mode settings."
  (setq mode-name
        (list "TP-Agenda"
              (if taskpaper-agenda-follow-mode " Follow" "")
              (force-mode-line-update))))

(defun taskpaper-agenda-error ()
  "Throw an error when a command is not allowed in the agenda."
  (user-error "Command not allowed in this line"))

(defun taskpaper-agenda-files ()
  "Get the list of agenda files."
  (let ((files
         (if (listp taskpaper-agenda-files)
             taskpaper-agenda-files
           (error "Invalid value of `taskpaper-agenda-files'"))))
    (setq files
          (apply 'append
                 (mapcar (lambda (f)
                           (if (file-directory-p f)
                               (directory-files
                                f t taskpaper-agenda-file-regexp)
                             (list f)))
                         files)))
    (when taskpaper-agenda-skip-unavailable-files
      (setq files
            (delq nil
                  (mapcar (function
                           (lambda (file)
                             (and (file-readable-p file) file)))
                          files))))
    files))

(defun taskpaper-agenda-file-p (&optional file)
  "Return non-nil, if FILE is an agenda file.
If FILE is omitted, use the file associated with the current
buffer."
  (let ((fname (or file (buffer-file-name))))
    (and fname
         (member (file-truename fname)
                 (mapcar #'file-truename
                         (taskpaper-agenda-files))))))

(defun taskpaper-agenda-collect-items (matcher)
  "Return list of items matching MATCHER.
Cycle through agenda files and collect items matching MATCHER.
MATCHER is a Lisp form to be evaluated at an item; returning a
non-nil value qualifies the item for inclusion. Return list of
items linked back to the corresponding buffer position where the
item originated."
  (let ((files (taskpaper-agenda-files))
        file buffer marker item items)
    ;; Cycle through agenda files
    (while (setq file (pop files))
      (setq buffer
            (if (file-exists-p file)
                (taskpaper-agenda-get-file-buffer file)
              (error "Non-existent agenda file: %s" file)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'taskpaper-mode)
          (error "Agenda file is not in TaskPaper mode: %s" file))
        (let ((re (concat "^" outline-regexp)))
          (save-excursion
            (save-restriction
              (widen) (goto-char (point-min))
              (while (let (case-fold-search)
                       (re-search-forward re nil t))
                (when (let ((case-fold-search t))
                        (save-excursion (eval matcher)))
                  ;; Set marker and add the item to the list
                  (setq marker (taskpaper-new-marker (point-at-bol))
                        item (taskpaper-item-get-attribute "text")
                        item (propertize item 'taskpaper-marker marker))
                  (push item items))))))))
    (nreverse items)))

(defun taskpaper-agenda-sort-init (list)
  "Sort list of items for Agenda view."
  (let ((sfunc taskpaper-agenda-sorting-predicate))
    (if sfunc (sort list sfunc) list)))

(defun taskpaper-agenda-insert-items (matcher)
  "Insert items matching MATCHER."
  (unless (and (derived-mode-p 'taskpaper-agenda-mode)
               (equal (buffer-name) taskpaper-agenda-buffer-name))
    (error "Not in TaskPaper Agenda buffer"))
  (let ((inhibit-read-only t)
        (items (taskpaper-agenda-collect-items matcher)))
    (erase-buffer)
    (goto-char (point-min))
    (when items
      (setq items (taskpaper-agenda-sort-init items))
      (dolist (item items) (insert (format "%s\n" item))))
    (goto-char (point-min))))

(defun taskpaper-agenda-redo ()
  "Re-buid the current agenda buffer."
  (interactive)
  (message "Re-building agenda buffer...")
  (let ((matcher taskpaper-agenda-matcher-form))
    (when matcher (taskpaper-agenda-insert-items matcher)))
  (goto-char (point-min))
  (message "Re-building agenda buffer...done"))

(defun taskpaper-agenda-goto ()
  "Go to the item at point."
  (interactive)
  (let* ((marker
          (or (taskpaper-get-at-bol 'taskpaper-marker)
              (taskpaper-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (switch-to-buffer-other-window buffer)
    (widen) (push-mark) (goto-char pos))
  (run-hooks 'taskpaper-agenda-after-show-hook))

(defun taskpaper-agenda-switch-to ()
  "Go to the item at points and delete other windows."
  (interactive)
  (let* ((marker
          (or (taskpaper-get-at-bol 'taskpaper-marker)
              (taskpaper-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (unless buffer
      (user-error "Trying to switch to non-existent buffer"))
    (pop-to-buffer-same-window buffer)
    (delete-other-windows)
    (widen) (goto-char pos)))

(defun taskpaper-agenda-show ()
  "Display the TaskPaper file which contains the item at point."
  (interactive)
  (let ((win (selected-window)))
    (taskpaper-agenda-goto) (select-window win)))

(defun taskpaper-agenda-show-recenter ()
  "Display to the item at point and recenter."
  (interactive)
  (let ((win (selected-window)))
    (taskpaper-agenda-goto)
    (recenter) (select-window win)))

(defun taskpaper-agenda-do-context-action ()
  "Show follow mode window."
  (let ((m (taskpaper-get-at-bol 'taskpaper-marker)))
    (and (markerp m) (marker-buffer m)
         taskpaper-agenda-follow-mode (taskpaper-agenda-show))))

(defun taskpaper-agenda-follow-mode ()
  "Toggle follow mode in an agenda buffer."
  (interactive)
  (unless taskpaper-agenda-follow-mode
    (setq taskpaper-agenda-pre-follow-window-conf
          (current-window-configuration)))
  (setq taskpaper-agenda-follow-mode
        (not taskpaper-agenda-follow-mode))
  (unless taskpaper-agenda-follow-mode
    (set-window-configuration
     taskpaper-agenda-pre-follow-window-conf))
  (taskpaper-agenda-set-mode-name)
  (taskpaper-agenda-do-context-action)
  (message "Follow mode is %s"
           (if taskpaper-agenda-follow-mode "on" "off")))

(defun taskpaper-agenda-next-line ()
  "Move cursor to the next line.
Display the TaskPaper file which contains the item at point if
follow mode is active."
  (interactive)
  (call-interactively 'next-line)
  (taskpaper-agenda-do-context-action))

(defun taskpaper-agenda-previous-line ()
  "Move cursor to the previous line.
Display the TaskPaper file which contains the item at point if
follow mode is active."
  (interactive)
  (call-interactively 'previous-line)
  (taskpaper-agenda-do-context-action))

(defun taskpaper-agenda-quit ()
  "Quit the agenda and kill the agenda buffer."
  (interactive)
  (let ((buf (current-buffer)))
    ;; Restore window configuration
    (cond
     ((eq taskpaper-agenda-window-setup 'other-frame)
      (delete-frame))
     ((and taskpaper-agenda-pre-window-conf
           taskpaper-agenda-restore-windows-after-quit)
      (set-window-configuration taskpaper-agenda-pre-window-conf)
      (setq taskpaper-agenda-pre-window-conf nil))
     (t
      (and (not (eq taskpaper-agenda-window-setup 'current-window))
           (not (one-window-p))
           (delete-window))))
    ;; Kill the agenda buffer
    (kill-buffer buf)))

(defun taskpaper-agenda-exit ()
  "Exit the agenda, killing TaskPaper buffers loaded by the agenda.
Like `taskpaper-agenda-quit', but kill any buffers that were
created by the agenda. TaskPaper buffers visited directly by the
user will not be touched."
  (interactive)
  (taskpaper-release-buffers taskpaper-agenda-new-buffers)
  (taskpaper-agenda-quit))

(defvar taskpaper-agenda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'taskpaper-agenda-redo)
    (define-key map (kbd "a") 'taskpaper-outline-show-all)
    (define-key map (kbd "c") 'taskpaper-show-in-calendar)
    (define-key map (kbd ">") 'taskpaper-goto-calendar)
    (define-key map (kbd "F") 'taskpaper-agenda-follow-mode)
    (define-key map (kbd "p") 'taskpaper-agenda-previous-line)
    (define-key map (kbd "n") 'taskpaper-agenda-next-line)
    (define-key map (kbd "<up>") 'taskpaper-agenda-previous-line)
    (define-key map (kbd "<down>") 'taskpaper-agenda-next-line)
    (define-key map (kbd "SPC") 'taskpaper-agenda-show)
    (define-key map (kbd "TAB") 'taskpaper-agenda-goto)
    (define-key map (kbd "L") 'taskpaper-agenda-show-recenter)
    (define-key map (kbd "RET") 'taskpaper-agenda-switch-to)
    (define-key map (kbd "I") 'taskpaper-iquery)
    (define-key map (kbd "Q") 'taskpaper-query)
    (define-key map (kbd "S") 'taskpaper-query-fast-select)
    (define-key map (kbd "t") 'taskpaper-search-tag-at-point)
    (define-key map (kbd "/") 'taskpaper-occur)
    (define-key map (kbd "C-c C-c") 'taskpaper-occur-remove-highlights)
    (define-key map (kbd "v") 'taskpaper-outline-copy-visible)
    (define-key map (kbd "o") 'delete-other-windows)
    (define-key map (kbd "q") 'taskpaper-agenda-quit)
    (define-key map (kbd "x") 'taskpaper-agenda-exit)
    map)
  "Keymap for `taskpaper-agenda-mode'.")

(defun taskpaper-agenda-prepare-window (abuf)
  "Setup agenda buffer in the window.
ABUF is the buffer for the agenda window."
  (setq taskpaper-agenda-pre-window-conf
        (current-window-configuration))
  (cond
   ((eq taskpaper-agenda-window-setup 'current-window)
    (pop-to-buffer-same-window abuf))
   ((eq taskpaper-agenda-window-setup 'other-window)
    (switch-to-buffer-other-window abuf))
   ((eq taskpaper-agenda-window-setup 'other-frame)
    (switch-to-buffer-other-frame abuf))
   ((eq taskpaper-agenda-window-setup 'only-window)
    (delete-other-windows)
    (pop-to-buffer-same-window abuf))
   ((eq taskpaper-agenda-window-setup 'reorganize-frame)
    (delete-other-windows)
    (switch-to-buffer-other-window abuf)))
  (unless (equal (current-buffer) abuf)
    (pop-to-buffer-same-window abuf)))

(defun taskpaper-agenda-build (matcher)
  "Build agenda buffer using MATCHER."
  (message "Building agenda...")
  (taskpaper-agenda-prepare-window
   (get-buffer-create taskpaper-agenda-buffer-name))
  (taskpaper-mode)
  (setq major-mode 'taskpaper-agenda-mode)
  (taskpaper-agenda-set-mode-name)
  (use-local-map taskpaper-agenda-mode-map)
  (taskpaper-agenda-insert-items matcher)
  (setq buffer-read-only t)
  (setq taskpaper-agenda-matcher-form matcher)
  (message "Building agenda...done"))

;;;###autoload
(defun taskpaper-agenda-search ()
  "Promt for query string and build agenda."
  (interactive)
  (let ((matcher (taskpaper-query-matcher
                  (taskpaper-query-read-query "Agenda query: "))))
    (taskpaper-agenda-build matcher)))

;;;###autoload
(defun taskpaper-agenda-select ()
  "Promts for query selection and build agenda."
  (interactive)
  (let ((matcher (taskpaper-query-matcher
                  (taskpaper-query-selection))))
    (taskpaper-agenda-build matcher)))

;;;; Provide `taskpaper-mode'

(provide 'taskpaper-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:

;;; taskpaper-mode.el ends here

