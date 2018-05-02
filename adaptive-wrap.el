;;; adaptive-wrap.el --- Smart line-wrapping with wrap-prefix

;; Copyright (C) 2011-2013, 2017  Free Software Foundation, Inc.

;; Author: Stephen Berman <stephen.berman@gmx.net>
;;         Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 0.5.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the `adaptive-wrap-prefix-mode' minor mode which sets
;; the wrap-prefix property on the fly so that single-long-line paragraphs get
;; word-wrapped in a way similar to what you'd get with M-q using
;; adaptive-fill-mode, but without actually changing the buffer's text.

;;; Code:

(require 'easymenu)

(defcustom adaptive-wrap-extra-indent 0
  "Number of extra spaces to indent in `adaptive-wrap-prefix-mode'.

`adaptive-wrap-prefix-mode' indents the visual lines to
the level of the actual line plus `adaptive-wrap-extra-indent'.
A negative value will do a relative de-indent.

Examples:

actual indent = 2
extra indent = -1

  Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
 eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
 enim ad minim veniam, quis nostrud exercitation ullamco laboris
 nisi ut aliquip ex ea commodo consequat.

actual indent = 2
extra indent = 2

  Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
    eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
    enim ad minim veniam, quis nostrud exercitation ullamco laboris
    nisi ut aliquip ex ea commodo consequat."
  :type 'integer
  :safe 'integerp
  :group 'visual-line)
(make-variable-buffer-local 'adaptive-wrap-extra-indent)

(defun adaptive-wrap-fill-context-prefix-mod (begin end &optional first-line-regexp)
  "Compute a fill prefix from the text between BEGIN and END.
Essentially a slightly modified version of `fill-context-prefix'
which does not reject a prefix based on a one-line paragraph if
that prefix would act as a paragraph-separator."
  (or first-line-regexp
      (setq first-line-regexp adaptive-fill-first-line-regexp))
  (save-excursion
    (goto-char begin)
    (if (eolp) (forward-line 1))
    (move-to-left-margin)
    (let (first-line-prefix
          second-line-prefix)
      (setq first-line-prefix
            (fill-match-adaptive-prefix))
      (forward-line 1)
      (if (< (point) end)
          (progn
            (move-to-left-margin)
            (setq second-line-prefix
                  (cond ((looking-at paragraph-start) nil)
                        (t (fill-match-adaptive-prefix))))
            (when second-line-prefix
              (unless first-line-prefix (setq first-line-prefix ""))
              (let ((tmp second-line-prefix)
                    (re "\\`"))
                (while (string-match "\\`[ \t]*\\([^ \t]+\\)" tmp)
                  (setq re (concat re ".*" (regexp-quote (match-string 1 tmp))))
                  (setq tmp (substring tmp (match-end 0))))
                (if (string-match re first-line-prefix)
                    second-line-prefix
                  (fill-common-string-prefix first-line-prefix
                                             second-line-prefix)))))
        (if first-line-prefix
            (let ((result
                   (if (or (and first-line-regexp
                                (string-match first-line-regexp
                                              first-line-prefix))
                           (and comment-start-skip
                                (string-match comment-start-skip
                                              first-line-prefix)))
                       first-line-prefix
                     (make-string (string-width first-line-prefix) ?\ ))))
              result))))))

(defun adaptive-wrap-fill-context-prefix (begin end)
  "Compute a fill prefix from the text between BEGIN and END.
Like `fill-context-prefix-mod', but with length adjusted by
`adaptive-wrap-extra-indent'."
  (let* ((fcp (or (adaptive-wrap-fill-context-prefix-mod begin end) ""))
         (fcp-len (string-width fcp)))
    (if (or (<= 0 adaptive-wrap-extra-indent)
            (<  0 (+ adaptive-wrap-extra-indent fcp-len)))
        (make-string (+ fcp-len adaptive-wrap-extra-indent) ?\s)
      "")))

(defun adaptive-wrap-prefix-function (begin end)
  "Indent the region between BEG and END with adaptive filling."
  (goto-char begin)
  (while (< (point) end)
    (let ((lbp (line-beginning-position)))
      (put-text-property (point)
                         (progn (search-forward "\n" end 'move) (point))
                         'wrap-prefix
                         (adaptive-wrap-fill-context-prefix lbp (point))))))

;;;###autoload
(define-minor-mode adaptive-wrap-prefix-mode
  "Wrap the buffer text with adaptive filling."
  :lighter ""
  :group 'visual-line
  (if adaptive-wrap-prefix-mode
      (progn
        ;; HACK ATTACK!  We need to run after font-lock, but jit-lock-register
        ;; doesn't accept an `append' argument, so we add ourselves beforehand,
        ;; to make sure we're at the end of the hook (bug#15155).
        (add-hook 'jit-lock-functions
                  #'adaptive-wrap-prefix-function 'append t)
        (jit-lock-register #'adaptive-wrap-prefix-function))
    (jit-lock-unregister #'adaptive-wrap-prefix-function)
    (with-silent-modifications
      (save-restriction
        (widen)
        (remove-text-properties (point-min) (point-max) '(wrap-prefix nil))))))

(define-key-after (lookup-key menu-bar-options-menu [line-wrapping])
  [adaptive-wrap]
  '(menu-item "Adaptive Wrap" adaptive-wrap-prefix-mode
	      :visible (menu-bar-menu-frame-live-and-visible-p)
	      :help "Show wrapped long lines with an adjustable prefix"
	      :button (:toggle . (bound-and-true-p adaptive-wrap-prefix-mode)))
  word-wrap)

(provide 'adaptive-wrap)

;;; adaptive-wrap.el ends here

