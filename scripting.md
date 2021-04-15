
# Scripting Guide

This document covers some areas where users can automate and extend the functionality of TaskPaper mode using predefined hooks and API functions.

# Contents

- [API Functions](#api-functions)
- [Hooks](#hooks)
- [Scripting Examples](#scripting-examples)

## API Functions

Following API functions can be used to automate and extend TaskPaper mode. For details see the documentation strings of these functions.

### Outline Mapping and Traversing

The mapping routines can call any arbitrary function.

- `taskpaper-outline-map-tree`
- `taskpaper-outline-map-region`
- `taskpaper-outline-map-descendants`
- `taskpaper-outline-up-level-safe`
- `taskpaper-outline-next-item`
- `taskpaper-outline-next-item-safe`
- `taskpaper-outline-previous-item`
- `taskpaper-outline-previous-item-safe`
- `taskpaper-outline-forward-same-level`
- `taskpaper-outline-forward-same-level-safe`
- `taskpaper-outline-backward-same-level`
- `taskpaper-outline-backward-same-level-safe`
- `taskpaper-outline-end-of-item`
- `taskpaper-outline-end-of-subtree`

### Item Path

- `taskpaper-item-has-children-p`
- `taskpaper-item-get-outline-path`

### Sorting and Filtering

General functions for extending sorting and filtering functionality.

- `taskpaper-query-matcher`
- `taskpaper-query-item-match-p`
- `taskpaper-match-sparse-tree`
- `taskpaper-sort-items-generic`
- `taskpaper-item-sorting-key-text`
- `taskpaper-item-sorting-key-type`
- `taskpaper-string-sorting-key-text`
- `taskpaper-string-sorting-key-type`

### Accessing and Setting Attributes

Functions for working with attributes.

- `taskpaper-item-get-attributes`
- `taskpaper-item-get-attribute`
- `taskpaper-item-has-attribute`
- `taskpaper-item-set-attribute`
- `taskpaper-item-remove-attribute`
- `taskpaper-string-get-attributes`
- `taskpaper-string-get-attribute`
- `taskpaper-string-has-attribute`
- `taskpaper-string-set-attribute`
- `taskpaper-string-remove-attribute`
- `taskpaper-attribute-value-to-list`

### Relation Functions for Attribute Values

Functions for checking and comparing attribute values.

- `taskpaper-num=`
- `taskpaper-num<`
- `taskpaper-num<=`
- `taskpaper-num>`
- `taskpaper-num>=`
- `taskpaper-num<>`
- `taskpaper-string=`
- `taskpaper-string<`
- `taskpaper-string<=`
- `taskpaper-string>`
- `taskpaper-string>=`
- `taskpaper-string<>`
- `taskpaper-string-match-p`
- `taskpaper-string-contain-p`
- `taskpaper-string-prefix-p`
- `taskpaper-string-suffix-p`
- `taskpaper-istring=`
- `taskpaper-istring<`
- `taskpaper-istring<=`
- `taskpaper-istring>`
- `taskpaper-istring>=`
- `taskpaper-istring<>`
- `taskpaper-istring-match-p`
- `taskpaper-istring-contain-p`
- `taskpaper-istring-prefix-p`
- `taskpaper-istring-suffix-p`
- `taskpaper-time=`
- `taskpaper-time<`
- `taskpaper-time<=`
- `taskpaper-time>`
- `taskpaper-time>=`
- `taskpaper-time<>`

Following functions interpret attribute values as comma-separated lists.

- `taskpaper-cslist-num=`
- `taskpaper-cslist-num<`
- `taskpaper-cslist-num<=`
- `taskpaper-cslist-num>`
- `taskpaper-cslist-num>=`
- `taskpaper-cslist-num<>`
- `taskpaper-cslist-num-match-p`
- `taskpaper-cslist-num-contain-p`
- `taskpaper-cslist-num-prefix-p`
- `taskpaper-cslist-num-suffix-p`
- `taskpaper-cslist-string=`
- `taskpaper-cslist-string<`
- `taskpaper-cslist-string<=`
- `taskpaper-cslist-string>`
- `taskpaper-cslist-string>=`
- `taskpaper-cslist-string<>`
- `taskpaper-cslist-string-match-p`
- `taskpaper-cslist-string-contain-p`
- `taskpaper-cslist-string-prefix-p`
- `taskpaper-cslist-string-suffix-p`
- `taskpaper-cslist-istring=`
- `taskpaper-cslist-istring<`
- `taskpaper-cslist-istring<=`
- `taskpaper-cslist-istring>`
- `taskpaper-cslist-istring>=`
- `taskpaper-cslist-istring<>`
- `taskpaper-cslist-istring-match-p`
- `taskpaper-cslist-istring-contain-p`
- `taskpaper-cslist-istring-prefix-p`
- `taskpaper-cslist-istring-suffix-p`
- `taskpaper-cslist-time=`
- `taskpaper-cslist-time<`
- `taskpaper-cslist-time<=`
- `taskpaper-cslist-time>`
- `taskpaper-cslist-time>=`
- `taskpaper-cslist-time<>`
- `taskpaper-cslist-time-match-p`
- `taskpaper-cslist-time-contain-p`
- `taskpaper-cslist-time-prefix-p`
- `taskpaper-cslist-time-suffix-p`

### Formatting of Items

- `taskpaper-item-format`

### Date and Time

- `taskpaper-read-date`
- `taskpaper-expand-time-string`

### Buffers and Files

Functions for working with TaskPaper files and buffers.

- `taskpaper-agenda-files`
- `taskpaper-agenda-file-p`
- `taskpaper-save-all-taskpaper-buffers`
- `taskpaper-outline-normalize-indentation`

### Quick Entry

- `taskpaper-add-entry`

## Hooks

Hooks are options containing functions to be run before or after a function.

- `taskpaper-mode-hook`
- `taskpaper-blocker-hook`
- `taskpaper-after-completion-hook`
- `taskpaper-archive-hook`
- `taskpaper-after-sorting-items-hook`
- `taskpaper-agenda-after-show-hook`

## Scripting Examples

Following are some scripting examples, which utilize the API functions mentioned above.

### Mapping and Structure Editing

The following two functions are similar to `taskpaper-outline-promote-subtree` and `taskpaper-outline-demote-subtree` but work on selected region instead of current subtree. They use `taskpaper-outline-map-region` mapping function together with `taskpaper-outline-promote` and `taskpaper-outline-demote` functions to promote/demote all items in a region.

    (defun my-taskpaper-outline-promote-region ()
      "Promote all items in region."
      (interactive)
      (taskpaper-outline-map-region
       'taskpaper-outline-promote
       (region-beginning) (region-end)))

    (defun my-taskpaper-outline-demote-region ()
      "Demote all items in region."
      (interactive)
      (taskpaper-outline-map-region
       'taskpaper-outline-demote
       (region-beginning) (region-end)))

### Mapping and Attribute Setting

The next example uses `taskpaper-outline-map-tree` outline mapping function and `taskpaper-item-remove-attribute` function to remove all occurrences of certain tag from all items in the subtree under cursor. The user will be prompted for tag name and tag value to remove. If no value is provided, tags with any value or no value at all will be considered.

    (defun my-taskpaper-outline-remove-tag ()
      "Remove certain tag from all items in the current subtree."
      (interactive)
      (let ((name (read-string "Tag name: "))
            (value (read-string "Tag value: ")))
        (when (equal value "") (setq value nil))
        (taskpaper-outline-map-tree
         `(lambda ()
            (taskpaper-item-remove-attribute ,name ,value)))))

The function described in the next example marks all items in the subtree under cursor as completed by adding a `@done` tag with the current date to them. Items, which have been completed before, remain untouched.

    (defun my-taskpaper-outline-complete-subtree ()
      "Complete all items in the current subtree."
      (interactive)
      (let ((ts (format-time-string "%Y-%m-%d" (current-time))))
        (taskpaper-outline-map-tree
         `(lambda ()
            (unless (taskpaper-item-has-attribute "done")
              (mapc (lambda (tag) (taskpaper-item-remove-attribute tag))
                    taskpaper-tags-to-remove-when-done)
              (taskpaper-item-set-attribute "done" ,ts))))))

The following more complex example utilizes `taskpaper-outline-map-region` outline mapping function and `taskpaper-archive-subtree` function to archive all completed items in the current buffer:

    (defun my-taskpaper-archive-done ()
      "Archive all completed items."
      (interactive)
      (taskpaper-outline-map-region
       '(lambda nil
          (when (taskpaper-item-has-attribute "done")
            (taskpaper-archive-subtree)
            (taskpaper-outline-previous-item-safe)))
       (point-min) (point-max)))

The function `taskpaper-outline-previous-item-safe` prevents the next entry to be skipped when the current subtree is removed (archived). You can extend the function definition to consider only items, which have been completed before certain date. The function below will archive all items, which have been completed before last 14 days:

    (defun my-taskpaper-archive-done ()
      "Archive all completed items."
      (interactive)
      (taskpaper-outline-map-region
       '(lambda nil
          (when (taskpaper-time<=
                 (taskpaper-item-get-attribute "done") "-14d")
            (taskpaper-archive-subtree)
            (taskpaper-outline-previous-item-safe)))
       (point-min) (point-max)))

### To-Do Dependencies

Usually, a parent action should not be marked as done until all sub-tasks are marked as done. Sometimes when pressing `C-c C-d` you may inadvertently complete items still containing open sub-tasks. Configuring the hook `taskpaper-blocker-hook` helps preventing this. The value of this hook may be nil, a function, or a list of functions. Functions in this hook should not modify the buffer. Each function gets as its single argument a buffer position at the beginning of item. If any of the functions in this hook returns nil, the completion is blocked.

The following code adds `my-taskpaper-blocker-func-1` function to the hook. The function will check the current item and return non-nil if all its actionable children, i.e. projects and tasks, are completed.

    (defun my-taskpaper-blocker-func-1 (pos)
      "Return non-nil if item at point can be marked as completed."
      (goto-char pos)
      (catch 'exit
        (taskpaper-outline-map-descendants
         '(lambda ()
            (when (taskpaper-query-item-match-p "not note and not @done")
              (throw 'exit nil))))
        t))

    (add-hook 'taskpaper-blocker-hook 'my-taskpaper-blocker-func-1)

Sometimes actions need to be completed in a predetermined order: The first task must be finished before you can move on to the next. In this case you can instruct another blocker function to check if all previous actionable siblings are completed:

    (defun my-taskpaper-blocker-func-2 (pos)
      "Return non-nil if item at point can be marked as completed."
      (goto-char pos)
      (catch 'exit
        (while (taskpaper-outline-backward-same-level-safe)
          (when (taskpaper-query-item-match-p "not note and not @done")
            (throw 'exit nil)))
        t))

    (add-hook 'taskpaper-blocker-hook 'my-taskpaper-blocker-func-2)

Attributes of the current item can be checked as well. The following blocker function will ensure that neither the item itself nor any of its ancestors is tagged as `@waiting` or `@blocked`:

    (defun my-taskpaper-blocker-func-3 (pos)
      "Return non-nil if item at point can be marked as completed."
      (goto-char pos)
      (taskpaper-query-item-match-p "not @waiting and not @blocked"))

    (add-hook 'taskpaper-blocker-hook 'my-taskpaper-blocker-func-3)

### Sorting

In addition to the existing sorting functions `taskpaper-sort-by-text` and `taskpaper-sort-by-type` you can define your own using the generic sorting function `taskpaper-sort-items-generic`. For details see the documentation string of this function. For example, the function below sorts items according to the value of the `@priority` tag. The sorting is done numerically, in ascending order. Items, which have no or empty `@priority` tag, are assumed to have 99 as priority value, effectively ending up at the bottom of the sorted list.

    (defun my-taskpaper-sort-by-priority ()
      "Sort items on a certain level by priority."
      (interactive)
      (taskpaper-sort-items-generic
       '(lambda nil
          (or (taskpaper-item-get-attribute "priority") "99"))
       'taskpaper-num<))

    (define-key taskpaper-mode-map (kbd "C-c C-s p")
                'my-taskpaper-sort-by-priority)

The next function sorts items according to their due dates. The sorting is done by date/time value (converted to float number of seconds since the beginning of the epoch). Items, which have no or empty `@due` tag, are assumed to have 2100-12-12 as due date, effectively ending up at the bottom of the sorted list.

    (defun my-taskpaper-sort-by-due-date ()
      "Sort items on a certain level by due date."
      (interactive)
      (taskpaper-sort-items-generic
       '(lambda nil
          (or (taskpaper-item-get-attribute "due") "2100-12-12"))
       'taskpaper-time<))

    (define-key taskpaper-mode-map (kbd "C-c C-s d")
                'my-taskpaper-sort-by-due-date)

As further examples see the `taskpaper-sort-by-text` and `taskpaper-sort-by-type` function definitions.

### Repeating Actions

You can specify the recurrence of repeating actions using a dedicated repeater tag. In the following example the time value of the `repeat` tag is the repeater specification:

    - Pay electricity bill @repeat(+1m) @due(2018-02-01)

The intended interpretation is that the task is due on 2018-02-01 and repeats itself every (one) month starting from that time. Meaningful repeater values are duration offsets and combination thereof, which shift time into the future, i.e., `+5d`, `+2w`, `+2 Tue`, etc.

The following function checks if the item at point has a repeater attribute `@repeat`, and if yes, shifts the time value of the `@due` attribute to the next possible future time as specified by the repeater value. If no due date is set or if the function is called with the `C-u` prefix argument, the time shift is calculated from the current time instead.

    (defun my-taskpaper-item-repeat-maybe (&optional from-now)
      "Re-schedule recurring item."
      (interactive "P")
      (let ((ts  (taskpaper-item-get-attribute "due"))
            (rep (taskpaper-item-get-attribute "repeat"))
            time-str ctime)
        (when rep
          (setq time-str (if (and ts (not from-now)) ts "now"))
          (while (taskpaper-time<= time-str "now")
            (setq ctime time-str
                  time-str (taskpaper-expand-time-string
                            (concat time-str " " rep)))
            (when (taskpaper-time<= time-str ctime)
              (error "Invalid repeater specification: %s" rep)))
          (taskpaper-item-remove-attribute "done")
          (taskpaper-item-set-attribute "due" time-str))))

### Summary Reports

Using mapping and querying functions you can generate your own custom summary reports and statistics. The following simple example creates a short summary for the current subtree and displays it in the echo area:

    (defun my-taskpaper-summary ()
      "Display summary report for the current subtree."
      (interactive)
      (let ((cnt-task-open 0) (cnt-task-hold 0))
        (taskpaper-outline-map-tree
         '(lambda ()
            (when (taskpaper-query-item-match-p "task and not @done")
              (setq cnt-task-open (1+ cnt-task-open)))
            (when (taskpaper-query-item-match-p "task and not @done and @hold")
              (setq cnt-task-hold (1+ cnt-task-hold)))))
        (message "Open tasks: %d; tasks on hold: %d." cnt-task-open cnt-task-hold)))

### Quick Entry

The API function `taskpaper-add-entry` can be used in Lisp programs to add entries inside Emacs or in shell scripts to add entries from the command line. On UNIX systems the following small shell script will add entries to the top-level project "Inbox" located in the file `~/gtd.taskpaper`. If the project doesn't exist, it will be created at the end of given file as top-level item. The entry text should be given as single argument (quoted or not) to the shell script. A tag `@added` with the current date will be appended to the entry.

    #!/bin/bash

    emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
    file="$HOME/gtd.taskpaper"
    location='Inbox:'
    time=$(date '+%Y-%m-%d')
    text="$@ @added($time)"

    $emacs \
        --batch --load "$HOME/.emacs" \
        --eval "(taskpaper-add-entry \"$text\" \"$location\" \"$file\")" \
        --funcall save-buffer --kill > /dev/null 2>&1 \
        && echo 'Entry added.'

Save the script as i.e., `tp-add`, make it executable by typing in your terminal

    chmod +x tp-add

and place it somewhere in the path where executable programs are located. Now you can quickly add an entry issuing

    tp-add - New task @today

in the command line.

### Synchronization

Activating the auto-revert minor mode can be useful if you want to edit your TaskPaper files from another location and sync them e.g. via [Dropbox][dropbox] or add entries using quick entry script as described above, so whenever a file changes and is re-synced, the corresponding buffer is updated. You can automate the activation of this mode via `taskpaper-mode-hook`:

    (add-hook 'taskpaper-mode-hook
              (lambda () (auto-revert-mode 1)))

[dropbox]: https://www.dropbox.com/

