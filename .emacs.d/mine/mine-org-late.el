
;; Courtsey : https://yqrashawn.com/2018/09/17/record-org-mode-recent-activity/

(defun +org/find-state (&optional end)
  "Used to search through the logbook of subtrees.

    Looking for - State.*:[2018-09-14 Fri 10:50] kind of time stamp in logbook."
  (let* ((closed (re-search-forward "^CLOSED: \\[" end t))
         (created (if (not closed) (re-search-forward "^:CREATED: \\[" end t)))
         (logbook (if (not closed) (re-search-forward "- State .*\\[" end t)))
         (result (or closed logbook created)))
    result))

(defun +org/date-diff (start end &optional compare)
  "Calculate difference between  selected timestamp to current date.

  The difference between the dates is calculated in days.
  START and END define the region within which the timestamp is found.
  Optional argument COMPARE allows for comparison to a specific date rather than to current date."
  (let* ((start-date (if compare compare (calendar-current-date))))
    (- (calendar-absolute-from-gregorian start-date) (org-time-string-to-absolute (buffer-substring-no-properties start end)))))

(defun +org/last-update-before (since)
  "List Agenda items that updated before SINCE day."
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (subtree-valid (save-excursion
                            (forward-line 1)
                            (if (and (< (point) subtree-end)
                                     ;; Find the timestamp to test
                                     (+org/find-state subtree-end))
                                (let ((startpoint (point)))
                                  (forward-word 3)
                                  ;; Convert timestamp into days difference from today
                                  (+org/date-diff startpoint (point)))
                              (point-max)))))
      (if (and subtree-valid (<= subtree-valid since))
          next-headline
        nil))))

(defun +org/has-child-p ()
  (save-excursion (org-goto-first-child)))

(defun +org/has-child-and-last-update-before (day)
  (if (+org/has-child-p) (point)
    (+org/last-update-before day)))
(provide 'mine-org-late)
