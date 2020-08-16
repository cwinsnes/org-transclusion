;;; org-transclusion.el -- Transclusion of files in org mode

;; Package-Requires: ((ov "1.0.6"))

;;; Commentary:
;; Coming later

;;; Code:
(require 'ov)

(defgroup org-transclusion nil
  "Transclusion minor mode for org"
  :group 'org
  :prefix 'org-transclusion-)

(defcustom org-transclusion-regex
  "{{Transclude: \\(.+?\\)\\([[:blank:]]+[[:digit:]]+\\)?\\([[:blank:]]+[[:digit:]]+\\)?}}"
  "String used to identify Transclusion areas.
The regex should include at least 1 group (\\1), naming the file to be
transcluded and 2 optional groups (\\2 and \\2) that inform which lines
to be included from the file."
  :group 'org-transclusion
  :type 'string)

(defvar org-transclusion-mode-map nil "Keymap for org-transclusion-mode.")
(progn
  (setq org-transclusion-mode-map (make-sparse-keymap))
  (define-key org-transclusion-mode-map (kbd "C-M-x") 'org-transclusion-find-transclusions))

(defun org-transclusion-read-file (path &optional start end)
  "Read the file at PATH and return its contents as a string.
Optionally only returns the lines between START and END."
  (with-temp-buffer
    (insert-file-contents path)
    (when end
      (end-of-line end)
      (let ((current-point (point)))
        (kill-region current-point (point-max))))
    (when (and start (> start 0))
      (goto-line 0)
      (kill-whole-line (- start 1)))
    (buffer-string)))

(defun org-transclusion-transclude-file (filename transclusion-point &optional start end)
  "Insert the transclusion of FILENAME at TRANSCLUSION-POINT.
Can optionally force the transclusion to only happen from the
lines START to END.
If only START is supplied, go to the end of the file from START.
This will add an overlay to the transcluded
text to keep the text read-only."
  (save-excursion
    (goto-char transclusion-point)
    (insert "\n") ;; Make sure a new line exists after transclusion regex text
    (let* ((file-string (org-transclusion-read-file filename start end))
           (overlay (ov-insert file-string)))
      (ov-read-only overlay t nil)
      (ov-set overlay 'face 'font-lock-warning-face)
      overlay)))

(defun org-transclusion-find-transclusions ()
  "Read the current buffer to find all instances that match the transclusion regex."
  (interactive)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (re-search-forward org-transclusion-regex nil t)
        (when (match-string 0)
          (let ((transclusion-point (match-end 0))
                (filename (match-substitute-replacement "\\1"))
                (start-point (match-substitute-replacement "\\2"))
                (end-point (match-substitute-replacement "\\3")))

            (if (equal start-point "")
              (setq start-point nil)
              (setq start-point (string-to-number start-point)))
            (if (equal end-point "")
              (setq end-point nil)
              (setq end-point (string-to-number end-point)))
            (org-transclusion-transclude-file filename transclusion-point start-point end-point)
            ))))))

(define-minor-mode org-transclusion-mode
  "Toggle org-transclusion-mode.
When org-transclusion-mode is enabled, add transclusion functionality to org-mode using
the syntax from the \\[org-transclusion-regex] variable."
  :init-value nil
  :lighter "org-tcx"
  :group 'org-transclusion
  :keymap org-transclusion-mode-map)

(provide 'org-transclusion)
;;; org-transclusion.el ends here
