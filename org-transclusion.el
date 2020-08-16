;;; org-transclusion.el -- Transclusion of files in org mode

;; Package-Requires: ((ov "1.0.6"))

;;; Commentary:
;; Coming later

;;; Code:
(require 'ov)

(defvar-local org-transclusion-overlay-points nil)

(defgroup org-transclusion nil
  "Transclusion minor mode for org"
  :group 'org
  :prefix 'org-transclusion-)

(defcustom org-transclusion-regex
  "{{Transclude: \\(.+?\\)\\([[:blank:]]+[[:digit:]]+\\)?\\([[:blank:]]+[[:digit:]]+\\)?}}"
  "String used to identify Transclusion areas.
The regex should include at least 1 group (\\1), naming the file to be
transcluded and 2 optional groups (\\2 and \\3) that inform which lines
to be included from the file."
  :group 'org-transclusion
  :type 'string)

(defvar org-transclusion-mode-map nil "Keymap for org-transclusion-mode.")
(progn
  (setq org-transclusion-mode-map (make-sparse-keymap))
  (define-key org-transclusion-mode-map (kbd "C-M-x") 'org-transclusion-find-transclusions)
  (define-key org-transclusion-mode-map (kbd "C-c e") 'org-transclusion-toggle-transclusion)
  (define-key org-transclusion-mode-map (kbd "C-M-z") 'org-transclusion-clear-all-transclusions))

(defun org-transclusion-read-file (path &optional start end)
  "Read the file at PATH and return its contents as a string.
Optionally only returns the lines between START and END."
  (with-temp-buffer
    (insert-file-contents path)
    (when end
      (end-of-line end)
      (kill-region (point) (point-max)))
    (when (and start (> start 0))
      (goto-line 0)
      (kill-whole-line (- start 1)))
    (buffer-string)))

(defun org-transclusion-clear-transclusion (overlay)
  "Remove a single transclusion using its OVERLAY."
  (goto-char (ov-beg overlay))
  (kill-region (ov-beg overlay) (ov-end overlay))
  (kill-whole-line) ;; Remove the \n inserted before
  (ov-reset overlay))

(defun org-transclusion-clear-all-transclusions ()
  "Remove all transclusions and transclusion overlays from current buffer."
  (interactive)
  (let ((buf-modified (buffer-modified-p)))
    (save-excursion
      (let ((transclusion-overlays (ov-in 'org-transclusion)))
        (mapc (lambda (overlay)
                (org-transclusion-clear-transclusion overlay))
              transclusion-overlays))
      (set-buffer-modified-p buf-modified))))

(defun org-transclusion-transclude-file (filename transclusion-point &optional start end)
  "Insert the transclusion of FILENAME at TRANSCLUSION-POINT.
Can optionally force the transclusion to only happen from the
lines START to END.
If only START is supplied, go to the end of the file from START.
This will add an overlay to the transcluded
text to keep the text read-only."
  (save-excursion
    (let* ((file-string (org-transclusion-read-file filename start end))
           (overlay (ov-insert file-string)))
      (ov-set overlay 'evaporate t)
      (unless (equal file-string "")
        (goto-char transclusion-point)
        (insert "\n") ;; Make sure a new line exists after transclusion regex text
        (ov-read-only overlay t nil)
        (ov-set overlay 'face 'font-lock-warning-face)
        (ov-set overlay 'org-transclusion t)
        overlay))))


(defun org-transclusion-toggle-transclusion ()
  "Toggle a single Transclusion."
  (interactive)
  (save-excursion
    (let ((overlay (or (ov-at (point)) (ov-at (+ (line-end-position) 1)))))
      (if overlay
          (progn
            ;; Remove one from ov-beg due to \n inserted by transclude
            (setq org-transclusion-overlay-points
                  (delete (- (ov-beg overlay) 1) org-transclusion-overlay-points))
            (org-transclusion-clear-transclusion overlay))
        (progn
          (beginning-of-line)
          (let ((begin-bound (line-end-position)))
            (when (re-search-forward org-transclusion-regex begin-bound t)
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
                (push transclusion-point org-transclusion-overlay-points)
                (org-transclusion-transclude-file filename transclusion-point
                                                  start-point end-point)))))))))

(defun org-transclusion-find-transclusions ()
  "Read the current buffer to find and transclude instances that match the transclusion regex."
  (interactive)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((buf-modified (buffer-modified-p)))
        (while (re-search-forward org-transclusion-regex nil t)
          (org-transclusion-toggle-transclusion))))))

(define-minor-mode org-transclusion-mode
  "Toggle org-transclusion-mode.
When org-transclusion-mode is enabled, add transclusion functionality to org-mode using
the syntax from the \\[org-transclusion-regex] variable."
  :init-value nil
  :lighter "org-tcx"
  :group org-transclusion
  :keymap org-transclusion-mode-map)

(defun org-transclusion--restore-transclusions ()
  "Restore all transclusions in the overlay-points after save."
  (mapc (lambda (p)
          (progn
            (goto-char p)
            (org-transclusion-toggle-transclusion)))
        org-transclusion-overlay-points))

(add-hook 'org-transclusion-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'org-transclusion-clear-all-transclusions)))

(add-hook 'org-transclusion-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'org-transclusion--restore-transclusions)))

(provide 'org-transclusion)
;;; org-transclusion.el ends here
