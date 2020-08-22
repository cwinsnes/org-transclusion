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

(defcustom org-transclusion-track-updates t
  "If non-nil, add save hooks to the transcluded files buffer to keep the transclusion updated with edits."
  :group 'org-transclusion
  :type 'boolean)

(defvar org-transclusion-mode-map nil "Keymap for 'org-transclusion-mode.")
(progn
  (setq org-transclusion-mode-map (make-sparse-keymap))
  (define-key org-transclusion-mode-map (kbd "C-M-x") 'org-transclusion-open-all-transclusions)
  (define-key org-transclusion-mode-map (kbd "C-c e") 'org-transclusion-toggle-transclusion)
  (define-key org-transclusion-mode-map (kbd "C-M-z") 'org-transclusion-close-all-transclusions))

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


(defun org-transclusion-buffer-update ()
  "Update transclusions based on another buffer being saved."
  (message "adsdsa")
  (message (buffer-file-name)))


(defun org-transclusion-add-save-hook (overlay)
  "Add an after-save hook to the buffer described in OVERLAY.
The save hook will call 'org-transclusion-buffer-update upon save in the described buffer"
  (let* ((file-path (ov-val overlay 'org-transclusion-file-path))
	 (file-buffer (get-file-buffer file-path)))
    (when file-buffer
      (with-current-buffer file-buffer
	(add-hook 'after-save-hook 'org-transclusion-buffer-update nil t)))))


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
	(ov-set overlay 'org-transclusion-file-path filename)
	(ov-set overlay 'org-transclusion-line-start start)
	(ov-set overlay 'org-transclusion-line-end end)
	(when org-transclusion-track-updates
	  (org-transclusion-add-save-hook overlay))
        overlay))))


(defun org-transclusion-toggle-transclusion (&optional force)
  "Toggle a single Transclusion.
If FORCE equals 'open, force the transclusion open.
If FORCE equals 'close, force the transclusion closed.
Else toggle it"
  (interactive)
  (save-excursion
    (let ((overlay (or (ov-at (point)) (ov-at (+ (line-end-position) 1))))
	  (buf-modified (buffer-modified-p)))
      (if (and overlay (not (equal 'open force)))
	  (progn
	    (org-transclusion-clear-transclusion overlay))
	(if (and (not overlay) (not (equal 'close force)))
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
		    (org-transclusion-transclude-file filename transclusion-point
						      start-point end-point)))))))
      (set-buffer-modified-p buf-modified))))

(defun org-transclusion-close-all-transclusions ()
  "Closes all transclusions."
  (interactive)
  (org-transclusion-open-all-transclusions 'close))

(defun org-transclusion-open-all-transclusions (&optional force)
  "Read the current buffer to find and open transclusions.
If FORCE is nil, open all transclusions
If FORCE is non-nil, instead close all transclusions."
  (interactive)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (re-search-forward org-transclusion-regex nil t)
	(if force
	    (org-transclusion-toggle-transclusion 'close)
	  (org-transclusion-toggle-transclusion 'open))))))

(define-minor-mode org-transclusion-mode
  "Toggle org-transclusion-mode.
When org-transclusion-mode is enabled, add transclusion functionality to org-mode using
the syntax from the \\[org-transclusion-regex] variable."
  :init-value nil
  :lighter "org-tcx"
  :group org-transclusion
  :keymap org-transclusion-mode-map)

(defun org-transclusion--clear-all-transclusions ()
  "Remove all transclusions and transclusion overlays from current buffer.
Will first set overlay points to nil and then fill it with points for restore."
  (setq org-transclusion-overlay-points nil)
  (let ((buf-modified (buffer-modified-p)))
    (save-excursion
      (let ((transclusion-overlays (ov-in 'org-transclusion)))
	(mapc (lambda (overlay)
		(progn
		  ;; Use ov-beg - 1 because of the inserted \n
		  (push (- (ov-beg overlay) 1) org-transclusion-overlay-points)
		  (org-transclusion-clear-transclusion overlay)))
	      transclusion-overlays))
      (set-buffer-modified-p buf-modified))))

(defun org-transclusion--restore-transclusions ()
  "Restore all transclusions in the overlay-points after save.
Will then clear overlay-points."
  (save-excursion
    (mapc (lambda (p)
	    (progn
	      (goto-char p)
	      (org-transclusion-toggle-transclusion)))
	  org-transclusion-overlay-points)
    (setq org-transclusion-overlay-points nil)))

(defun org-transclusion--toggle-restoration-hooks ()
  "Activate and deactivate restorations when entering and exiting org-transclusion mode."
  ;; Make the restoration hooks buffer local.
  (if org-transclusion-mode
      (progn
	(add-hook 'before-save-hook 'org-transclusion--clear-all-transclusions nil t)
	(add-hook 'after-save-hook 'org-transclusion--restore-transclusions nil t))
    (progn
      (remove-hook 'before-save-hook 'org-transclusion--clear-all-transclusions t)
      (remove-hook 'after-save-hook 'org-transclusion--restore-transclusions t))))

(add-hook 'org-transclusion-mode-hook 'org-transclusion--toggle-restoration-hooks)

(provide 'org-transclusion)
;;; org-transclusion.el ends here
