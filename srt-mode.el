;; Emacs mode for editing SRT files
;; Copyright (C) 2011-2014  disbeliever
;; URL: https://github.com/disbeliever/srt-mode

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-derived-mode srt-mode fundamental-mode
  (setq mode-name "SRT")
  (setq font-lock-defaults '(srt-font-lock-keywords nil t nil nil))
  )


(setq srt-hash-event-format (make-hash-table :test 'equal))

(defvar srt-media-player "mplayer2")
(defvar srt-media-player-parameters "")

(defvar srt-font-lock-keywords
  (list
   '("\\d+\:\\d+\:\\d+,\\d+" 'font-lock-type-face)
   ;; '("\\;.*" . 'font-lock-comment-face)
   ;; '("^Comment" . 'font-lock-comment-face)
   ;; '("^[ \t]*\\[\\(.+\\)\\]" . 'font-lock-type-face)
   ;; '("^\\(\\w+\\)\:" . 'font-lock-keyword-face)
   ))

(defun srt-get-frame-rate (file-name)
  "Get frame rate of video file. mediainfo is needed"
  (let
      (
       (output (process-lines "mediainfo" file-name))
       )
    (save-match-data
      (let (
            (matched (nth 0 (remove-if (lambda (x) (not (string-match "^Frame rate  .+\:" x))) output)))
            )
        (string-match "^Frame rate  .+\: \\([0-9]+\.[0-9]+\\) fps" matched)
        (string-to-number (match-string 1 matched))
        )
      )
    )
  )

(defun srt-timestamp-to-seconds (timestamp)
  (let (
        (minutes (string-to-number (nth 1 (split-string (car (split-string timestamp "\\.")) ":" 2))))
        (seconds (string-to-number (nth 2 (split-string (car (split-string timestamp "\\.")) ":" 2))))
        (mseconds (string-to-number (concat "0." (nth 1 (split-string timestamp "\\," 1)))))
        )
    (+
     (* minutes 60)
     seconds
     mseconds
     )
    )
  )

(defun srt-seconds-to-timestamp (sec)
  (let* (
        (hours (floor (/ sec 3600)))
        (minutes (floor (/ (- sec (* hours 3600)) 60)))
        (seconds (floor (- sec (* hours 3600) (* minutes 60))))
        (mseconds (string-to-number (nth 1 (split-string (number-to-string sec) "\\."))) )
        )
    (format "%d:%02d:%02d,%d" hours minutes seconds mseconds)
    )
  )

(defun srt-shift-timestamp (timestamp shift-amount)
  (let (
        (shifted-seconds (+ (srt-timestamp-to-seconds timestamp) shift-amount))
        )
    (srt-seconds-to-timestamp shifted-seconds)
    )
  )

(srt-timestamp-to-seconds "00:00:0,500")
(srt-seconds-to-timestamp 0.500)
(srt-shift-timestamp "00:00:2,00" 2)

(defun srt-change-frame-rate (timestamp fps-old fps-new)
  ""
  (let* (
         (factor (/ fps-old fps-new))
         (shifted-seconds (* (srt-timestamp-to-seconds timestamp) factor))
         )
    (srt-seconds-to-timestamp shifted-seconds)
    )
)

(defun srt-get-buffer-file-name (ext)
  ""
  (concat (file-name-sans-extension (buffer-file-name))
          ext
          ))

(defun srt-get-video-name ()
  "Construct the name of the video file"
  (cond
   ((file-readable-p (srt-get-buffer-file-name ".mp4")) (srt-get-buffer-file-name ".mp4"))
   ((file-readable-p (srt-get-buffer-file-name ".mkv")) (srt-get-buffer-file-name ".mkv"))
   ((file-readable-p (srt-get-buffer-file-name ".avi")) (srt-get-buffer-file-name ".avi"))
   )
  )

(defun srt-get-current-block ()
  (save-excursion
    (backward-sentence)
    (let* (
           (point-start (point))
           (point-end (search-forward-regexp "

" nil t))
           )
      (buffer-substring-no-properties point-start point-end)
      )
    )
  )

(defun srt-get-current-number ()
  (string-to-number (nth 0 (split-string (srt-get-current-block) "
")))
  )

(defun srt-get-current-start-time ()
  "Get start time of the event under point"
  (nth 0 (split-string (nth 1 (split-string (srt-get-current-block) "
")) " --> "))
  )

(defun srt-get-current-end-time ()
  "Get end time of the event under point"
  (nth 1 (split-string (nth 1 (split-string (srt-get-current-block) "
")) " --> "))
  )

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun mplayer ()
  "Run mplayer"
  (interactive)
  (apply 'start-process srt-media-player nil srt-media-player "-ss" (replace-regexp-in-string "," "." (srt-get-current-start-time)) (srt-get-video-name) (split-string srt-media-player-parameters " "))
  )

(defun srt-shift-time (shift-amount)
  (interactive "nEnter shift amount in seconds: ")
  (save-excursion
    (let*
        (
         (start-time (srt-get-current-start-time))
         (end-time (srt-get-current-end-time))
         (shifted-start-time (srt-shift-timestamp start-time shift-amount))
         (shifted-end-time (srt-shift-timestamp end-time shift-amount))
         )
      (beginning-of-line)
      (search-forward start-time)
      (replace-match shifted-start-time)

      (search-forward end-time)
      (replace-match shifted-end-time)
      )
    )
  )

(defun srt-change-fps (fps-old fps-new)
  (interactive
   (list
    (read-number "Old FPS: " (srt-get-frame-rate (srt-get-video-name)))
    (read-number "New FPS: ")
    ))
  (save-excursion
    (let*
      (
       (start-time (srt-get-current-start-time))
       (end-time (srt-get-current-end-time))
       (shifted-start-time (srt-change-frame-rate start-time fps-old fps-new))
       (shifted-end-time (srt-change-frame-rate end-time fps-old fps-new))
       )
      (beginning-of-line)
      (search-forward start-time)
      (replace-match shifted-start-time)

      (search-forward end-time)
      (replace-match shifted-end-time)      
      )
    )
  )

(defun srt-new-entry ()
  (interactive)
  (let* (
         (start-time (srt-get-current-start-time))
         (end-time (srt-get-current-end-time))
         )

    (forward-sentence)
    (newline)
    (newline)
    (princ (1+ (srt-get-current-number)) (current-buffer))
    (newline)
    (princ (format "%s --> %s" end-time (srt-shift-timestamp end-time 2)) (current-buffer))
    (newline)
    
    )
  )

(add-to-list 'auto-mode-alist '("\\.srt$" . srt-mode))

(defvar srt-mode-map (make-keymap))
(define-key srt-mode-map "\C-c\C-o" 'mplayer)
(define-key srt-mode-map "\C-c\C-s" 'srt-shift-time)
;(define-key srt-mode-map "\C-c\C-f" 'srt-change-fps)
(define-key srt-mode-map "\C-c\C-n" 'srt-new-entry)
(use-local-map srt-mode-map)

(provide 'srt-mode)
