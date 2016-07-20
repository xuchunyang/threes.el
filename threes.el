;;; threes.el --- Emacs clone of Threes! (game)  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>
;; Package-Requires: ((emacs "24"))
;; Keywords: game
;; URL: https://github.com/xuchunyang/threes.el

;; This file is not part of GNU Emacs

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

;;

;;; Code:

(require 'cl-lib)

(defconst threes-buffer-name "*Threes*" "Name used for Threes buffer.")

(defgroup threes nil
  "A little puzzle game."
  :group 'games
  :prefix "threes-")

(defface threes-face-0
  '((t . (:background "#bcd7d8")))
  "Face for the empty title."
  :group 'threes)

(defface threes-face-1
  '((t . (:background "#79ccfc" :foreground "white")))
  "Face for the tile 1."
  :group 'threes)

(defface threes-face-2
  '((t . (:background "#ef7986" :foreground "white")))
  "Face for tile 2."
  :group 'threes)

(defface threes-face-3
  '((t . (:background "white" :foreground "black")))
  "Face for tile 3."
  :group 'threes)

(defface threes-face-max
  '((t . (:background "white" :foreground "red")))
  "Face for maximum tile."
  :group 'threes)

(defvar threes-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map [left]  #'threes-left)
    (define-key map [up]    #'threes-up)
    (define-key map [right] #'threes-right)
    (define-key map [down]  #'threes-down)
    map))

(define-derived-mode threes-mode special-mode "threes-mode"
  "A mode for play Threes."
  (buffer-disable-undo))

(defun threes-string-center (len s)
  (let* ((s-len (length s))
         (leading (/ (- len s-len) 2))
         (tailing (- (- len s-len) leading)))
    (concat (make-string leading ?\s)
            s
            (make-string tailing ?\s))))

(defvar threes-cells '((1 2 0 0)
                       (2 0 0 3)
                       (3 3 1 1)
                       (0 3 0 6)))

(defvar threes-cells-last nil)

(defun threes-cells-max ()
  (apply #'max (apply #'append threes-cells)))

(defun threes-add-p (n1 n2)
  (or (and (= 1 n1) (= 2 n2))
      (and (= 1 n2) (= 2 n1))
      (and (> n1 2) (= n1 n2))))

(defun threes-move (nums &optional right-or-down)
  (let ((result nums))
    (when right-or-down
      (setq nums (nreverse nums)))
    (if (zerop (car nums))
        (setq result (append (cdr nums) '(0)))
      (let ((i 0)
            has-added)
        (while (and (not has-added)
                    (< i 3))
          (let ((a (nth i nums))
                (b (nth (+ 1 i) nums)))
            (when (threes-add-p a b)
              (setq result
                    (append
                     (cl-subseq nums 0 i)
                     (list (+ a b))
                     (cl-subseq nums (+ i 2) 4)
                     '(0))
                    has-added t)))
          (setq i (+ i 1)))))
    (when right-or-down
      (setq result (nreverse result)))
    result))

(defun threes-left ()
  "Shift the board left."
  (interactive)
  (let (new-cells)
    (dotimes (i 4)
      (push (threes-move (nth i threes-cells)) new-cells))
    (setq threes-cells-last threes-cells
          threes-cells (nreverse new-cells))
    (threes-print-board)))

(defun threes-undo ()
  "Undo last move."
  (interactive)
  (setq threes-cells threes-cells-last)
  (threes-print-board))

(defun threes-print-board ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert
     "
+-----+-----+-----+-----+
|     |     |     |     |
|xxxxx|xxxxx|xxxxx|xxxxx|
|     |     |     |     |
+-----+-----+-----+-----+
|     |     |     |     |
|xxxxx|xxxxx|xxxxx|xxxxx|
|     |     |     |     |
+-----+-----+-----+-----+
|     |     |     |     |
|xxxxx|xxxxx|xxxxx|xxxxx|
|     |     |     |     |
+-----+-----+-----+-----+
|     |     |     |     |
|xxxxx|xxxxx|xxxxx|xxxxx|
|     |     |     |     |
+-----+-----+-----+-----+
")
    (goto-char 1)
    (delete-char 1)

    (dotimes (row 4)
      (dotimes (col 4)
        (search-forward "xxxxx" nil 'noerror)
        (let* ((val (nth col (nth row threes-cells)))
               (text (threes-string-center
                      (length "xxxxx")
                      (if (zerop val) "" (number-to-string val))))
               (face (cdr (assq val `((0 . threes-face-0)
                                      (1 . threes-face-1)
                                      (2 . threes-face-2)
                                      (3 . threes-face-3)
                                      (,(threes-cells-max) . threes-face-max))))))
          (when face
            (setq text (propertize text 'face face))
            (let* ((pt (point))
                   ;; 26 is the length of a line
                   (end1 (- pt 26)) (beg1 (- end1 5))
                   (end2 (+ pt 26)) (beg2 (- end2 5)))
              ;; (message ">>> (%s, %s)" beg1 end1)
              (save-excursion
                (delete-region beg1 end1)
                (goto-char beg1)
                (insert (propertize "     " 'face face)))
              (save-excursion
                (delete-region beg2 end2)
                (goto-char beg2)
                (insert (propertize "     " 'face face)))))
          (replace-match text))))

    (goto-char 1)))

;;;###autoload
(defun threes ()
  "Play the Threes game."
  (interactive)
  (switch-to-buffer threes-buffer-name)
  (threes-mode)
  (threes-print-board))

(provide 'threes)
;;; threes.el ends here
