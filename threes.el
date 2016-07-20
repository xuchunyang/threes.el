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

(defconst threes-buffer-name "*Threes*" "Name used for Threes buffer.")

(define-derived-mode threes-mode special-mode "threes-mode"
  "A mode for play Threes.")

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
                       (0 3 0 0)))

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
                      (if (zerop val) "" (number-to-string val)))))
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
