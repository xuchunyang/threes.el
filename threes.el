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

;;;###autoload
(defun threes ()
  "Play the Threes game."
  (interactive)
  (switch-to-buffer threes-buffer-name)
  (threes-mode))

(provide 'threes)
;;; threes.el ends here
