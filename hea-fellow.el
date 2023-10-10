;;; hea-fellow.el --- Find and show dimensions of HEA Fellow annotations

;; Copyright (C) 2023 Phillip Lord <phillip.lord@newcastle.ac.uk>

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Version: 4.2.2

;; The contents of this file are subject to the GPL License, Version 3.0.
;;
;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package looks for annotations about HEA dimensions
;; (https://www.advance-he.ac.uk/fellowship/fellowship) showing those
;; that are present or missing.

;;; Code:
(require 'chart)
(require 'm-buffer)

(defvar hea-fellow-dimensions
  '(("V1" "respect individual learners and diverse groups of learners")
    ("V2" "promote engagement in learning and equity of opportunity for all to reach their potential")
    ("V3" "use scholarship, or research, or professional learning, or other evidence-informed approaches as a basis for effective practice")
    ("V4" "respond to the wider context in which higher education operates, recognising implications for practice")
    ("V5" "collaborate with others to enhance practice")

    ("K1" "how learners learn, generally and within specific subjects")
    ("K2" "approaches to teaching and/or supporting learning, appropriate for subjects and level of study")
    ("K3" "critical evaluation as a basis for effective practice")
    ("K4" "appropriate use of digital and/or other technologies, and resources for learning")
    ("K5" "requirements for quality assurance and enhancement, and their implications for practice")

    ("A1" "design and plan learning activities and/or programmes")
    ("A2" "teach and/or support learning through appropriate approaches and environments")
    ("A3" "assess and give feedback for learning")
    ("A4" "support and guide learners")
    ("A5" "enhance practice through own continuing professional development")))

(defun hea-fellow-get-all-dimensions()
  (m-buffer-match-string-no-properties
   (m-buffer-match :regexp (rx (or "A" "K" "V") (or "1" "2" "3" "4" "5"))
                   :buffer (current-buffer))))

;;;###autoload
(defun hea-fellow-all-dimensions()
  (interactive)
  (message "Dimensions: %s" (sort (hea-fellow-get-all-dimensions) 'string<)))

;;;###autoload
(defun hea-fellow-missing()
  (interactive)
  (message "Missing Dimensions: %s"
           (seq-difference
            '("A1" "A2" "A3" "A4" "A5"
              "K1" "K2" "K3" "K4" "K5"
              "V1" "V2" "V3" "V4" "V5")
            (seq-uniq
             (sort
              (hea-fellow-get-all-dimensions)
              #'string<)
             #'string=
             )
            #'string=)))

;;;###autoload
(defun hea-fellow-chart ()
  (interactive)
  (let* ((grouped-dimensions
          (sort (seq-group-by 'identity (hea-fellow-get-all-dimensions))
                (lambda (x y) (string< (car x) (car y)))))
         (counted-dimensions
          (seq-map
           (lambda (l)
             (list (car l)
                   (- (length l) 1)))
           grouped-dimensions)))
    (chart-bar-quickie
     'vertical "HEA Dimensions"
     (seq-map 'first counted-dimensions) "Dimension"
     (seq-map 'second counted-dimensions) "Occurences")))

;;;###autoload
(defun hea-fellow-list ()
  (interactive)
  (with-output-to-temp-buffer
      "HEA Fellow Dimensions"
    (seq-do
     (lambda (l)
       (princ (format "%s\t%s\n" (first l) (second l))))
     hea-fellow-dimensions)))

(provide 'hea-fellow)
;;; End:
