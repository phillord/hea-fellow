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

(defvar hea-fellow-dimension :2023)

(defvar hea-fellow-dimensions-2011
  '(
    ("V1" "Respect individual learners and diverse learning communities")
    ("V2" "Promote participation in higher education and equality of opportunity for learners")
    ("V3" "Use evidence-informed approaches and the outcomes from research, scholarship and continuing professional development")
    ("V4" "Acknowledge the wider context in which higher education operates recognising the implications for professional practice")

    ("K1" "The subject material")
    ("K2" "Appropriate methods for teaching, learning and assessing in the subject area and at the level of the academic programme")
    ("K3" "How students learn, both generally and within their subject/disciplinary area(s)")
    ("K4" "The use and value of appropriate learning technologies")
    ("K5" "Methods for evaluating the effectiveness of teaching")
    ("K6" "The implications of quality assurance and quality enhancement for academic and professional practice with a particular focus on teaching")
    

    ("A1" "Design and plan learning activities and/or programmes of study")
    ("A2" "Teach and/or support learning")
    ("A3" "Assess and give feedback to learners")
    ("A4" "Develop effective learning environments and approaches to student support and guidance")
    ("A5" "Engage in continuing professional development in subjects/disciplines and their pedagogy, incorporating research, scholarship and the evaluation of professional practices")
    )

  )

(defvar hea-fellow-dimensions-2023
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

(defvar hea-fellow-2011-2023-mapping
  "Mapping between 2011 and 2023 PSF.

This is my own mapping and is not official. It is based on word usage."
  '(
    ("K1" nil)
    ("K2" "K2")
    ("K3" "K1")
    ("K4" "K4")
    ("K5" "K3")
    ("K6" "K5")

    ("A1" "A1")
    ("A2" "A2")
    ("A3" "A3")
    ("A4" "A4")
    ("A5" "A5")

    ("V1" "V1")
    ("V2" "V2")
    ("V3" "V3")
    ("V4" "V4")
    (nil "V5")))


(defvar hea-fellow-citation-regexp (rx (or "A" "K" "V") (or "1" "2" "3" "4" "5")))

(defun hea-fellow-get-all-dimensions()
  (m-buffer-match-string-no-properties
   (m-buffer-match :regexp hea-fellow-citation-regexp
                   :buffer (current-buffer))))

;;;###autoload
(defun hea-fellow-all-dimensions()
  (interactive)
  (message "Dimensions: %s" (sort (hea-fellow-get-all-dimensions) 'string<)))

(defun hea-fellow-missing ()
  (seq-difference
   '("A1" "A2" "A3" "A4" "A5"
     "K1" "K2" "K3" "K4" "K5"
     "V1" "V2" "V3" "V4" "V5")
   (seq-uniq
    (sort
     (hea-fellow-get-all-dimensions)
     #'string<)
    #'string=)
   #'string=))

;;;###autoload
(defun hea-fellow-show-missing()
  (interactive)
  (message "Missing Dimensions: %s"
           (hea-fellow-missing)))

;;;###autoload
(defun hea-fellow-chart ()
  (interactive)
  (save-excursion
    (let* (
           ;; Group them
           (grouped-dimensions
            (seq-group-by 'identity (hea-fellow-get-all-dimensions)))
           ;; Count the groups
           (counted-dimensions
            (seq-map
             (lambda (l)
               (list (car l)
                     (- (length l) 1)))
             grouped-dimensions))
           ;; Add in the missing and sort
           (counted-with-missing-dimensions
            (sort
             (seq-union counted-dimensions
                        (seq-map
                         (lambda (dimension)
                           `(,dimension 0))
                         (hea-fellow-missing)))
             (lambda (x y) (string< (car x) (car y))))))
      (chart-bar-quickie
       'vertical "HEA Dimensions"
       (seq-map 'first counted-with-missing-dimensions) "Dimension"
       (seq-map 'second counted-with-missing-dimensions) "Occurences"))))

;;;###autoload
(defun hea-fellow-chart-no-switch ()
  "Display a chart but do not switch buffer to it"
  (interactive)
  (save-window-excursion
    (hea-fellow-chart)))

;;;###autoload
(defun hea-fellow-list ()
  (interactive)
  (with-output-to-temp-buffer
      "HEA Fellow Dimensions"
    (seq-do
     (lambda (l)
       (princ (format "%s\t%s\n" (first l) (second l))))
     hea-fellow-dimensions)))


(defun hea-fellow-chart-in-buffer (buffer)
  (with-current-buffer buffer
    (hea-fellow-chart-no-switch)))

(defvar hea-fellow-idle-timer nil)
(defun hea-fellow-chart-on-idle ()
  "Update the HEA fellow char on idle."
  (interactive)
  (when hea-fellow-idle-timer
    (cancel-timer hea-fellow-idle-timer))
  ;; It's a bit flicky and distruptive
  (setq hea-fellow-idle-timer
        (run-with-idle-timer 4 t #'hea-fellow-chart-in-buffer (current-buffer))))

(defun hea-fellow-chart-off-idle ()
  (interactive)
  (when hea-fellow-idle-timer
    (cancel-timer hea-fellow-idle-timer))
  (setq hea-fellow-idle-timer nil))

(defun hea-fellow-show-dimension ()
  (interactive)
  (let ((wap (word-at-point)))
    (when-let*
        ((_ (stringp wap))
         (_ (string-match-p
             hea-fellow-citation-regexp
             wap))
         (item (assoc wap
                      (cl-case hea-fellow-dimension
                        (:2011 hea-fellow-dimensions-2011)
                        (:2023 hea-fellow-dimensions-2023))
                      hea-fellow-dimensions))
         (definition (second item)))
      (message "%s: %s" wap definition))))

(defvar hea-fellow-show-dimension-timer
  (run-with-idle-timer 1 t #'hea-fellow-show-dimension))

(provide 'hea-fellow)
;;; End:
