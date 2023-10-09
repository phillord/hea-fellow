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

(defun hea-fellow-get-all-dimensions()
  (m-buffer-match-string-no-properties
   (m-buffer-match :regexp (rx upper digit)
                   :buffer (current-buffer))))

(defun hea-fellow-all-dimensions()
  (interactive)
  (message "Dimensions: %s" (sort (hea-fellow-get-all-dimensions) 'string<)))

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
     (seq-map 'firstq counted-dimensions) "Dimension"
     (seq-map 'second counted-dimensions) "Occurences")))

(provide 'hea-fellow)
;;; End:
