;;;; map-viz.lisp 
;;
;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :map-viz)

(defun show-bounding-boxes (&key shape-file-name output-file-name)
  (declare (ignorable output-file-name shape-file-name))

  (gdal:gdal-all-register)
  (loop
     with dataset = (gdal:gdal-open-ex shape-file-name gdal:+of-vector+ +np+ +np+ +np+)
     for i below (gdal:ogr-ds-get-layer-count dataset)
     for layer = (gdal:ogr-ds-get-layer dataset i)
     for layer-name = (gdal:ogr-l-get-name layer)
     do
       (format t "~a has ~a features.~%" layer-name (gdal:ogr-l-get-feature-count layer 1))
       (multiple-value-bind (min-x max-x min-y max-y) (gdal:get-layer-envelope layer)
          (format t
                  "Layer ~a bounding box is: (~a ~a) - (~a ~a)~%"
                  layer-name min-x min-y max-x max-y)
          (autowrap:free layer))
     finally (autowrap:free dataset)))
