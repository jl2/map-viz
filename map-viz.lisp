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

(defun show-bounding-boxes (&key
                              (shape-file-name)
                              (output-file-name)
                              (width 1600)
                              (height 1600)
                              (image-type "BMP"))
  (declare (ignorable output-file-name shape-file-name))
  (ensure-directories-exist output-file-name)

  (bl:with-image-context*
      (img ctx output-file-name
           :width width
           :height height
           :codec-name image-type)
      ((rect bl:rect))

    (gdal:gdal-all-register)
    (let ((dataset (gdal:gdal-open-ex
                    shape-file-name
                    gdal:+of-vector+
                    gdal:+np+ gdal:+np+ gdal:+np+)))
      (format t "dataset: ~a~%" dataset)
      (multiple-value-bind (x-min x-max y-min y-max) (gdal:get-dataset-envelope dataset)
        (format t "x-min ~a y-min ~a x-max ~a y-max ~a w ~a h ~a~%" x-min y-min x-max y-max (- x-max x-min) (- y-max y-min))
        (bl:setup-window ctx :width width :height height :x-min x-min :y-min y-min :x-max x-max :y-max y-max :stroke-width 10.0)
        (setf (bl:rect.x rect) x-min)
        (setf (bl:rect.y rect) y-min)
        (setf (bl:rect.w rect) (- x-max x-min))
        (setf (bl:rect.h rect) (- y-max y-min))
        (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
        (bl:context-set-stroke-style-rgba32 ctx #16rffffffff)
        ;; (bl:context-stroke-geometry ctx bl:+geometry-type-line+ line)
        (bl:context-set-stroke-width ctx 20.0)
        (bl:lookup-error (bl:context-stroke-geometry ctx bl:+geometry-type-rectd+ rect)))
      ;; (autowrap:free dataset)
      )))
