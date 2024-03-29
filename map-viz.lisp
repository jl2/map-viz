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

(defconstant +np+ (cffi:null-pointer))

(defun show-bounding-boxes (&key
                              (shape-file-name)
                              (output-file-name)
                              (width 1600)
                              (height 1600)
                              (stroke-width 1.0)
                              (image-type "BMP")
                              (show t))
  (declare (ignorable output-file-name shape-file-name))
  (ensure-directories-exist output-file-name)

  (bl:with-image-context*
      (img ctx output-file-name
           :width width
           :height height
           :codec-name image-type)
      ((rect bl:rect)
       (line bl:line))

    (gdal:gdal-all-register)
    (let ((dataset (gdal:gdal-open-ex shape-file-name
                                      gdal:+of-vector+
                                      +np+ +np+ +np+)))
      (format t "dataset: ~a~%" dataset)
      (multiple-value-bind (x-min x-max y-min y-max) (gdal:get-dataset-envelope dataset)
        (let ((dx (- x-max x-min))
              (dy (- y-max y-min)))
          (format t "x-min ~a y-min ~a x-max ~a y-max ~a w ~a h ~a~%" x-min y-min x-max y-max dx dy)
          (bl:setup-window ctx (- x-min (/ dx 20.0)) (+ x-max (/ dx 20.0)) (- y-min (/ dy 20.0)) (+ y-max (/ dy 20.0)) width height stroke-width)
          (setf (bl:line.x0 line) x-min)
          (setf (bl:line.y0 line) y-min)

          (setf (bl:line.x1 line) x-max)
          (setf (bl:line.y1 line) y-max)
          ;; (bl:context-set-stroke-width ctx  stroke-width)
          (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
          (bl:context-set-stroke-style-rgba32 ctx #16rff00ff00)
          (bl:context-stroke-geometry ctx bl:+geometry-type-line+ line)

          (setf (bl:line.x0 line) x-min)
          (setf (bl:line.y0 line) y-max)

          (setf (bl:line.x1 line) x-max)
          (setf (bl:line.y1 line) y-min)
          ;; (bl:context-set-stroke-width ctx  stroke-width)
          (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
          (bl:context-set-stroke-style-rgba32 ctx #16rff0000ff)
          (bl:context-stroke-geometry ctx bl:+geometry-type-line+ line)

          (setf (bl:rect.x rect) x-min)
          (setf (bl:rect.y rect) y-min)
          (setf (bl:rect.w rect) (- x-max x-min))
          (setf (bl:rect.h rect) (- y-max y-min))
          (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
          (bl:context-set-stroke-style-rgba32 ctx #16rffffffff)
          ;; (bl:context-stroke-geometry ctx bl:+geometry-type-line+ line)
          ;; (bl:context-set-stroke-width ctx 0.01)
          (bl:lookup-error (bl:context-stroke-geometry ctx bl:+geometry-type-rectd+ rect)))
        ;; (autowrap:free dataset)
        )))
  (when show
    (uiop:run-program (format nil "display ~a" output-file-name))))
