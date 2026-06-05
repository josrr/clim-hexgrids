;;;; operations.lisp
;;;; Copyright © 2026 José M. Á. Ronquillo Rivera

(in-package #:clim-hexgrids)

;;; Vectors
(declaim (inline vec2 vec3))

(deftype vec2 ()
  `(array single-float (2)))

(deftype vec3 ()
  `(array single-float (3)))

(deftype vec ()
  `(or vec3 vec2))

(declaim (ftype (function (real real real) vec3) vec3)
         (ftype (function (real real) vec2) vec2))

(defun vec2 (x y)
  (make-array 2 :initial-contents (list (float x 1.0s0) (float y 1.0s0))
                :element-type 'single-float))

(defun vec3 (x y z)
  (make-array 3 :initial-contents (list (float x 1.0s0) (float y 1.0s0) (float z 1.0s0))
                :element-type 'single-float))

(declaim (inline vx vy vz vxy))
(declaim (ftype (function (vec) single-float) vx vy))
(declaim (ftype (function (vec3) single-float) vz))
(declaim (ftype (function (vec3) vec2) vxy))

(defun vx (v)
  (aref v 0))

(defun vy (v)
  (aref v 1))

(defun vz (v)
  (aref v 2))

(defun vxy (v)
  (vec2 (vx v) (vy v)))

(declaim (ftype (function (vec vec) boolean) v=))

(defun v= (v w)
  (typecase v
    (vec3 (and (= (vx v) (vx w))
               (= (vy v) (vy w))
               (= (vz v) (vz w))))
    (vec2 (and (= (vx v) (vx w))
               (= (vy v) (vy w))))))

(declaim (ftype (function (vec vec) vec) v+ v- v* v/))

(defun v+ (v w)
  (typecase v
    (vec3 (vec3 (+ (vx v) (vx w))
                (+ (vy v) (vy w))
                (+ (vz v) (vz w))))
    (vec2 (vec2 (+ (vx v) (vx w))
                (+ (vy v) (vy w))))))

(defun v- (v w)
  (typecase v
    (vec3 (vec3 (- (vx v) (vx w))
                (- (vy v) (vy w))
                (- (vz v) (vz w))))
    (vec2 (vec2 (- (vx v) (vx w))
                (- (vy v) (vy w))))))

(defun v* (v w)
  (typecase v
    (vec3 (vec3 (* (vx v) (vx w))
                (* (vy v) (vy w))
                (* (vz v) (vz w))))
    (vec2 (vec2 (* (vx v) (vx w))
                (* (vy v) (vy w))))))

(defun v/ (v w)
  (typecase v
    (vec3 (vec3 (/ (vx v) (vx w))
                (/ (vy v) (vy w))
                (/ (vz v) (vz w))))
    (vec2 (vec2 (/ (vx v) (vx w))
                (/ (vy v) (vy w))))))

;;; Matrices
(deftype mat ()
  `(array single-float (2 2)))

(declaim (ftype (function (real real real real) mat) mat))

(defun mat (a b c d)
  (make-array '(2 2) :initial-contents (list (list (float a 1.0s0) (float b 1.0s0))
                                             (list (float c 1.0s0) (float d 1.0s0)))
                     :element-type 'single-float))

(declaim (ftype (function (mat vec2) vec2) m*))

(defun m* (m v)
  (flet ((col (i)
           (loop for j from 0 below 2 sum (* (aref m i j) (aref v j)))))
    (vec2 (col 0) (col 1))))
