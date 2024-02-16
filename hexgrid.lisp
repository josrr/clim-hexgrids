;;;; hexgrid.lisp

(in-package #:clim-hexgrids)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defclass cell ()
    ((qrs :initarg :qrs :accessor qrs)
     (hexagon :initarg :hexagon :accessor hexagon)
     (selected :initform nil :accessor selected-p)))

  (defmethod print-object ((obj cell) stream)
    (print-unreadable-object (obj stream :identity nil :type t)
      (format stream "~A" (qrs obj))))

  (defun make-cell (q r &optional s)
    (make-instance 'cell :qrs (vec3 q r (if s s (- (+ q r)))))))

(defconstant +cell-directions+
  (make-array 6 :initial-contents (list (make-cell  1  0 -1)
                                        (make-cell  1 -1  0)
                                        (make-cell  0 -1  1)
                                        (make-cell -1  0 1)
                                        (make-cell -1  1  0)
                                        (make-cell  0  1 -1))))

(defconstant +cell-diagonal-directions+
  (make-array 6 :initial-contents (list (make-cell +2 -1 -1)
                                        (make-cell +1 -2 +1)
                                        (make-cell -1 -1 +2)
                                        (make-cell -2 +1 +1)
                                        (make-cell -1 +2 -1)
                                        (make-cell +1 +1 -2))))

(defun q (cell)
  (vx (qrs cell)))

(defun r (cell)
  (vy (qrs cell)))

(defun s (cell)
  (vz (qrs cell)))

(defgeneric format-cell (cell)
  (:method ((cell cell))
    (format nil "~d,~d,~d" (truncate (q cell)) (truncate (r cell)) (truncate (s cell)))))

(defgeneric equality (a b)
  (:method ((a cell) (b cell))
    (v= (qrs a) (qrs b))))

(defgeneric add (a b)
  (:method ((a cell) (b cell))
    (make-instance 'cell :qrs (v+ (qrs a) (qrs b)))))

(defgeneric subtract (a b)
  (:method ((a cell) (b cell))
    (make-instance 'cell :qrs (v- (qrs a) (qrs b)))))

(defgeneric multiply (a k)
  (:method ((a cell) (k number))
    (make-instance 'cell :qrs (v* k (qrs a)))))

(defgeneric len (a)
  (:method ((a cell))
    (let ((qrs (qrs a)))
     (truncate (+ (abs (vx qrs)) (abs (vy qrs)) (abs (vz qrs))) 2))))

(defgeneric distance (a b)
  (:method ((a cell) (b cell))
    (len (subtract a b))))

(defun cell-direction (direction)
  (assert (<= 0 direction 5))
  (aref +cell-directions+ direction))

(defun cell-diagonal-direction (direction)
  (assert (<= 0 direction 5))
  (aref +cell-diagonal-directions+ direction))

(defgeneric cell-neighbor (a direction)
  (:method ((a cell) (direction integer))
    (add a (cell-direction direction))))

(defgeneric cell-diagonal-neighbor (a direction)
  (:method ((a cell) (direction integer))
    (add a (cell-diagonal-direction direction))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defclass orientation ()
    ((f :initarg :f :reader orientation-f)
     (b :initarg :b :reader orientation-b)
     (start-angle :initarg :start-angle :reader orientation-start-angle)))

  (defmethod print-object ((obj orientation) stream)
    (print-unreadable-object (obj stream :identity nil :type t)
      (format stream "f:~A b:~A start-angle:~,4F"
              (orientation-f obj) (orientation-b obj) (orientation-start-angle obj))))

  (defmethod make-load-form ((obj orientation) &optional env)
    (declare (ignore env))
    (with-slots (f b start-angle) obj
      `(make-instance 'orientation :f ,f :b ,b :start-angle ,start-angle))))

(defconstant +orientation-pointy+
  (make-instance 'orientation
                 :f (mat (sqrt 3.0) (/ (sqrt 3.0) 2.0) 0.0 (/ 3.0 2.0))
                 :b (mat (/ (sqrt 3.0) 3.0) (/ -1.0 3.0) 0.0 (/ 2.0 3.0))
                 :start-angle 0.5))

(defconstant +orientation-flat+
  (make-instance 'orientation
                 :f (mat (/ 3.0 2.0) 0.0 (/ (sqrt 3.0) 2.0) (sqrt 3.0))
                 :b (mat (/ 2.0 3.0) 0.0 (/ -1.0 3.0) (/ (sqrt 3.0) 3.0))
                 :start-angle 0.0))

(defclass layout ()
  ((name :initarg :name :reader layout-name)
   (orientation :initarg :orientation :reader layout-orientation)
   (size :initarg :size :reader layout-size)
   (origin :initarg :origin :initform (vec2 0 0) :accessor layout-origin)))

(defmethod print-object ((obj layout) stream)
  (print-unreadable-object (obj stream :identity nil :type t)
    (format stream "~A" (layout-name obj))))

(defgeneric cell-to-point (cell layout)
  (:method ((cell cell) (layout layout))
    (let ((p (v+ (layout-origin layout)
                 (v* (layout-size layout)
                     (m* (orientation-f (layout-orientation layout))
                         (vxy (qrs cell)))))))
      (make-point (vx p) (vy p)))))

(defgeneric point-to-cell (p layout)
  (:method ((p point) (layout layout))
    (let ((qr (m* (orientation-b (layout-orientation layout))
                  (v/ (v- (vec (point-x p) (point-y p)) (layout-origin layout))
                      (layout-size layout)))))
      (make-cell (vx qr) (vy qr)))))

(defclass hexagon (standard-polygon)
  ((center :initarg :center :reader hexagon-center)
   (cell :initarg :cell :reader hexagon-cell)))

(defmethod print-object ((obj hexagon) stream)
  (print-unreadable-object (obj stream :identity nil :type t)
    (format stream "~A" (qrs (hexagon-cell obj)))))

(defgeneric draw (object &optional pane &rest drawing-options))

(defmethod draw ((hexagon hexagon) &optional (pane *standard-output*) &rest drawing-options)
  (apply #'draw-design pane hexagon drawing-options))

(defgeneric draw-coords (hexagon &optional pane &rest drawing-options)
  (:method ((hexagon hexagon) &optional (pane *standard-output*) &rest drawing-options)
    (let ((center (hexagon-center hexagon))
          (text (format-cell (hexagon-cell hexagon))))
      (multiple-value-bind (text-w text-h) (text-size pane text)
        (apply #'draw-text* pane text
               (- (point-x center) (/ text-w 2.0))
               (+ (point-y center) (/ text-h 2.0))
               drawing-options)))))

(defgeneric make-hexagon (layout cell)
  (:method ((layout layout) (cell cell))
    (flet ((corner-offset (corner)
             (let ((angle (* pi 1/3 (+ (orientation-start-angle (layout-orientation layout))
                                       corner))))
               (v* (layout-size layout) (vec2 (cos angle) (sin angle))))))
      (let ((center (cell-to-point cell layout)))
        (make-instance 'hexagon
                       :center center
                       :cell cell
                       :points (loop for corner from 0 below 6
                                     for off = (corner-offset corner)
                                     collect (make-point (+ (point-x center) (vx off))
                                                         (+ (point-y center) (vy off)))))))))

(defclass hexgrid ()
  ((layout :initarg :layout :accessor layout)
   (cells :initarg :cells :accessor cells)
   (draw-coords :initarg :draw-coords :initform t :accessor draw-coords-p)))

(defclass hexagonal (hexgrid)
  ())

(defclass triangular (hexgrid)
  ())

(defclass rectangular (hexgrid)
  ())

(defclass parallelogram (hexgrid)
  ((coords :initarg :coords :reader coords)))

(defgeneric make-cells (type layout size &optional height coords))

(defmethod make-cells ((type (eql 'hexagonal)) layout size &optional height coords)
  (declare (ignore height coords))
  (loop for q from (- size) to size
        append (loop for r from (max (- size) (- (+ q size))) to (min size (- size q))
                     collect (make-cell q r))))

(defmethod make-cells ((type (eql 'triangular)) layout size &optional height coords)
  (declare (ignore height coords))
  (loop for q from 0 to size
        append (loop for r from 0 to (- size q)
                     collect (make-cell q r))))

(defgeneric rect-grid (orientation width height))

(defmethod rect-grid ((orientation (eql +orientation-pointy+)) width height)
  (loop for r from 0 to height
        for r-offset = (floor r 2)
        append (loop for q from (- r-offset) to (- width r-offset)
                     collect (make-cell q r))))

(defmethod rect-grid ((orientation (eql +orientation-flat+)) width height)
  (loop for q from 0 to width
        for q-offset = (floor q 2)
        append (loop for r from (- q-offset) to (- height q-offset)
                     collect (make-cell q r))))

(defmethod make-cells ((type (eql 'rectangular)) layout width &optional height coords)
  (declare (ignore coords))
  (rect-grid (layout-orientation layout) width height))

(defmethod make-cells ((type (eql 'parallelogram)) layout size &optional height coords)
  (declare (ignore height))
  (destructuring-bind (c1 c2) coords
    (labels ((genval (c1 c2 v1 v2)
               (if (eq c1 c2) v1 v2))
             (gencoords (v1 v2)
               (let ((diff (set-difference '(q r s) (list c1 c2))))
                 (case (first diff)
                   (q (list (- (+ v1 v2)) (genval 'r c1 v1 v2) (genval 's c2 v2 v1)))
                   (r (list (genval 'q c1 v1 v2) (- (+ v1 v2)) (genval 's c2 v2 v1)))
                   (s (list (genval 'q c1 v1 v2) (genval 'r c2 v2 v1) (- (+ v1 v2))))))))
      (loop for v1 from (- size) to size
            append (loop for v2 from (- size) to size
                         for cell = (apply #'make-cell (gencoords v1 v2))
                         collect cell)))))

(defmethod make-cells :around (type layout size &optional height coords)
  (declare (ignore height coords size))
  (let ((cells (call-next-method)))
    (dolist (cell cells)
      (setf (hexagon cell) (make-hexagon layout cell)))
    cells))

(defun make-hexgrid (layout type size &optional height coords)
  (case type
    (parallelogram (make-instance type
                                  :coords coords
                                  :layout layout
                                  :cells (make-cells type layout size height coords)))
    (t (make-instance type
                      :layout layout
                      :cells (make-cells type layout size height coords)))))

(defun make-hexagonal-grid (layout size)
  (make-instance 'hexagonal
                 :layout layout :cells (make-cells 'hexagonal layout size)))

(defun make-triangular-grid (layout size)
  (make-instance 'triangular
                 :layout layout :cells (make-cells 'triangular layout size)))

(defun make-rectangular-grid (layout width height)
  (make-instance 'rectangular
                 :layout layout
                 :cells (make-cells 'rectangular layout width height)))

(defun make-parallelogram-grid (layout size &optional (c1 'q) (c2 'r))
  (let ((coords (list c1 c2)))
    (make-instance 'parallelogram
                   :coords coords
                   :layout layout
                   :cells (make-cells 'parallelogram layout size nil coords))))

(defmethod draw ((grid hexgrid) &optional (pane *standard-output*) &rest drawing-options)
  (apply #'invoke-with-drawing-options pane
         (lambda (medium)
           (loop for cell in (cells grid)
                 for hexagon = (hexagon cell)
                 do (present hexagon 'hexagon :stream medium)
                 if (draw-coords-p grid)
                   do (draw-coords hexagon medium)))
         drawing-options))

(defgeneric neighbors (hexagon)
  (:method ((hexagon hexagon))
    (let ((neighbors (loop for direction from 0 below 6
                           collect (cell-neighbor (hexagon-cell hexagon) direction))))
      (remove-if-not (lambda (cell)
                       (member cell neighbors :test #'equality))
                     (cells (hexgrids-selected-grid *application-frame*))))))

(defgeneric diagonal-neighbors (hexagon)
  (:method ((hexagon hexagon))
    (let ((neighbors (loop for direction from 0 below 6
                           collect (cell-diagonal-neighbor (hexagon-cell hexagon)
                                                           direction))))
      (remove-if-not (lambda (cell)
                       (member cell neighbors :test #'equality))
                     (cells (hexgrids-selected-grid *application-frame*))))))
