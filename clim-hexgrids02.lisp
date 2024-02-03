;;;; clim-hex.lisp

(in-package #:clim-hexgrids)

(defclass cell ()
  ((qrs :initarg :qrs :accessor qrs)))

(defun q (cell)
  (vx (qrs cell)))

(defun r (cell)
  (vy (qrs cell)))

(defun s (cell)
  (vz (qrs cell)))

(defun make-cell (q r &optional s)
  (make-instance 'cell :qrs (vec3 q r (if s s (- (+ q r))))))

(defmethod format-cell ((cell cell))
  (format nil "~2d,~2d,~2d" (truncate (q cell)) (truncate (r cell)) (truncate (s cell))))

(defparameter *cell-directions*
  (make-array 6 :initial-contents (list (make-cell  1  0 -1)
                                        (make-cell  1 -1  0)
                                        (make-cell  0 -1  1)
                                        (make-cell -1  0 -1)
                                        (make-cell -1  1  0)
                                        (make-cell  0  1 -1))))

(defparameter *cell-diagonal-directions*
  (make-array 6 :initial-contents (list (make-cell +2 -1 -1)
                                        (make-cell +1 -2 +1)
                                        (make-cell -1 -1 +2)
                                        (make-cell -2 +1 +1)
                                        (make-cell -1 +2 -1)
                                        (make-cell +1 +1 -2))))

(defmethod equality ((a cell) (b cell))
  (v= (qrs a) (qrs b)))

(defmethod add ((a cell) (b cell))
  (make-instance 'cell :qrs (v+ (qrs a) (qrs b))))

(defmethod subtract ((a cell) (b cell))
  (make-instance 'cell :qrs (v- (qrs a) (qrs b))))

(defmethod multiply ((a cell) (k number))
  (make-instance 'cell :qrs (v* k (qrs a))))

(defmethod len ((a cell))
  (truncate (+ (abs (vx a)) (abs (vy a)) (abs (vz a))) 2))

(defmethod distance ((a cell) (b cell))
  (len (subtract a b)))

(defun cell-direction (direction)
  (assert (<= 0 direction 5))
  (aref *cell-directions* direction))

(defun cell-diagonal-direction (direction)
  (assert (<= 0 direction 5))
  (aref *cell-directions* direction))

(defmethod cell-neighbor ((a cell) (direction integer))
  (add a (cell-direction direction)))

(defmethod cell-diagonal-neighbor ((a cell) (direction integer))
  (add a (cell-diagonal-direction direction)))

(defclass orientation ()
  ((f :initarg :f :reader orientation-f)
   (b :initarg :b :reader orientation-b)
   (start-angle :initarg :start-angle :reader orientation-start-angle)))

(defparameter *orientation-pointy*
  (make-instance 'orientation
                 :f (mat (sqrt 3.0) (/ (sqrt 3.0) 2.0) 0.0 (/ 3.0 2.0))
                 :b (mat (/ (sqrt 3.0) 3.0) (/ -1.0 3.0) 0.0 (/ 2.0 3.0))
                 :start-angle 0.5))

(defparameter *orientation-flat*
  (make-instance 'orientation
                 :f (mat (/ 3.0 2.0) 0.0 (/ (sqrt 3.0) 2.0) (sqrt 3.0))
                 :b (mat (/ 2.0 3.0) 0.0 (/ -1.0 3.0) (/ (sqrt 3.0) 3.0))
                 :start-angle 0.0))

(defclass layout ()
  ((name :initarg :name :reader layout-name)
   (orientation :initarg :orientation :reader layout-orientation)
   (size :initarg :size :reader layout-size)
   (origin :initarg :origin :accessor layout-origin)))

(defmethod cell-to-point ((cell cell) (layout layout))
  (let ((p (v+ (layout-origin layout)
               (v* (layout-size layout)
                   (m* (orientation-f (layout-orientation layout))
                       (vxy (qrs cell)))))))
    (make-point (vx p) (vy p))))

(defmethod point-to-cell ((p point) (layout layout))
  (let ((qr (m* (orientation-b (layout-orientation layout))
                (v/ (v- (vec (point-x p) (point-y p)) (layout-origin layout))
                    (layout-size layout)))))
    (make-cell (vx qr) (vy qr))))

(defclass hexagon (standard-polygon)
  ((center :initarg :center :reader hexagon-center)
   (cell :initarg :cell :reader hexagon-cell)))

(defmethod draw ((hexagon hexagon) &optional (pane *standard-output*) &rest drawing-options)
  (apply #'draw-design pane hexagon drawing-options))

(defmethod draw-coords ((hexagon hexagon) &optional (pane *standard-output*) &rest drawing-options)
  (let ((center (hexagon-center hexagon))
        (text (format-cell (hexagon-cell hexagon))))
    (multiple-value-bind (text-w text-h) (text-size pane text)
      (apply #'draw-text* pane text
             (- (point-x center) (/ text-w 2.0))
             (+ (point-y center) (/ text-h 2.0))
             drawing-options))))

(defmethod make-hexagon ((layout layout) (cell cell))
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
                                                      (+ (point-y center) (vy off))))))))

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

(defmethod rect-grid ((orientation (eql *orientation-pointy*)) width height)
  (declare (ignore coords))
  (loop for r from 0 to height
        for r-offset = (floor r 2)
        append (loop for q from (- r-offset) to (- width r-offset)
                     collect (make-cell q r))))

(defmethod rect-grid ((orientation (eql *orientation-flat*)) width height)
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
                         collect (apply #'make-cell (gencoords v1 v2)))))))

(defun make-hexgrid (layout type size &optional height)
  (declare (ignore height))
  (make-instance type
                 :layout layout
                 :cells (make-cells type layout size height)))

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
                 for hexagon = (make-hexagon (layout grid) cell)
                 do (present hexagon 'hexagon :stream medium)
                 if (draw-coords-p grid)
                   do (draw-coords hexagon medium)))
         drawing-options))

(defclass graphical-view (view) ())

(defparameter *graphical-view* (make-instance 'graphical-view))
(defparameter *width* 1024)
(defparameter *height* (+ 1024 64 32))

(defun display-controls (frame pane)
  (let ((*standard-output* pane))
    (setf (cursor-position (stream-text-cursor pane)) (values 20 20))
    (formatting-table (t :x-spacing '(4 :character) :equalize-column-widths t)
      (formatting-row ()
        (formatting-cell (t :align-x :right)
          (formatting-table (t :x-spacing '(3 :character) :y-spacing '(3/4 :character))
            (formatting-column ()
              (dolist (layout (hexgrids-grid-layouts frame))
                (formatting-cell (t :align-x :right)
                  (present layout))))))
        (surrounding-output-with-border (t )
          (formatting-cell (t :align-x :right)
            (formatting-table (t :x-spacing '(3 :character) :y-spacing '(3/4 :character))
              (dolist (grid (hexgrids-grids frame))
                (formatting-row ()
                  (if (consp grid)
                      (dolist (g grid)
                        (formatting-cell (t :align-x :right)
                          (present g)))
                      (formatting-cell (t :align-x :right)
                        (present grid))))))))))))

(defun display-canvas (frame pane)
  (window-clear pane)
  (let ((*standard-output* pane)
        (layout (hexgrids-selected-layout frame)))
    (with-bounding-rectangle* (x0 y0 x1 y1) pane
      (setf (layout-origin layout) (vec2 (/ (- x1 x0) 2) (/ (- y1 y0) 2))
            (layout (hexgrids-selected-grid frame)) layout))
    (draw (hexgrids-selected-grid frame))))

(defparameter *layouts* (list (make-instance 'layout
                                             :name 'flat
                                             :orientation *orientation-flat*
                                             :size (vec2 32.0 32.0)
                                             :origin (vec2 (/ *width* 2)
                                                           (/ *height* 2)))
                              (make-instance 'layout
                                             :name 'pointy
                                             :orientation *orientation-pointy*
                                             :size (vec2 32.0 32.0)
                                             :origin (vec2 (/ *width* 2)
                                                           (/ *height* 2)))))

(defparameter *grids* (list (make-parallelogram-grid (second *layouts*) 5)
                            (make-parallelogram-grid (first *layouts*) 5 's 'q)
                            (make-parallelogram-grid (first *layouts*) 5 'r 's)
                            (make-hexagonal-grid (first *layouts*) 5)
                            (make-triangular-grid (first *layouts*) 5)
                            (make-rectangular-grid (first *layouts*) 6 5)))

(define-application-frame hexgrids ()
  ((grid-layouts :initarg :hex-layouts
                 :initform *layouts*
                 :reader hexgrids-grid-layouts)
   (grids :initarg :grids
          :initform *grids*
          :reader hexgrids-grids)
   (selected-grid :initarg :selected-grid
                  :initform (first *grids*)
                  :accessor hexgrids-selected-grid)
   (selected-layout :initarg :selected-layout
                    :initform (first *layouts*)
                    :accessor hexgrids-selected-layout))
  (:panes (controls (make-pane 'application-pane
                               :name 'controls
                               :default-view *graphical-view*
                               :background +white+
                               :display-function 'display-controls
                               :display-time :command-loop))
          (canvas (make-pane 'application-pane
                             :name 'canvas
                             :default-view *graphical-view*
                             :background +grey90+
                             :display-function 'display-canvas
                             :display-time t))
          (interactor :interactor))
  (:layouts (default
             (horizontally (:min-width (* 6/5 *width*)
                            :max-width (* 7/5 *width*))
               (1/5 controls)
               (5/6 (vertically (:min-height (* 5/4 *height*) :max-height (* 5/4 *height*)
                                 :min-width *width* :max-width *width*)
                      (5/6 canvas)
                      (make-pane 'clime:box-adjuster-gadget)
                      (1/6 interactor))))))
  (:menu-bar t)
  (:pointer-documentation t))

(define-presentation-method present (obj (type hexagon) stream (view graphical-view)
                                         &key &allow-other-keys)
  (draw obj stream :ink (if (and (zerop (q (hexagon-cell obj))) (zerop (r (hexagon-cell obj))))
                            +gray90+ +white+)
                   :filled t :line-thickness 3.0)
  (draw obj stream :ink +black+ :filled nil :line-thickness 3.0))

(define-presentation-method present (obj (type hexgrid) stream (view graphical-view)
                                         &key &allow-other-keys)
  (if (eq (hexgrids-selected-grid *application-frame*) obj)
      (surrounding-output-with-border (stream :move-cursor nil :shape :drop-shadow)
        (format stream (string-capitalize (symbol-name type))))
      (format stream (string-capitalize (symbol-name type))))
  (terpri stream)
  (terpri stream))

(define-presentation-method present (obj (type parallelogram) stream (view graphical-view)
                                         &key &allow-other-keys)
  (if (eq (hexgrids-selected-grid *application-frame*) obj)
      (surrounding-output-with-border (stream :move-cursor nil :shape :drop-shadow)
        (format stream "~a ~{~a~^ y ~}" (string-capitalize (symbol-name type)) (coords obj)))
      (format stream "~a ~{~a~^ y ~}" (string-capitalize (symbol-name type)) (coords obj)))
  (terpri stream)
  (terpri stream))

(define-presentation-method present (obj (type layout) stream (view graphical-view)
                                         &key &allow-other-keys)
  (if (eq (hexgrids-selected-layout *application-frame*) obj)
      (surrounding-output-with-border (stream :move-cursor nil :shape :drop-shadow)
        (format stream "~:(~a~)" (symbol-name (layout-name obj))))
      (format stream "~:(~a~)" (symbol-name (layout-name obj))))
  (terpri stream)
  (terpri stream))

(define-hexgrids-command (com-redraw :name "Redraw" :menu t) ()
  (setf (pane-needs-redisplay (find-pane-named *application-frame* 'canvas)) t))

(define-hexgrids-command (com-show-details :name "Show details") ((obj 'hexagon))
  (let ((interactor (find-pane-named *application-frame* 'interactor))
        (coords (hexagon-cell obj)))
    (format interactor "~2d, ~2d, ~2d~%" (q coords) (r coords) (s coords))))

(define-hexgrids-command (com-change-grid :name "Change grid") ((obj 'hexgrid))
  (setf (hexgrids-selected-grid *application-frame*) obj
        (pane-needs-redisplay (find-pane-named *application-frame* 'canvas)) t))

(define-hexgrids-command (com-change-layout :name "Change layout") ((obj 'layout))
  (setf (hexgrids-selected-layout *application-frame*) obj
        (pane-needs-redisplay (find-pane-named *application-frame* 'canvas)) t))

(define-presentation-to-command-translator show-hexagon
    (hexagon com-show-details hexgrids
     :gesture :select
     :documentation "Show details"
     :echo nil)
    (object)
  (list object))

(define-presentation-to-command-translator change-grid
    (hexgrid com-change-grid hexgrids
     :gesture :select
     :documentation "Change grid"
     :echo nil)
    (object)
  (list object))

(define-presentation-to-command-translator change-layout
    (layout com-change-layout hexgrids
     :gesture :select
     :documentation "Change layout"
     :echo nil)
    (object)
  (list object))

(defun start ()
  (find-application-frame 'hexgrids))
