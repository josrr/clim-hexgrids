;;;; clim-hex.lisp

(in-package #:clim-hexgrids)

(defclass cell ()
  ((q :initarg :q :accessor q)
   (r :initarg :r :accessor r)
   (s :initarg :s :accessor s)))

(defun make-cell (q r &optional s)
  (make-instance 'cell :q q :r r :s (if s s (- (+ q r)))))

(defmethod format-cell ((cell cell))
  (format nil "~2d,~2d,~2d" (q cell) (r cell) (s cell)))

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
  (and (= (q a) (q b))
       (= (r a) (r b))
       (= (s a) (s b))))

(defmethod add ((a cell) (b cell))
  (make-cell (+ (q a) (q b))
                    (+ (r a) (r b))
                    (+ (s a) (s b))))

(defmethod subtract ((a cell) (b cell))
  (make-cell (- (q a) (q b))
                    (- (r a) (r b))
                    (- (s a) (s b))))

(defmethod multiply ((a cell) (k number))
  (make-cell (* (q a) k)
                    (* (r a) k)
                    (* (s a) k)))

(defmethod len ((a cell))
  (truncate (+ (abs (q a)) (abs (r a)) (abs (s a))) 2))

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
   (origin :initarg :origin :reader layout-origin)))

(defmethod cell-to-point% ((cell cell) (layout layout))
  (let* ((m (marr (orientation-f (layout-orientation layout))))
         (x (* (point-x (layout-size layout)) (+ (* (aref m 0) (q cell))
                                                 (* (aref m 1) (r cell)))))
         (y (* (point-y (layout-size layout)) (+ (* (aref m 2) (q cell))
                                                 (* (aref m 3) (r cell))))))
    (make-point (+ x (point-x (layout-origin layout)))
                (+ y (point-y (layout-origin layout))))))

(defmethod cell-to-point ((cell cell) (layout layout))
  (let ((p (v+ (layout-origin layout)
               (v* (layout-size layout)
                   (m* (vec2 (q cell) (r cell))
                       (orientation-f (layout-orientation layout)))))))
    (make-point (vx p) (vy p))))

(defmethod point-to-cell ((p point) (layout layout))
  (let* ((m (marr (orientation-b (layout-orientation layout))))
         (x (/ (- (point-x p) (point-x (layout-origin layout)))
               (point-x (layout-size layout))))
         (y (/ (- (point-y p) (point-y (layout-origin layout)))
               (point-y (layout-size layout))))
         (q (+ (* (aref m 0) x) (* (aref m 1) y)))
         (r (+ (* (aref m 2) x) (* (aref m 3) y))))
    (make-cell q r)))

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
           (let ((angle (* 2.0 pi 1/6 (+ (orientation-start-angle (layout-orientation layout))
                                         corner))))
             (values (* (vx (layout-size layout)) (cos angle))
                     (* (vy (layout-size layout)) (sin angle))))))
   (let ((center (cell-to-point cell layout)))
     (make-instance 'hexagon
                    :center center
                    :cell cell
                    :points (loop for corner from 0 below 6
                                  for (offx offy) = (multiple-value-list (corner-offset corner))
                                  collect (make-point (+ (point-x center) offx)
                                                      (+ (point-y center) offy)))))))

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

(defun make-hexagonal-grid (layout size)
  (let ((cells (loop for q from (- size) to size
                     append (loop for r from (max (- size) (- (+ q size))) to (min size (- size q))
                                  collect (make-cell q r)))))
    (make-instance 'hexagonal
                   :layout layout :cells cells)))

(defun make-triangular-grid (layout size)
  (let ((cells (loop for q from 0 to size
                     append (loop for r from 0 to (- size q)
                                  collect (make-cell q r)))))
    (make-instance 'triangular
                   :layout layout :cells cells)))

(defmethod rect ((orientation (eql *orientation-flat*)) width height)
  (loop for r from 0 to height
        for r-offset = (floor r 2)
        append (loop for q from (- r-offset) to (- width r-offset)
                     collect (make-cell q r))))

(defun make-rectangular-grid (layout width height)
  (let ((cells (loop for r from 0 to height
                     for r-offset = (floor r 2)
                     append (loop for q from (- r-offset) to (- width r-offset)
                                  collect (make-cell q r)))))
    (make-instance 'rectangular
                   :layout layout :cells cells)))

(defun make-parallelogram-grid (layout size &optional (c1 'q) (c2 'r))
  (labels ((genval (c1 c2 v1 v2)
             (if (eq c1 c2) v1 v2))
           (gencoords (v1 v2)
             (let ((diff (set-difference '(q r s) (list c1 c2))))
               (case (first diff)
                 (q (list (- (+ v1 v2)) (genval 'r c1 v1 v2) (genval 's c2 v2 v1)))
                 (r (list (genval 'q c1 v1 v2) (- (+ v1 v2)) (genval 's c2 v2 v1)))
                 (s (list (genval 'q c1 v1 v2) (genval 'r c2 v2 v1) (- (+ v1 v2))))))))
    (make-instance 'parallelogram
                   :coords (list c1 c2)
                   :layout layout
                   :cells (loop for v1 from (- size) to size
                                append (loop for v2 from (- size) to size
                                             collect (apply #'make-cell (gencoords v1 v2)))))))

(defmethod draw ((grid hexgrid) &optional (pane *standard-output*) &rest drawing-options)
  (apply #'invoke-with-drawing-options pane
         (lambda (medium)
           (loop for cell in (cells grid)
                 for hexagon = (make-hexagon (layout grid) cell)
                 do (present hexagon 'hexagon :stream medium)
                    (when (draw-coords-p grid)
                      (draw-coords hexagon medium))))
         drawing-options))

(defclass graphical-view (view) ())

(defparameter *graphical-view* (make-instance 'graphical-view))
(defparameter *width* 1024)
(defparameter *height* (+ 1024 64 32))

(defun display-canvas (frame pane)
  (window-clear pane)
  (let ((*standard-output* pane))
    (setf (layout (hexgrids-selected-grid frame)) (hexgrids-selected-layout frame))
    (draw (hexgrids-selected-grid frame))
    (setf (cursor-position (stream-text-cursor pane)) (values 20 20))
    (formatting-table (t :x-spacing '(1 :character))
      (formatting-row ()
        (formatting-cell (t :align-x :right)
          (formatting-table (t :x-spacing '(3 :character))
            (formatting-column ()
              (dolist (layout (hexgrids-grid-layouts frame))
                (formatting-cell (t :align-x :right)
                  (present layout))))))
        (formatting-cell (t :align-x :right)
          (formatting-table (t :x-spacing '(1 :character))
            (dolist (grid (hexgrids-grids frame))
              (formatting-row ()
                (if (consp grid)
                    (dolist (g grid)
                      (formatting-cell (t :align-x :right)
                        (present g)))
                    (formatting-cell (t :align-x :right)
                      (present grid)))))))))))

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

(defparameter *grids* (list (list (make-parallelogram-grid (second *layouts*) 5)
                                  (make-parallelogram-grid (first *layouts*) 5 's 'q)
                                  (make-parallelogram-grid (first *layouts*) 5 'r 's))
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
                  :initform (caar *grids*)
                  :accessor hexgrids-selected-grid)
   (selected-layout :initarg :selected-layout
                    :initform (first *layouts*)
                    :accessor hexgrids-selected-layout))
  (:panes (canvas (make-pane 'application-pane
                             :name 'canvas
                             :default-view *graphical-view*
                             :background +grey90+
                             :display-function 'display-canvas
                             :display-time t))
          (interactor :interactor))
  (:layouts (default
             (vertically (:min-height (* 5/4 *height*)
                          :max-height (* 5/4 *height*)
                          :height (* 5/4 *height*)
                          :min-width *width*
                          :max-width *width*
                          :width *width*)
               (4/5 canvas)
               (make-pane 'clime:box-adjuster-gadget)
               (1/5 interactor))))
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
    (format interactor "~2d,~2d,~2d~%" (q coords) (r coords) (s coords))))

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
