;;;; clim-hexgrids.lisp

(in-package #:clim-hexgrids)

(defclass graphical-view (view) ())

(defparameter *graphical-view* (make-instance 'graphical-view))
(defparameter *width* 1280)
(defparameter *height* 1536)
(defparameter *coords-style* (make-text-style :sans-serif :roman :tiny))

(defun display-canvas (frame pane)
  (window-clear pane)
  (let ((*standard-output* pane)
        (layout (hexgrids-selected-layout frame)))
    (with-bounding-rectangle* (x0 y0 x1 y1) pane
      (setf (layout-origin layout) (vec2 (/ (- x1 x0) 2) (/ (- y1 y0) 2))))
    (draw (hexgrids-selected-grid frame) pane :text-style *coords-style*)))

(defparameter *grids* '((parallelogram (q r)) (parallelogram (s q))
                        (parallelogram (r s)) hexagonal triangular rectangular))

(defun make-selected-grid (type layout &optional frame)
  (let ((type (or type (gadget-value (find-pane-named frame 'grids)))))
    (make-hexgrid (or layout (hexgrids-selected-layout frame))
                  (if (listp type) (first type) type)
                  5 5 (and (listp type) (second type)))))

(defun change-grid (gadget value)
  (declare (ignore gadget))
  (let ((frame *application-frame*))
    (execute-frame-command frame (list 'com-change-grid
                                       (make-selected-grid value nil frame)))))

(defun change-layout (gadget value)
  (declare (ignore gadget))
  (let ((frame *application-frame*))
    (setf (hexgrids-selected-layout *application-frame*) (find value (hexgrids-grid-layouts frame)
                                                               :key #'layout-name))
    (execute-frame-command frame (list 'com-change-grid
                                       (make-selected-grid nil nil frame)))))

(define-application-frame hexgrids ()
  ((grid-layouts :initarg :hex-layouts
                 :initform (list (make-instance 'layout
                                                :name 'flat
                                                :orientation +orientation-flat+
                                                :size (vec2 40.0 40.0)
                                                :origin (vec2 (/ *width* 2)
                                                              (/ *height* 2)))
                                 (make-instance 'layout
                                                :name 'pointy
                                                :orientation +orientation-pointy+
                                                :size (vec2 40.0 40.0)
                                                :origin (vec2 (/ *width* 2)
                                                              (/ *height* 2))))
                 :reader hexgrids-grid-layouts)
   (grids :initarg :grids
          :initform *grids*
          :reader hexgrids-grids)
   (selected-grid :initarg :selected-grid
                  :accessor hexgrids-selected-grid)
   (selected-layout :initarg :selected-layout
                    :accessor hexgrids-selected-layout))
  (:panes (grids (make-pane 'list-pane
                            :name 'grids
                            :value (first *grids*)
                            :name-key (lambda (x)
                                        (format nil "~:[~:(~a~)~;~{~:(~a~) ~{~a~^ & ~a~}~}~]"
                                                (listp x) x))
                            :mode :exclusive
                            :items *grids*
                            :value-changed-callback 'change-grid
                            :test 'eq))
          (layouts (make-pane 'list-pane
                              :name 'layouts
                              :value 'flat
                              :name-key 'string-capitalize
                              :mode :exclusive
                              :items '(flat pointy)
                              :value-changed-callback 'change-layout
                              :test 'eq))
          (canvas (make-pane 'application-pane
                             :name 'canvas
                             :default-view *graphical-view*
                             :display-function 'display-canvas
                             :display-time t))
          (interactor :interactor))
  (:layouts (default
             (vertically (:min-height *height* :max-height *height*
                          :min-width *width* :max-width *width*)
               (12/16 (horizontally (:min-width (* 5/4 *width*) :max-width (* 5/4 *width*))
                      (1/5 (vertically ()
                             (labelling (:label "Type of grid") grids)
                             (labelling (:label "Type of layout") layouts)))
                      (4/5 (scrolling () canvas))))
               (make-pane 'clime:box-adjuster-gadget)
               (4/16 interactor))))
  (:menu-bar t)
  (:pointer-documentation t))

(defmethod run-frame-top-level :before ((frame hexgrids) &key &allow-other-keys)
  (setf (hexgrids-selected-layout frame) (first (hexgrids-grid-layouts frame))
        (hexgrids-selected-grid frame) (make-selected-grid (first (hexgrids-grids frame))
                                                           (first (hexgrids-grid-layouts frame)))))

(define-presentation-method present (obj (type hexagon) stream (view graphical-view)
                                         &key &allow-other-keys)
  (let ((cell (hexagon-cell obj)))
    (draw obj stream :ink (if (selected-p cell) +cyan+
                              (if (and (zerop (q cell)) (zerop (r cell)))
                                  +gray90+ +white+))
                     :filled t :line-thickness 3.0))
  (draw obj stream :ink +black+ :filled nil :line-thickness 3.0))

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

(define-hexgrids-command (com-show-cell-neighbors :name "Show cell neighbors") ((hexagon 'hexagon))

  (let ((frame *application-frame*))
    (dolist (cell (cells (hexgrids-selected-grid frame)))
      (setf (selected-p cell) nil))
    (dolist (cell (neighbors hexagon))
      (setf (selected-p cell) t))
    (setf (pane-needs-redisplay (find-pane-named frame 'canvas)) t)))

(define-hexgrids-command (com-distance-between-cells :name "Calculate distance between cells")
    ((a 'hexagon) (b 'hexagon))
  (let ((frame *application-frame*))
    (format (frame-query-io frame) "~D~%" (distance (hexagon-cell a) (hexagon-cell b)))))

(define-hexgrids-command (com-cells-at-the-same-distance :name "Cells at the same distance")
    ((a 'hexagon) (b 'hexagon))
  (let* ((frame *application-frame*)
         (cell (hexagon-cell a))
         (distance (distance cell (hexagon-cell b))))
    (dolist (cell (cells (hexgrids-selected-grid frame)))
      (setf (selected-p cell) nil))
    (dolist (cell (remove-if (lambda (b)
                               (/= (distance cell b) distance))
                             (cells (hexgrids-selected-grid frame))))
      (setf (selected-p cell) t))
    (setf (pane-needs-redisplay (find-pane-named frame 'canvas)) t)))

(define-presentation-to-command-translator show-cell-neighbors
    (hexagon com-show-cell-neighbors hexgrids
     :gesture :select
     :documentation ""
     :echo nil)
    (object)
  (list object))

#|(define-presentation-to-command-translator show-hexagon
    (hexagon com-show-details hexgrids
     :gesture :select
     :documentation "Show details"
     :echo nil)
    (object)
  (list object))|#

(defun start ()
  (find-application-frame 'hexgrids))
