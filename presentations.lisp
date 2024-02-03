#|

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

|#
