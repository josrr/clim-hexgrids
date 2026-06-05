;;;; clim-hexgrids.asd

(asdf:defsystem #:clim-hexgrids
  :description "Implementation of Hex Grids using McCLim"
  :author "José Miguel Ángel Ronquillo Rivera <jose@rufina.link>"
  :license "GPL Ver. 3"
  :version "0.0.1"
  :serial t
  :depends-on (#:mcclim)
  :components ((:file "package")
               (:file "operations")
               (:file "hexgrid")
               (:file "clim-hexgrids")))
