;; This file is part of Senna
;; (c) 2019 - Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(asdf:defsystem :senna
  :author "Abraham Aguilar"
  :maintainer "Abraham Aguilar"
  :license "Mozilla Public License 2.0"
  :version "0.1.0"
  :homepage "https://gitlab.com/a.aguilar/senna"
  :description "Utilities for abstract lists"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :class :package-inferred-system
  :pathname "src"
  :depends-on (:senna/all))
