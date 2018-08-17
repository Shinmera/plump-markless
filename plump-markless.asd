#|
 This file is a part of plump-markless
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem plump-markless
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An implementation of the Markless document markup standard for Plump."
  :homepage "https://Shinmera.github.io/plump-markless/"
  :bug-tracker "https://github.com/Shinmera/plump-markless/issues"
  :source-control (:git "https://github.com/Shinmera/plump-markless.git")
  :serial T
  :components ((:file "package")
               (:file "parser")
               (:file "identifier")
               (:file "directives")
               (:file "documentation"))
  :depends-on (:plump
               :cl-ppcre
               :documentation-utils))
