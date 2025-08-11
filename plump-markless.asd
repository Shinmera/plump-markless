(asdf:defsystem plump-markless
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An implementation of the Markless document markup standard for Plump."
  :homepage "https://shinmera.com/docs/plump-markless/"
  :bug-tracker "https://shinmera.com/project/plump-markless/issues"
  :source-control (:git "https://shinmera.com/project/plump-markless.git")
  :serial T
  :components ((:file "package")
               (:file "parser")
               (:file "identifier")
               (:file "directives")
               (:file "documentation"))
  :depends-on (:plump
               :cl-ppcre
               :documentation-utils))
