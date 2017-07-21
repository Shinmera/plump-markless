#|
 This file is a part of plump-markless
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem plump-markless
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An implementation of the Markless document markup standard for Plump."
  :homepage "https://github.com/Shinmera/plump-markless"
  :serial T
  :components ((:file "package")
               (:file "parser")
               (:file "documentation"))
  :depends-on (:plump
               :documentation-utils))
