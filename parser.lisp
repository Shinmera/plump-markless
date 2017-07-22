#|
 This file is a part of plump-markless
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.markless)

(defvar *directives* (make-hash-table :test 'equalp))
(defvar *line-break-mode*)
(defvar *disabled-directives*)
(defvar *root*)

(defun directive (name)
  (or (gethash name *directives*)
      (error "No such directive ~s" name)))

(defun (setf directive) (value name)
  (setf (gethash name *directives*) value))

(defun remove-directive (name)
  (remhash name *directives*))

(defmacro define-directive (name superclasses  &body initargs)
  `(progn (defclass ,name ,superclasses
            ())
          (setf (directive ,(string-downcase name))
                (make-instance ',name ,@initargs))))

(defclass directive ()
  ())

(defgeneric directive-matches-p (directive))
(defgeneric process (directive))

(defclass line-directive (directive)
  ())

(defun line-directives ()
  (loop for v being the hash-values of *directives*
        when (typep v 'line-directive) collect v))

(defclass singular-line-directive (line-directive)
  ())

(defclass spanning-line-directive (line-directive)
  ())

(defclass guarded-line-directive (line-directive)
  ())

(defclass inline-directive (directive)
  ())

(defun inline-directives ()
  (loop for v being the hash-values of *directives*
        when (typep v 'inline-directive) collect v))

(defclass surrounding-inline-directive (inline-directive)
  ())

(defclass entity-inline-directive (inline-directive)
  ((entity :initarg :entity :accessor entity)))

(defmethod process :after ((directive entity-inline-directive))
  (insert (entity directive)))

(defclass compound-inline-directive (inline-directive)
  ())

(defclass simple-directive (directive)
  ((tag :initarg :tag :accessor tag)
   (class :initarg :class :initform "" :accessor class)))

(defmethod process :around ((directive simple-directive))
  (with-textual-component (plump-dom:make-element *root* (tag directive))
    (setf (plump-dom:attribute *root* "class") (class directive))
    (call-next-method)))

(defun parse (thing)
  (etypecase thing
    (string
     (%parse thing))
    (stream
     (%parse (with-output-to-string (out)
               (loop with buffer = (make-string 4096)
                     for read = (read-sequence buffer thing)
                     do (write-sequence buffer out)
                     while (= read 4096)))))
    (pathname
     (with-open-file (thing stream :direction :input
                                   :element-type 'character)
       (parse stream)))))

(defun line-beginning-p ()
  (or (= *index* 0)
      (eql (peek -1) #\Linefeed)))

(defun line-ending-p ()
  (or (= *index* *length*)
      (eql (peek) #\Linefeed)))

(defun insert (char)
  (let ((string (plump-dom:text (aref (plump-dom:children *root*))
                                (1- (fill-pointer (plump-dom:children *root*))))))
    (vector-push-extend char string)))

(defun make-text-node (parent)
  (plump-dom:make-text-node parent (make-array 0 :element-type 'character :adjustable T :fill-pointer T)))

(defmacro with-textual-component (component &body body)
  ;; This way we ensure that the last node in the root is always
  ;; a text node. Makes it fast to insert text.
  `(let ((*root* ,c))
     (plump-dom:append-child *root* (make-text-node *root*))
     (catch *root*
       ,@body)
     (plump-dom:append-child *root* (make-text-node *root*))))

(defun end (component)
  (throw component component))

(defun %parse (in)
  (let ((*disabled-directives* ())
        (*line-break-mode* :unescaped)
        (*root* (plump:make-root)))
    (with-lexer-environment (in)
      (loop while (< *index* *length*)
            do (loop for directive in (line-directives)
                     do (when (and (not (typep directive 'paragraph))
                                   (not (find directive *disabled-directives*))
                                   (directive-matches-p directive))
                          (return (process directive)))
                     finally (process (directive "paragraph"))))
      (block NIL
        (tagbody
         step-0 (when (<= *length* *index*)
                  (return NIL))
         step-1 (when (eql (peek) #\\)
                  (advance)
                  (unless (eql (peek) #\Linefeed)
                    (insert (consume))
                    (go step-0)))
         step-2 (when (component-beginning-p)
                  (loop for directive in (line-directives)
                        do (when (and (not (find directive *disabled-directives*))
                                      (directive-matches-p directive))
                             (return (process directive)))
                        finally (process (directive "paragraph"))))
         step-3 (when (line-ending-p)
                  (ecase *line-break-mode*
                    (:always
                     (insert #\Linefeed))
                    (:unescaped
                     (when (or (= *index* 0) (char/= (aref *string* (1- *index*)) #\\))
                       (insert #\Linefeed)))
                    (:escaped
                     (when (eql (aref *string* (1- *index*)) #\\)
                       (insert #\Linefeed))))
                  (advance)
                  (go step-0))
         step-4 (loop for directive in (inline-directives)
                      do (when (and (not (find directive *disabled-directives*))
                                    (directive-matches-p directive))
                           (process directive)
                           (go step-0)))
         step-5 (insert (consume)))))))
