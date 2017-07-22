#|
 This file is a part of plump-markless
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.markless)

(defun parse-name ()
  (let ((out (with-output-to-string (out)
               (loop for char = (consume)
                     do (cond ((not char) (return))
                              ((not (find char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
                               (unread) (return))
                              (T (write-char char out)))))))
    (when (string/= out "") out)))

(defun parse-string ()
  (let ((out (with-output-to-string (out)
               (loop for char = (consume)
                     do (case char
                          ((NIL) (return))
                          (#\\ (write-char (consume) out))
                          ((#\( #\) #\~ #\[ #\] #\. #\! #\| #\< #\> #\+ #\* #\?)
                           (unread) (return))
                          (T (write-char char out)))))))
    (when (string/= out "")
      out)))

(defun parse-char-class ()
  (when (eql (peek) #\~)
    (advance)
    (list :char-class (consume))))

(defun parse-some-characters ()
  (when (eql (peek) #\[)
    (advance)
    (prog1 (list :some-characters (consume-until (matcher-character #\])))
      (advance))))

(defun parse-any-character ()
  (when (eql (peek) #\.)
    (advance)
    (list :any-character)))

(defun parse-not ()
  (when (eql (peek) #\!)
    (advance)
    (list :not (parse-rule))))

(defun parse-binding ()
  (when (eql (peek) #\<)
    (advance)
    (let ((name (parse-name)))
      (ecase (consume)
        (#\> (list :binding-reference name))
        (#\  (prog1 (list :binding name (parse-rule))
               (ecase (consume) (#\>))))))))

(defun parse-identifier-reference ()
  (when (eql (peek) #\{)
    (advance)
    (let ((name (parse-name)))
      (ecase (consume)
        (#\} (list :identifier-reference name))))))

(defun parse-matcher ()
  (or (parse-compound)
      (parse-char-class)
      (parse-some-characters)
      (parse-any-character)
      (parse-not)
      (parse-binding)
      (parse-identifier-reference)
      (parse-string)
      (unless (find (peek) "?*+|>)")
        (parse-rule))))

(defun parse-either ()
  (let ((left (parse-rule)))
    (cond ((not left) NIL)
          ((eql (peek) #\|)
           (advance)
           (let ((right (parse-one-or-none)))
             (unless right (error "Right hand side to either missing."))
             (list :either left right)))
          (T left))))

(defun parse-quantifier ()
  (let ((rule (parse-matcher)))
    (case (peek)
      (#\? (advance) (list :one-or-none rule))
      (#\* (advance) (list :none-or-more rule))
      (#\+ (advance) (list :one-or-more rule))
      (T rule))))

(defun parse-compound ()
  (when (eql (peek) #\()
    (advance)
    (let ((matchers (loop for match = (or (parse-compound) (parse-quantifier))
                          while match collect match until (eql (peek) #\)))))
      (ecase (consume)
        (#\) (if (cdr matchers)
                 (list* :and matchers)
                 (first matchers)))))))

(defun parse-rule ()
  (let ((matchers (loop for rule = (parse-quantifier)
                        while rule collect rule until (<= *length* *index*))))
    (if (cdr matchers)
        (list* :and matchers)
        (first matchers))))

(defun parse-identifier (identifier)
  (with-lexer-environment (identifier)
    (parse-rule)))

(defun %compile-string (string)
  `(let* ((string ,string)
          (length (length string)))
     (when (and (<= length (- *length* *index*))
                (string= string *string* :start2 *index* :end2 (+ *index* length)))
       (advance-n length) T)))

(defun %compile (identifier)
  (etypecase identifier
    (string
     (%compile-string identifier))
    (cons
     (ecase (first identifier)
       (:and
        `(let ((start *index*))
           (if (and ,@(mapcar #'%compile (rest identifier)))
               T
               (progn (setf *index* start) NIL))))
       (:either
        `(or ,@(mapcar #'%compile (rest identifier))))
       (:not
        `(let ((start *index*))
           (if ,(%compile (second identifier))
               (progn (setf *index* start) NIL)
               (progn (incf *index*) T))))
       (:one-or-none
        `(or ,(%compile (second identifier))
             T))
       (:none-or-more
        `(loop while ,(%compile (second identifier))
               finally (return T)))
       (:one-or-more
        `(when ,(%compile (second identifier))
           (loop while ,(%compile (second identifier)))
           T))
       (:any-character
        `(consume))
       (:some-characters
        `(if (find (consume) ,(second identifier))
             T
             (progn (unread) NIL)))
       (:char-class
        `(if (find (consume) ,(case (second identifier)
                                (#\a "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
                                (#\n "0123456789")
                                (#\w "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
                                (#\_ (with-output-to-string (out)
                                       (loop for c across #(#x0009 #x0020 #x00A0 #x2000 #x2001 #x2002
                                                            #x2003 #x2004 #x2005 #x2006 #x2007 #x2008
                                                            #x2009 #x200A #x202F #x205F #x3000 #x180E
                                                            #x200B #x200C #x200D #x2060 #xFEFF)
                                             do (write-char (code-char c) out))))))
             T
             (progn (unread) NIL)))
       (:binding
        `(setf (gethash ,(second identifier) bindings)
               (let ((start *index*))
                 (when ,(%compile (third identifier))
                   (subseq *string* start *index*)))))
       (:binding-reference
        (%compile-string `(gethash ,(second identifier) bindings)))
       (:identifier-reference)))))

(defun compile-identifier (identifier)
  (let ((identifier (etypecase identifier
                      (string (parse-identifier identifier))
                      (cons identifier))))
    `(lambda ()
       (let ((start *index*)
             (bindings (make-hash-table :test 'equal)))
         (values (when ,(%compile identifier)
                   (list start *index*))
                 bindings)))))
