#|
 This file is a part of plump-markless
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.markless)

(define-directive paragraph (spanning-line-directive simple-directive)
  :tag "p"
  :matcher "<spaces [ ]*><content ![ ].*>"
  :allowed :inline)

(define-directive blockquote-header (singular-line-directive)
  :matcher "\\~ <content .+>"
  :allowed :inline)

(define-directive blockquote-body (spanning-line-directive)
  :matcher "| <content .+>"
  :tag "blockquote")

(define-directive ordered-list (spanning-line-directive simple-directive)
  :matcher "<number ~d+> <content .*>"
  :tag "ol")

(define-directive unordered-list (spanning-line-directive simple-directive)
  :matcher "- <content .*>"
  :tag "ul")

(define-directive header (singular-line-directive)
  :matcher "<level #+> <content .+>"
  :allowed :inline)

(define-directive horizontal-rule (singular-line-directive simple-directive)
  :matcher "==+"
  :tag "hr")

(define-directive code-block (guarded-line-directive simple-directive)
  :matcher "::<colons :*> <language ![ ]+>?<options .*>"
  :end     "::<colons>"
  :tag "code" :class "block")

(define-directive instruction (singular-line-directive)
  :matcher "! <instruction .*>")

(define-directive comment (singular-line-directive)
  :matcher ";+ .*")

(define-directive embed (singular-line-directive)
  :matcher "\\[<type ![ ]*> <target ![ ]*>( <parameter ![ ]*>)*\\]")

(define-directive footnote (singular-line-directive)
  :matcher "\\[<number ~n+>\\] <content .+>")

(define-directive bold (surrounding-inline-directive simple-directive)
  :matcher "<start [*]+><content ![*].*><start>"
  :tag "em" :class "bold")

(define-directive italic (surrounding-inline-directive simple-directive)
  :matcher "<start [/]+><content ![/].*><start>"
  :tag "em" :class "italic")

(define-directive underline (surrounding-inline-directive simple-directive)
  :matcher "<start [_]+><content ![_].*><start>"
  :tag "em" :class "underline")

(define-directive strikethrough (surrounding-inline-directive simple-directive)
  :matcher "\\<<start [-]+><content ![-].*><start>\\>"
  :tag "span" :class "strikethrough")

(define-directive code (surrounding-inline-directive simple-directive)
  :matcher "<start [`]+><content ![`].*><start>"
  :tag "code")

(define-directive dash (entity-inline-directive)
  :entity (code-char #x2014))

(define-directive subtext (surrounding-inline-directive simple-directive)
  :matcher "v<start [(]+><content ![(].*><end [)]+>"
  :tag "span" :class "subtext")

(define-directive supertext (surrounding-inline-directive simple-directive)
  :matcher "^<start [(]+><content ![(].*><end [)]+>"
  :tag "span" :class "supertext")

(define-directive url (inline-directive simple-directive)
  :matcher "<target ~a(~w|[+-.])*://(~w|[$-_.+!*'()&+,/:;=?@%])+>"
  :tag "a")

(define-directive compound (compound-inline-directive)
  :matcher "(<start [\"]+><content ![\"].*><start>|<content ![ (]+>)\\((in|to) <option .*>(, <option .*>)*\\)")

(define-directive footnote-reference (surrounding-inline-directive simple-directive)
  :matcher "\\[<target ~n+>\\]"
  :tag "a" :class "footnote-reference")
