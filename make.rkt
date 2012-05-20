;; make.rkt
;;
;; This script performs template expansion to generate website pages.
;; It is written in Racket, tested with Racket 5.2.1
;;
;; See http://racket-lang.org

#lang racket

(require make xml)

;; read-xml-file :: string -> document
;;
;; Reads an XML file.
(define (read-xml-file file)
  (call-with-input-file file read-xml))

;; write-html-file :: string * content -> unit
;;
;; Writes XML file taking some care of HTML tags.
(define (write-html-file file content)
  (call-with-output-file file
    #:exists 'replace
    (lambda (port)
      (display "<!DOCTYPE html>\n" port)
      (parameterize
       ((empty-tag-shorthand html-empty-tags))
       (write-xml/content content port)))))

;; get-attr :: symbol * element -> string | #f
;;
;; Finds an XML attribute by name on a given element.
(define (get-attr name element)
  (let ((e
         (findf (lambda (attr)
                  (equal? (attribute-name attr) name))
                (element-attributes element))))
    (if (eq? e #f)
        #f
        (attribute-value e))))

;; has-attr? :: symbol * element -> bool
;;
;; Tests if the element has an attribute.
(define (has-attr? name element)
  (if (eq? (get-attr name element) #f) #f #t))

;; read-page :: string -> (symbol * content list) hash
;;
;; Reads an XML page that defines content blocks like this:
;;
;; <page>
;;  <block id="">..</block>
;;  ..
;; </page>
(define (read-page file)
  (let* ((doc (read-xml-file file))
         (root (document-element doc))
         (blocks (element-content root))
         (table (make-hash)))
    (for-each (lambda (block)
                (when (element? block)
                      (hash-set! table
                                 (string->symbol (get-attr 'id block))
                                 (element-content block))))
              blocks)
    table))

;; un-list :: 'a list -> 'a
;;
;; Takes the head of a singleton list, fails if the list is not
;; singleton.
(define (un-list x)
  (cond ((and (list? x)
              (not (null? x))
              (null? (cdr x)))
         (car x))
        (else (error "not a singleton list"))))

;; apply-template :: doc * (symbol * content list) hash -> element
;;
;; Applies a template by replacing elements marked with data-replace
;; and replacing the contents of elements marked with data-hole
;; attributes.
(define (apply-template doc page)
                                        ; Functinal update of the
                                        ; children of anXML element.
  (define (map-element children elem)
    (make-element
     (source-start elem)
     (source-stop elem)
     (element-name elem)
     (element-attributes elem)
     children))
                                        ; Converts an XML node.
  (define (conv-node node)
    (cond ((element? node) (conv-element node))
          (else (list node))))
                                        ; Converts an XML element.
  (define (conv-element elem)
    (cond ((has-attr? 'data-replace elem)
           (hash-ref page
                     (string->symbol
                      (get-attr 'data-replace elem))))
          ((has-attr? 'data-hole elem)
           (list
            (map-element
             (hash-ref page
                       (string->symbol
                        (get-attr 'data-hole elem)))
             elem)))
          (else
           (list
            (map-element
             (append-map conv-node (element-content elem))
             elem)))))
  (un-list
   (conv-element
    (document-element doc))))

;; make-html-page :: string * string * string -> unit
;;
;; Applies a template transformation and generates HTML.
(define (make-html-page target-file template-file source-file)
  (write-html-file target-file
                   (apply-template
                    (read-xml-file template-file)
                    (read-page source-file))))

;; pages :: string list
;;
;; Lists the name of html pages that have to be generated.
(define pages
  (list
   "bulletins"
   "calendar"
   "contact"
   "beliefs"
   "index"
   "our-story"
   "tumblr"
   "welcome"
   "worship"))

;; main :: unit -> unit
;;
;; Generates all pages using make.
(define (main)
  (let*
      ((++ string-append)
       (page
        (lambda (name)
          (let ((html-file (++ name ".html"))
                (xml-file (++ name ".xml")))
            `(,html-file
              ("template.html" ,xml-file)
              ,(lambda ()
                 (make-html-page
                  html-file
                  "template.html"
                  xml-file)))))))
    (make/proc
     (cons
      (list "all-pages"
            (map (lambda (n) (++ n ".html")) pages))
      (map page pages)))))

(main)
