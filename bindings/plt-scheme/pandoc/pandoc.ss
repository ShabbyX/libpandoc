#lang scheme
;; A simple wrapper for PANDOC.
;; Author: Anton Tayanovskyy <name.surname@gmail.com>

(require srfi/1
	 (rename-in scheme/foreign [-> -->])
	 (prefix-in js: (planet dherman/json:1:2/json)))

(unsafe!)

(define pandoc-formats
  #(markdown 
    rst html latex s5 docbook odt context texinfo man mediawiki rtf))

(define pandoc-format->number  
  (let ([hash (make-immutable-hash 
	       (zip 
		(vector->list pandoc-formats)
		(iota (vector-length pandoc-formats))))])
    (lambda (fmt) 
      (car (hash-ref hash fmt)))))

(define pandoc-input-format/c
  (apply symbols '(html markdown latex rst)))

(define pandoc-output-format/c 
  (apply symbols (vector->list pandoc-formats)))

(define pandoc-c
  (get-ffi-obj "pandoc" (ffi-lib "libpandoc")
	       (_fun _int _string _int _string _string --> _string)))

(provide/contract
 [pandoc 
  (->* 
   (string?)
   (#:input-format   pandoc-input-format/c		     
    #:output-format  pandoc-output-format/c
    #:parser-state   js:json?
    #:writer-options js:json?)
   string?)])

(define (json->string json) 
  (let ([s (open-output-string)])
    (js:write json s)
    (get-output-string s)))

(define (pandoc text
		#:input-format   [input-format  'markdown]
		#:output-format  [output-format 'html]
		#:parser-state   [parser-state   (void)]
		#:writer-options [writer-options (void)])

  (pandoc-c (pandoc-format->number input-format)
	    (json->string parser-state)
	    (pandoc-format->number output-format)
	    (json->string writer-options)
	    text))
