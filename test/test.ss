#lang scheme

(require scheme/foreign)
(unsafe!)

(define lib (ffi-lib "libpandoc"))

(define pandoc
  (get-ffi-obj "pandoc" lib
	       (_fun _string _string _string -> _string/utf-8)))

(define (markdown->html x)
  (pandoc "markdown" "html" x))

(define (file->string f)
  (let([sp (open-output-string)])
   (call-with-input-file f
     (lambda (fp) (copy-port fp sp)))
   (get-output-string sp)))

(define (main)
  (let* ([filename (vector-ref (current-command-line-arguments) 0)]
	 [input-str (file->string filename)]
	 [output-str (markdown->html input-str)])
    (display output-str)))

(main)
  
	       
