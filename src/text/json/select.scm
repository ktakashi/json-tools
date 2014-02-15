;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/select.scm - JSONSelect library
;;;
;;;   Copyright (c) 2014  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

#!r6rs
(library (text json select)
    (export json:select)
    (import (rnrs)
	    (only (srfi :1) reverse! filter-map)
	    (text json tools)
	    (text json select parser))

  (define type-predicates
    `((number  . ,json:number?)
      (string  . ,json:string?)
      (boolean . ,json:boolean?)
      (null    . ,json:null?)
      (object  . ,json:node?)
      (array   . ,json:array?)))

  ;; should we make this configurable?
  (define (nth-function n last? pred?)
    (lambda (node)
      (let ((set ((json:descendant-or-self json:array?) node)))
	(if (json:empty-nodeset? set)
	    set
	    (let ((set (json:nodeset-set set)))
	      (apply json:nodeset
		     (filter-map 
		      (lambda (node)
			;; unlikely it's not starting 0 but 1
			(let ((e (if last?
				     (json:array-ref node 
				      (- (json:array-length node) n))
				     (json:array-ref node (- n 1)))))
			  (if pred?
			      (and (pred? e) e)
			      e)))
		      set)))))))
  (define (only-child pred?)
    (lambda (node) 
      ((json:descendant-or-self 
	(lambda (node) 
	  (and (not (json:array? node))
	       (not (json:map-entry? node))
	       (or pred? (pred? node)))))
       node)))

  (define (root nested?)
    (lambda (root)
      (lambda (node)
	(if (eq? root node)
	    (json:as-nodeset node)
	    (json:empty-nodeset)))))

  (define (or-select selectors)
    (lambda (node)
      ;; union it
      (json:union-nodeset
       (map (lambda (selector) (selector node)) selectors))))

  (define (empty-array? node)
    (and (json:array? node) (zero? (json:array-length node))))

  (define (has selector)
    (lambda (node)
      ((json:descendant-or-self 
	(lambda (node) (not (json:empty-nodeset? (selector node)))))
       node)))

  (define (val value)
    (lambda (node)
      ((json:descendant-or-self 
	(lambda (node) 
	  (or (equal? value (json:node-value node))
	      (and (json:map-entry? node)
		   (equal? value 
			   (json:node-value (json:map-entry-value node)))))))
       node)))
  
  ;; construct select
  (define (json:select select)
    (let ((rules (if (string? select) 
		     (json:parse-selector (open-string-input-port select))
		     select)))
      (define (key-name=? name pred?)
	(lambda (node)
	  (and (json:map-entry? node)
	       (or (not pred?) (pred? (json:map-entry-key node)))
	       (equal? name (json:node-value (json:map-entry-key node))))))

      (let loop ((rules rules) (converters '()) (nested? #f))
	(cond ((null? rules)
	       (if nested?
		   (reverse! converters)
		   (lambda (node)
		     (define root-node (json:as-nodeset node))
		     (let loop ((nodeset root-node)
				(conv (reverse! converters)))
		       (cond ((null? conv) ;; finish
			      nodeset)
			     ;; need root-node
			     ((pair? (car conv))
			      (let ((c (caar conv)))
				(loop ((c root-node) nodeset)
				      (cdr conv))))
			     (else
			      (loop ((car conv) nodeset)
				    (cdr conv))))))))
	      ;; or
	      ((eq? (car rules) 'or)
	       (let loop2 ((groups (cdr rules)) (r '()))
		 (if (null? groups)
		     (loop '() 
			   (cons (or-select (reverse! r)) converters)
			   nested?)
		     (loop2 (cdr groups) 
			    (cons (loop (car groups) '() #f) r)))))
	      ((string? (car rules)) ;; class
	       (loop (cdr rules)
		     (cons (json:descendant-or-self 
			    (key-name=? (car rules) nested?))
			   converters)
		     nested?))
	      ;; (class expr)
	      ((and (pair? (car rules))
		    (string? (caar rules)))
	       (let ((cls (loop (list (caar rules)) '() #f))
		     (exp (loop (cdar rules) '() #f)))
		 ;; combine it
		 (loop (cdr rules)
		       (cons (lambda (node)
			       (let ((set (cls node)))
				 (json:union-nodeset
				  (map (lambda (node) (exp node))
				       (json:nodeset-set set)))))
			     converters)
		       nested?)))
	      ;; *
	      ((eq? (car rules) '*)
	       (loop (cdr rules)
		     (cons (json:descendant-or-self json:node?) converters)
		     nested?))
	      ;; >>
	      ((eq? (car rules) '>>)
	       (loop (cdr rules)
		     (cons (json:descendant json:node?) converters)
		     nested?))
	      ;; >
	      ((eq? (car rules) '>)
	       (loop (cdr rules)
		     (cons (json:child json:node?) converters)
		     nested?))
	      ;; ~
	      ((eq? (car rules) '~)
	       (loop (cdr rules)
		     (cons (list (json:sibling json:node?)) converters)
		     nested?))
	      ;; simple types
	      ((assq (car rules) type-predicates)
	       => (lambda (slot)
		    (loop (cdr rules)
			  (cons (json:descendant (cdr slot))
				converters)
			  nested?)))
	      ;; (type somethig)
	      ((and (pair? (car rules))
		    (assq (caar rules) type-predicates))
	       => (lambda (slot)
		    (loop (cdr rules)
			  `(,@(loop (cdar rules) '() (cdr slot))
			    ,@converters)
			  nested?)))
	      ;; first-child
	      ((eq? (car rules) 'first-child)
	       (loop (cdr rules)
		     (cons (nth-function 1 #f nested?) converters)
		     nested?))
	      ((eq? (car rules) 'last-child)
	       (loop (cdr rules)
		     (cons (nth-function 1 #t nested?) converters)
		     nested?))
	      ((eq? (car rules) 'only-child)
	       (loop (cdr rules)
		     (cons (only-child nested?) converters)
		     nested?))
	      ;; (nth-child index)
	      ((and (pair? (car rules))
		    (eq? (caar rules) 'nth-child))
	       (loop (cdr rules) 
		     (cons (nth-function (cadar rules) #f nested?) converters)
		     nested?))
	      ;; (nth-last-child index)
	      ((and (pair? (car rules))
		    (eq? (caar rules) 'nth-last-child))
	       (loop (cdr rules) 
		     (cons (nth-function (cadar rules) #t  nested?) converters)
		     nested?))
	      ((eq? (car rules) 'root)
	       (loop (cdr rules)
		     (cons (list (root nested?)) converters)
		     nested?))
	      ((eq? (car rules) 'empty)
	       (loop (cdr rules)
		     (cons (json:descendant-or-self empty-array?)
			   converters)
		     nested?))
	      ;; level 3 functins
	      ;; has
	      ((and (pair? (car rules))
		    (eq? (caar rules) 'has))
	       (loop (cdr rules)
		     (cons (has (loop (cadar rules) '() #f)) converters)
		     nested?))
	      ;; val
	      ((and (pair? (car rules))
		    (eq? (caar rules) 'val))
	       (loop (cdr rules)
		     (cons (val (cadar rules)) converters)
		     nested?))
	      (else
	       (error 'json:select "not supported" rules select))))))
  )