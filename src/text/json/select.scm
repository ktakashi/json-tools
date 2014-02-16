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
	    (only (srfi :13) string-contains)
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
  (define (nth-function n last?)
    (lambda (node)
      (let ((set ((json:descendant-or-self json:array?) node)))
	(if (json:empty-nodeset? set)
	    set
	    (let ((set (json:nodeset-set set)))
	      (apply json:nodeset
		     (filter-map 
		      (lambda (node)
			;; unlikely it's not starting 0 but 1
			(if last?
			    (json:array-ref node 
					    (- (json:array-length node) n))
			    (json:array-ref node (- n 1))))
		      set)))))))
  (define only-child
    (lambda (node) 
      ((json:descendant-or-self 
	(lambda (node) 
	  (and (not (json:array? node))
	       (not (json:map-entry? node)))))
       node)))

  (define root
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
  
  (define (contains value)
    (lambda (node)
      ((json:descendant-or-self 
	(lambda (node) 
	  (or (and (string? (json:node-value node))
		   (string-contains (json:node-value node) value))
	      (and (json:map-entry? node)
		   (string-contains 
		    (json:node-value (json:map-entry-value node))
		    value)))))
       node)))

  ;; construct select
  (define (json:select select)
    (let ((rules (if (string? select) 
		     (json:parse-selector (open-string-input-port select))
		     select)))
      (define (key-name=? name)
	(lambda (node)
	  (and (json:map-entry? node)
	       (equal? name (json:node-value (json:map-entry-key node))))))

      (define (type-pred pred?)
	(lambda (node)
	  (if (json:map-entry? node)
	      (if (pred? (json:map-entry-value node)) 
		  (json:nodeset node)
		  (json:empty-nodeset))
	      (if (pred? node)
		  (json:nodeset node)
		  (json:empty-nodeset)))))

      (let loop ((rules rules) (converters '()))
	(cond ((null? rules)
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
				(cdr conv)))))))
	      ;; or
	      ((eq? (car rules) 'or)
	       (let loop2 ((groups (cdr rules)) (r '()))
		 (if (null? groups)
		     (loop '() 
			   (cons (or-select (reverse! r)) converters))
		     (loop2 (cdr groups) 
			    (cons (loop (car groups) '()) r)))))
	      ((string? (car rules)) ;; class
	       (loop (cdr rules)
		     (cons (json:descendant-or-self 
			    (key-name=? (car rules)))
			   converters)))
	      ;; (class expr)
	      ((and (pair? (car rules))
		    (string? (caar rules)))
	       (let ((cls (loop (list (caar rules)) '()))
		     (exp (loop (cdar rules) '())))
		 ;; combine it
		 (loop (cdr rules)
		       (cons (lambda (node)
			       (let ((set (cls node)))
				 (json:union-nodeset
				  (map (lambda (node) (exp node))
				       (json:nodeset-set set)))))
			     converters))))
	      ;; *
	      ((eq? (car rules) '*)
	       (loop (cdr rules)
		     (cons (json:descendant-or-self json:node?) converters)))
	      ;; >>
	      ((eq? (car rules) '>>)
	       (loop (cdr rules)
		     (cons (json:descendant json:node?) converters)))
	      ;; >
	      ((eq? (car rules) '>)
	       (loop (cdr rules)
		     (cons (json:child json:node?) converters)))
	      ;; ~
	      ((eq? (car rules) '~)
	       (loop (cdr rules)
		     (cons (list (json:sibling-and-self json:node?))
			   converters)))
	      ;; simple types
	      ((assq (car rules) type-predicates)
	       => (lambda (slot)
		    (loop (cdr rules)
			  (cons (json:descendant-or-self (cdr slot))
				converters))))
	      ;; (type somethig)
	      ((and (pair? (car rules))
		    (assq (caar rules) type-predicates))
	       => (lambda (slot)
		    (let ((typ (type-pred (cdr slot)))
			  (els (loop (cdar rules) '())))
		      (loop (cdr rules)
			    (cons (lambda (node)
				    (let ((set (els node)))
				      (json:union-nodeset
				       (map (lambda (node) (typ node))
					    (json:nodeset-set set)))))
				  converters)))))
	      ;; first-child
	      ((eq? (car rules) 'first-child)
	       (loop (cdr rules)
		     (cons (nth-function 1 #f) converters)))
	      ((eq? (car rules) 'last-child)
	       (loop (cdr rules)
		     (cons (nth-function 1 #t) converters)))
	      ((eq? (car rules) 'only-child)
	       (loop (cdr rules)
		     (cons only-child converters)))
	      ;; (nth-child index)
	      ((and (pair? (car rules))
		    (eq? (caar rules) 'nth-child))
	       (loop (cdr rules) 
		     (cons (nth-function (cadar rules) #f) converters)))
	      ;; (nth-last-child index)
	      ((and (pair? (car rules))
		    (eq? (caar rules) 'nth-last-child))
	       (loop (cdr rules) 
		     (cons (nth-function (cadar rules) #t) converters)))
	      ((eq? (car rules) 'root)
	       (loop (cdr rules)
		     (cons (list root) converters)))
	      ((eq? (car rules) 'empty)
	       (loop (cdr rules)
		     (cons (json:descendant-or-self empty-array?)
			   converters)))
	      ;; level 3 functins
	      ;; has
	      ((and (pair? (car rules))
		    (eq? (caar rules) 'has))
	       (loop (cdr rules)
		     (cons (has (loop (cadar rules) '())) converters)))
	      ;; val
	      ((and (pair? (car rules))
		    (eq? (caar rules) 'val))
	       (loop (cdr rules)
		     (cons (val (cadar rules)) converters)))
	      ((and (pair? (car rules))
		    (eq? (caar rules) 'contains))
	       (loop (cdr rules)
		     (cons (contains (cadar rules)) converters)))
	      (else
	       (error 'json:select "not supported" rules select))))))
  )