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
	    (only (srfi :1) reverse!)
	    (text json tools)
	    (text json select parser))

  (define type-predicates
    `((number  .  ,number?)
      (string  . ,string?)
      (boolean . ,boolean?)
      (null    . ,(lambda (o) (eq? o 'null))) ;; TODO
      (object  . ,json:node?)
      (array   . ,json:array?)))

  ;; construct select
  (define (json:select select)
    (let ((rules (if (string? select) 
		     (json:parse-selector (open-string-input-port select))
		     select)))
      (define (key-name=? name)
	(lambda (node)
	  (equal? name (json:map-entry-key node))))
      (define (value-type=? pred)
	(lambda (node)
	  (if (json:map? node)
	      (pred (json:map-entry-value node))
	      ;; array will be passed per element :)
	      (pred node))))
      (let loop ((rules rules) (converters '()))
	(cond ((null? rules)
	       (lambda (node)
		 (let loop ((nodeset (json:as-nodeset node))
			    (conv (reverse! converters)))
		   (if (null? conv) ;; finish
		       nodeset
		       (loop ((car conv) nodeset)
			     (cdr conv))))))
	      ((string? (car rules)) ;; class
	       (loop (cdr rules)
		     (cons (json:descendant (key-name=? (car rules)))
					    converters)))
	      ;; >>
	      ((and (symbol? (car rules))
		    (eq? (car rules) '>>))
	       (loop (cdr rules)
		     (cons (json:descendant json:node?) converters)))
	      ;; >
	      ((and (symbol? (car rules))
		    (eq? (car rules) '>))
	       (loop (cdr rules)
		     (cons (json:child json:node?) converters)))
	      ;; simple types
	      ((and (symbol? (car rules))
		    (assq (car rules) type-predicates))
	       => (lambda (slot)
		    (loop (cdr rules)
			  (cons (json:descendant (value-type=? (cdr slot)))
				converters))))
	      (else
	       (error 'json:select "not supported" rules select))))))
  )