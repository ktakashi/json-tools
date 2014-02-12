;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/tools.scm - JSON structure utilities
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

;; the APIs' name and behaviours are highly inspired by
;; SXPath and related libraries

#|
JSON structure memo;
The structure must be the same as Chicken's json egg which is
ported to Sagittarius and Mosh. (I may add it to this package
to make this R6RS compatible).

{}: associative array => vector
[]: array             => list
true, false           => boolean
null                  => 'null (this may not be the same per implementation...)
string                => string
number                => number

|#
(library (text json tools)
    (export json:nodeset?
	    json:nodeset->list
	    json:make-nodeset
	    json:as-nodeset
	    json:node?
	    json:map?
	    json:map-size
	    json:map-ref
	    json:map-entry?
	    json:map-entry-key
	    json:map-entry-value
	    json:array?
	    json:array-ref
	    ;; common utilities
	    json:filter
	    json:child-nodes-as-list
	    json:child-nodes
	    json:child-as-list
	    ;; selectors
	    json:parent
	    json:child
	    json:ancestor
	    json:descendant
	    json:following-sibling
	    json:preceding-sibling
	    json:sibling		; for convenience
	    )
    (import (rnrs)
	    (only (srfi :1) append-map reverse!))

  ;; type thing
  (define (json:node? o)
    ;; symbol can be a key but for now don't accept
    (or (string? o) (number? o) (boolean? o) (vector? o) (pair? o)))
  (define (json:map? o) (vector? o))
  (define (json:map-size o) (vector-length o))
  (define (json:map-ref o key . default)
    (let ((len (json:map-size o)))
      (let loop ((i 0))
	(if (= i len)
	    (if (pair? default)
		(car default)
		(error 'json:map-ref "no entry found" key))
	    (let ((e (vector-ref o i)))
	      (if (and (json:map-entry? e) (equal? key (json:map-entry-key e)))
		  (json:map-entry-value e)
		  (loop (+ i 1))))))))
  (define (json:map-entry? node)
    (and (pair? node) (string? (car node))))
  (define (json:map-entry-key node) 
    (and (json:map-entry? node) (car node)))
  (define (json:map-entry-value node)
    (and (json:map-entry? node) (cdr node)))
  (define (json:array? o) (list? o))
  (define (json:array-ref o n . default)
    (apply list-ref o n default))

  ;; nodeset
  ;; introduce nodeset type. since the JSON structure doesn't have
  ;; any hint if it's nodeset or not. e.g) array can contain anything
  ;; to make sure it we introduce this
  (define-record-type (json:nodeset json:make-nodeset json:nodeset?)
    (fields (immutable set json:nodeset->list))
    (protocol (lambda (p) (lambda set (p set)))))

  (define *json:empty-nodeset* (json:make-nodeset))
  (define (json:empty-nodeset? nodeset)
    (and (json:nodeset? nodeset) (null? (json:nodeset->list nodeset))))
  (define (json:as-nodeset node)
    (if (json:nodeset? node) node (json:make-nodeset node)))
  ;; shortcut
  (define (json:as-nodeset->list node)
    (if (json:nodeset? node) (json:nodeset->list node) (list node)))
  ;; utilities for nodeset
  (define (json:union-nodeset nodeset-list)
    (let ((sets (append-map json:nodeset->list nodeset-list)))
      (apply json:make-nodeset sets)))

  ;; misc utils
  ;; returns nodeset
  (define (json:map-union proc node-list)
    (let loop ((nodes node-list) (r '()))
      (if (null? nodes)
	  (json:union-nodeset (reverse! r))
	  (loop (cdr nodes) (cons (proc (car nodes)) r)))))

  ;; tools
  (define (json:filter pred?)
    (lambda (node-list)
      (let loop ((lst (json:as-nodeset->list node-list)) (r '()))
	(if (null? lst)
	    (apply json:make-nodeset (reverse! r))
	    (let ((result (pred? (car lst))))
	      (loop (cdr lst) (if (and result (not (null? result)))
				  (cons (car lst) r)
				  r)))))))
  (define (json:child-nodes-as-list node)
    (cond ((pair? node) 
	   (if (pair? (cdr node))
	       ;; array the child nodes are cdr
	       (cdr node)
	       ;; atom or associative array. return it as a list
	       (list (cdr node))))
	  ((vector? node)
	   ;; each element of the node is child node
	   (do ((len (vector-length node)) (i 0 (+ i 1))
		(r '() (cons (vector-ref node i) r)))
	       ((= i len) (reverse! r))))
	  ;; atom doesn't have child nodes
	  (else '())))
  (define (json:child-nodes node)
    (apply json:make-nodeset (json:child-nodes-as-list node)))

  ;; selectors
  (define (json:child pred?)
    (lambda (node)
      (cond ((json:nodeset? node)
	     (json:map-union (json:child pred?) (json:nodeset->list node)))
	    ((pair? node) 
	     (if (pair? (cdr node)) ;; array needs to be treated as a nodeset
		 (let loop ((nodes (cdr node)) (r '()))
		   (if (null? nodes)
		       (apply json:make-nodeset (reverse! r))
		       (let ((result (pred? (car nodes))))
			 (loop (cdr nodes)
			       (if (and result
					(not (json:empty-nodeset? result)))
				   (cons (car nodes) r)
				   r)))))
		 (json:as-nodeset ((json:filter pred?) (cdr node)))))
	    ((vector? node)
	     (let ((len (vector-length node)))
	       (let loop ((i 0) (r '()))
		 (if (= i len)
		     (apply json:make-nodeset (reverse! r))
		     (let ((result ((json:filter pred?) (vector-ref node i))))
		       (loop (+ i 1) 
			     (if (and result (not (json:empty-nodeset? result)))
				 (cons (vector-ref node i) r)
					 r)))))))
	    ;; atom doesn't have child
	    (else *json:empty-nodeset*))))

  (define (json:child-as-list pred?)
    (lambda (node)
      (json:nodeset->list ((json:child pred?) node))))

  (define (json:parent pred?)
    (lambda (root-node)
      (lambda (node)
	(if (json:nodeset? node)
	    (json:map-union ((json:parent pred?) root-node)
			    (json:nodeset->list node))
	    ;; pairs ::= ((child . parent) ...)
	    (let loop ((pairs (append-map
			       (lambda (root-n)
				 (map (lambda (arg) (cons arg root-n))
				      (json:child-nodes-as-list root-n)))
			       (json:as-nodeset->list root-node))))
	      (if (null? pairs)
		  *json:empty-nodeset*
		  (let ((pair (car pairs)))
		    (if (eq? (car pair) node)
			((json:filter pred?) (cdr pair))
			(loop (append
			       (map (lambda (arg) (cons arg (car pair)))
				    (json:child-nodes-as-list (car pair)))
			       (cdr pairs)))))))))))

  (define (json:ancestor pred?)
    (lambda (root-node)
      (lambda (node)
	(if (json:nodeset? node)
	    (json:map-union ((json:ancestor pred?) root-node)
			    (json:nodeset->list node))
	    ;; paths ::= ((child parents ...) ...)
	    (let loop ((paths (list (json:as-nodeset->list root-node))))
	      (if (null? paths)
		  *json:empty-nodeset*
		  (let ((path (car paths)))
		    (if (eq? (car path) node)
			((json:filter pred?) 
			 (apply json:make-nodeset (cdr path)))
			(loop (append
			       (map (lambda (arg) (cons arg path))
				    (json:child-nodes-as-list (car path)))
			       (cdr paths)))))))))))

  (define (json:descendant pred?)
    (lambda (node)
      (if (json:nodeset? node)
	  (json:map-union (json:descendant pred?) (json:nodeset->list node))
	  (let loop ((r '())
		     (more ((json:child-as-list json:node?) node)))
	    (if (null? more)
		(apply json:make-nodeset (reverse! r))
		(loop (if (pred? (car more)) (cons (car more) r) r)
		      (append ((json:child-as-list json:node?) (car more))
			      (cdr more))))))))

  (define (json:following-sibling pred?)
    (lambda (root-node)
      (lambda (node)
	(if (json:nodeset? node)
	    (json:map-union ((json:sibling pred?) root-node)
			    (json:nodeset->list node))
	    ;; seqs ::= ((child siblings ...) ...)
	    (let loop ((seqs (list (json:as-nodeset->list root-node))))
	      (if (null? seqs)
		  *json:empty-nodeset*
		  (let rpt ((seq (car seqs)))
		    (cond ((null? seq)
			   (loop (append (map (json:child-as-list json:node?)
					      (car seqs))
					 (cdr seqs))))
			  ((eq? (car seq) node)
			   ((json:filter pred?) 
			    (apply json:make-nodeset (cdr seq))))
			  (else (rpt (cdr seq)))))))))))
  (define (json:preceding-sibling pred?)
    (lambda (root-node)
      (lambda (node)
	(if (json:nodeset? node)
	    (json:map-union ((json:sibling pred?) root-node)
			    (json:nodeset->list node))
	    ;; seqs ::= ((child siblings ...) ...)
	    (let loop ((seqs (list (json:as-nodeset->list root-node))))
	      (if (null? seqs)
		  *json:empty-nodeset*
		  (let rpt ((seq (car seqs)))
		    (cond ((null? seq)
			   (loop (append 
				  (map (lambda (n) 
					 (reverse!
					  ((json:child-as-list json:node?) n)))
				       (car seqs))
				  (cdr seqs))))
			  ((eq? (car seq) node)
			   ((json:filter pred?) 
			    (apply json:make-nodeset (reverse (cdr seq)))))
			  (else (rpt (cdr seq)))))))))))

  (define (json:sibling pred?)
    (lambda (root-node)
      (lambda (node)
	(define ->list json:nodeset->list)
	(define (merge p f)
	  (cond ((null? p) f)
		((null? f) p)
		(else
		 `(,@p ,@f))))
	(apply json:make-nodeset
	       (merge
		(->list (((json:preceding-sibling pred?) root-node) node))
		(->list (((json:following-sibling pred?) root-node) node)))))))
)