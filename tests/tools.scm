(import (rnrs) (text json tools)
	(srfi :64))

(test-begin "JSON Tools")

(define json1 '#(("name" . #(("first" . "Lloyd") ("last" . "Hilaiel")))
		 ("favoriteColor" . "yellow")
		 ("languagesSpoken"
		  #(("lang" . "Bulgarian") ("level" . "advanced"))
		  #(("lang" . "English")
		    ("level" . "native")
		    ("preferred" . #t))
		  #(("lang" . "Spanish") ("level" . "beginner")))
		 ("seatingPreference" "window" "aisle")
		 ("drinkPreference" "whiskey" "beer" "wine")
		 ("weight" . 156)))

(test-equal "json:filter"
	    '(("name" . #(("first" . "Lloyd") ("last" . "Hilaiel"))))
	    (let ((set ((json:filter 
			 (lambda (node)
			   (equal? "name" (json:map-entry-key node))))
			(json:child-nodes json1))))
	      (json:nodeset->list set)))

(test-equal "json:child-as-list"
	    '(("favoriteColor" . "yellow"))
	    ((json:child-as-list (lambda (node)
				   (and (pair? node)
					(equal? "favoriteColor" (car node)))))
	     json1))

(test-equal "json:parent (single node)"
	    '(("name" . #(("first" . "Lloyd") ("last" . "Hilaiel"))))
	    (let ((set (((json:parent
			  (lambda (parent) 
			    (and (pair? parent)
				 (string=? "name" (car parent)))))
			 json1)
			(cdr (vector-ref json1 0)))))
	      (json:nodeset->list set)))

(test-equal "json:parent (nodeset)"
	    '(("name" . #(("first" . "Lloyd") ("last" . "Hilaiel")))
	      ("languagesSpoken"
		  #(("lang" . "Bulgarian") ("level" . "advanced"))
		  #(("lang" . "English")
		    ("level" . "native")
		    ("preferred" . #t))
		  #(("lang" . "Spanish") ("level" . "beginner"))))
	    (let ((set (((json:parent values) json1)
			(json:make-nodeset (cdr (vector-ref json1 0))
					   (cadr (vector-ref json1 2))))))
	      (json:nodeset->list set)))

(test-equal "json:parent (nodeset only name)"
	    '(("name" . #(("first" . "Lloyd") ("last" . "Hilaiel"))))
	    (let ((set (((json:parent
			  (lambda (parent)
			    (equal? "name" (json:map-entry-key parent))))
			 json1)
			(json:make-nodeset (cdr (vector-ref json1 0))
					   (cadr (vector-ref json1 2))))))
	      (json:nodeset->list set)))

(test-equal "json:ancestor (all)"
	    '(#(("first" . "Lloyd") ("last" . "Hilaiel"))
	      ("name" . #(("first" . "Lloyd") ("last" . "Hilaiel")))
	      #(("name" . #(("first" . "Lloyd") ("last" . "Hilaiel")))
		 ("favoriteColor" . "yellow")
		 ("languagesSpoken"
		  #(("lang" . "Bulgarian") ("level" . "advanced"))
		  #(("lang" . "English")
		    ("level" . "native")
		    ("preferred" . #t))
		  #(("lang" . "Spanish") ("level" . "beginner")))
		 ("seatingPreference" "window" "aisle")
		 ("drinkPreference" "whiskey" "beer" "wine")
		 ("weight" . 156)))
	    (let ((set (((json:ancestor values) json1)
			(vector-ref (cdr (vector-ref json1 0)) 0))))
	      (json:nodeset->list set)))

(test-equal "json:ancestor (only name)"
	    '(("name" . #(("first" . "Lloyd") ("last" . "Hilaiel"))))
	    (let ((set (((json:ancestor 
			  (lambda (parent) 
			    (equal? "name" (json:map-entry-key parent))))
			 json1)
			(vector-ref (cdr (vector-ref json1 0)) 0))))
	      (json:nodeset->list set)))

(test-equal "json:descendant (all)"
	    '(#(("first" . "Lloyd") ("last" . "Hilaiel"))
	      ("first" . "Lloyd")
	      "Lloyd"
	      ("last" . "Hilaiel")
	      "Hilaiel")
	    (let ((set ((json:descendant values) (vector-ref json1 0))))
	      (json:nodeset->list set)))

(test-equal "json:following-sibling (all first)"
	    '(#(("lang" . "English") ("level" . "native") ("preferred" . #t))
	      #(("lang" . "Spanish") ("level" . "beginner")))
	    (let ((set (((json:following-sibling values) json1)
			(car ((json:child-as-list values)
			      ((json:child 
				(lambda (node)
				  (equal? "languagesSpoken"
					  (json:map-entry-key node))))
			       json1))))))
	      (json:nodeset->list set)))

(test-equal "json:sibling (all first)"
	    '(#(("lang" . "English") ("level" . "native") ("preferred" . #t))
	      #(("lang" . "Spanish") ("level" . "beginner")))
	    (let ((set (((json:sibling values) json1)
			(car ((json:child-as-list values)
			      ((json:child 
				(lambda (node)
				  (equal? "languagesSpoken"
					  (json:map-entry-key node))))
			       json1))))))
	      (json:nodeset->list set)))

(test-equal "json:sibling (all second)"
	    '(#(("lang" . "Bulgarian") ("level" . "advanced"))
	      #(("lang" . "Spanish") ("level" . "beginner")))
	    (let ((set (((json:sibling values) json1)
			(cadr ((json:child-as-list values)
			       ((json:child 
				(lambda (node)
				  (equal? "languagesSpoken"
					  (json:map-entry-key node))))
			       json1))))))
	      (json:nodeset->list set)))

(test-equal "json:sibling (all third)"
	    '(#(("lang" . "Bulgarian") ("level" . "advanced"))
	      #(("lang" . "English") ("level" . "native") ("preferred" . #t)))
	    (let ((set (((json:sibling values) json1)
			(caddr ((json:child-as-list values)
			       ((json:child 
				(lambda (node)
				  (equal? "languagesSpoken"
					  (json:map-entry-key node))))
			       json1))))))
	      (json:nodeset->list set)))

(test-equal "json:sibling (all bit big)"
	    '(("name" . #(("first" . "Lloyd") ("last" . "Hilaiel")))
	      ("favoriteColor" . "yellow")
	      ("languagesSpoken"
	       #(("lang" . "Bulgarian") ("level" . "advanced"))
	       #(("lang" . "English")
		 ("level" . "native")
		 ("preferred" . #t))
	       #(("lang" . "Spanish") ("level" . "beginner")))
	      ;; target here
	      ("drinkPreference" "whiskey" "beer" "wine")
	      ("weight" . 156))
	    (let ((set (((json:sibling values) json1)
			(cadddr ((json:child-as-list values) json1)))))
	      (json:nodeset->list set)))
(test-end)
