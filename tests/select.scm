#!r6rs
(import (rnrs) 
	(text json select)
	(text json tools)
	(srfi :64))

(test-begin "JSONSelect")

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

(test-equal ".languagesSpoken"
	    '(("languagesSpoken"
	       #(("lang" . "Bulgarian") ("level" . "advanced"))
	       #(("lang" . "English")
		 ("level" . "native")
		 ("preferred" . #t))
	       #(("lang" . "Spanish") ("level" . "beginner"))))
	    (json:nodeset->list ((json:select ".languagesSpoken") json1)))

(test-equal ".\"weight\""
	    '(("weight" . 156))
	    (json:nodeset->list ((json:select ".\"weight\"") json1)))
;; TODO add more descendants...
(test-equal ".languagesSpoken .lang"
	    '(("lang" . "Bulgarian")
	      ("lang" . "English")
	      ("lang" . "Spanish"))
	    (json:nodeset->list ((json:select ".languagesSpoken .lang") json1)))

(test-equal ".languagesSpoken > .lang"
	    '(("lang" . "Bulgarian")
	      ("lang" . "English")
	      ("lang" . "Spanish"))
	    (json:nodeset->list ((json:select ".languagesSpoken > .lang")
				 json1)))

(test-equal "number" '(156)
	    (json:nodeset->list ((json:select "number") json1)))
(test-equal "array" 
	    '((#(("lang" . "Bulgarian") ("level" . "advanced"))
	       #(("lang" . "English")
		 ("level" . "native")
		 ("preferred" . #t))
	       #(("lang" . "Spanish") ("level" . "beginner")))
	      ("window" "aisle")
	      ("whiskey" "beer" "wine"))
	    (json:nodeset->list ((json:select "array") json1)))

(test-equal "string:favoriteColor" '("yellow")
	    (json:nodeset->list ((json:select "string.favoriteColor") json1)))

(test-equal ".preferred ~ .lang" '(("lang" . "English"))
	    (json:nodeset->list ((json:select ".preferred ~ .lang") json1)))

(test-equal ".drinkPreference :first-child" 
	    '("whiskey")
	    (json:nodeset->list ((json:select ".drinkPreference :first-child")
				 json1)))
(test-equal ".drinkPreference :last-child" 
	    '("wine")
	    (json:nodeset->list ((json:select ".drinkPreference :last-child")
				 json1)))

;; for some reason it starts with 1
(test-equal ".drinkPreference :nth-child(2)" 
	    '("beer")
	    (json:nodeset->list ((json:select ".drinkPreference :nth-child(2)")
				 json1)))
(test-equal ".drinkPreference :nth-last-child(1)" 
	    '("wine")
	    (json:nodeset->list 
	     ((json:select ".drinkPreference :nth-last-child(1)")
	      json1)))

(test-end)