#!r6rs
(import (rnrs) 
	(json)
	(srfi :64))

(test-begin "JSON read/write")

(let ((s (utf8->string #vu8(240 159 152 186))))
  (test-equal "Emoji" s (json-read (open-string-input-port "\"\\uD83D\\uDE3A\""))))

(test-end)
