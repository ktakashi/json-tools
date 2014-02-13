JSON Tools for R6RS Scheme
---------------------------------

Collection of JSON utilities.

JSON is lightweight data format and widely used. It is convenient to have
a library which can handle it.

TODO document for S-JSON structure (Chichken's json module format)



JSON Tools
-----------

TODO API docs


JSON Select
-----------

As JSON query, this library (will) provide JSONSelect which is based on
[jsonselect.org](http://jsonselect.org/#docs) with S-expression selector.

Language support
================

-- **Level 1** -- `*`

Any node

-- **Level 1** -- `T` - done!

A node of type T, where T is one string, number, object, array, boolean, or null

-- **Level 1** -- `T.key` - done!

A node of type T which is the child of an object and is the value its parents
key property

-- **Level 1** -- `T."complex key"` - done!

Same as previous, but with property name specified as a JSON string

-- **Level 1** -- `T:root`

A node of type T which is the root of the JSON document

-- **Level 1** -- `T:nth-child(n)` - done!

A node of type T which is the nth child of an array parent

-- **Level 1** -- `T:first-child` - done!

A node of type T which is the first child of an array parent (equivalent
to T:nth-child(1)

-- **Level 1** -- `T U` - done!

A node of type U with an ancestor of type T

-- **Level 1** -- `T > U` - done!

A node of type U with a parent of type T

-- **Level 1** -- `S1, S2`

Any node which matches either selector S1 or S2

-- **Level 2** -- `T:nth-last-child(n)` - done!

A node of type T which is the nth child of an array parent counting from the end

-- **Level 2** -- `T:last-child` - done!

A node of type T which is the last child of an array parent (equivalent
to T:nth-last-child(1)

-- **Level 2** -- `T:only-child`

A node of type T which is the only child of an array parent

-- **Level 2** -- `T:empty`

A node of type T which is an array or object with no child

-- **Level 2** -- `T ~ U` - done!

A node of type U with a sibling of type T

NOTE: original implementation contains self node but I think it's weird
so the result nodeset doesn't contain self node but only siblings.

-- **Level 3** -- `T:has(S)`

A node of type T which has a child node satisfying the selector S

-- **Level 3** -- `T:expr(E)`

A node of type T with a value that satisfies the expression E

-- **Level 3** -- `T:val(V)`

A node of type T with a value that is equal to V

-- **Level 3** -- `T:contains(S)`

A node of type T with a string value contains the substring S


Supporting implementations
--------------------------

* Sagittarius Scheme 0.5.0 (or later)
* Mosh 0.2.7
* Ypsilon 0.9.6-update3

Your contribution or testing is always welcome!
