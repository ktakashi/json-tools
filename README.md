JSON Tools for R6RS Scheme
==========================

Collection of JSON utilities.

JSON is lightweight data format and widely used. It is convenient to have
a library which can handle it.

The expected JSON structure mapping is like this;

- JSON map     -> vector
- JSON array   -> list
- JSON boolean -> boolean
- JSON null    -> 'null
- JSON string  -> string
- JSON number  -> number

Above mapping is used json module from Chicken's egg. (except the null mapping).

As an extension, JSON Tools also handles binary type which mapped to Scheme's
bytevector. If S-expression JSON representation contains bytevector, it will
be mapped to JSON binary.

JSON Tools
==========

JSON Tools provides utilities for JSON. The most of the procedures implicitly
converts given S-expression JSON structure to provided JSON types. Followings
are the provided types;

- `<json:node>` - abstract type for JSON nodes
- `<json:map>`  - associative array
- `<json:map-entry>` - entry of `<json:map>`
- `<json:array>` - array
- `<json:string>` - string
- `<json:number>` - number
- `<json:boolean>` - boolean
- `<json:null>` - null
- `<json:binary>` - extension for Scheme bytevector

The selectors return following nodeset type;

- `<json:nodeset>` - set of nodes defined above.

Constructors
------------

In some case, you may need to use constructors to make sure the node type is
indeed you are expecting. For example, there is no way to detect the difference
between JSON array and JSON map entry if it's still S-expression.

```(json:node o)```
Converts given _o_ to suitable JSON node. If the _o_ is already a JSON node
then it won't re-convert.

```(json:map vector)```
Converts given _vector_ to `<json:map>` object.

```(json:map-entry pair)```
Converts given _pair_ to `<json:map-entry>` object.

```(json:map-array list)```
Converts given _list_ to `<json:map-array>` object.

```(json:string string)```
Converts given _string_ to `<json:string>` object.

```(json:number number)```
Converts given _number_ to `<json:number>` object.

```(json:boolean boolean)```
Converts given _boolean_ to `<json:boolean>` object.

```(json:binary bytevector)```
Converts given _bytevector_ to `<json:binary>` object.

```(json:null null)```
Converts given _null_ to `<json:null>` object. The _null_ must be a symbol
`null`.

```(json:nodeset nodes ...)```
Converts given _nodes_ to `<json:nodeset>` object. The returning value has
no duplicated nodes.

```(json:as-nodeset o)```
Converts given _o_ to `<json:nodeset>` object. If the _o_ is already 
a JSON nodeset then it won't re-convert.

```(json:empty-nodeset )```
Returns empty nodeset.

Predicates
----------

```(json:node? o)```
Returns #t if given _o_ is one of the JSON node.

```(json:map? o)```
Returns #t if given _o_ is JSON map.

```(json:map-entry? o)```
Returns #t if given _o_ is JSON map entry.

```(json:array? o)```
Returns #t if given _o_ is JSON array.

```(json:string? o)```
Returns #t if given _o_ is JSON string.

```(json:number? o)```
Returns #t if given _o_ is JSON number.

```(json:boolean? o)```
Returns #t if given _o_ is JSON boolean.

```(json:binary? o)```
Returns #t if given _o_ is JSON binary.

```(json:null? o)```
Returns #t if given _o_ is JSON null.

```(json:nodeset? o)```
Returns #t if given _o_ is JSON nodeset.

```(json:empty-nodeset? o)```
Returns #t if given _o_ is JSON nodeset and doesn't contain any node.

Accessors
---------

```(json:node-value node)```
Retrieves original node value from given _node_.

```(json:map-ref map key)```
```(json:map-ref map key default)```
Retrieves JSON map value from given JSON map _map_ associated with _key_. The
returning value is JSON node.

When the value does not exist and if the _default_ is specified then it will 
return _default_ as its value.

When the value does not exist and if the _default_ is not specified then it 
will raise an error.

```(json:map-entry-key map-entry)```
```(json:map-entry-value map-entry)```
Retrieves JSON map entry's key or value. The returning value is JSON node.

```(json:array-elements array)```
Returns all array elements of _array_.

```(json:array-ref array n)```
```(json:array-ref array n default)```
Retrieves _n_th JSON array element from given JSON array _array_. The
returning value is JSON node.

When the _n_ is out of range and if the _default_ is specified then it will 
return _default_ as its value.

When the _n_ is out of range and if the _default_ is not specified then it 
will raise an error.

```(json:nodeset-set nodeset)```
Returns all nodes of given _nodeset_.

```(json:nodeset->list nodeset)```
Returns all nodes of given _nodeset_ as S-expression.


Others
------

```(json:map-size map)```
Returns size of given _map_.

```(json:array-length array)```
Returns size of given _array_.

```(json:union-nodeset nodeset-list)```
Returns a nodeset merged from _nodeset-list_. The returning value doesn't
contain duplicate nodes.

Selectors
---------

To be documented


JSON Select
===========

As JSON query, this library (will) provide JSONSelect which is based on
[jsonselect.org](http://jsonselect.org/#docs) with S-expression selector.

NOTE: original implementation doesn't match with map entries however
this consider it as a node so this may return the different result.


Language support
----------------

      | level |   Selector           |  Description
:---: | :---: | :------------------: | :----------------
 [X]  |   1   | `*`                  | Any node
 [X]  |   1   | `T`                  | A node of type T, where T is one string, number, object, array, boolean, or null
 [X]  |   1   | `T.key`              | A node of type T which is the child of an object and is the value its parents key property
 [X]  |   1   | `T."complex key"`    | Same as previous, but with property name specified as a JSON string
 [X]  |   1   | `T:root`             | A node of type T which is the root of the JSON document
 [p]  |   1   | `T:nth-child(n)`     | A node of type T which is the nth child of an array parent
 [X]  |   1   | `T:first-child`      | A node of type T which is the first child of an array parent (equivalent to T:nth-child(1))
 [X]  |   1   | `T U`                | A node of type U with an ancestor of type T
 [X]  |   1   | `T > U`              | A node of type U with a parent of type T
 [X]  |   1   | `S1, S2`             | Any node which matches either selector S1 or S2
 [p]  |   2   | `T:nth-last-child(n)`| A node of type T which is the nth child of an array parent counting from the end
 [X]  |   2   | `T:last-child`       | A node of type T which is the last child of an array parent (equivalent to T:nth-last-child(1))
 [X]  |   2   | `T:only-child`       | A node of type T which is the only child of an array parent
 [X]  |   2   | `T:empty`            | A node of type T which is an array or object with no child
 [X]  |   2   | `T ~ U`              | A node of type U with a sibling of type T
 [X]  |   3   | `T:expr(E)`          | A node of type T with a value that satisfies the expression E
 [x]  |   3   | `T:has(S)`           | A node of type T which has a child node satisfying the selector S
 [x]  |   3   | `T:val(V)`           | A node of type T with a value that is equal to V
 [x]  |   3   | `T:contains(S)`      | A node of type T with a string value contains the substring S

Types:

The original specification specifies following types;

- object
- array
- number
- string
- boolean
- null

As an extension, JSON Tools can handle following type as well;

- binary

The binary type described above however can be only searched by type name.

Notations:

- X: the same as original implementation
- p: nth-child related only supports number as _n_
- x: To make it close to original, comparison happens both node value and
     map entry value. This is because map entry is a node in this implementation
     and to retrive sibling properly.

Required SRFI
=============

Following SRFIs are used in JSON tools and select;

- SRFI-1  - list
- SRFI-13 - string
- SRFI-14 - character sets

Supporting implementations
==========================

* Sagittarius Scheme 0.5.0 (or later)
* Mosh 0.2.7
* Ypsilon 0.9.6-update3
* Racket (plt-r6rs)

I believe it's not difficult to port to other R6RS implementations as long as
it supports the required SRFI.

NOTE: Mosh contains own porting for (json) however it has different mark
as JOSN null and I think it's a design bug since it makes empty array and
null indistinguishable.

NOTE: For Racket, `ext/packrat.sls` for some reason must be installed using
`--install` option.

Your contribution or testing is always welcome!
