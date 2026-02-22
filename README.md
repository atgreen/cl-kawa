# cl-kawa

Common Lisp / [Kawa Scheme](https://www.gnu.org/software/kawa/) interop via
[OpenLDK](https://github.com/atgreen/openldk).

Kawa Scheme, created by [Per Bothner](https://per.bothner.com/), runs on the
JVM. OpenLDK is a JVM implemented in Common Lisp. Since both share the same
SBCL process and heap, cl-kawa enables deep interoperability between Common
Lisp and Scheme with no serialization or process boundaries.

**Project status:** technology demonstration. Not a performant or production
ready implementation.

## What you can do

* Evaluate Scheme from Common Lisp (strings or s-expressions).
* Call Scheme procedures from Common Lisp.
* Register Common Lisp functions and call them from Scheme.
* Exchange basic values (numbers, strings, booleans, lists) across the boundary.

## Prerequisites

* [SBCL](http://www.sbcl.org/)
* [OpenLDK](https://github.com/atgreen/openldk)
* Java 8 JDK (`JAVA_HOME` pointing to a JRE with `lib/rt.jar`)
* [Kawa 3.1.1 JAR](https://www.gnu.org/software/kawa/) (downloaded automatically
  via Maven, or manually placed in `libs/`)

## Install

1. Ensure prerequisites are installed and `JAVA_HOME` is set.
2. Download Kawa (or let Maven fetch it) into `libs/` as
   `kawa-3.1.1.jar`.
3. Load the ASDF system from your Common Lisp image.

## Quick start

```lisp
(asdf:load-system :cl-kawa)

;; Initialize the runtime (point to the Kawa JAR)
(kawa:startup :classpath "libs/kawa-3.1.1.jar")

;; Evaluate Scheme expressions -- as s-expressions or strings
(kawa:eval '(+ 1 2))              ; => 3
(kawa:eval '(string-length "hello")) ; => 5
(kawa:eval '(list 1 2 3))         ; => (1 2 3)
(kawa:eval "(* 6 7)")             ; => 42  (strings still work)

;; Look up a Scheme procedure and call it from CL
(let ((add (kawa:lookup "+")))
  (kawa:funcall add 10 20))        ; => 30

;; Register a CL function so Scheme can call it
(kawa:register "cl-square" (lambda (x) (* x x)))
(kawa:eval '(cl-square 7))        ; => 49
```

## Environment variables

* `JAVA_HOME`: path to a Java 8 JRE that contains `lib/rt.jar`.
* `LDK_CLASSPATH`: optional classpath override for OpenLDK.

## API

### `(kawa:startup &key classpath)`

Initialize the Kawa Scheme runtime.  `classpath` is a colon-separated
string of JAR paths.  Safe to call multiple times (subsequent calls are
no-ops).

### `(kawa:eval expr &optional env)` => value

Evaluate a Scheme expression.  `expr` can be a string of Scheme source
or a Common Lisp s-expression (symbols are automatically downcased,
`quote` becomes `'`, `t`/`nil` become `#t`/`'()`).  Returns the result
converted to a Common Lisp value.

### `(kawa:lookup name &optional env)` => object

Look up a Scheme binding by name.  Returns the raw Java/Kawa object
(typically a `Procedure`).

### `(kawa:funcall proc &rest args)` => value

Call a Scheme procedure with Common Lisp arguments.  Arguments are
automatically converted to Kawa values; the result is converted back.

### `(kawa:register name function &optional env)`

Register a Common Lisp function as a Scheme procedure.  The function
becomes callable from Scheme code via `kawa:eval`.

### `(kawa:scheme->cl obj)` => value

Convert a Java/Kawa object to a Common Lisp value.  Handles integers,
floats, strings, booleans, and lists.  Returns the object unchanged if
no conversion applies.

### `(kawa:cl->scheme val)` => object

Convert a Common Lisp value to a Java/Kawa object.  Handles integers,
floats, strings, and cons cells.

### `(kawa:make-environment &optional parent)` => environment

Create a new Scheme environment, optionally inheriting from a parent.

## Value conversions

| Kawa type | Common Lisp type |
|-----------|-----------------|
| `gnu.math.IntNum` | integer |
| `gnu.math.DFloNum` | double-float |
| `java.lang.String` | string |
| `java.lang.Boolean` | T / NIL |
| `gnu.lists.Pair` | cons |
| `gnu.lists.LList` (empty) | NIL |

## Running the tests

```sh
make check
```

Or manually:

```sh
LDK_CLASSPATH=libs/kawa-3.1.1.jar \
  JAVA_HOME=/path/to/java8/jre \
  sbcl --load test.lisp
```

## Limitations and notes

* Designed as a proof-of-concept; performance and completeness are not goals.
* The conversion layer only handles basic scalar and list types.
* Requires Java 8 because OpenLDK depends on `rt.jar`.

## Author and License

cl-kawa was written by Anthony Green and is distributed under the terms
of the MIT license.
