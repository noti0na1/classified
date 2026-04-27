# Classified

A Scala 3 library that confines values to a level of the program: a
classified value can only be observed by code holding a capability for
the corresponding level, and the type system enforces it under capture
checking and safe mode.

## Prerequisites

* sbt
* java 17 or later

## Running the tests

From this directory:

```
sbt test
```
