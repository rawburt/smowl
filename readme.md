# Smowl

A small object-oriented programming language to explore programming language design and implementation.

## Goal

Design an object-oriented programming language to make a binary search tree ADT.

## Example

```
class Node(key) {
  var left
  var right

  def insert(n) {
    if n < key {
      if l = left {
        l.insert(n)
      } else {
        left = Node(n)
      }
    } else if n > key {
      if r = right {
        r.insert(n)
      } else {
        right = Node(n)
      }
    }
  }

  def to_array(a) {
    if l = left {
      l.to_array(a)
    }
    a.push(key)
    if r = right {
      r.to_array(a)
    }
    return a
  }
}

class Tree() {
  var root

  def insert(n) {
    if r = root {
      r.insert(n)
    } else {
      root = Node(n)
    }
  }

  def to_array() {
    var a = []
    if r = root {
      return r.to_array(a)
    } else {
      return a
    }
  }
}

var t = Tree()
t.insert(1)
t.insert(2)
t.insert(5)
t.insert(0)
t.insert(-1)
t.insert(4)

var a = t.to_array()
var i = 0
while i < a.len() {
  print(a[i])
  i += 1
}
```
