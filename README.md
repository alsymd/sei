# sei

## Introduction
sei is a toy interpreter for a very simple language in lispy
syntax. It is based off of the free book [Programming Languages:
Application and
Interpretation](http://cs.brown.edu/courses/cs173/2012/book/). How far
this project goes depends on how much time I have for 
reading. Currently no error message is emitted when there's syntax
error. Nice error message is important but it is currently in low
priority(I know you won't be using this toy anyway :-)). Old examples
might not be interpreted properly by the latest version. The examples
on this page, however, are more likely to work.

## Build
```
git clone https://github.com/alsymd/sei/
cd sei
stack build
```
Note that I added 'ghc-build:nopie' to stack.yaml because otherwise
intero won't compile, feel free to delete that line.

## Run
```
sei
```
It reads a single form from standard input and prints the result.

## Examples
```racket
; Recursion
(letrec ([fact (fun (x) (blt x 1 1 (* x (fact (- x 1)))))])
  (fact 10))
```

```racket
; Set
(let ([f (fun (x) (set! x 3))])
  (let ([y 5])
    (begin
      (f y)
      y)))
```

```racket
; Control & misc
(let [(eq (fun (lhs rhs) (blt lhs rhs false (blt rhs lhs false true))))
      (cons (fun (x xs) (fun (f) (f x xs))))
      (fst (fun (p q) p))
      (snd (fun (p q) q))
      (car (fun (lst) (lst fst)))
      (cdr (fun (lst) (lst snd)))]
 (let [(p (cons 1 2))
       (first-element (car p))
       (second-element (cdr p))]
       (if (eq first-element (- first-element 1)) 233 4399)))
```

## Features
* Recursion
* First-class function
* Mutable variable
* Currying by default (Core language only supports functions with single element)