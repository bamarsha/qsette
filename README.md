# Qsette - Q# verification

## Editing and Translating from Q#

First enable rosette by setting the language: `#lang rosette`

Next include the `qsette.rkt` file to enable the dsl and `probability.rkt` for verifying specific probabilities:


`(requires"{qsette dir}/qsette.rkt" "{qsette dir}/probability.rkt")`



The general syntax matches Q#'s keywords with rackets infix lisp notation.

[The Q# Programming Language](https://docs.racket-lang.org/guide/index.html)

[The Racket Guide](https://docs.racket-lang.org/guide/index.html)

Operations can be created using the `operation` keyword and can then be used as normal defined functions. They can either be evaluated on concrete arguments or given symbolic-values to verify specifications. To verify a specific program use Rosettes's `define-symbolic` in combination with `verify`. For more information on using Rosette, see:

[The Rosette Guide](https://docs.racket-lang.org/rosette-guide/index.html)


## Verifying a probability

The `probability.rkt` file provides `probability/v` which can take the output of an `operation` and compare it to an expected value.

`(verify (assert (<= 0.8 (probability/v (grover-search n) n))))`

## Running the verifier

Finally, if your verification queries are defined in the same file, to test the program can use:

 `$ racket {dir/example}.rkt`

 To load the operations interactively first open the racket repl:

 `$ racket`

 And then load the file

 `> (enter! (file "{dir/example}.rkt"))`

## Examples

### Teleportation
Extract from `examples/teleportation.rkt`
```
(operation (teleport)
  (begin
    (mutable result #f)
    (using ([msg (qubit)]
            [here (qubit)]
            [there (qubit)])
      (x msg)
      (h msg)

      (h here)

      (cnot here there)

      (cnot msg here)
      (h msg)

      (if (m msg)
          (z there))
      (if (m here)
          (x there))

      (h there)
      (set result (m there)))
    (return result)))
```

### Entanglement
Extract from `examples/entanglement.rkt`
```
(operation (entanglement b)
  (begin
    (mutable r1 #f)
    (mutable r2 #f)
    (using ([q1 (qubit)]
            [q2 (qubit)])
           (h q1)
           (cnot q1 q2)
           (if b
               (x q1))
           (set r1 (m q1))
           (set r2 (m q2))
           (reset q1)
           (reset q2))
    (return (= r1 r2))))
```

To see how the examples are tested, see the full example files.