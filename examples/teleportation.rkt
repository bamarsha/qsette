#lang rosette

(require "../qsette.rkt"
         "../probability.rkt")

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

(verify (assert (= 1 (probability/v (teleport) #t))))
