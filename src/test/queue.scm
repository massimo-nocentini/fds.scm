
(import unittest aux fds-queue)

(define-suite queue-suite
    
    ((test-empty _)
        (⊦= 2 (cadr '(1 2)))
        (letqueue ((q '())) (⊦= #t (fds-queue-empty? q))))

)

(unittest/✓ queue-suite)