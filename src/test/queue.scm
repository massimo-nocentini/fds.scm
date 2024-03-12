
(import unittest aux fds-queue)

(define-suite queue-suite
    
    ((test-empty _)
        (⊦= 2 (cadr '(1 2)))
        (letqueue ((q '())
                   (w '(a))
                   (e '(a b))
                   (r '(a b c))
                   (t '(a b c d))) 
            (⊦= '(0 ()) q)
            (⊦= '(1 (a)) w)
            (⊦= '(2 (a) b) e)
            (⊦= '(3 (a) c b) r)
            (⊦= '(4 (a) d c b) t)))
)

(unittest/✓ queue-suite)