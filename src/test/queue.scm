
(import unittest aux fds-queue)

(define-suite queue-suite

    ((test-letqueue _)
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

    ((test-manipulation _)
        (letqueue ((q '()))
            (⊦= '(0 ()) q)

            (set! q (fds-queue-cons 'a q))
            (⊦= '(1 (a)) q)
            (⊦= 'a (fds-queue-car q))

            (set! q (fds-queue-cons 'b q))
            (⊦= '(2 (a) b) q)
            (⊦= 'a (fds-queue-car q))

            (set! q (fds-queue-cons 'c q))
            (⊦= '(3 (a) c b) q)
            (⊦= 'a (fds-queue-car q))

            (set! q (fds-queue-cdr q))
            (⊦= '(2 (b c)) q)
            (⊦= 'b (fds-queue-car q))

            (set! q (fds-queue-cdr q))
            (⊦= '(1 (c)) q)
            (⊦= 'c (fds-queue-car q))
            
            (set! q (fds-queue-cdr q))
            (⊦= '(0 ()) q)))
)

(unittest/✓ queue-suite)