
(import unittest aux (fds sbral))

(define-suite sbral-suite

  ((test-cons _)

   (let1 (q empty/sbral) 
         (set! q (cons/sbral 'a q))
         (⊦= '((1 a)) q)
         (⊦= 'a (car/sbral q))
         (⊦= '() (cdr/sbral q))

         (set! q (cons/sbral 'b q))
         (⊦= '((1 b) (1 a)) q)
         (⊦= 'b (car/sbral q))
         (⊦= '((1 a)) (cdr/sbral q))

         (set! q (cons/sbral 'c q))
         (⊦= '((3 c (b) (a))) q)
         (⊦= 'c (car/sbral q))
         (⊦= '((1 (b)) (1 (a))) (cdr/sbral q))

         (set! q (cons/sbral 'd q))
         (⊦= '((1 d) (3 c (b) (a))) q)
         (⊦= 'd (car/sbral q))
         (⊦= '((3 c (b) (a))) (cdr/sbral q))

         (set! q (cons/sbral 'e q))
         (⊦= '((1 e) (1 d) (3 c (b) (a))) q)
         (⊦= 'e (car/sbral q))
         (⊦= '((1 d) (3 c (b) (a))) (cdr/sbral q))

         (set! q (cons/sbral 'f q))
         (⊦= '((3 f (e) (d)) (3 c (b) (a))) q)
         (⊦= 'f (car/sbral q))
         (⊦= '((1 (e)) (1 (d)) (3 c (b) (a))) (cdr/sbral q))

         (set! q (cons/sbral 'g q))
         (⊦= '((7 g (f (e) (d)) (c (b) (a)))) q)
         (⊦= 'g (car/sbral q))
         (⊦= '((3 (f (e) (d))) (3 (c (b) (a)))) (cdr/sbral q))))

  ((test-ref _)

   (let1 (q (list->sbral '(a b c d e f g)))
         (⊦= 'a (ref/sbral 0 q))
         (⊦= 'b (ref/sbral 1 q))
         (⊦= 'c (ref/sbral 2 q))
         (⊦= 'd (ref/sbral 3 q))
         (⊦= 'e (ref/sbral 4 q))
         (⊦= 'f (ref/sbral 5 q))
         (⊦= 'g (ref/sbral 6 q))
         ))

  ((test-length _)

   (let1 (q (list->sbral '(a b c d e f g)))         
         (⊦= '((7 a (b (c) (d)) (e (f) (g)))) q)
         (⊦= 7 (length/sbral q))))

  ((test-update _)
   (let1 (q (list->sbral '(a b c d e f g)))         
         (⊦= '((7 a (b (c) (d)) (e (f) (g)))) q)
         (⊦= '((7 a (b (c) (d)) (hello (f) (g)))) (update/sbral 4 'hello q))))



  )

(unittest/✓ sbral-suite)