
(module fds-queue *

	(import scheme (chicken base) aux)

    (define fds-queue-empty (cons 0 (cons '() '())))

    (define (fds-queue-size q) (car q))

	(define (fds-queue-empty? q) (null? (cadr q)))

	(define (fds-queue-car q)
	  (cond
	   ((fds-queue-empty? q) (error 'empty))
	   (else (caar q))))
 
    (define (fds-queue-check q)
	  (cond
	   ((null? (car q)) (cons (reverse (cdr q)) '()))
	   (else q)))
 
   	(define (fds-queue-cdr q)
	  (cond
	   ((fds-queue-empty? q) (error 'empty))
	   (else (letcar&cdr (((a d) q))
                (fds-queue-check (cons (cdr a) d))))))
 
    (define (fds-queue-push q x)
        (letcar&cdr (((a d) q)) (fds-queue-check (cons a (cons x d)))))

	(define-syntax letqueue
	  (syntax-rules ()
	  	((_ ((q expr) ...) body ...)
		 (let ((q fds-queue-empty) ...)
		 	(for-each (lambda (x) (set! q (fds-queue-push q x))) expr) ...
			body ...))))
)