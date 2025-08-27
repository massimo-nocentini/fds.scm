
(module (fds sbral) *

  (import scheme (chicken base) (chicken fixnum) aux)

  (define empty/sbral '())

  (define (cons/sbral v sbral)
    (cond
      ((and (not (null? sbral)) (not (null? (cdr sbral))))
       (let ((fst (car sbral))
             (snd (cadr sbral))
             (rest (cddr sbral)))
         (letcar&cdr (((x xtree) fst)
                      ((y ytree) snd))
                     (cond
                       ((equal? x y) (cons (list (+ 1 x y) v xtree ytree) rest))
                       (else (cons (list 1 v) sbral))))))
      (else (cons (list 1 v) sbral))))

  (define (sbral-leaf? tree) (null? (cdr tree)))
  (define (sbral-node? tree) (pair? (cdr tree)))
  (define (sbral-tree-value tree) (car tree))
  (define (sbral-tree-left tree) (cadr tree))
  (define (sbral-tree-right tree) (caddr tree))

  (define (car/sbral sbral)
    (letcar&cdr (((size tree) (car sbral)))
                (cond
                  ((and (equal? size 1) (sbral-leaf? tree)) (sbral-tree-value tree))
                  ((sbral-node? tree) (sbral-tree-value tree))
                  (else (error "car/sbral: not a valid sbral")))))

  (define (cdr/sbral sbral)
    (letcar&cdr (((size tree) (car sbral)))
                (cond
                  ((and (equal? size 1) (sbral-leaf? tree)) (cdr sbral))
                  ((sbral-node? tree) (let1 (w (fxshr size 1))
                                            (cons (list w (sbral-tree-left tree)) (cons (list w (sbral-tree-right tree)) (cdr sbral)))))
                  (else (error "cdr/sbral: not a valid sbral")))))

  (define (sbral-lookup w i tree)
    (cond
      ((and (equal? w 1) (equal? i 0) (sbral-leaf? tree)) (sbral-tree-value tree))
      ((and (equal? i 0) (sbral-node? tree)) (sbral-tree-value tree))
      ((sbral-node? tree) (let1 (whalf (fxshr w 1))
                                (cond
                                  ((<= i whalf) (sbral-lookup whalf (sub1 i) (sbral-tree-left tree)))
                                  (else (sbral-lookup whalf (- i 1 whalf) (sbral-tree-right tree))))))
      (else (error "sbral-lookup: not a valid sbral"))))

  (define (ref/sbral i sbral)
    (letcar&cdr (((size tree) (car sbral)))
                (cond
                  ((< i size) (sbral-lookup size i tree))
                  (else (ref/sbral (- i size) (cdr sbral))))))

		(define (list->sbral lst)
			(define (helper lst acc)
				(if (null? lst)
						acc
						(helper (cdr lst) (cons/sbral (car lst) acc))))
			(helper (reverse lst) empty/sbral))

  )