
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

  (define (sbral-tree-leaf? tree) (null? (cdr tree)))
  (define (sbral-tree-node? tree) (pair? (cdr tree)))
  (define (sbral-tree-value tree) (car tree))
  (define (sbral-tree-left tree) (cadr tree))
  (define (sbral-tree-right tree) (caddr tree))

  (define (car/sbral sbral)
    (letcar&cdr (((size tree) (car sbral)))
                (cond
                  ((or (and (equal? size 1) (sbral-tree-leaf? tree))
                       (sbral-tree-node? tree)) (sbral-tree-value tree))
                  (else (error "car/sbral: not a valid sbral")))))

  (define (cdr/sbral sbral)
    (letcar&cdr (((size tree) (car sbral)))
                (cond
                  ((and (equal? size 1) (sbral-tree-leaf? tree)) (cdr sbral))
                  ((sbral-tree-node? tree) (let* ((w (fxshr size 1))
                                                  (f (cons w (sbral-tree-left tree)))
                                                  (s (cons w (sbral-tree-right tree))))
                                             (cons f (cons s (cdr sbral)))))
                  (else (error "cdr/sbral: not a valid sbral")))))

  (define (sbral-tree-lookup w i tree)
    (cond
      ((and (equal? w 1) (equal? i 0) (sbral-tree-leaf? tree)) (sbral-tree-value tree))
      ((and (equal? i 0) (sbral-tree-node? tree)) (sbral-tree-value tree))
      ((sbral-tree-node? tree) (let1 (whalf (fxshr w 1))
                                     (cond
                                       ((<= i whalf) (sbral-tree-lookup whalf (sub1 i) (sbral-tree-left tree)))
                                       (else (sbral-tree-lookup whalf (- i 1 whalf) (sbral-tree-right tree))))))
      (else (error "sbral-tree-lookup: not a valid sbral"))))

  (define (sbral-tree-update w i y tree)
    (cond
      ((and (equal? w 1) (equal? i 0) (sbral-tree-leaf? tree)) (list y))
      ((and (equal? i 0) (sbral-tree-node? tree)) (list y (sbral-tree-left tree) (sbral-tree-right tree)))
      ((sbral-tree-node? tree) (let1 (whalf (fxshr w 1))
                                     (cond
                                       ((<= i whalf) (list (sbral-tree-value tree)
                                                           (sbral-tree-update whalf (sub1 i) y (sbral-tree-left tree))
                                                           (sbral-tree-right tree)))
                                       (else (list (sbral-tree-value tree)
                                                   (sbral-tree-left tree)
                                                   (sbral-tree-update whalf (- i 1 whalf) y (sbral-tree-right tree)))))))
      (else (error "sbral-tree-update: not a valid sbral"))))

  (define (ref/sbral i sbral)
    (letcar&cdr (((size tree) (car sbral)))
                (cond
                  ((< i size) (sbral-tree-lookup size i tree))
                  (else (ref/sbral (- i size) (cdr sbral))))))

  (define (update/sbral i y sbral)
    (letcar&cdr (((size tree) (car sbral)))
                (cond
                  ((< i size) (cons (cons size (sbral-tree-update size i y tree)) (cdr sbral)))
                  (else (cons (car sbral) (update/sbral (- i size) y (cdr sbral)))))))

  (define (list->sbral lst) (foldr cons/sbral empty/sbral lst))

  (define (sbral->list sbral)
    (cond
      ((null? sbral) '())
      (else (cons (car/sbral sbral) (sbral->list (cdr/sbral sbral))))))

  (define (length/sbral sbral) (foldr (λ (each acc) (+ (car each) acc)) 0 sbral))

  )