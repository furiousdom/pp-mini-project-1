#lang racket



(define all-students
  (read (open-input-file "all-students-small.lsp")))

(define (find-student-by-id lst id)
  (if (null? lst)
      '()
      (if (not (string? id))
          (error "Id is not type of string")
          (if (equal? (car (car lst)) id)
              (car lst)
              (find-student-by-id (cdr lst) id)))))

(define (create-student-object student-attributes-lst)
 (if (= (length student-attributes-lst) 5)
     (let ((id (car student-attributes-lst))
           (name (list-ref student-attributes-lst 1))
           (sex (list-ref student-attributes-lst 2))
           (ethnicity (list-ref student-attributes-lst 3))
           (age (list-ref student-attributes-lst 4)))
        (new-instance id name sex ethnicity age))
      (error "student-attributes-lst needs to contain 5 attributes")))

(define (new-instance class . parameters)
  (let ((instance (apply class parameters)))
    instance))

(define (method-lookup object selector)
  (cond ((procedure? object)
         (object selector))
        (else (error "Inappropriate object in method-lookup: " object))))

(define (send message object . args)
  (let ((method (method-lookup object message)))
    (cond ((procedure? method) (apply method args))
          ((null? method) (error "Message not understood: " message))
          (else (error "Inappropriate result of method lookup: " method)))))

(define (student id name sex ethnicity age)
  (let ((id id)
        (name name)
        (sex sex)
        (ethnicity ethnicity)
        (age age)
        )
    (define (get-id) id)

    (define (get-name) name)

    (define (get-sex) sex)

    (define (get-ethnicity) ethnicity)

    (define (get-age) age)

    (define (add s)
      (student
       (+ id (send 'get-id s))
       (+ name (send 'get-name s))
       (+ sex (send 'get-sex s))
       (+ ethnicity (send 'get-ethnicity s))
       (+ age (send 'get-age s))))

    (define (type-of) 'student)

    (define (dispatch message)
      (cond ((eqv? message 'get-id) get-id)
            ((eqv? message 'get-name) get-name)
            ((eqv? message 'get-sex) get-sex)
            ((eqv? message 'get-ethnicity) get-ethnicity)
            ((eqv? message 'get-age) get-age)
            ((eqv? message 'add) add)
            ((eqv? message 'type-of) type-of)))

    dispatch))

; ((lambda (a b) (+ a b)) 1 2)

; (let ((a 1)
;      (b 2))
;      (+ a b))

;((+ (lambda (a) ((lambda (b) (b)) a)) 1))

; (let ((a 1))
;      (let ((b (+ a 1)))
;      (+ a b)))

; (let* ((a 1)
;      (b (+ a 1)))
;      (+ a b))

(define (point x0 y0)
  (let ((x x0)
        (y y0))

     (letrec ((getx       (lambda () x))
              (gety       (lambda () y))
              (add        (lambda (p)
                            (point
                              (+ x (send 'getx p))
                              (+ y (send 'gety p)))))
              (type-of (lambda () 'point))
              )
     (lambda (message)
       (cond ((eq? message 'getx) getx)
             ((eq? message 'gety) gety)
             ((eq? message 'add)  add)
             ((eq? message 'type-of) type-of)
             (else (error "Message not understood")))))))

(define (new-instance0 class . parameters)
  (apply class parameters))

(define (method-lookup0 object selector)
  (cond ((procedure? object)
         (object selector))
        (else (error "Inappropriate object in method-lookup: " object))))

(define (send0 message object . args)
  (let ((method (method-lookup object message)))
    (cond ((procedure? method) (apply method args))
          ((null? method) (error "Message not understood: " message))
          (else (error "Inappropriate result of method lookup: " method)))))

;from programming random grouping
(define (random-grouping2 sl gsl)
  (random-grouping-helper sl gsl 0))

(define (random-grouping-helper2 sl gsl cnt)
  (if (= cnt (car gsl))
      '()
      (let ((random-student (list-ref sl (random (length sl)))))
        (cons random-student
              (random-grouping-helper2 (remove random-student sl equal?) gsl (+ cnt 1)))
        )))

(define (random-grouping1 sl gsl)
  (if (= (car gsl) 0)
      '()
      (let ((random-student (list-ref sl (random (length sl)))))
        (cons (cons 1 (car random-student))
              (random-grouping1 (remove random-student sl equal?) (cons (- (car gsl) 1)
                                                                       (cdr gsl))))
        )))

(define (remove-student id sl)
  (remove (find-student-by-id sl id) sl equal?))

;helper function maybe?
(define (group-students grouping-selector . args)
  (cond ((eqv? grouping-selector 'random-grouping) (apply random-grouping args))
        ((eqv? grouping-selector 'group-by-counting) (apply group-by-counting args))
        ((eqv? grouping-selector 'balanced-grouping-by-counting) (apply balanced-grouping-by-counting args))
        ;((eqv? grouping-selector 'random-grouping-with-predicate) (apply group-by-counting args))
        (else "There is no matching function fro the entered selector!")))

;oop simulation
(define (create-student-object student-attributes-lst)
 (if (= (length student-attributes-lst) 5)
     (let ((id (car student-attributes-lst))
           (name (list-ref student-attributes-lst 1))
           (sex (list-ref student-attributes-lst 2))
           (ethnicity (list-ref student-attributes-lst 3))
           (age (list-ref student-attributes-lst 4)))
        (new-instance student id name sex ethnicity age))
      (error "student-attributes-lst needs to contain 5 attributes")))

(define (new-instance class . parameters)
  (apply class parameters))

(define (method-lookup object selector)
  (cond ((procedure? object)
         (object selector))
        (else (error "Inappropriate object in method-lookup: " object))))

(define (send message object . args)
  (let ((method (method-lookup object message)))
    (cond ((procedure? method) (apply method args))
          ((null? method) (error "Message not understood: " message))
          (else (error "Inappropriate result of method lookup: " method)))))

(define (student id name sex ethnicity age)
  (let ((id id)
        (name name)
        (sex sex)
        (ethnicity ethnicity)
        (age age)
        )

    (define (get-id) id)

    (define (get-name) name)

    (define (get-sex) sex)

    (define (get-ethnicity) ethnicity)

    (define (get-age) age)

    (define (student-info)
      (list (get-name) (get-sex) (get-ethnicity) (get-age)))

    (define (type-of) 'student)

    (define (dispatch message)
      (cond ((eqv? message 'get-id) get-id)
            ((eqv? message 'get-name) get-name)
            ((eqv? message 'get-sex) get-sex)
            ((eqv? message 'get-ethnicity) get-ethnicity)
            ((eqv? message 'get-age) get-age)
            ((eqv? message 'student-info) student-info)
            ((eqv? message 'type-of) type-of)))

    dispatch))
