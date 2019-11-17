; Full name: Dominik Tabak
; AAU study number: 20196701
; AAU mail: dtabak19@student.aau.dk
; Program status: Program contains implementation of: Random grouping,
; grouping by counting, balanced grouping by counting.
; Program also contains functions for selector functions for groups,
; maximum and minum number of group members, number of groups and
; various helper methods for all functions.
; Scheme system used: Racket

#lang racket

; Returns a list of all the students contained in the file "all-students.lsp".
; Uses functions imported from the racket system to complete this task.
(define all-students
  (read (open-input-file "all-students.lsp")))

; Saller version of the file "all-students", which contains only 20 students,
; and is meant for testing purposes.
(define all-students-small
  (read (open-input-file "all-students-small.lsp")))

; Returns a list of students that are part of a group with
; all the atributes of the student. Since the group is composed as a
; association list with student id's, the function "fnid-student-by-id" is
; called and it finds the rest of the students attributes.
(define (group-with-full-student-info group)
  (if (null? group)
      '()
      (cons (find-student-by-id (car group))
            (group-with-full-student-info (cdr group)))))

; Returns a list which represents a student which contains
; all of the students attributes. Uses a helper method to complete the task.
(define (find-student-by-id id)
  (find-student-by-id-helper all-students id))

; Iterates over the list of all students and compares their id's to the
; given id. Once it finds a match it returns the student, which is a list
; of all the students attributes.
(define (find-student-by-id-helper lst id)
  (if (null? lst)
      '()
      (if (not (string? id))
          (error "Id is not type of string")
          (if (equal? (car (car lst)) id)
              (car lst)
              (find-student-by-id-helper (cdr lst) id)))))

; Given a grouping and a group-id this function creates a list of id's
; which correspond to students who have been assigned to the group whose
; group id is given as a parameter.
(define (get-group grouping group-id)
  (if (null? grouping)
      '()
      (if (= (car (car grouping)) group-id)
          (cons (cdr (car grouping))
                (get-group (cdr grouping) group-id))
          (get-group (cdr grouping) group-id))
          ))

; This function will filter a grouping so that only a few members remain.
; The function "remove-duplicates" filters the group so that only one
; occurrence of the group-id remains. Then the length function just counts
; the left members and that number corresponds with the number of groups
; in a grouping.
(define (get-no-of-groups grouping)
  (length (remove-duplicates grouping #:key car)))

; Returns the max group size of all the groups in a given grouping by means
; of a helper function.
(define (get-max-group-size grouping)
  (get-max-group-sizes-helper grouping (get-all-group-ids grouping) 0))

; Returns the largest group size of all the groups in a grouping by iterating
; over all the group id's which are then sent along with the grouping to a
; function that calulates the max number of members for that specific group.
; It compares the most recent max number with the currently calculated group size.
; The bigger one is sent as a parameter to the next iteration.
(define (get-max-group-sizes-helper grouping group-ids max)
  (if (null? group-ids)
      max
      (let ((temp (get-max-of-group grouping (car group-ids) 0)))
        (if (> temp max)
            (get-max-group-sizes-helper grouping (cdr group-ids) temp)
            (get-max-group-sizes-helper grouping (cdr group-ids) max)))))

; Returns the size of a group in grouping by counting the members of a group
; whose group id matches the one given.
(define (get-max-of-group grouping group-id cnt)
  (if (null? grouping)
      cnt
      (if (= (car (car grouping)) group-id)
          (get-max-of-group (cdr grouping) group-id (+ cnt 1))
          (get-max-of-group (cdr grouping) group-id cnt))))

; Iterates over a grouping and returns all the group id's.
(define (get-all-group-ids grouping)
  (remove-duplicates (map (lambda (lst) (car lst)) grouping)))

; Returns the minimum group size in a given grouping by means of a helper function.
(define (get-min-group-size grouping)
  (let* ((group-ids (get-all-group-ids grouping))
         (id-of-first-group (car group-ids))
         (max-of-first-group (get-max-of-group grouping id-of-first-group 0)))
    (get-min-group-size-helper grouping (cdr group-ids) max-of-first-group)))

; Returns the minimum group size of all the groups in a grouping by iterating
; over all the group id's which are then sent along with the grouping to a
; function that calulates the max number of members for that specific group.
; It compares the most recent min number with the currently calculated group size.
; The bigger one is sent as a parameter to the next iteration.
(define (get-min-group-size-helper grouping group-ids min)
  (if (null? group-ids)
      min
      (let ((temp (get-max-of-group grouping (car group-ids) 0)))
        (if (< temp min)
            (get-min-group-size-helper grouping (cdr group-ids) temp)
            (get-min-group-size-helper grouping (cdr group-ids) min)))))

; Returns a random grouping by means of a helper function.
(define (random-grouping sl gsl)
  (if (is-gsl-ok gsl sl)
      (random-grouping-helper sl gsl 1)
      (error "gsl should be a list of non-negative integers and whose sum should be equal to the number of students")))

(define (random-grouping-helper sl gsl id-cnt)
  (if (null? gsl)
      '()
      (if (= (car gsl) 0)
      (random-grouping-helper sl (cdr gsl) (+ id-cnt 1))
      (let ((random-student (list-ref sl (random (length sl)))))
        (cons (cons id-cnt (car random-student))
              (random-grouping-helper
               (remove random-student sl equal?)
               (cons (- (car gsl) 1) (cdr gsl))
               id-cnt))
        ))))

; Checks if all the numbers combined from the gsl list are equal to the number of students.
(define (is-gsl-ok gsl sl)
  (if (= (foldr add-gsl-numbers 0 gsl) (length sl))
      #t
      #f))

; Checks if a number from the gsl list is greater than zero an then adds it.
(define (add-gsl-numbers num1 num2)
  (if (not (<= num1 0))
      (+ num1 num2)
      0))

(define (group-by-counting per-group-limit student-list)
  (if (and (> per-group-limit 0) (integer? per-group-limit))
      (group-by-cnt-helper per-group-limit student-list 1)
      (error "Number of students per group has to be a non-negative integer")));

(define (group-by-cnt-helper boundary lst cnt)
  (if (not (null? lst))
      (cons (cons (my-modulo cnt boundary)
                  (car (car lst)))
            (group-by-cnt-helper boundary (cdr lst) (+ cnt 1)))
      '()))

(define (my-modulo dividend divisor)
  (let ((remainder (modulo dividend divisor)))
       (if (= remainder 0)
           divisor
           remainder)))

(define (balanced-grouping-by-counting per-group-limit sl)
  (group-by-counting per-group-limit
                     (sort-students-for-cnt
                      (sort-by-ethn-then-by-sex sl (get-all-nationalities sl)))))

(define (sort-students-for-cnt sl)
  (if (null? sl)
      '()
      (append (car sl) (sort-students-for-cnt (cdr sl)))))

(define (sort-by-ethn-then-by-sex sl nationalities)
  (if (null? nationalities)
      '()
      (cons (sort (group-by-ethnicity sl (car nationalities)) #:key third string<?)
            (sort-by-ethn-then-by-sex sl (cdr nationalities)))))

(define (group-by-ethnicity sl ethnicity)
  (if (not (null? sl))
      (if (equal? (fourth (car sl)) ethnicity)
      (cons (car sl) (group-by-ethnicity (cdr sl) ethnicity))
      (group-by-ethnicity (cdr sl) ethnicity))
      '()))

(define (get-all-nationalities sl)
  (remove-duplicates (map (lambda (lst) (fourth lst)) sl)))




















