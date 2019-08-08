;; Scheme ;;


(define lst
  '((1) 2 (3 4) 5))

(define (composed f g)
  (lambda (x) (f (g x))))

(define (remove item lst)
  (filter (lambda (x) (eq? item x)) lst))

;;; Tests
(remove 3 nil)
; expect ()
(remove 3 '(1 3 5))
; expect (1 5)
(remove 5 '(5 3 5 5 1 4 5 4))
; expect (3 1 4 4)

; Haskell:
; nub eq []       =  []
; nub eq (x : xs) =  x : nub (filter (/= x) xs)
(define (no-repeats s)
  (if (null? s)
    nil
    (cons
      (car s)
      (no-repeats (filter (lambda (x) (not (eq? x (car s)))) (cdr s))))))

(define (substitute s old new)
  (if (null? s)
    nil
    (cons
      (if (not (pair? (car s)))
        (if (eq? (car s) old) new (car s))
        (substitute (car s) old new))
      (substitute (cdr s) old new))))

; Haskell: replace x y = map (\z -> if z == x then y else z)
(define (sub-all s olds news)
  (define (zip xs ys)
    (if (or (null? xs) (null? ys))
      nil
      (cons (list (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))
  ((reduce composed (map (lambda (x-y) (lambda (s) (substitute s (car x-y) (car (cdr x-y))))) (zip olds news))) s))
