#lang racket

;; UPDATED ps3a (version 2)

;; Name: Roy Van Liew
;; Section: 91.301.201 - Organization of Programming Languages
;; Note: As usual, heavy commenting but is used to guide me through doing
;;       these problems.

;; +++++++++++++++ Required for auto grading ++++++++++++++++++++++++++++
(define nil '())

(define (double x) (* x 2))

(define (square x) (* x x))
 
;; This definition of scale-list was taken from the SICP reading.
(define (scale-list items factor)
  (if (null? items)
      nil ;; This means we reached the end of the list
      (cons (* (car items) factor) ;; Apply scale to first item in pair
            (scale-list (cdr items) ;; Go through the cdr of the list
                        factor)))) ;; End define of scale-list

;; This definition of map was taken from the SICP reading.
(define (map proc items)
  (if (null? items)
      nil ;; This means we reached the end of the list
      (cons (proc (car items)) ;; Apply the procedure to the first item in the pair
            (map proc (cdr items))))) ;; Go down the sublists with "cdr"
			
;; This definition of accumulate was taken from the SICP reading.
(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))
		 

(define x (list (list 1 2) (list 3 (list 4 5)))) 

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; **********************************************************************

;; 1. SICP exercise 2.2 (pp. 89-90). In this problem you create data
;; structures in Scheme representing line segments in a plane.
;("(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 2 2))))"), "'(1 1)")
; DONE

(define (print-point p) 
   ;(newline) 
   (display "'(")   ;;don't remove the single quote. It is needed for
		    ;;auto grader
   (display (x-point p)) 
   (display " ") 
   (display (y-point p)) 
   (display ")")) 
;; Fill in the below procedures  

;; Point Constructors and Selectors
(define (make-point x y) 
  (cons x y)) ;; Creates one point coordinate (pair) through cons

(define (x-point p) 
  (car p)) ;; Selector for x-coordinate (first thing in coordinate)

(define (y-point p) 
  (cdr p)) ;; Selector for y-coordinate (second thing in coordinate)
 
;; Segment Constructor. A line segment is a pair of coordinate pairs.
(define (make-segment start-point end-point)
  (cons start-point end-point)) ;; start-point = first pair, end-point = second pair

(define (start-segment segment) 
  (car segment)) ;;  Line segment contains two coordinate pairs, return first pair. 

(define (end-segment segment)
  (cdr segment)) ;;  Line segment contains two coordinate pairs, return second pair.
  
 ;; Midpoint adds the x-values of two coordinate pairs, divides them by 2, and
 ;; adds the y-value of the two coordinate pairs, divides them by 2 as well.
 ;; The resulting x and y coordinates are the new midpoint.
(define (midpoint-segment segment)
  ;; Begin let local variable definitions
  (let ((x_1 (car (start-segment segment))) ;; returns the x-value of the first coordinate pair
        (x_2 (car (end-segment segment))) ;; returns the x-value of the second coordinate pair
        (y_1 (cdr (start-segment segment))) ;; returns the y-value of the first coordinate pair
        (y_2 (cdr (end-segment segment))) ;; returns the y-value of the second coordinate pair        
        ) ;; End let local variable definitions 
    ;; Body to act on these local variables
    (define mid_x (/ (+ x_1 x_2) 2)) ;; Adds two x values and divides by 2
    (define mid_y (/ (+ y_1 y_2) 2)) ;; Adds two y values and divides by 2
    (make-point mid_x mid_y) ;; Make a new coordinate pair for the midpoint
    ) ;; End entire let
) ;; End define

;; **********************************************************************

;; 2. SICP exercise 2.3 (pp. 90). Here you represent rectangles and
;; construct procedures to compute perimeter and area.  Fill in the
;; below procedures
 
;; Constructor for rectangle, originally accepting bottom-left and top-right pairs
;; The first pair is bottom-left, second pair is top-right. From these pairs we can
;; also make bottom-right and top-left.
(define (make-rect bottom-left top-right)
  (make-segment bottom-left top-right)
  ) 

;; Remember rect simply just holds two points: bottom-left and top-right respectively
(define (bottom-left rect)
  (start-segment rect)) ;; (start-segment rect) == bottom-left coordinate pair

(define (top-right rect) 
  (end-segment rect)) ;; (end-segment rect) == top-right coordinate pair

;; The bottom-right coordinates are as follows.
;; x: The x coordinate of top-right
;; y: The y coordinate of bottom-left
(define (bottom-right rect)
  ;; Begin let local variable definitions
  (let ((x_top-right (x-point (top-right rect))) ;; Get the x coordinate from top-right
        (y_bottom-left (y-point (bottom-left rect))) ;; Get the y coordinate from bottom-left      
        ) ;; End let local variable definitions 
    (make-point x_top-right y_bottom-left) ;; Create bottom-right point through these coordinates
  ) ;; End entire let
)

;; The top-left coordinates are as follows.
;; x: The x coordinate of bottom-left
;; y: The y coordinate of top-right
(define (top-left rect)
   ;; Begin let local variable definitions
  (let ((x_bottom-left (x-point (bottom-left rect))) ;; Get the x coordinate from bottom-left
        (y_top-right (y-point (top-right rect))) ;; Get the y coordinate from top-right  
        ) ;; End let local variable definitions 
    (make-point x_bottom-left y_top-right) ;; Create top-left point through these coordinates
  ) ;; End entire let
) 
 
;; Width relates to the x-values of the rectangle.
;; Use the abs function so we don't result with negative width.
;; Take x-coordinates of bottom-left and top-right, subtract them.
(define (width-rect rect)
   (let ((x_bottom-left (x-point (bottom-left rect)))
         (x_top-right (x-point (top-right rect)))
         ) ;; End let local variable definitions 
       (abs (- x_bottom-left x_top-right)) ;; Subtract the two x coordinates
   ) ;; End entire let
) 

;; Height relates to the y-values of the rectangle.
;; Use the abs function so we don't result with negative height.
;; Take y-coordinates of bottom-left and top-right, subtract them.
(define (height-rect rect) 
   (let ((y_bottom-left (y-point (bottom-left rect)))
         (y_top-right (y-point (top-right rect)))
         ) ;; End let local variable definitions
     (abs (- y_bottom-left y_top-right)) ;; Subtract the two y coordinates
   )
)

;; Area = width * height
(define (area-rect rect)
   (* (width-rect rect) (height-rect rect))
) 

;; Perimeter = 2 * (width + height)
(define (perimeter-rect rect)
   (* 2 (+ (width-rect rect) (height-rect rect)))
)
 
;; **********************************************************************

;; 3. SICP exercise 2.4. Here is an alternative procedural
;; representation of pairs. Note that (my-car
;; (my-cons x y)) yields x for any objects x and y.

;; Here, my-cons returns a procedure that takes one argument, and this argument
;; is then applied to a pair. first is the 1st element of the pair, etc.
(define (my-cons first-element second-element)
  ;; lambda is a procedure that takes an argument "m" (a procedure) and applies
  ;; it to a pair of arguments. In our test, these two arguments are 14 and 15.
  (lambda (m) (m first-element second-element)))

(define (my-car z)
  ;; "z" is a procedure that takes a procedure
  ;; as an argument, and applies said procedure to its stored pair.
  (z (lambda (first-element second-element) first-element)))

;; What is the corresponding definition of my-cdr? (Hint: To verify
;; that this works, make use of the substitution model of Section
;; 1.1.5.)  Fix the code below to work.

(define (my-cdr z)
  ;; "z" is a procedure that takes a procedure
  ;; as an argument, and applies said procedure to its stored pair.
  (z (lambda (first-element second-element) second-element)))

;; **********************************************************************

;; 4. Consider the following recursive procedure:
(define (list-prod lst) 
  (if (null? lst) 
      1 
    (* (car lst) 
       (list-prod (cdr lst))))) 

;; a) How many times is list-prod called when evaluating the
;; expression (list-prod '(1 2 3 4)) ?
;(list-prod '(1 2 3 4))		(* 1 (24) )
;  (list-prod '(2 3 4))		(* 2 (12) )
;    (list-prod '(3 4))		(* 3 (4) )
;      (list-prod '(4)))	(* 4 (1) )
;        (list-prod '()))	(* 1)
; list-prod recursively calls itself four times.

;; b) What is the order of growth in space and time for the list-prod
;; procedure above?
; The order of growth is O(n), and the time is O(n). In other words,
; it's linear in both time and space being a recursive process.

;; c) Write an iterative version of the procedure list-prod.

;; d) What is the order of growth in space and time for your iterative
;; list-prod procedure from part c?
; The order of growth is O(1), and the time is O(n). In other words,
; the iterative version is constant in space, but linear in time as it
; has to do the same number of steps.

;; Answer (a) (b) and (d) using comment line and change #f to #t 
(define p4 #t)

;; For (c), fill in the below procedure
(define (list-prod-iter lst)
  (define (list-iter-helper lst prod)
    (if (null? lst) ;; There isn't a need for a "counter" here, null denotes end of list
        prod ;; If we've reached the end of the list, return current product
        (list-iter-helper (cdr lst) (* (car lst) prod)) ;; use cdr to get the sublist (rest of the list)
        ) ;; end if
   ) ;; end inner define
  (list-iter-helper lst 1) ;; Product starts at 1, end of list is termination
)

;; **********************************************************************

;; 5. Write a procedure called double-list in three different ways:
;; a) Write a list manipulation procedure of your own to do this.
;; b) Use the scale-list procedure on pp. 105 in your definition. < 143 in SICP
;; c) Use the map procedure on pp. 105 in your definition. < 143/144

;;Answer:
;a)
;; So essentially, the recursive call will eventually get to the end of the list,
;; then work towards the beginning of the list from there. The cons statement
;; in the recursive call has to wait for the previous recursive calls in order
;; to pair up the doubled first element with the second element of the pair,
;; this second element being the sublists throughout the list.
(define (double-list1 lst)
  (if (not (null? lst)) ;; If the list passed isn't null, double the first element as stated below.
      (cons (double (car lst)) ;; Double the first element and pair it the pairs from previous recursive calls.
            (double-list1 (cdr lst))) ;; Go through the rest of the list with "cdr".
      nil ;; Getting down to here means we reached the end of the list.
  ) ;; end if
)

;b)
;; using scale-list 
(define (double-list2 lst)
  (scale-list lst 2) ;; Scale by a factor of 2 for doubling.
)

;c)
;; map takes a procedure (in this case, double) and applies it to the first
;; element in a pair in the passed list, then calls itself with the procedure
;; again and a sublist by "cdr" to try applying this procedure to every element
;; in the list. lst is the full list at the start, but the recursion goes down
;; all the way to nil, then works its way up from there (this is how the cons
;; is able to connect everything as well).
(define (double-list3 lst)
  (map double lst) ;; Apply doubling procedure to each item in the list "lst" with map
)

;; **********************************************************************

;; 6. SICP exercise 2.21 (pp. 106), on square-list. < 144/145

(define (square-list1 items)
  (if (null? items)
      nil ;; This means we reached the end of the list.
      (cons (square (car items)) ;; Apply square procedure to first element
            (square-list1 (cdr items))) ;; Go through sublists using cdr
  ) ;; end if
) ;; end define

;; Like in double-list3, map takes a procedure (in this case, square) and
;; applies it to every element in the list "items".
(define (square-list2 items)
  (map square items)
)


;; **********************************************************************

;; 7. SICP exercise 2.33 (pp. 119), implementing map, append, and

(define (my-map proc-to-apply sequence)
  ;; In the lambda here that is the "op" argument to accumulate:
  ;; "proc-to-apply" is the procedure to apply to the first term (the "car") of the passed sequence
  ;; before pairing it up with the "cdr" of the sequence (the previous accumulated terms).
 (accumulate (lambda (first-term accumulated-terms)
               (cons (proc-to-apply first-term) accumulated-terms)) ;; end lambda
             nil ;; value to return for base case
             sequence) ;; sequence to be passed to go through with the cdr calls
) ;; end define

;; So in terms of (accumulate op initial sequence) definition, cons is the op argument,
;; "initial" in our case is seq2. sequence is seq1. The base case is if seq1 is nil, return seq2.
;; This means when we get to the end of seq1 (i.e. accumulated all its elements),
;; then just simply cons seq2 with whatever chain of elements were accumulated from seq1.
(define (my-append seq1 seq2)
 (accumulate cons seq2 seq1))

;; Note here that despite having first-term as an argument in the lambda procedure for the
;; "op" argument in accumulate, we actually aren't doing anything with the terms in the
;; list itself. We are simply just trying to get the length of the list, so first-term
;; remains unused but is needed as a placeholder argument. accumulated-terms just tracks
;; how many times we've added 1 to itself to track the length of the list.
(define (my-length sequence) 
   (accumulate (lambda (first-term accumulated-terms) ;; Adds 1 to current accumulated list length.
                 (+ 1 accumulated-terms)) ;; end lambda
               0 ;; value to return for base case.
               sequence) ;; sequence to be passed to go through with the cdr calls
) ;; end define

;; **********************************************************************

;; 8. SICP exercise 2.35 (pp. 120), redefining count-leaves as an
;; accumulation.  Fill in the below procedure. Replace '<??>. < 163

;; This is a recursive process to count the number of leaves in a given sequence resembling a tree.
;; A tree, in what we're working with, is actually just several lists inside lists.
;; To describe how map is used here:
;; map's lambda procedure checks to see if the current "node" passed is a pair. If it is a pair,
;; we're going to have to keep going down the subtrees until we find a list element that isn't
;; a pair. So the map statement then is generating a new list describing how many leaves were
;; found in each sublist. For '(1 (2 3) 4 (5 6)) which is tested, it returns '(1 2 1 2), indicating
;; that the (2 3) was a subtree with two leaves along with (5 6), and 1 and 4 were not subtrees, they
;; were just single nodes. accumulate adds up the numbers in this new list to generate an integer count.
(define (count-leaves tree)
	(accumulate + ;; The operation we want to apply here is addition to add up numbers in new list
                    0 ;; Leaves count starts at 0.
                    (map (lambda (tree-node)
                           (if (pair? tree-node) ;; A node in the tree with a pair is not a single leaf.
                               (count-leaves tree-node) ;; Keep going down the subtrees until we reach a leaf
                               1)) ;; If we get here, it means we reached a leaf (not a pair). Also, end if and lambda.
                         tree) ;; Our sequence is a tree, so it's our second argument to map. Also, end map
        ) ;; end accumulate call
) ;; end define

;; ************************ END OF FILE *********************************
(provide (all-defined-out))
