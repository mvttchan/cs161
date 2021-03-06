;
; Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that
; "node n gets color c" (when there are k possible colors).
; This function returns an integer. The formula used was
; provided within the homework's spec
(defun node2var (n c k)
  (+ c (* k (- n 1)))
  )

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
; This function returns the clause c or c+1 or c+2 or ... k
; The logic is that if even one of those values are true,
; then the whole clause is true
(defun at-least-one-color (n c k)
  (cond
    ((< c k) (cons (node2var n c k) (at-least-one-color n (+ c 1) k)))
    ((= c k) (list (node2var n c k)))
    ((> c k) nil)
    ))

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
; The helper function does most of the work here. I have to
; make sure that c cannot be the same value as i, otherwise
; we will have a statement that always evaluates to false.
; The logic is -c or -i. This is the same as "not and"
; where it evaluates to false if two of them are true.
; The returned clause is exhaustive so if any combination
; of two unique numbers are found to be true, then the
; clause is false.
(defun most-helper (n c i k)
  (cond
    ((> i k) nil)
    ((= i (- k 1)) (cons (list (- (node2var n c k)) (- (node2var n (+ i 1) k))) nil))
    ((< i k) (cons (list (- (node2var n c k)) (- (node2var n (+ i 1) k)))
             (most-helper n c (+ i 1) k)))
  ))
; The top level function is mostly just to iterate through the values
; of c all the way to k
(defun at-most-one-color (n c k)
  (cond
    ((= c k) nil)
    (t (append (most-helper n c c k) (at-most-one-color n (+ c 1) k)))
  ))
; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
; If n only receives one and only one color, then n receives
; at least one color while at the same time n receives at
; most one color
(defun generate-node-clauses (n k)
  (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k))
  )

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
; This helper function adds additional contraints that we could not have added within
; the original function, which is not to be touched. This simply builds the clause where
; the two nodes sharing an edge cannot both be true at the same time.
(defun edge-clauses-helper (x y c k)
  (cond
    ((> c k) nil)
    ((= c k) (cons (list (- (node2var x c k)) (- (node2var y c k))) nil))
    ((< c k) (cons (list (- (node2var x c k)) (- (node2var y c k)))
                   (edge-clauses-helper x y (+ c 1) k))))
)
;The top level has to split e into bindings we can use
(defun generate-edge-clauses (e k)
  (edge-clauses-helper (car e) (cadr e) 1 k)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
;
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn
  );end defun

;
; A utility function for parsing a pair of integers.
;
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun


;TESTING
(graph-coloring-to-sat "graph2.txt" "a.out" 8)

