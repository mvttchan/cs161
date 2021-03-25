;PAD takes an integer N and returns the Nth value of
;the Padovan sequence through recursion
(defun PAD (N)
  (cond ((< N 3) 1) ;the first 3 values are 1, base cases
        ;PAD(n + 1) = PAD(n - 1) + PAD(n - 2)
        ;PAD(n)     = PAD(n - 2) + PAD(n - 3)
        (t (+ (PAD(- N 2)) (PAD(- N 3))))
        )
  )

;Sums take an integer N and calculates the operations
;required to reach that Nth Padovan number
(defun SUMS (N)
  (cond ((< N 3) 0) ;Base cases, they require no calculations/operations
        ;This uses the same logic as PAD, but add one since
        ;we aim to count the number of calcuations
        (t (+ 1 (SUMS(- N 2)) (SUMS(- N 3))))
        )
  )

;ANON takes a tree TREE in list form and changes all terminals
;to be the token '?
(defun ANON (TREE)
  (cond ((null TREE) ()) ;Base case, empty list returns empty list
        ((atom TREE) '?) ;If singleton, then return just one '?
        ;this one below is deep replacement, take the car of TREE
        ;and replace everything inside with '? and cons it with
        ;the cdr of TREE with everything replaced with '?
        (t (cons (ANON (car TREE)) (ANON (cdr TREE))))
        )
  )
