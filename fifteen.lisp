;;; solve fifteen puzzle     2020.9.10 aor

(defparameter *n* 4)
;(defparameter b0 #(9 2 3 1 5 6 4 7 8))
;(defparameter b0 #(1 2 3 4 5 6 7 8 9 10 11 12 16 15 14 13))
;(defparameter b0 #(1 11 3 4 7 10 5 8 9 7 2 12 16 15 14 13))
;(defparameter b0 #(1 7 8 3 6 11 16 14 9 10 5 2 15 4 13 12))

(defun manhattan (b)
  "calculate manhattan distance summed over all board pieces"
  (let ((i 0)(sum 0))
    (mapcar (lambda (e)
	      (multiple-value-bind (r c) (floor (1- e) *n*)
		(multiple-value-bind (r1 c1) (floor i *n*)
		  (setf sum (+ sum (abs (- r r1)) (abs (- c c1))))
		  (incf i))))
	    (coerce b 'list)) sum))

(defun rank (b)
  (if (null b) (return-from rank 0))
  (let ((sum 0))
    (dotimes (i (* *n* *n*))
      (if (eql (+ 1 i) (elt b i)) (incf sum))) sum))

(defun swap (b i j)
  (let ((temp))
    (setf temp (elt b i))
    (setf (elt b i) (elt b j))
    (setf (elt b j) temp) b))

(defun moveright (b)
  (let ((pos (position (* *n* *n*) b)))
    (if (/= (mod pos *n*) (1- *n*)) (swap b pos (+ pos 1)))))

(defun moveleft (b)
  (let ((pos (position (* *n* *n*) b)))
    (if (/= (mod pos *n*) 0) (swap b pos (- pos 1)))))

(defun moveup (b)
  (let ((pos (position (* *n* *n*) b)))
    (if (> pos (1- *n*)) (swap b pos (- pos *n*)))))

(defun movedown (b)
  (let ((pos (position (* *n* *n*) b)))
    (if (< pos (* *n* (1- *n*))) (swap b pos (+ pos *n*)))))

(defparameter oldbs nil)

(defun noexists (b)
  (if (null b) (return-from noexists nil))
  (dolist (i oldbs)
    (if (every #'= b i) (return-from noexists nil)))
;  (setf oldbs (nconc oldbs (list (copy-seq b))))
  b)

(defparameter oldrank 0)
(defparameter ply 0)
(defun next (b)
  (cond
    ((null b) (return-from next nil))
    ((= (rank b) (* *n* *n*)) (print "solved")(exit))
    ((>= (rank b) (- oldrank 5))
     (incf ply)
     (if (> (rank b) oldrank) (setf oldrank (rank b)))
;     (when (= 0 (mod ply 100))
       (sleep 0.01) (print b) (princ (rank b)) (princ " ") (princ ply) 
     (next (noexists (moveright (copy-seq b))))
     (next (noexists (movedown (copy-seq b))))
     (next (noexists (moveleft (copy-seq b))))
     (next (noexists (moveup (copy-seq b))))
     (decf ply))))

(defun breadth (b)
  (do ((bmax) (bt))
      ((>= (rank b) (* *n* *n*)))	;found solution
    (setf bmax nil)
    (dolist (i '(moveright movedown moveleft moveup))
      (setf bt (copy-seq b))
      (when (not (null (funcall i bt)))
;	(print bt)(princ (rank bt))
	(if (and (> (rank bt)(rank bmax)) (noexists bt))
	    (setf bmax bt))))
;	    (setf bmax (copy-seq bt)))))
;    (setf b (copy-seq bmax))
    (setf b bmax)
;    (setf oldbs (nconc oldbs (list (copy-seq b))))
    (setf oldbs (nconc oldbs (list b)))
    (format t "*~%~A ~A" b (rank b))
    (sleep 0.1)))

(defun game ()
  (setf b b0 oldbs nil oldrank 0 ply 0)
  ;;(next b)
  (breadth b)
  )


;; maintain a copy of game tree independently of call stack
(defparameter tree (cons b nil))

(defun traverse (b tree)
  (print (car tree))
  (cond
    ((null (car tree)))
    ((not (every #'= b (car tree)))
     (dolist (node (cdr tree))
       (traverse b (cdr node))))))

(defun nextmoves(tree)
  (print (car tree))
  (sleep 1)
  (cond
    ((null (car tree)))	;end recursion
    ((eql (rank (car tree)) 16) (print "you won")(quit))
    (t
     (push (cons (moveright (copy-seq (car tree))) nil) (cdr tree))
     (if (not (traverse (car (car (cdr tree))) tree))
	 (nextmoves (car (cdr tree))))

     (push (cons (movedown (copy-seq (car tree))) nil) (cdr tree))
     (if (not (traverse (car (car (cdr tree))) tree))
	 (nextmoves (car (cdr tree))))

     (push (cons (moveleft (copy-seq (car tree))) nil) (cdr tree))
     (if (not (traverse (car (car (cdr tree))) tree))
	 (nextmoves (car (cdr tree))))

     (push (cons (moveup (copy-seq (car tree))) nil) (cdr tree))
     (if (not (traverse (car (car (cdr tree))) tree))
	 (nextmoves (car (cdr tree))))
					;(nextmoves (car (cdr tree))))))
     )))
