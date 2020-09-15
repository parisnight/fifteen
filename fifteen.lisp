;;; solve fifteen puzzle     2020.9.10 aor

(defparameter *n* 3)
(defparameter b #(9 2 3 1 5 6 4 7 8))
;(defparameter b #(1 2 3 4 5 6 7 8 9 10 11 12 16 15 14 13))
;(defparameter b #(1 11 3 4 7 10 5 8 9 7 2 12 16 15 14 13))

(defun rank (b)
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
  (setf oldbs (nconc oldbs (list (copy-seq b))))
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

(defun game ()
  (setf oldbs nil oldrank 0 ply 0)
  (next b))
