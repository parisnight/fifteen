;;; solve fifteen puzzle     2020.9.10 aor

;(defparameter b #(1 2 3 4 5 6 7 8 9 10 11 12 16 15 14 13))
(defparameter b #(1 11 3 4 7 10 5 8 9 7 2 12 16 15 14 13))

(defun rank (b)
  (let ((sum 0))
    (dotimes (i 16)
      (if (eql (+ 1 i) (elt b i)) (incf sum))) sum))

(defun swap (b i j)
  (let ((temp))
    (setf temp (elt b i))
    (setf (elt b i) (elt b j))
    (setf (elt b j) temp) b))

(defun moveright (b)
  (let ((pos (position 16 b)))
    (if (not (find pos #(3 7 11 15))) (swap b pos (+ pos 1)))))

(defun moveleft (b)
  (let ((pos (position 16 b)))
    (if (not (find pos #(0 4 8 12))) (swap b pos (- pos 1)))))

(defun moveup (b)
  (let ((pos (position 16 b)))
    (if (not (find pos #(0 1 2 3))) (swap b pos (- pos 4)))))

(defun movedown (b)
  (let ((pos (position 16 b)))
    (if (not (find pos #(12 13 14 15))) (swap b pos (+ pos 4)))))

(defparameter oldbs nil)

(defun noexists (b)
  (if (null b) (return-from noexists nil))
  (dolist (i oldbs)
    (if (every #'= b i) (return-from noexists nil)))
  (setf oldbs (nconc oldbs (list (copy-seq b))))
  b)

(defparameter oldrank 0)
(defun next (b)
  (cond
    ((null b) (return-from next nil))
    ((= (rank b) 16) (print "solved")(exit))
    ((>= (rank b) (- oldrank 3))
     (if (> (rank b) oldrank) (setf oldrank (rank b)))
     (sleep 0.1) (print b) (prin1 (rank b))
     (next (noexists (moveright (copy-seq b))))
     (next (noexists (movedown (copy-seq b))))
     (next (noexists (moveleft (copy-seq b))))
     (next (noexists (moveup (copy-seq b)))))))

(defun game ()
  (setf oldbs nil oldrank 0)
  (next b))
