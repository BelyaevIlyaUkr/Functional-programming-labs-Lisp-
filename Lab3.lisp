;; map - is defined word in package CL 
(defun map-lst (func lst)
  (when lst
    (cons (funcall func (car lst))
          (map-lst func (cdr lst)))))

(defun filter (func lst)
  (when lst
    (if (funcall func (car lst))
        (cons (car lst) (filter func (cdr lst)))
        (filter func (cdr lst)))))

(defun fold (func lst &optional (initial-value nil initial-value-p) )
  (print lst) (print initial-value) ( if lst
                      (if initial-value-p
                          (fold func (cdr lst) (funcall func initial-value (car lst)))
                          (fold func (cdr lst) (car lst) ))
                      initial-value))
