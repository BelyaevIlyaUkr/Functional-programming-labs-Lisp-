(defun associative-list-add (lst key val)
 (cons (cons key value) lst)
  )

(defun associative-list-get (lst key)
  (if lst
    (let ((frst (car lst)))
      (if (eq (car frst) key)
          (values (cdr frst) T)
          (associative-list-get (cdr lst) key)))
    (values nil nil))
  )

(defun property-list-add (lst key val)
  (cons key (cons val lst)))

(defun property-list-get (lst key)
  (when lst
    (if (eq (first lst) key)
        (second lst)
        (property-list-get (nthcdr 2 lst) key))))

(defun binary-tree-add (tree key val)
  (if tree
      (let* ((node (first tree))
            (left (second tree))
            (right (third tree))
            (key-node (car node)))
        (cond
          ((string= key-node key)
           (list (cons key-node val) left right))
          ((string< key key-node)
           (list node (binary-tree-add left key val) right))
          (t
           (list node left (binary-tree-add right key val)))))
      (list (cons key val) nil nil))) 

(defun binary-tree-get (tree key)
  (when tree
    (let ((node (first tree)))
      (cond
        ((string= (car node) key)
         (cdr node))
        ((string< key (car node))
         (binary-tree-get (second tree) key))
        (t
         (binary-tree-get (third tree) key))))))
