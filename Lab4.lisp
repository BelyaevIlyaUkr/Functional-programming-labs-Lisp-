(defclass associative-list ()
  ((lst :accessor lst-data
        :initform '())))

(defclass binary-tree ()
  ((lst :accessor lst-data
        :initform '())))

(defgeneric dict-add (dict key val))
(defgeneric dict-get (dict key))
(defgeneric dict-delete (dict key))

(defmethod dict-add ((dict associative-list) key val)
  (let* ((prev (dict-get dict key))
         (cur-lst (lst-data dict))
         (new-lst (if prev
                      (mapcar (lambda (elm)
                                (if (eq (car elm) key)
                                    (cons key val)
                                    elm))
                              cur-lst)
                      (cons (cons key val) cur-lst))))
    (setf (lst-data dict) new-lst)
    (if prev
        (values prev t)
        (values nil nil))))

(defmethod dict-get ((dict associative-list) key)
  (let ((found-elm (find-if (lambda (elm)
                              (eq key elm))
                            (lst-data dict)
                            :key #'car)))
    (if found-elm
        (values (cdr found-elm) t)
        (values nil nil))))

(defmethod dict-delete ((dict associative-list) key)
  (let ((found-elm (dict-get dict key)))
    (if found-elm
        (progn
          (setf (lst-data dict)
                (remove-if (lambda (elm)
                             (eq (car elm) key))
                           (lst-data dict)))
          (values found-elm t))
        (values nil nil))))



(defmethod dict-add ((dict binary-tree) key val)
  (let (prev-val)
    (labels ((%traverse-tree (tree)
               (if tree
                   (cond
                     ((eq key (car (car tree)))
                      (setf prev-val (second (car tree)))
                      (list (list key val) (second tree) (third tree)))
                     ((string< key (car (car tree)))
                      (list (car tree) (%traverse-tree (second tree)) (third tree)))
                     ((string> key (car (car tree)))
                      (list (car tree) (second tree) (%traverse-tree (third tree)))))
                   (list (list key val)))))
      (let ((new-tree (%traverse-tree (lst-data dict))))
        (setf (lst-data dict) new-tree)
        (if prev-val
            (values prev-val t)
            (values nil nil))))))


(defmethod dict-get ((dict binary-tree) key)
  (labels ((%traverse-tree (tree)
             (when tree
               (cond
                 ((eq key (car (car tree)))
                  (second (car tree)))
                 ((string< key (car (car tree)))
                  (%traverse-tree (second tree)))
                 ((string> key (car (car tree)))
                  (%traverse-tree (third tree)))))))
    (let ((res (%traverse-tree (lst-data dict))))
      (values res
              (and res t)))))

(defmethod dict-delete ((dict binary-tree) key)
  (let ((found-val (dict-get dict key)))
    (labels ((%collect-roots (tree)
               (when tree
                 (cons (car tree)
                       (append (%collect-roots (second tree))
                               (%collect-roots (third tree)))))))
      (if found-val
          (let ((roots (remove-if (lambda (root)
                                    (or (eq key (car root))
                                        (null root)))
                                  (%collect-roots (lst-data dict)))))
            (setf (lst-data dict) nil)
            (mapc (lambda (root)
                    (dict-add dict (car root) (second root)))
                  roots)
            (values found-val t))
          (values nil nil)))))

