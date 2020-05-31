(defun differentiate (var expr)
  (labels ((%differentiate (expr)
             (typecase expr
               (number 0)
               (symbol (if (eq expr var)
                           1
                           (error "Unknown ~A variable" expr)))
               (list
                (let ((operator (car expr))
                      (left-operand (second expr))
                      (right-operand (third expr))
                      (redundant-operand (fourth expr)))
                  (case operator
                    ((+ -)
                     (%check-redundant-operand operator redundant-operand)
                     (let ((diff-left-operand (%differentiate left-operand))
                           (diff-right-operand (%differentiate right-operand)))
                       (%create-sub-or-add-expr operator
                                                diff-left-operand
                                                diff-right-operand)))
                    ((* /)
                     (%check-redundant-operand operator redundant-operand)
                     (let ((main-sub-expr
                            (%create-sub-or-add-expr (case operator
                                                       (* '+)
                                                       (/ '-))
                                                     (%create-product-expr
                                                      (%differentiate left-operand)
                                                      right-operand)
                                                     (%create-product-expr
                                                      left-operand
                                                      (%differentiate right-operand)))))
                       (if (eq operator '/)
                           (if (compare-numbers 0 right-operand)
                               main-sub-expr
                               (list operator
                                     main-sub-expr
                                     (list 'expt right-operand 2)))
                           main-sub-expr)))
                    ((sin cos)
                     (%create-product-expr
                           (%differentiate left-operand)
                           (if (eq operator 'sin)
                               (list 'cos left-operand)
                               (list '*
                                     -1
                                     (list 'sin left-operand)))))
                    (expt
                     (let ((new-power (- right-operand 1))
                           (diff-operand (%differentiate left-operand)))
                       (%create-product-expr
                        diff-operand
                        (%create-product-expr
                         right-operand
                         (if (compare-numbers 1 new-power)
                             left-operand
                             (if (compare-numbers 0 new-power)
                                 1
                                 (list 'expt left-operand new-power))))))))))))
           (%check-redundant-operand (operator redundant-operand)
             (when redundant-operand
               (error "~A is binary operator" operator)))
           (%create-product-expr (left-operand right-operand)
             (cond
               ((or (compare-numbers 0 left-operand)
                    (compare-numbers 0 right-operand))
                0)
               ((compare-numbers 1 left-operand)
                right-operand)
               ((compare-numbers 1 right-operand)
                left-operand)
               (t (list '*
                        left-operand
                        right-operand))))
           (%create-sub-or-add-expr (operator left-operand right-operand)
             (if (compare-numbers 0 right-operand)
                 left-operand
                 (if (and (eq operator '+) (compare-numbers 0 left-operand))
                     right-operand
                     (list operator left-operand right-operand)))))
    (when expr
      (%differentiate expr))))

(defun compare-numbers (x y)
  (and (numberp x)
       (numberp y)
       (= x y)))
