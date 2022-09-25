(set xxx (lambda (n) (cons n (lambda () (xxx (+ n 1))))))

(set yyy (xxx 1))

(print (car yyy))
(set yyy ((cdr yyy)))

(print (car yyy))
(set yyy ((cdr yyy)))

(print (car yyy))
(set yyy ((cdr yyy)))

(print (car yyy))
(set yyy ((cdr yyy)))

(print (car yyy))
(set yyy ((cdr yyy)))
