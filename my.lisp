;################(5)#################
;Определите функцию, которая увеличивает элементы исходного списка на единицу
(defun inc_list (lst)
	(cond 
		((null lst) nil)
		(T (cons (+ 1 (car lst)) (inc_list (cdr lst))))))

(inc_list `(1 2 3))
;################(6)#################
;Определите функцию, переводящую список чисел в список соответствующих
;им названий.
(defun num_to_txt (lst)
	(let ((res))
		(dolist (a lst res)
			(setq res (append res (list (nth a `(ноль один два три четыре пять шесть семь восемь девять))))))))

(num_to_txt `(2 5 7))
;################(10)################
;Определите функцию, осуществляющую удаление указанного количества последних элементов исходного списка.
(defun deln (n lst)
	(cond
		((<= (list-length lst) n) nil)
		(T (cons (car lst) (deln n (cdr lst))))))

(deln 3 `(0 1 2 3 4 5 6 7 8 9))
;
;################(18)################
;Определите предикат, проверяющий, является ли аргумент одноуровневым
;списком.
(defun is_linear (lst)
	(cond
		((null lst) T)
		((atom (car lst)) (is_linear(cdr lst)))
		(T nil)))

(is_linear `(1 2 2 5))
(is_linear `(1 (2 3) 5))
;################(24)################
;Определите функции, осуществляющие преобразования между видами (a b c)
;и (((а) b) с).
(defun to_line (lst)
	(cond
		((null lst) nil)
		((atom (car lst)) (cons (car lst) (to_line (cdr lst))))
		(T (append (to_line (car lst)) (to_line (cdr lst))))))

(defun neck (lst)
	(cond
		((null (cdr lst)) nil)
		(T (cons (car lst) (neck (cdr lst))))))

(defun to_tree (lst)
	(cond
		((= 1 (list-length lst)) lst)
		(T (list (to_tree (neck lst)) (car (last lst))))))

(to_line `((1 2) 3 5 (7 8 (0 1 2))))
(to_tree `(1 2 3 4 5))
;################(26)################
;Определите функцию, разбивающую список (a b с d...) на пары ((а b) (с
;d)...).
(defun to_pairs (lst)
	(cond
		((<= (list-length lst) 2) (list lst))
		(T (append (list (list (car lst) (cadr lst))) (to_pairs (cddr lst))))))

(to_pairs `(1 2 3 4 5 6 7 8))

;################(37)################
;Определите функцию ПЕРЕСЕЧЕНИЕ, формирующую пересечение двух множеств,
;т.е. множество из их общих элементов.
(defun is_member (a s)
	(cond
		((null s) nil)
		((eq a (car s)) T)
		(T (is_member a (cdr s)))))

;(is_member 1 `(2 4 1 5 6 3))

(defun intersect (a b)
	(let ((res))
		(dolist (i a res)
			(cond
				((null b) nil)
				((is_member i b) (setq res (cons i res)))))))

(intersect `(1 2 3 4 5) `(3 5))

;################(42)################
;Определите функцию, находящую максимальное из значений, находящихся в
;вершинах дерева.
(defun to_line (lst)
	(cond
		((null lst) nil)
		((atom (car lst)) (cons (car lst) (to_line (cdr lst))))
		(T (append (to_line (car lst)) (to_line (cdr lst))))))

(defun max_list (lst)
	(cond
		((null lst) nil)
		((= 1 (list-length lst)) (car lst))
		(T (cond
				((> (car lst) (max_list (cdr lst))) (car lst))
				(T (max_list (cdr lst)))))))

(defun max_vert (tree)
	(max_list (to_line tree)))

(max_vert `((1 2) ((3) (3 3)) 4 (5 5 (5 (5 5)))))

;################(46)################
;Предположим, что отец и мать некоторого лица, хранятся как значения соответствующих свойств у символа, 
;обозначающего это лицо. Напишите функцию (РОДИТЕЛИ x), которая возвращает в качестве значения родителей, 
;и предикат (СЕСТРЫ-БРАТЬЯ x1 x2), который истинен в случае, если x1 и x2 — сестры или братья,
;родные или с одним общим родителем.
(defun is_member (a s)
	(cond
		((null s) nil)
		((eq a (car s)) T)
		(T (is_member a (cdr s)))))

(defun intersect (a b)
	(let ((res))
		(dolist (i a res)
			(cond
				((null b) nil)
				((is_member i b) (setq res (cons i res)))))))

(defun parents (child)
	(list (get child `mom) (get child `dad)))
(defun siblings (ch1 ch2)
	(cond 
		((null (intersect (parents ch1) (parents ch2))) nil)
		(T T)))
	

(setf (get `alice `mom) `ann)
(setf (get `alice `dad) `brad)
(setf (get `bob `mom) `dan)
(setf (get `bob `dad) `brad)

(siblings `alice `bob)

;################(48)################
;Функция GET возвращает в качестве результата NIL в том случае, если у символа нет данного свойства, 
;либо если значением этого свойства является NIL.
;Следовательно, функцией GET нельзя проверить, есть ли некоторое свойство в
;списке свойств. Напишите предикат (ИМЕЕТ-СВОЙСТВО символ свойство), который проверяет,
;обладает ли символ данным свойством.
(defun hasprop (symb prop)
	(cond
		((null (remprop symb prop)) nil)
		(T T)))

(hasprop `alice `mom)

;####################################
;####################################
;################(1)#################
;Определите FUNCALL через функционал APPLY.
(defun my_funcall (f &rest r) 
	(apply f r))

(funcall `+ 1 2 3 4)
(my_funcall `+ 1 2 3 4)

;################(3)#################
;Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка
;(f1 f2 ... fn)
;к соответствующему элементу списка
;x = (x1 x2 ... xn)
;и возвращает список, сформированный из результатов.
(defun APL_APPLY (funs els)
	(cond
		((null els) nil)
		(T (cons (funcall (car funs) (car els)) (APL_APPLY (cdr funs) (cdr els))))))

(APL_APPLY `(numberp atom null) `(1 (1 1) ()))

;################(5)#################
;Определите функциональный предикат (НЕКОТОРЫй пред список), который истинен,
;когда, являющейся функциональным аргументом предикат пред истинен хотя бы для одного элемента списка список.

(mapcan `numberp `(a b c 1 n))

;################(7)#################
;Определите фильтр (УДАЛйЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список
;все элементы, которые не обладают свойством, наличие которого проверяет
;предикат пред.

