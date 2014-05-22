;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

(in-package :cldm)

;;; Copyright 2007 by Robert St. Amant.

;;; To download the code for the simple planners, visit
;;; <http://www.csc.ncsu.edu/faculty/stamant/simple-planners/simple-planners.html>.

;;; This software is released under the license described by Peter
;;; Norvig: <http://www.norvig.com/license.html>.  If you find this
;;; software useful, please send email to <stamant@csc.ncsu.edu>.

;;; About simple-utilities.lisp:

;;; The code in this file provides general-purpose macros and
;;; functions used by the planners.  All should be self-explanatory.

;;; ==============================
;;; General utility functions used in simple-pddl-processing.lisp and
;;; planning code.

(defun bipartition-if (predicate list)
  "Partition list into two subsets, preserving order.
  The first subset is those for which predicate returns non-nil;
  the second all other elements.  Return lists as two values."
  (loop for element in list
     if (funcall predicate element)
     collect element into true-list
     else collect element into false-list
     finally (return (values true-list false-list))))

;;; Binding utilities

(defun merge-bindings (bindings-1 bindings-2)
  (remove-duplicates (append bindings-1 bindings-2) :test #'equal))

;;; Bindings are valid as long as no codesignating variables or
;;; constants are on the list of inequality constraints.
(defun valid-bindings-p (bindings inequalities)
  (not (some #'(lambda (neq)
                 (codesignate-p (neq-lhs neq) (neq-rhs neq) bindings t))
             inequalities)))

(defun new-bindings-only (x y bindings)
  "Return only the bindings that are new, if unification is successful."
  (set-difference (unify x y bindings) bindings :test #'equal))

;;; Two variables or constants codesignate if they unify without
;;; causing a new binding to be created.  They *may* codesignate
;;; otherwise.
(defun codesignate-p (x y bindings &optional (must-codesignate-p nil))
  "Can (or do) variables x and y codesignate?"
  (let ((new-bindings (unify x y bindings)))
    (if must-codesignate-p
        (eq new-bindings bindings)      ; Do x and y codesignate?
        new-bindings)))                 ; Could x and y codesignate?

;;; Inequalities.

(defun inequalityp (form)
  ;; Some may prefer (not (= A B)), but we'll test for an explicit NEQ.
  (and (listp form) (eq (first form) 'NEQ)))

;;; neq objects look just like bindings, but are used differently.
(defun make-neq (form)
  (when (inequalityp form)
    (cons (second form) (third form))))

(defun neq-lhs (form) (car form))
(defun neq-rhs (form) (cdr form))

(defun make-neq-from-binding (binding)
  (cons (binding-var binding) (binding-val binding)))

;;; These are lists of neq objects.
(defun merge-inequalities (inequalities-1 inequalities-2)
  (remove-duplicates (append inequalities-1 inequalities-2) :test #'equal))

;;; Literals and negation

;;; A literal is either an atom or a pair, (NOT <atom>).  This doesn't
;;; distinguish literals from other kinds of objects, unfortunately.
;;; In all of the planning code, atomic literals are symbols.
(defun literalp (object)
  (or (atom object)
      (negated-literal-p object)))

(defun negated-literal-p (literal)
  (and (listp literal) (eq (first literal) 'NOT) (second literal)))

(defun negate (literal)
  (if (negated-literal-p literal)
      (second literal)
      `(NOT ,literal)))

;;; Testing when we know that literals contain no variables.

(defun literals-eql (a b)
  ;; We never create new literals (symbols or lists) but we do need to
  ;; compare them between actions, goals, and so forth: structural
  ;; isomorphism is sufficient.
  (equal a b))

(defun literals-negate-p (a b)
  (literals-eql a (negate b)))

;;; ==============================
;;; Other macros and general utility functions

(defmacro awhen (test &body body)
  "Anaphoric when: bind IT to the value of test."
  `(let ((IT ,test))
     (when IT ,@body)))

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric when: bind IT to the value of test."
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
	    (sym (gensym)))
	`(let ((,sym ,(car cl1)))
	   (if ,sym
	       (let ((it ,sym)) ,@(cdr cl1))
	       (acond ,@(cdr clauses)))))))

;;; PAIP-specific: This could be done using tree-search, in
;;; paip/search.lisp.  Students may be interested in the tradeoff
;;; between using existing tools and writing more specialized code.
(defun bounded-dfs (state goal-state successor-fn max-depth
                    &optional
                    (valid-goal-depth-p #'identity) (current-depth 0))
  "Depth-bounded DFS tree search, with a test of the level at
  which a goal is found.  Find goal-state, starting start from
  state and searching according to successors.  Use max-depth as
  a bound on the depth of the tree.  Only return goal-state if
  it's at a valid depth (by default any depth, but we could be
  looking for goals beyond a specific depth, or at odd-numbered
  depths, etc)."
  (cond ((and (eql state goal-state)
              (funcall valid-goal-depth-p current-depth))
         goal-state)
        ((>= current-depth max-depth) nil)
        (t (some #'(lambda (s)
                     (bounded-dfs s goal-state successor-fn max-depth
                                  valid-goal-depth-p (1+ current-depth)))
                 (funcall successor-fn state)))))

(defun arg-min (sequence accessor)
  "Return the element of sequence for which the value of accessor is minimized.
  When the minimal value is not unique, return the first associated element."
  (let* ((best (elt sequence 0))
         (min (funcall accessor best)))
    (map nil #'(lambda (e)
                 (let ((value (funcall accessor e)))
                   (when (< value min)
                     (setf best e
                           min value))))
         (subseq sequence 1))
    (values best min)))

(defmacro do-forever ((&key (practical-limit nil))
		      &body body)
  "Iterate BODY either in an infinite loop or until
  PRACTICAL-LIMIT iterations have been completed."
  (if practical-limit
      (let ((counter (gensym)))
	`(loop for ,counter below ,practical-limit do
	      (progn ,@body)))
      `(loop (progn ,@body))))

(defmacro repeat-while-flag (&body body)
  "Iterate BODY in an infinite loop.  An internal flag is reset
  to nil on every pass.  As long as there is some call to the
  local function UPDATE-FLAG with a non-nil argument, the loop
  continues. Otherwise the loop exits."
  (let ((flag (gensym)))
    `(loop for ,flag = nil do
	  (flet ((update-flag (value)
		   (setf ,flag (or value ,flag))))
	    ,@body)
	(when (not ,flag)
	  (return)))))

;;; Thanks to Alan Crowe (<alan@cawtech.demon.co.uk>) for this macro.
(defmacro do-unordered-pairs ((var1 var2 list-form &optional return-form)
			      &body code)
  "Run BODY with X and Y bound to each unorderded pair of LIST."
  (let ((function-name (gensym))
        (data-holder (gensym)))
    `(let ((,data-holder ,list-form))
       (flet ((,function-name (,var1 ,var2)
		,@code))
	 (loop for (x . rest) on ,data-holder do
              (loop for y in rest do (,function-name x y)))
	 ,return-form))))

(defun some-unordered-pair (function list)
  "Return the first pair of values in LIST for FUNCTION returns
  non-NIL."
  (loop for (x . rest) on list do
       (loop for y in rest
	  for value = (funcall function x y) do
	  (when value
	    (return-from some-unordered-pair value)))))

(defun every-unordered-pair (function list)
  (not (some-unordered-pair (complement function) list)))

(defun intersection-p (list1 list2 &key (test #'eql))
  "Analogous to CL:INTERSECTION, which returns the set
  intersection of LIST1 and LIST2.  Return only the first
  element (from LIST1) that passes TEST."
  (find-if #'(lambda (e1)
	       (find e1 list2 :test test))
	   list1))

(defun sets-equal (set1 set2 &rest args)
  ;; Really inefficient
  (and (null (apply #'set-difference set1 set2 args))
       (null (apply #'set-difference set2 set1 args))))

(defmacro do-subsets ((subset list &optional return-form) &body body)
  "Run BODY with SUBSET bound to each of the subsets of LIST."
  (let ((list-data (gensym))
	(counter (gensym))
	(function-name (gensym)))
    `(let ((,list-data ,list))
       (flet ((,function-name (,subset)
		,@body))
	 (dotimes (,counter (1+ (length ,list-data)) ,return-form)
	   (mapc #',function-name (subsets-of-size-k ,list-data ,counter)))))))

;;; Common Lisp translation of a Scheme function in comp.lang.scheme:
;;; http://groups.google.com/group/comp.lang.scheme/msg/70b55d5643274ead
(defun subsets-of-size-k (l k)
  (labels ((ss (len n little-acc big-acc)
	     (cond ((zerop n) (cons little-acc big-acc))
		   ((> n len) big-acc) 
		   (t
		    (let ((last-pair (nthcdr (- len 1) l))) 
		      (ss (- len 1) (- n 1) 
			  (if (eq (cdr last-pair) little-acc) 
			      last-pair 
			      (cons (car last-pair) little-acc)) 
			  (ss (- len 1) n little-acc big-acc)))))))
    (ss (length l) k '() '())))

(defun all-such-that (predicate list &key key)
  "Return the non-nil predicate values for elements in the list.
  Conceptually: (remove nil (mapcar predicate list)), if key is not provided."
  (loop for elt in list
     as value = (if key
                    (funcall predicate (funcall key elt))
                    (funcall predicate elt))
     when value
     collect value))

;;; ==============================
;;; EOF
