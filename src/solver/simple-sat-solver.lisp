;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

(in-package :cldm)

;;; Copyright 2010 by Stephen G. Ware and Robert St. Amant.

;;; To download the code for the simple planners, visit
;;; <http://www.csc.ncsu.edu/faculty/stamant/simple-planners/simple-planners.html>.

;;; This software is released under the license described by Peter
;;; Norvig: <http://www.norvig.com/license.html>.  If you find this
;;; software useful, please send email to <stamant@csc.ncsu.edu>.

;;; About simple-SAT-solver.lisp:

;;; These functions define a general purpose satisfiability (SAT)
;;; problem solver to be used with the simple-SAT planner.  Any SAT
;;; solver can be used with simple-SAT, but this one is an
;;; implementation of the DPLL (Davis Putnam Logemann Loveland)
;;; algorithm [AIMA, p. 222].  This algorithm was chosen because it
;;; is a fairly simple, complete, depth-first, backtracking method.

;;; The main function is solve-SAT.  It expects a propositional
;;; logical sentence in conjunctive normal form (CNF).  CNF simply
;;; means many 'or' clauses 'and'ed together.  The sentence should
;;; be phrased as Lisp code, e.g.:
;;;
;;; (solve-SAT '(and (or :X (not :Y) :Z) (or :X :Z) (not :A) :B))
;;;
;;; The function returns a set of variable bindings which makes the
;;; sentence evaluate to true (or NIL if no solution exists), e.g.:
;;;
;;; ((:Z . T) (:B . T) (:A . NIL))
;;;
;;; This means that making Z and B true will make the whole logical
;;; sentence true.  Note that A needs to be false, and hence it is
;;; bound to NIL.  Also note that no bindings are given for X and Y.
;;; This is because the logical sentence will be true no matter which
;;; truth value X and Y are given (as long as Z, B, and A are given
;;; the correct values).

;;; Some important vocabulary:
;;; A 'variable' is an element in the logical sentence which can be T
;;;   or NIL, e.g. :X.
;;; A 'literal' is a variable or its negation, e.g. :X or (not :X).
;;; A 'clause' is a disjunction of literals, e.g. (or :X (not :Y)),
;;;   or a literal, or T, or NIL.
;;; A 'sentence' is a conjunction of clauses, e.g.
;;; (and (or :X (not :Y)) :Z), or T, or NIL.

;;; See utility functions at the end of this file for relevant
;;; predicates.

;;; ==============================
;;; Top-level function

;;; Solve-SAT finds a satisfying assignment of truth values to a
;;; logical expression in conjunctive normal form, using the DPLL
;;; algorithm described in AIMA, p. 222.

;;; Some changes in structure: unit propogation is done first because
;;; it is cheaper and probably more likely to be applicable.

;;; Implementation note: multiple-value-setq returns the primary value
;;; of the form that's evaluated, with additional variables being set
;;; as a side effect.

(defun solve-SAT (cnf-exp &optional bindings)
  "Find a satisfying assignment of truth values to a logical
  expression in conjunctive normal form."
  (let (variable
        truth-value)
    (cond ((sentence-true-p cnf-exp) bindings) ; Sentence is true; return bindings.
	  ((sentence-false-p cnf-exp) nil)     ; Sentence is false.
	  ;; Otherwise find known truth value assignments in the sentence
	  ;; Unit clause propagation
	  ((multiple-value-setq (variable truth-value) (find-unit-clause cnf-exp))
	   (solve-SAT (assign-value-to-SAT-sentence cnf-exp variable truth-value)
		      (match-variable variable truth-value bindings)))
	  ;; Pure variable eliminiation
	  ((multiple-value-setq (variable truth-value) (find-pure-variable cnf-exp))
	   (solve-SAT (assign-value-to-SAT-sentence cnf-exp variable truth-value)
		      (match-variable variable truth-value bindings)))
	  ;; Choose a variable on which to branch; try both T and NIL as values.
	  ;; NIL goes first, to produce more concise plans.
	  (t (setf variable (choose-variable-from-SAT cnf-exp))
	     (or (solve-SAT (assign-value-to-SAT-sentence cnf-exp variable NIL)
			    (match-variable variable nil bindings))
		 (solve-SAT (assign-value-to-SAT-sentence cnf-exp variable T)
			    (match-variable variable t bindings)))))))

;;; This function simplifies the SAT problem and is meant to be used
;;; before calling solve-SAT.  Right now it is pretty useless; all it
;;; does is remove the constants T and NIL from a sentence where
;;; possible.  Simplifying a SAT problem can have a big effect on how
;;; long it takes to solve.  This would be a good method to improve
;;; upon.

(defun simplify-SAT (cnf-exp)
  ;; Since assign-value-to-SAT-sentence does simplification, use that.
  (assign-value-to-SAT-sentence cnf-exp nil nil))

;;; ==============================
;;; Unit clause propagation

;;; A unit clause is a clause with only one literal.

(defun find-unit-clause (cnf-exp)
  "Find a unit clause in a SAT sentence.  Return the variable of
  that literal and the truth value that must be assigned to it to
  make the clause true."
  (dolist (element (conjuncts cnf-exp))
    ;; A CNF sentence is a conjunction of clauses and literals.
    (if (literalp element)
        ;; A literal in a conjunction must have a specific truth value.
        (return (values (literal-var element) (required-truth-value element)))
	;; Otherwise, check only clauses with a single disjunct.
        (awhen (and (null (rest (disjuncts element)))
		    (first (disjuncts element)))
          ;; Return the literal's variable and the truth value needed
	  ;; to make it true.
          (return (values (literal-var it) (required-truth-value it)))))))

;;; ==============================
;;; Pure variable eliminiation

;;; Find a pure variable in a SAT sentence.  A pure variable is one
;;; that always appears with the same polarity (e.g. always :X or
;;; always (not :X)).  Return the variable and the truth value that
;;; must be assigned it if the SAT problem has a solution.  This is
;;; outright discrimination against mixed-polarity variables.  We
;;; must never show this algorithm to variable rights activists.

;; (defun find-pure-variable (cnf-exp)
;;   (let ((pure-table (make-hash-table)))
;;     (dolist (element (conjuncts cnf-exp))
;;       (if (clausep element)
;;           ;; If this element is a clause, check the purity of each literal
;;           (dolist (literal (disjuncts element))
;;             (update-pure-table pure-table
;;                                (literal-var literal)
;;                                (if (negated-literal-p literal) 'negative 'positive)))
;;           ;; Else this element is a literal, so check its purity
;;           (update-pure-table pure-table
;;                              (literal-var element)
;;                              (if (negated-literal-p element) 'negative 'positive))))
;;     ;; Check the purity table for a pure variable
;;     (maphash (lambda (key val)
;;                (when (not (eq val 'impure))
;; 		 ;; Return the variable and the truth value needed to
;; 		 ;; make its literal(s) positive in the SAT problem
;; 		 (return-from find-pure-variable
;; 		   (values key (eq val 'positive)))))
;;              pure-table)))

;; ;;; Update the entry for a variable in the variable purity hash table.

;; (defun update-pure-table (table variable polarity)
;;   (aif (gethash variable table)
;;        ;; If the variable is already in the table, check for consistency
;;        (when (not (eq it polarity))
;; 	 (setf (gethash variable table) 'impure))
;;        ;; Else, add the variable to the table
;;        (setf (gethash variable table) polarity)))

;;; A bit more concisely... 
;;; (Performance doesn't suffer for the size of problems we deal with.)
(defun find-pure-variable (cnf-exp)
  "Return a variable :X from cnf-exp that appears only in literals
  of the form :X or only in literals of the form (not :X)."
  (let ((positives nil)
	(negatives nil))
    (flet ((add-literal (literal)
	     (let ((var (literal-var literal)))
	       ;; If a literal is negated, add the variable to negatives, otherwise to positives.
	       (if (negated-literal-p literal)
		   (pushnew var negatives)
		   (pushnew var positives)))))
      (dolist (element (conjuncts cnf-exp))
	(if (literalp element)
	    ;; Add a literal to one of the purity lists.
	    (add-literal element)
	    ;; The element is a clause; add each literal.
	    (mapc #'add-literal (disjuncts element))))
      ;; Does any variable appear in one list but not the other?  It's pure.
      (acond ((set-difference positives negatives)
	      (values (first it) T))
	     ((set-difference negatives positives)
	      (values (first it) NIL))))))

;;; ==============================
;;; Variable assignments

;;; Find a variable to which a guessed truth value should be assigned.
;;; This function is very naive; it just returns the first variable.
;;; This would be a good place to improve performance.

(defun choose-variable-from-SAT (cnf-exp)
  (let ((first-element (first (conjuncts cnf-exp))))
    (if (literalp first-element)
        ;; First literal
	(literal-var first-element)
        ;; Or first literal in first clause
        (literal-var (first (disjuncts first-element))))))

;;; Assign a truth value to one of the variables in a SAT sentence
;;; and simplify the expression.

(defun assign-value-to-SAT-sentence (cnf-sentence variable value)
  ;; Base cases: sentence is T or NIL
  (if (atom cnf-sentence)
      cnf-sentence
      ;; Walk through the clauses, doing the assignment.
      (let ((new-sentence nil))
	(dolist (clause (conjuncts cnf-sentence))
	  ;; Assign the variable, see if it comes back non-nil.
	  (aif (assign-value-to-SAT-clause clause variable value)
	       ;; The assignment gives us a clause that might be true.
	       ;; If it's true, it's superfluous.
	       ;; Otherwise, add it to the new sentence.
	       (unless (sentence-true-p it)
		 (push it new-sentence)) ; Reverse order doesn't matter.
	       ;; If any clause is false, the sentence is false.
	       (return-from assign-value-to-SAT-sentence nil)))
	;; If no new clauses have been collected, it's true (by definition of conjunction).
	;; Otherwise, return the new sentence.
	(or (null new-sentence) `(and ,@new-sentence)))))

;;; Assign a truth value to one of the variables in a SAT 'or' clause
;;; and simplify the expression.

(defun assign-value-to-SAT-clause (cnf-clause variable value)
  ;; Base case: the clause is a single literal
  (if (literalp cnf-clause)
      (if (eq (literal-var cnf-clause) variable)
	  (literal-val cnf-clause value)
	  cnf-clause)
      ;; Check the assignment for possible simplifications, walking
      ;; through the literals in the conjunction.
      (let ((new-clause nil))
	(dolist (literal (conjuncts cnf-clause))
	  (if (eq (literal-var literal) variable)
	      ;; Found a literal equal to the variable.
	      ;; If the literal is being set to false, nothing happens--it's a disjunction.
	      ;; But if it's being set to true...
	      (when (literal-val literal value)
		;; ...the whole clause is true.
		(return-from assign-value-to-SAT-clause t))
	      ;; If the literal is not the variable, add it to the new clause.
	      (push literal new-clause))) ; Reverse order doesn't matter.
	;; Return the new clause if it is not empty--we've been able
	;; to add some not-necessarily false literal to it.
	(and new-clause `(or ,@new-clause)))))

;;; ==============================
;;; Utility functions and syntactic sugar

;;; literalp is called above; see simple-utilities.lisp for
;;; definition.  (Literals are just atoms, and we reuse planning
;;; utility functions for SAT literals.)

(defun literal-var (literal)
  "Get a literal's variable.  A literal is either :X or (not :X)"
  ;; Assume literal is actually a literal.
  (if (negated-literal-p literal)
      (negate literal)
      literal))

(defun literal-val (literal var-value)
  "The truth value of a literal given a truth assignment for its variable.
A literal is either :X or (not :X), so var-value is negated if literal is a list."
  ;; Assume literal is actually a literal.
  (if (negated-literal-p literal)
      (not var-value)
      var-value))

(defun required-truth-value (literal)
  "A literal is is either :X or (not :X). Return T if :X, nil if (not :X)."
  ;; Assume literal is actually a literal.
  (not (negated-literal-p literal)))

(defun sentence-true-p (sentence) (eq sentence T))
(defun sentence-false-p (sentence) (eq sentence NIL))

(defun disjunctionp (form) (and (listp form) (eq (first form) 'OR)))
(defun conjunctionp (form) (and (listp form) (eq (first form) 'AND)))
(defun implicationp (form) (and (listp form) (eq (first form) 'IF)))

(defun conjuncts (conjunction) (rest conjunction))
(defun disjuncts (disjunction) (rest disjunction))

(defun operands (form) (rest form)) ; any logical form

;;; ==============================
;;; EOF
