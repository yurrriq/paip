(in-package #:paip)
(defpackage #:paip.simple
  (:use #:cl #:lisp-unit))
(in-package #:paip.simple)
(defun sentence ()
  (append (noun-phrase) (verb-phrase)))

(defun verb-phrase ()
  (append (Verb) (noun-phrase)))

(defun Article ()
  (one-of '(the a)))

(defun Noun ()
  (one-of '(man ball woman table)))

(defun Verb ()
  (one-of '(hit took saw liked)))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))
(defun Adj* ()
  (if (= (random 2) 0)
      nil
    (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
    nil))

(defun noun-phrase ()
  (append (Article) (Adj*) (Noun) (PP*)))

(defun PP ()
  (append (Prep) (noun-phrase)))

(defun Adj ()
  (one-of '(big little blue green adiabatic)))

(defun Prep ()
  (one-of '(to in by with on)))
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is *simple-grammar*,
   but we can switch to other grammars.")
(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))
(defmacro if-let ((name test) then &optional else)
  `(let ((,name ,test))
     (if ,name ,then ,else)))

(defun generate (phrase)
  "Generate a random sentence or phrase."
  (if (listp phrase)
      (mappend #'generate phrase)
    (if-let (choices (rewrites phrase))
        (generate (random-elt choices))
      (list phrase))))
(defun generate-alt (phrase)
  "Generate a random sentence or phrase,
   differentiating between terminal and nonterminal symbols."
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((non-terminal-p phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun non-terminal-p (category)
  "Return true iff this is a category in the grammar."
  (not (null (rewrites category))))
(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she they it these those that)))

;; (setf *grammar* *bigger-grammar*)
(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
   with a complete parse tree."
  (if (listp phrase)
      (mapcar #'generate-tree phrase)
    (if-let (choices (rewrites phrase))
        (cons phrase
              (generate-tree (random-elt (rewrites phrase))))
      (list phrase))))
(defun generate-all (phrase)
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        (t (if-let (choices (rewrites phrase))
               (mappend #'generate-all choices)
             (list (list phrase))))))

(defun combine-all (xs ys)
  "Return a list of lists formed by appending a y to an x."
  (cross-product #'append xs ys))
(defparameter *grammática-simple*
  '((sentence -> (frase-sustantiva frase-verbal))
    (frase-sustantiva -> (Artículo Sustantivo))
    (frase-verbal -> (Verbo frase-sustantiva))
    (Artículo -> el la un una)
    (Sustantivo -> hombre pelota mujer mesa)
    (Verbo -> pegó tomó gustó))
  "Una grammática simple para un subconjunto trivial del español.")
(defun cross-product (func xlist ylist)
  "Return a list of all (func x y) values."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall func x y))
                       xlist))
           ylist))
;; (setf (fdefinition 'zip-with) #'cross-product)

(define-test test-cross-product
  (assert-equal '(11 12 13
                  21 22 23
                  31 32 33)
                (cross-product #'+ '(1 2 3) '(10 20 30))))
