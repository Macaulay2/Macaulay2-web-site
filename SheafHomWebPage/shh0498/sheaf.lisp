;;; Here are programs for ranked posets and sheaves, emphasizing their
;;; combinatorial nature.

(proclaim '(optimize speed))

(proclaim '(freeze-type poset-elt ranked-poset quiver-rep
			quiver-rep-map
			; sheaf-morphism
			))

(defvar *load-automatically* t
  "If this is true [the default], then when you load a Sheafhom file,
all its subordinate files will be loaded automatically.  To disable
this feature, type
   (setf *load-automatically* nil)
at a Lisp prompt.")

(when *load-automatically*
  (load "homolalg"))

(proclaim '(ftype (function (t) nnfixnum)
		  dim))

(deftype poset-elt ()
  "A poset-elt is a list of distinct non-negative integers [fixnums],
sorted by < .
  Motivating example: if the facets [i.e. codimension-one faces] of a
convex polytope Delta are represented by singleton lists (i), then a
general face F of Delta is represented by the poset-elt (i1 i2 ...),
where the i's stand for all the facets that meet F.  The empty
poset-elt () represents the whole of Delta.
  The partial ordering relation between poset-elt's x and y is
computed by (preceq x y).  See the documentation on preceq."
  'list)

;;; I will assume that all the elements of a poset-elt are
;;; non-negative FIXNUMs.

(deftype ranked-poset ()
  "A ranked-poset is a list (y_n y_{n-1} ...), where each y_i is a
list of poset-elt's.  Each y_i should be lexicographically sorted,
using the predicate lexicogp.  No y_i should be null--this is
equivalent to saying that n-i is a rank function on the poset.
If c and d are poset-elts in the ranked-poset, and c belongs to y_j
and d to y_i, then `(preceq c d) is true' implies j >= i.
  The empty poset-elt () is viewed as a maximal element, called 1-hat
in the poset literature.  It may or may not be stored in the
ranked-poset.  If it is, then y_0 is (()) .

Definition 1: The **open star of the poset-elt c in the ranked-poset y**
is the set of all poset-elt's d such that (preceq c d) is true.  This
is computed by (open-star c y).  The d's will be arranged as a
ranked-poset, in tiers (y'_m y'_{m-1} ...) [for some m <= n depending
on c.]

Definition 2: An **open set** in y is any union of open stars.

Definition 3: The **punctured open star of c in y** is (open-star c y)
with the element c removed.  This is computed by
(puncture (open-star c y)).

Example: The face poset of a triangle in the plane is
 y = (((0 1) (0 2) (1 2))  ; three vertices
      ((0) (1) (2))        ; three edges
      (()))                ; one two-dimensional face [the whole triangle].

Calling (open-star '(0 1) y) returns
 (((0 1))
  ((0) (1))
  (())
This represents the vertex V for (0 1), the interiors of its two
adjoining edges, and the interior of the whole triangle.  The union of
these cells is indeed an open set around V, and it is the smallest
open set containing V that is a union of interiors of cells.

Calling (puncture (open-star '(0 1) y)) returns 
 (((0) (1))
  (()))
which represents the [interiors of] the two edges adjoining V and the
whole triangle.  This is the previous open set with the vertex V
removed."
  ;; After all the hoopla, this is all I can define the type to be,
  ;; until I get a language like ML with better support for nested
  ;; types.
  'list)

(defgeneric open-star (c x))

(setf (documentation 'open-star 'function)
 "Arguments c, x.  Find the open star of the poset-elt c in x, which
may be a ranked-poset or a sheaf.  See the documentation on
ranked-poset for more information.")

;;; Sheaves have moved to toric*.lisp.

;;; --------------- METHODS, etc. ---------------

(defmacro subset (pred lyst) ; from Symbolics Lisp
  `(remove-if-not ,pred ,lyst))

;;; Some standard perversity functions.

(defun zero-perversity (k)
  (declare (ignore k))
  "A perversity is a function p(k) defined for integers k >= 2, with
p(2) = 0 and p(k+1) equal to either p(k) or p(k)+1.  Here k stands for
the real codimension of a singular stratum in a stratified space X.
The intersection homology of X is a function of both X and the
choice of perversity.
  The zero perversity is the function that's identically zero.  For X
of real dimension N that are normal compact oriented pseudomanifolds
[normal means all links are connected], the i-th intersection homology
of X in perversity zero is the same as the ordinary cohomology in
degree N-i."
  0)

(defun middle-perversity (k)
  (declare (fixnum k))
  "A perversity is a function p(k) defined for integers k >= 2, with
p(2) = 0 and p(k+1) equal to either p(k) or p(k)+1.  Here k stands for
the real codimension of a singular stratum in a stratified space X.
The intersection homology IH_i(X) is a function of both X and the
choice of perversity.
  Assume your space X is (an open subset of) a complex analytic or
algebraic variety.  Middle perversity is then the most important
perversity, since middle-perversity IH satisifies the `Kahler
package'--Poincare duality, hard Lefschetz, mixed Hodge structure, etc.
  Middle perversity is defined, for even k, to be (k-2)/2."
  (unless (evenp k)
    (break
"We're in middle perversity, but I was asked about a stratum of odd
codimension.  If you don't think this is a problem, continue out of
this breakpoint, and I will compute the lower middle perversity."))
  (max 0
       (the fixnum (floor (the fixnum (- k 2)) 2))))

(defun top-perversity (k)
  (declare (fixnum k))
  "A perversity is a function p(k) defined for integers k >= 2, with
p(2) = 0 and p(k+1) equal to either p(k) or p(k)+1.  Here k stands for
the real codimension of a singular stratum in a stratified space X.
The intersection homology IH_i(X) is a function of both X and the
choice of perversity.
  The top perversity is the function k-2.  For normal compact oriented
pseudomanifolds [normal means all links are connected], the i-th
intersection homology of X in top perversity is the same as the
ordinary homology in degree i."
  (max 0 (the fixnum (- k 2))))

;;; Combinatorial algorithms for posets and poset-elt's.

;;; Note: because of our conventions about sorting by < in poset-elts,
;;; and lex-sorting in lists of poset-elts, we can say that
;;;    two poset-elt's are equal iff Lisp's EQUAL says they are
;;;    two lexicographically sorted lists of poset-elt's are equal iff
;;;       EQUAL says they are
;;;    two ranked-poset's are equal iff EQUAL says they are
;;; However, for numerical efficiency, it seems better to have a test
;;; `poset-elt-compare' that checks  whether two poset-elt's are <, =,
;;; or >, and takes advantage of the fact that the elements of
;;; poset-elt's are fixnums.  Similarly, there should be a
;;; `ranked-poset-equal'.

;;; Look into local definitions for the subfunctions of
;;; ranked-poset-equal, -union, and -intersection.

(defun poset-elt-compare (a b)
  (declare (type poset-elt a b))
  "Arguments a,b, which are poset-elt's [see the documentation for
poset-elt].  This function returns -1 if a is lexicographically less
than b, +1 if a is lex. greater than b, and 0 if they're equal.
  To see what to do with two lists of unequal length, pad the shorter
one with -infinity on the right till its length equals that of the
longer one (just as in the dictionary, where cow comes before coworker)."
  (cond ((null a)
	 (if (null b)
	     0 ; original a,b were equal
	   -1)) ; cow/coworker case
	((null b)
	 +1) ; coworker/cow case
	(t
	 (let ((a1 (car a))
	       (b1 (car b)))
	   (declare (fixnum a1 b1))
	   (cond ((= a1 b1)
		  (poset-elt-compare (cdr a) (cdr b)))
		 ((< a1 b1)
		  -1)
		 (t
		  +1))))))

(defun poset-elt-equal (x y)
  ;; This could be implemented as
  ;;    (zerop (poset-elt-compare x y))
  ;; --but what the heck, it's written already.
  (declare (type poset-elt x y))
  "Arguments x,y, which are poset-elt's [see the documentation on
poset-elt].  Returns true iff x and y are equal."
  (cond ((null x)
	 (null y)) ; true iff y is null
	((null y)
	 nil) ; here y is null and x is not
	(t
	 (let ((a (car x))
	       (b (car y)))
	   (declare (fixnum a b))
	   (if (= a b)
	       (poset-elt-equal (cdr x) (cdr y))
	     nil)))))

(defun ranked-poset-equal (x y)
  (declare (type ranked-poset x y))
  "Arguments x, y, which must be ranked posets.  Returns true iff x
and y are equal.  See the documentation on ranked-poset for more
information."
  (labels ((ranked-poset-equal-aux (z w)
	     (declare (list z w)) ; lists of poset-elt's
	     (cond ((null z)
		    (null w))
		   ((null w)
		    nil)
		   ((poset-elt-equal (car z) (car w))
		    (ranked-poset-equal-aux (cdr z) (cdr w)))
		   (t
		    nil))))
    (cond ((null x)
	   (null y))
	  ((null y)
	   nil)
	  ;; here (car x), (car y) are lists of poset-elts
	  ((ranked-poset-equal-aux (car x) (car y))
	   (ranked-poset-equal (cdr x) (cdr y)))
	  (t
	   nil))))

(defun sorted-subsetp (x y)
  "The arguments x, y are lists of distinct integers [fixnums] sorted
by < .  The result is true if and only if every member of x is a
member of y."
  (cond ((null x)
	 t)
        ((null y)
	 nil)
	(t
	 (let ((a (car x))
	       (b (car y)))
	   (declare (fixnum a b))
	   (cond ((< a b)
		  nil)
		 ((> a b)
		  (sorted-subsetp x (cdr y)))
		 (t
		  (sorted-subsetp (cdr x) (cdr y))))))))

(defmacro preceq (x y)
  "preceq gives the partial order on poset-elt's.  That is, (preceq x y)
is true if and only if x is less than or equal to y with respect to
the partial order.  This is named after the TeX symbol $\\preceq$.
   In the present implementation, (preceq x y) holds if and only if x
is a superset of y."
  `(sorted-subsetp ,y ,x))

(defun poset-elt-intersection (x y)
  "The arguments x, y are list of distinct integers [fixnums] sorted
by <.  The result is the intersection of the two lists, sorted by <."
  (labels ((aux (x y ans)
	     (if (or (null x) (null y))
		 (nreverse ans)
	       (let ((a (car x))
		     (b (car y)))
		 (declare (fixnum a b))
		 (cond ((< a b)
			(aux (cdr x) y ans))
		       ((> a b)
			(aux x (cdr y) ans))
		       (t
			(aux (cdr x) (cdr y) (cons a ans))))))))
    (aux x y '())))

(defun remove-leading-nils (lyst)
  (declare (list lyst))
  "If the first one or more elements of the argument are nil, remove
them, and return the rest."
  (cond ((null lyst)
	 '())
	((null (car lyst))
	 (remove-leading-nils (cdr lyst)))
	(t
	 lyst)))

(defun ranked-poset-union (z w)
  (declare (type ranked-poset z w))
  "z and w are ranked-poset's.  See the documentation on the class
ranked-poset for the rather delicate syntax of such objects.
(ranked-poset-union z w) returns the union of z and w, as a
ranked-poset."
  (let ((lz (length z))
	(lw (length w)))
    (declare (fixnum lz lw))
    (cond ((< lz lw)
	   (cons (car w) (ranked-poset-union z (cdr w))))
	  ((> lz lw)
	   (cons (car z) (ranked-poset-union (cdr z) w)))
	  ((null z)
	   nil)
	  (t
	   (cons (lexicog-union (car z) (car w))
		 (ranked-poset-union (cdr z) (cdr w)))))))

(defun lexicog-union (x y &optional (ans '()))
  "x, y are lists of distinct poset-elt's.  x and y are each sorted
lexicographically.  Output: the union of x and y, also lex-sorted."
  (cond ((null x)
	 (nreconc ans y))
	((null y)
	 (nreconc ans x))
	(t
	 (let ((test (poset-elt-compare (car x) (car y))))
	   (declare (fixnum test))
	   (cond ((zerop test)
		  (lexicog-union (cdr x) (cdr y) (cons (car x) ans)))
		 ((= -1 test)
		  (lexicog-union (cdr x) y (cons (car x) ans)))
		 (t
		  (lexicog-union x (cdr y) (cons (car y) ans))))))))

(defun ranked-poset-intersection (z w)
  (declare (type ranked-poset z w))
  "z and w are ranked-poset's.  See the documentation on the class
ranked-poset for the rather delicate syntax of such objects.
(ranked-poset-intersection z w) returns the intersection of z and w,
as a ranked-poset."
  (let ((lz (length z))
	(lw (length w)))
    (declare (fixnum lz lw))
    (cond ((< lz lw)
	   (ranked-poset-intersection z (cdr w)))
	  ((> lz lw)
	   (ranked-poset-intersection (cdr z) w))
	  ((null z)
	   '())
	  (t
	   (remove-leading-nils
	    (cons (lexicog-intersection (car z) (car w))
		  (ranked-poset-intersection (cdr z) (cdr w))))))))

(defun lexicog-intersection (x y &optional (ans '()))
  "x, y are lists of poset-elt's.  x and y are each sorted
lexicographically.  Output: the intersection, lex-sorted."
  (cond ((null x)
	 (nreverse ans))
	((null y)
	 (nreverse ans))
	(t
	 (let ((test (poset-elt-compare (car x) (car y))))
	   (declare (fixnum test))
	   (cond ((zerop test)
		  (lexicog-intersection (cdr x) (cdr y) (cons (car x) ans)))
		 ((= -1 test)
		  (lexicog-intersection (cdr x) y ans))
		 (t
		  (lexicog-intersection x (cdr y) ans)))))))

(defmethod open-star (c y)
  ;; Could be sped up a lot.  Take advantage of the lex ordering.  Be
  ;; faster about when not to check an element.
  (declare (type poset-elt c)
	   (type ranked-poset y))
  (remove-leading-nils
   (mapcar #'(lambda (elt-list-of-given-rank)
	       (subset #'(lambda (elt) (preceq c elt))
		       elt-list-of-given-rank))
	   y)))

(defun puncture (u)
  (declare (type ranked-poset u))
  "The argument u is a ranked-poset with only one element in its top
tier.
   For the rest of this documentation-string, assume u is the open
star of some poset-elt c in a ranked-poset x.  Then u begins
(( c ) ; the top tier of the open star
 ( ...more poset-elts... )
 ( ...still more poset-elts... )
 ...)
The functions removes the top tier, and return the resulting
sub-ranked-poset.  This represents the punctured open star of c, as u
was the full open star."
  (assert (= 1 (the fixnum (length (the list (car u)))))
	  ()
"I was asked to puncture something that doesn't have a singleton in
its leftmost element:
~S" u)
  ;; After all the fuss, this is what it really does:
  (cdr u))

(defun union-of-open-stars (poset-elts x)
  (declare (list poset-elts)
	   (type ranked-poset x))
  "The second argument x is a ranked-poset, and the first argument is
a list L of poset-elt's in x.  Result is the ranked-poset-union of the
sub-posets (open-star c x) for all c in L."
  (remove-leading-nils
   (mapcar #'(lambda (elt-list-of-given-rank)
	       (subset #'(lambda (elt)
			   (declare (type poset-elt elt))
			   (some #'(lambda (c)
				     (declare (type poset-elt c))
				     (preceq c elt))
				 poset-elts))
		       elt-list-of-given-rank))
	   x)))

(defun ranked-poset-rank-function (U)
  (declare (type ranked-poset U))
  "Returns a rank function rf for the ranked-poset U.  If c is a
poset-elt in U, then (funcall rf c) returns the number r such that c
is in (nth r U).  Minimal elts for preceq have rank 0, the
next-highest elts have rank 1, etc."
  (let ((ans (make-hash-table :test #'equal)))
    (do ((r 0 (1+ r))
	 (rows U (cdr rows)))
	((null rows)
	 #'(lambda (c) (gethash c ans)))
      (declare (fixnum r))
      (dolist (c (car rows))
        (setf (gethash c ans) r)))))

#|
(defun minimal-spanning-tree (lyst)
  "Argument is a list of triples (a b n), where a and b are
poset-elt's and n is an integer.  Regard these as giving a graph G,
where the a's and b's are the vertices of G, the triples are the
edges, and the n's are weights.  The output is a minimal spanning tree
of G, again represented as a list of triples [edges].  An attempt is
made to make the sum of the weights of the tree's edges as small as
possible, using a greedy algorithm."
  (labels ((subset-cons-not-subset (pred lyst)
	     ;; Returns (y-list . n-list), where these are the
	     ;; sublists of lyst that satisfy--resp., do not
	     ;; satisfy--the predicate pred.  The order is preserved
	     ;; in each sublist.
	     (let ((y '())
		   (n '()))
	       (dolist (x lyst)
                 (if (funcall pred x)
		     (push x y)
		   (push x n)))
               (cons (nreverse y) (nreverse n))))
	   (aux (edges valences ans)
	     ;; edges is a list of edges that we're still deciding
	     ;; where to put.  valences is a hash-table, keyed by
	     ;; vertices [the a's and b's] and giving the valence of
	     ;; that vertex.  ans is a list of edges forming the
	     ;; spanning tree as we've built it so far.
	     (if (null edges)
		 ;; Done
		 ans
	       (let ((a0 '())) ; dummy poset-elt
		 (let ((found-valence-1
			(block valence-1-search
			  (maphash #'(lambda (k v)
				       (declare (fixnum v))
				       (when (= v 1)
					 (setq a0 k)
					 (return-from valence-1-search
						      t)))
				   valences))))
		   (cond (found-valence-1
			  ;; You get here iff there is a vertex of
			  ;; valence 1.  We remove the unique edge that's
			  ;; on it, put that edge onto the answer ans,
			  ;; and recurse.
			  (let ((meet-a0-cons-dont
				 (subset-cons-not-subset
				  #'(lambda (e)
				      (or (poset-elt-equal (car e)
							   a0)
					  (poset-elt-equal (cadr e)
							   a0)))
				  edges)))
			    (assert (= 1 (length (the list
                                                   (car meet-a0-cons-dont))))
				    () "Valence-1 error.")
			    (decf (the fixnum
				    (gethash (first (caar meet-a0-cons-dont))
					     valences)))
			    (decf (the fixnum
				    (gethash (second (caar meet-a0-cons-dont))
					     valences)))
			    (aux (cdr meet-a0-cons-dont)
				 valences
				 (cons (caar meet-a0-cons-dont)
				       ans))))
			 (t
			  ;; Otherwise, pull off any edge and throw it
			  ;; away.
			  (decf (the fixnum
                                  (gethash (first (car edges)) valences)))
			  (decf (the fixnum
                                  (gethash (second (car edges)) valences)))
			  (aux (cdr edges) valences ans))))))))
    ;; Here is where we start.  Set up the valences table, and sort
    ;; the edges in descending order of weight.
    (let ((valences (make-hash-table :test #'equal))
	  (sorted-edges (sort lyst #'(lambda (tri1 tri2)
					(> (the fixnum (third tri1))
					   (the fixnum (third tri2)))))))
      (dolist (e sorted-edges)
        ;; The next do iterates over the first two elements of e.
        (do ((v (first e) (second e))
	     (i 0 (1+ i)))
	    ((= i 2))
	  (declare (fixnum i))
	  (if (null (gethash v valences))
	      (setf (gethash v valences) 1)
	    (incf (the fixnum (gethash v valences))))))
      (aux sorted-edges valences '()))))
|#

;;; ---------- QUIVERS ----------------------------------------

(defstruct quiver-rep
  (r-p (dummy) :type ranked-poset)
  (vertices (dummy) :type hash-table)
  (edges (dummy) :type hash-table)
  (quiver-rank-function nil)
  (single-cx nil :type (or null cochain-cx)))

(defstruct quiver-rep-map
  (sou (dummy) :type quiver-rep)
  (tar (dummy) :type quiver-rep)
  (mat (dummy) :type hash-table)
  (single-cx-map nil :type (or null cochain-map)))

(defmacro do-vertices (pe-U &body body)
  ;; Use like (do-vertices (c U) [body]).  Here [body] will be
  ;; evaluated once for every c in U, and you can use the variables c
  ;; and U in the body.
  `(dolist (pe-list ,(second pe-U))
     (dolist (,(first pe-U) pe-list)
       ,@body)))

(defmacro do-edges (pair-U &body body)
  ;; Use like (do-edges (pair U) [body]).  Here [body] will be
  ;; evaluated once for every (tau . sigma) in U with (preceq sigma
  ;; tau) true and sigma,tau of consecutive rank.  Use the symbols
  ;; pair and U in the body.
  `(do ((V ,(second pair-U) (cdr V)))
       ((null (cdr V)))
     (dolist (sigma (car V))
       (dolist (tau (cadr V))
         (when (preceq sigma tau)
           (let ((,(first pair-U) (cons tau sigma)))
	     ,@body))))))

(defun quiver-rank-function (qu)
  (declare (type quiver-rep qu))
  (let ((z (quiver-rep-quiver-rank-function qu)))
    (if (not (null z))
	z
      (setf (quiver-rep-quiver-rank-function qu)
	    (ranked-poset-rank-function (quiver-rep-r-p qu))))))

#|
(defun single-cx (qu)
  (declare (type quiver-rep qu))
  (let ((z (quiver-rep-single-cx qu)))
    (if (not (null z))
	z
      (setf (quiver-rep-single-cx qu)
	(let* ((U (quiver-rep-r-p qu))
	       (r (quiver-rank-function qu))
	       (v (quiver-rep-vertices qu))
	       (e (quiver-rep-edges qu))
	       (heap (empty-hash-table))
	       (ccx-len (length U))
	       (ccx (make-instance 'cellcx :cellcx-dim (1- ccx-len))))
	  (declare (type nnfixnum ccx-len))
	  ;; Set up the pre-cellcx.
	  (setf (cellcx-cells ccx)
		(make-vector ccx-len))
	  (setf (cellcx-oriens ccx)
		(make-vector ccx-len))
	  (dotimes (k ccx-len)
            (declare (type nnfixnum k))
	    (setf (svref (cellcx-cells ccx) k)
	      (mapcar #'(lambda (c)
			  (cons
			   (position c (nth k U) :test #'equal)
			   c))
		      (nth k U))))
	  (setf (cellcx-dims ccx)
	    (map 'vector #'length (cellcx-cells ccx)))
	  (fac-oriens ccx 1 (1- ccx-len))
	  ;; The q-degree term in the c-th part of the quiver-rep
	  ;; is founds in the (funcall r c)+q degree term of the single
	  ;; complex.  Its directsum index in that term of the single
	  ;; complex is (c . q) .
	  (do-vertices (c U)
	    (let ((x (gethash c v)))
	      ;; a "stalk", assumed to be a cochain-cx
	      (map-objects
	       #'(lambda (q xq)
		   (push (cons (cons c q) xq)
			 (gethash
			  (+ (funcall r c) q)
			  heap)))
	       x)))
	  (let ((o (empty-hash-table)))
	    (maphash #'(lambda (deg lyst)
			 (let ((su (coerce (mapcar #'cdr lyst)
					   'vector))
			       (ind (coerce (mapcar #'car lyst)
					    'vector)))
			   (setf (gethash deg o)
				 (make-directsum-object su ind))))
		     heap)
	    (let ((m (empty-hash-table)))
	      (maphash #'(lambda (k sk)
			   (let ((sk1 (gethash (1+ k) o)))
			     (unless (null sk1)
			       (setf (gethash k m)
				 (make-directsum-morphism
				  sk
				  sk1
				  #'(lambda (j i)
				      ;; j = (sigma . degj)
				      ;; i = (tau . degi)
				      (let ((sigma (car j))
					    (tau (car i))
					    (degj (cdr j))
					    (degi (cdr i)))
					(cond ((and (= degj degi)
						    (preceq sigma
							    tau))
					       (assert (= (1+
							   (funcall r
								    sigma))
							  (funcall r
								   tau))
						       () "sigma-tau error")
					       ;; horizontal maps
					       (scmult
						(get-orien
						 sigma
						 tau
						 (funcall r tau)
						 ccx)
						(term (gethash
						       (cons tau sigma)
						       e)
						      degj)))
					      ((poset-elt-equal
						sigma tau)
					       (assert (= degi
							  (1+ degj))
						       () "deg error")
					       ;; vertical maps
					       (scmult
						(if (evenp
						     (funcall r
							      sigma))
						    +1 -1)
						(d degj
						   (gethash sigma
							    v))))
					      (t 0)))))))))
		       o)
	      ;; answer:
	      (test-cochain-cx
	       (make-cochain-cx :objects o :maps m)))))))))

(defun single-cx-map (f)
  (declare (type quiver-rep-map f))
  (let ((z (quiver-rep-map-single-cx-map f)))
    (if (not (null z))
	z
      (setf (quiver-rep-map-single-cx-map f)
	(let* ((so (sou f))
	       (ta (tar f))
	       (U (quiver-rep-r-p so))
	       (r (quiver-rank-function so))
	       (x (single-cx so))
	       (y (single-cx ta))
	       (ans (empty-hash-table)))
	  (assert (ranked-poset-equal U (quiver-rep-r-p ta)) ()
		  "Underlying ranked-posets are not equal.")
	  (map-objects
	   #'(lambda (deg xdeg)
	       (let ((ydeg (term y deg)))
		 (unless (zero-object-p ydeg)
		   (setf (gethash deg ans)
		     (make-directsum-morphism
		      xdeg
		      ydeg
		      #'(lambda (j i)
			  ;; j = (sigma . degj)
			  ;; i = (sigma' . degi)
			  ;; deg = (r sigma)+degj
			  ;;     = (r sigma')+degi
			  (cond ((poset-elt-equal (car j) (car i))
				 (let ((degj (cdr j))
				       (degi (cdr i))
				       (r-sig (funcall r (car j))))
				   (declare (fixnum degj degi r-sig))
				   (assert (= degj degi) ()
					   "Degree screw-up.")
				   (assert (= deg (+ r-sig degj)) ()
						   "Degree error.")
				   (term (gethash (car j) (mat f))
					 degj)))
				(t 0))))))))
	   x)
	  (test-cochain-map
	   (make-cochain-map :sou x :tar y :mat ans)))))))
|#

(defun user-input-quiver-rep-map (so ta)
  (declare (type quiver-rep so ta))
  (make-quiver-rep-map
   :sou so
   :tar ta
   :mat (with-equal-ans
	 (do-vertices (c (quiver-rep-r-p so))
	   (format t "~&~%Input the map for ~S" c)
	   (setf (gethash c ans)
	     (user-input-cochain-map
	      (gethash c (quiver-rep-vertices so))
	      (gethash c (quiver-rep-vertices ta))))))))

#|
(defun edge-morphisms (qu)
  (declare (type quiver-rep qu))
  "Let sigma run through the rank-0 elements of (ranked-poset qu).
The function returns a list of pair (sigma . f_sigma), one for each
sigma.  Here f_sigma is the map from (single-cx qu) to the sigma-th
column of qu."
  (let ((x (single-cx qu))
	(U (quiver-rep-r-p qu))
	(v (quiver-rep-vertices qu)))
    (mapcar #'(lambda (c)
		(cons c
		      (make-cochain-map
		       :sou x
		       :tar (gethash c v)
		       :mat (with-eql-ans
			     (map-objects
			      #'(lambda (i xi)
				  (setf (gethash i ans)
				    (make-directsum-morphism-to-singleton
				     xi
				     (term (gethash c v) i)
				     #'(lambda (j)
					 (if (poset-elt-equal
					      (car j) c)
					     1
					   0)))))
			      x)))))
	    (car U))))
|#

;;; ----------------------------------------

;;; The next material, together with sp4.lsp, sp4-?d.lsp [for ? = r or
;;; v], and sparse.lsp, contain a workable version of the
;;; homology-finding Lisp programs I ran at Brown in 1986-87.

;;; This code contains the programs for setting up the incidence
;;; matrices for a general finite regular cell complex.

;;; A CELLCX x is a structure with four parts:
;;;  dim, a non-negative integer (call it N in these comments).
;;;  cells, an array with slots 0,...,N.  The k-th entry is the
;;;    list of k-cells.  An entry e on this list has (car e) = code
;;;    number, destined to be the cell's column # in DELk, and (cdr e)
;;;    = any list of data, however crazy, which indexes the k-cell
;;;    having that code number.
;;;  dims, an array with slots 0,...,N.  k-th entry = #(k-cells).
;;;  oriens, an array with slots 0,...,N.  k-th entry is an array of
;;;    length (aref (cellcx-dims x) k).  This array's j-th entry is a
;;;    rowv ((a1 b1) (a2 b2) ...), where the a's are the code numbers
;;;    of the j-th k-cell's boundary cells, and the b's are +/- 1 in
;;;    an appropriate way.

(defclass cellcx ()
  ((cellcx-cells :accessor cellcx-cells :initarg :cellcx-cells)
   (cellcx-dims :accessor cellcx-dims :initarg :cellcx-dims)
   (cellcx-oriens :accessor cellcx-oriens :initarg :cellcx-oriens)
   (cellcx-dim :accessor cellcx-dim :initarg :cellcx-dim)))

;;; Assume you're given a pre-CELLCX x : the first three slots are
;;; present in full, but (cellcx-oriens x) is a blank-array with
;;; indices 0,...,N.  Assume also there is a functions BDDRY-CELLS,
;;; which accepts a typical (cdr e) of the k-th cell list (and also
;;; accepts k, perhaps), and returns a list of (cdr e)-type elts of
;;; the (k-1)-st cell list, rep'g the cell's boundary.  FAC-ORIENS
;;; will create the k-th value of (aref (cellcx-oriens x) k), for
;;; k = k1 through k2.  (Note: (aref (cellcx-oriens x) 0) can and
;;; should remain nil.)  NOTE: the cells must be constructed in all
;;; dimensions, since the induction starts at dim 0.

; For this Sheafhom version:

(defun bddry-cells (c next-lyst-down)
  (mapcar #'cdr
	  (subset #'(lambda (e) (preceq (cdr e) c)) ; 4/11/98
		  next-lyst-down)))

(defun fac-oriens (x k1 k2)
  (do ((k (max k1 1) (1+ k)))
      ((> k k2))
    (if (= k 1)
	(fac-oriens-1 x)
      (fac-oriens-aux x k))))

(defun fac-oriens-1 (x) ; x is a pre-CELLCX
  (setf (aref (cellcx-oriens x) 1)
	(make-array (list (aref (cellcx-dims x) 1))))
  (let ((or-ar-1 (aref (cellcx-oriens x) 1))
	(hash-0 (make-hash-table :test #'equal
				 :size (aref (cellcx-dims x) 0)))
	z)
    (dolist (a (aref (cellcx-cells x) 0))
      (setf (gethash (cdr a) hash-0) (car a)))
    (dolist (y (aref (cellcx-cells x) 1))
      (setq z (bddry-cells (cdr y)
			   (aref (cellcx-cells x) 0) ; added 5/11/96
			   ))
      (setf (aref or-ar-1 (car y))
	    (list (list (gethash (car  z) hash-0)  1)
		  (list (gethash (cadr z) hash-0) -1))))))

(defun fac-oriens-aux (x k)
  ;; Will create the k-th orien array, given all the (k-1) data.
  (setf (aref (cellcx-oriens x) k)
	(make-array (list (aref (cellcx-dims x) k))))
  (let ((or-ar-k (aref (cellcx-oriens x) k))
	(or-ar-bdd (aref (cellcx-oriens x) (1- k)))
	(hash-bdd (make-hash-table :test #'equal
				   :size (aref (cellcx-dims x) (1-
								k))))
	z)
    (dolist (a (aref (cellcx-cells x) (1- k)))
      (setf (gethash (cdr a) hash-bdd) (car a)))
    (dolist (y (aref (cellcx-cells x) k))
      (setq z (mapcar #'(lambda (l) (gethash l hash-bdd))
		      (bddry-cells (cdr y)
				   (aref (cellcx-cells x) (1- k)) ; 5/11/96
				   )))
      (setq z (mapcar #'(lambda (n) (cons n (aref or-ar-bdd n)))
		      z))
      (setf (aref or-ar-k (car y))
	    (fac-oriens-aux2 z '() '() '())))))

(defun fac-oriens-aux2 (draw matched unmatched answer)
  (cond ((and (null draw) (null unmatched))
	 answer)
	((null draw)
	 (fac-oriens-aux2 unmatched matched '() answer))
	((null matched)
	 (fac-oriens-aux2
	  (cdr draw)
	  (list (car draw))
	  unmatched
	  (list (list (caar draw) 1))))
	(t
	 (do ((a matched (cdr a)))
	     ((null a) (fac-oriens-aux2
			(cdr draw)
			matched
			(cons (car draw) unmatched)
			answer))
	   (let ((b (fac-oriens-aux3 (cdar draw) (cdar a))))
	     (when b
		   (return ; from the do
		    (fac-oriens-aux2
		     (cdr draw)
		     (cons (car draw) matched)
		     unmatched
		     (cons (list (caar draw)
				 (* b (our-fetch (caar a) answer)))
			   answer)))))))))

(defun fac-oriens-aux3 (x y)
  (do ((a x (cdr a))
       (b nil))
      ((null a) nil)
    (setq b (our-fetch (caar a) y))
    (when b
	  (return (* -1 b (cadar a))))))

(defun our-fetch (n l)
  ;; l = association list with integer keys.  Result is NIL if it's
  ;; not there.
  (cond ((null l) nil)
	((= n (caar l)) (cadar l))
	(t (our-fetch n (cdr l)))))

;;; Slow(!) interface to the present version of Sheafhom:

(defun get-orien (sigma tau degtau ccx)
  (declare (type poset-elt sigma tau)
	   (type nnfixnum degtau)
	   (type cellcx ccx))
  (let* ((codetau (dolist (pair (svref (cellcx-cells ccx) degtau)
			        (error "Tau not found."))
		    (when (poset-elt-equal tau (cdr pair))
                      (return (car pair)))))
         (codesigma (dolist (pair (svref (cellcx-cells ccx) (1- degtau))
			          (error "Sigma not found."))
		      (when (poset-elt-equal sigma (cdr pair))
                        (return (car pair)))))
	 (bdry-list (svref
		     (svref (cellcx-oriens ccx) degtau)
		     codetau)))
    (declare (type nnfixnum codetau codesigma))
    (dolist (lyst bdry-list
		  (error "Orien not found."))
      (when (= codesigma (first lyst))
	(return (second lyst))))))

