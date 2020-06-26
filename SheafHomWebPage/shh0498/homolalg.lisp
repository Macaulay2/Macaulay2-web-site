;;; Here is the latest version of SHEAFHOM.  This set of programs was
;;; begun in June 1993 (at J.T.'s house) and revised several times
;;; through September 1993, all in non-CLOS versions.  The CLOS
;;; version was begun in spring 1995 at the IAS, with most of the work
;;; dating from spring 1996 and later.  The present version replaces
;;; CLOS, in most cases, with defstructs for objects and explicit
;;; conditional branches in the functions.

;;; This is a major revision of Sheafhom, emphasizing
;;; quiver-representations and double-complexes rather than spectral
;;; sequences.  It is influenced by my visit to Grayson in late July
;;; of 1997.

;;; ------------------------------------------------------------

;;; The network of types and sub-types, to date:
;;;
;;; QVSP
;;;    qv ; the general qvsp
;;;    directsum-qvsp
;;;    tensor-qvsp
;;;    wedge-qvsp
;;;
;;; QVSP-MORPHISM
;;;    qm ; the general qvsp-morphism
;;;    zero-qvsp-morphism
;;;    id-qvsp-morphism
;;;
;;; MATRIX
;;;    [The file named in *name-of-matrix-file* contains an
;;;    implementation that models 2-dim'l arrays of integers.]
;;;
;;; MATRIX+
;;;
;;; FORMAL-INVERSE-QVSP-MORPHISM [a kludge--see documentation]
;;;
;;; COCHAIN-CX
;;;    directsum-cochain-cx
;;;    tensor-cochain-cx
;;;    exterior-algebra
;;;
;;; COCHAIN-MAP
;;;
;;; DIRECTSUM
;;;    directsum-qvsp
;;;    directsum-cochain-cx
;;;
;;; TENSOR
;;;    tensor-qvsp
;;;    tensor-cochain-cx

;;; ------------------------------------------------------------

;;; Basics.

(proclaim '(optimize speed))

(setq *gc-verbose* nil) ; make garbage collector run silently

(proclaim '(ftype (function (t) nnfixnum)
		  dim))

(defconstant *name-of-matrix-file* "csparsez"
  "Stores the name of the file [e.g. \"csparsez\"] that currently
contains our implemention of matrices, LLL, etc.")

(defvar *load-automatically* t
  "If this is true [the default], then when you load a Sheafhom file,
all its subordinate files will be loaded automatically.  To disable
this feature, type
   (setf *load-automatically* nil)
at a Lisp prompt.")

(when *load-automatically*
  (load *name-of-matrix-file*))	; Our current implementation of matrices.

(defmacro make-vector (n)
  "Input n.  Makes a new vector [one-dimensional array] of length n."
  `(make-array (list (the nnfixnum ,n))))

(defmacro with-gensyms (syms &body body)
  ;; From Graham, _On Lisp_
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

(defmacro empty-hash-table ()
  `(make-hash-table :test #'eql))

(defmacro with-equal-ans (&body body)
  ;; body will refer to the variable ans.  This macro defines an empty
  ;; equal-hash-table called ans, evaulates body in this context, and
  ;; returns ans at the end.
  `(let ((ans (make-hash-table :test #'equal)))
     (progn ,@body)
     ans))

(defmacro with-eql-ans (&body body)
  ;; Same as with-equal-ans, but uses #'eql.
  `(let ((ans (make-hash-table :test #'eql)))
     (progn ,@body)
     ans))

(defun get-one-hash-value (h)
  (declare (type hash-table h))
  "Looks over the key-value pairs in the hash table h and returns the
first value it finds.  It is an error if the table is empty."
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (return-from get-one-hash-value v))
	   h)
  ;; If you get here...
  (error "Empty hash table in get-one-hash-value."))

;;; ----------------------------------------

;;; The naming functions.

(defvar *named-items* '()
  "This is an association list whose entries are pairs (item . name).
Whenever a user creates an item [object or morphism] using a
make-... or user-input-... function by giving the optional arguments
   :name 'such-and-such
a pair (item . such-and-such) is stored on this list.  From then on,
the item will be printed as #<SUCH-AND-SUCH>.")

(defun name (x)
  "Returns the name of the object or morphism x.  Returns nil if no
name has been specified."
  (cdr (assoc x *named-items*)))

(defsetf name (x) (val)
  `(progn
     (push (cons ,x ,val) *named-items*)
     ,val))

(defmacro with-names (&body body)
  "Wrap (with-names ...) around any code that creates an object that
might be named."
  `(let ((ans (progn ,@body)))
     (unless (null name)
       ;; name will be bound in the calling function
       (setf (name ans) name))
     ans))

;;; ------------------------------------------------------------
;;; ------------------- STRUCTURES and TYPES -------------------
;;; ------------------------------------------------------------

;;; Delete this whole proclaim form if you're not using CMU CL.

(proclaim '(freeze-type QVSP qv directsum-qvsp tensor-qvsp wedge-qvsp
QVSP-MORPHISM qm zero-qvsp-morphism id-qvsp-morphism
FORMAL-INVERSE-QVSP-MORPHISM COCHAIN-CX directsum-cochain-cx
tensor-cochain-cx exterior-algebra COCHAIN-MAP DIRECTSUM TENSOR))

(defstruct (qv (:print-function
		(lambda (v s k)
		  (declare (type qv v)
			   (ignore k))
		  (format s "#<~S>"
			  (or (name v)
			      (list 'qvsp
				    (list 'dim (qv-dim v))))))))
  "QV is the most general type of qvsp.  See the documentation on qvsp."
  (dim (dummy) :type nnfixnum))

(defvar *zero-qvsp*
  (make-qv :dim 0)
  "*zero-qvsp* is the canonical 0 object in the category of qvsp's
[rational vector spaces].")

(setf (name *zero-qvsp*) 'zero-qvsp)

(defvar *groundfield*
  (make-qv :dim 1)
  "*groundfield* is a rational vector space of dimension 1.  We
imagine that it has the canonical basis {1}.  In the rare cases when
we want one, a `vector' in a qvsp V is specified by giving a
qvsp-morphism from *groundfield* to V.")

(setf (name *groundfield*) 'groundfield)

(defmacro get-summand-indexed-by (a index)
  "The first argument is a directsum object [or partially-known-qvsp]
A, and the second is an index i.  The function returns the i-th direct
summand of A.  It returns nil if no summand is found."
  ;; The test for equality of indices is Lisp's predicate equal,
  ;; except for p.-k.-qvsp's, where it is eql.  Do not use eq for the
  ;; test on indices, since the indices in the tensor functions are
  ;; dotted pairs, and eq doesn't even work correctly on integers.
  `(gethash ,index (summands ,a)))

(defstruct (directsum-qvsp
	    (:include qv)
	    (:print-function
	     (lambda (d s k)
	       (declare (ignore k))
	       (format s "#<~S>"
		       (or (name d)
			   (list 'directsum-qvsp
				 (list 'dim (dim d))
				 (list 'summands
				       (map 'list
					    #'(lambda (i)
						(get-summand-indexed-by d i))
					    (directsum-qvsp-indices d)))
				 (list 'indices
				       (coerce
					(directsum-qvsp-indices d)
					'list))))))))
  "A directsum-qvsp is a directsum in the category qvsp.  See the
documentation on directsum for more information."
  (summands (dummy) :type hash-table)
  (indices (dummy) :type simple-vector))


(defstruct (tensor-qvsp
	    (:include qv)
	    (:print-function
	     (lambda (v s k)
	       (declare (ignore k))
	       (format s "#<~S>"
		       (or (name v)
			   (list 'tensor-qvsp
				 (list 'dim (dim v))
				 (list 'first-factor
				       (tensor-qvsp-from1 v))
				 (list 'second-factor
				       (tensor-qvsp-from2 v))))))))
  "A tensor-qvsp V is the tensor product of two qvsp's, which can be
recovered by (from1 V) and (from2 V).  The basis for V is obtained by
tensoring the standard bases for (from1 V) and (from2 V), in row-major
order.  For instance, if (from1 V) has basis {e0, e1}, and (from2 V)
has basis {f0, f1}, then V has basis {e0f0, e0f1, e1f0, e1f1}."
  (from1 (dummy) :type qvsp)
  (from2 (dummy) :type qvsp))

(defstruct (wedge-qvsp (:include qv)
		       (:print-function
			(lambda (w s k)
			  (declare (ignore k))
			  (if (name w)
			      (format s "#<~S>" (name w))
			    (format s
				    "#<WEDGE-QVSP, ~:R exterior power of ~S>"
				    (wedge-qvsp-deg w)
				    (wedge-qvsp-from w))))))
"Wedge-qvsp is the type of objects obtained by applying the b-th
exterior power operator [wedge^b] to a qvsp.  If C = wedge^b A is a
wedge-qvsp, then (from C) returns A, and (deg C) returns b.
   We require a and b to be non-negative integers.  We do not require
b <= a, though the results are trivial if this condition is not
satisfied.
   We imagine that C has a fixed basis e_{ijk...} made of b-element
wedge-products
   e_i wedge e_j wedge e_k wedge ...
of members of the standard basis for A.
   It would be elegant if exterior powers (and symmetric powers) were
implemented as quotients of tensor powers.  However, it seems too
inefficient.
   The way the multi-indices {ijk...} are ordered is given by the
three variables of the ODOM- family.  For their documentation, see the
source code."
#|
   These variables are initially set to the empty list, and are
created as needed as the program runs.  The user should never have to
see these variables, but here is their description.  Let a = dim A.
   (i) *ODOM-IND2TUPLE* is an association list where the keys are
2-elt lists (a b), and the attached object is a vector V_ab of length
(binom a b).  The i-th elt of V_ab is a b-tuple of integers from 0
thru a-1, sorted by < and with no duplicates.  To see V_ab, call
(fetchequal (list a b) *odom-ind2tuple*) .
   (ii) *ODOM-TUPLE2IND* gives the inverse. It is a association list
with the same keys, where the attached object is an association list
A_ab with keys EQUAL to the tuples, and values = the i's.  To see
A_ab, call (fetchequal (list a b) *odom-tuple2ind*) .
   (iii) *ODOM-LISTS* is an association list with the same keys, just
storing the list of tuples with no indexing data.  This makes it
easier to create later lists by mapping.
|#
  (from (dummy) :type qvsp)
  (deg (dummy) :type fixnum))

(deftype qvsp ()
"QVSP is the type of objects symbolizing finite-dimensional rational
vector spaces.  If V is a qvsp, then (dim v) is its dimension.  There
are no other interesting functions that you can apply to a qvsp.
After all, in a category, the objects aren't interesting--all the
interest resides in the morphisms.  :-)
   It is taken for granted that every qvsp has a fixed basis.  All
qvsp-morphisms f are represented by matrices [accessible as (mat f)]
with respect to the fixed bases.  However, there is nothing in the
system that gives you or returns these bases.  They exist only in the
mind's eye.
   There is a special object, *zero-qvsp*, representing the zero qvsp.
There is another special object, *groundfield*, which is a rational
vector space of dimension 1.  We imagine that it has the canonical
basis {1}.  A `vector' in a qvsp V, in the rare cases that we want
one, is specified by giving a qvsp-morphism from *groundfield* to V.

To make a new qvsp of dimension 17 [say], use
     (make-qvsp 17)
or
     (user-input-qvsp 17).
To assign this qvsp as the value of the variable foo, give the command
     (setf foo (make-qvsp 17)).

You can give a qvsp a name, as follows:
     (setf foo (make-qvsp 17 :name 'beethoven))
This means that the 17-dimensional vector space in question will
always be printed as #<BEETHOVEN>.  [Recall that Lisp is
case-insensitive.]  Notice the distinction between a variable and its
print-name.  Foo is a variable which holds the vector space;
BEETHOVEN is merely the print-name of the space.  The use of names is
certainly optional, but it may make it easier to read your results at
the terminal."
  '(or qv directsum-qvsp tensor-qvsp wedge-qvsp))

(defstruct (qvsp-morphism
	    ;; make-qvsp-m'm and copy-qvsp-m'm are defined below as
	    ;; functions
	    (:constructor nil)
	    (:copier nil))
  (sou (dummy) :type qvsp)
  (tar (dummy) :type qvsp))

(setf (documentation 'qvsp-morphism 'type)
  (concatenate 'string
   "When f is a qvsp-morphism, the source (sou f) and target (tar f) are
rational vector spaces [which are called qvsp's in these programs].
(mat f) gives a matrix which represents the map f with respect to
standard bases in the source and target.
   (ker f) gives an injective morphism from a kernel object K to (sou f).
Dually, (coker f) gives a surjective morphism from (tar f) to a
cokernel object C.  Combining these operators gives a canonical way to
factor f using
   injections `----> [or u at the bottom of a vertical arrow]
   surjections ---->> [or two v's at the bottom of a vertical arrow]
   and one isomorphism --~-->
as follows.

             coker f                  f                ker f
   C <<--------------------- T <------------ S <-------------------' K
     `-------------------->  ^ |           ^ |  ------------------>>
         coker-section f     | |           | |   ker-retraction f
                             | |           | |
                             | |           | |
                             | |           | |
                       im f  | |           | | coim f
                             | v           | v
                             u v           u v
                              I <----~----- CI

Here (im f) is the image of f, defined to be the injection (ker (coker
f)) with source an image object I.  Dually, (coim f) is the coimage of
f, defined to be the surjection (coker (ker f)) with target a coimage
object CI.  In a category of vector spaces over a field, f determines
a canonical isomorphism between CI and I, and the isomorphism is
recoverable as (coim-to-im f).  The other four arrows in the diagram--
ker-retraction, coker-section, and the two unlabelled vertical
arrows-- are appropriate one-sided inverses for the arrows they're
drawn next to.  These ideas lie at the core of Sheafhom.  See the
documentation for ker-retraction, coker-section, pullback-to-ker and
pushforward-to-coker for more information.
   While the morphism f is between rational vector spaces, as the name
qvsp-morphism suggests, the matrix (mat f) representing it must have
INTEGER entries.  If A is a matrix of integers, the function ismith
and the LLL functions [currently in "
*name-of-matrix-file*
".lisp] give support
for `factoring' A as in the above diagram.  See the documentation on
kernel-etc-LLL, or on ismith, for the details of how this is done.
   If f is represented by a square matrix A of integers with
determinant of absolute value greater than 1, there is only limited
support for working with the inverses of f and A [see `inverse']."))

(defstruct (qm (:include qvsp-morphism)
	       (:print-function
		(lambda (f s k)
		  (declare (ignore k))
		  (format s "#<~S>"
			  (or (name f)
			      (list 'qvsp-morphism
				    (list 'from (qm-sou f))
				    (list 'to (qm-tar f))
				    (list 'matrix (qm-mat f))))))))
  "QM is the type of any qvsp-morphism other than a zero or identity
morphism.  See the documentation on qvsp-morphism."
  (mat (dummy) :type matrix)
  (ker nil :type (or null qvsp-morphism))
  (coker nil :type (or null qvsp-morphism))
  (ker-retraction nil :type (or null qvsp-morphism))
  (coker-section nil :type (or null qvsp-morphism)))
  ;; Zero- and id-qvsp-morphisms have only sou and tar slots.

(defstruct (zero-qvsp-morphism
	    (:include qvsp-morphism)
	    (:print-function
	     (lambda (f s k)
	       (declare (ignore k))
	       (format s "#<~S>"
		       (or (name f)
			   (list
			    'zero-qvsp-morphism
			    (list 'from
				  (zero-qvsp-morphism-sou f))
			    (list 'to (zero-qvsp-morphism-tar f))))))))
  "A zero-qvsp-morphism is a qvsp-morphism [see doc.] which is a zero
morphism.")

(defstruct (id-qvsp-morphism
	    (:include qvsp-morphism)
	    (:print-function
	     (lambda (f s k)
	       (declare (ignore k))
	       (format s "#<~S>"
		       (or (name f)
			   (list 'id-qvsp-morphism
				 (list 'from 
				       (id-qvsp-morphism-sou f))
				 (list 'to 
				       (id-qvsp-morphism-tar f))))))))
  "An id-qvsp-morphism is a qvsp-morphism whose matrix, with respect to
the bases in use in its source and target, is an identity matrix.
   This is technically not what we want.  An id-qvsp-morphism `should'
be an identity morphism in the sense of category theory, one that
realizes the fact that its source and target are exactly the same
object [Lisp's `eq'].  However, it makes the program more efficient if
we enlarge the definition the way we have.")

;;; Here is a cheap "inverse" patch.  It assumes inverses won't be
;;; used outside of a (compose ...).

(defstruct (formal-inverse-qvsp-morphism
	    (:print-function
	     (lambda (f s k)
	       (declare (ignore k))
	       (format s "#<~S>"
		       (or (name f)
			   (list 'formal-inverse-qvsp-morphism
				 (list 'inverse-of 
		  (formal-inverse-qvsp-morphism-is-inverse-of f))))))))
  "Let g be an formal-inverse-qvsp-morphism.  (is-inverse-of g) is a regular
qvsp-morphism f, and g formally represents the inverse of f.  We will
be using (ismith (mat f)) a lot, so we compute it and store it in
(mat+ g)."
  (is-inverse-of (dummy) :type qvsp-morphism)
  (mat+ nil :type (or null matrix+)))

(defstruct (cochain-cx (:print-function
			(lambda (x s k)
			  (declare (type cochain-cx x)
				   (ignore k))
			  (format s "#<~S>"
				  (or (name x)
				      (let ((d-list '()))
					(map-objects
					 #'(lambda (i v)
					     (declare (ignore v))
					     (push
					      (cons i (d i x))
					      d-list))
					 x)
					(append
					 (list 'cochain-cx
					       (cons
						'dims
						(dims x)))
					 d-list)))))))
  (objects (dummy) :type hash-table)
  (maps (dummy) :type hash-table)
  (cocycle-inclusion-map nil :type (or null cochain-map))
  (cocycle-retraction nil :type (or null cochain-map))
  (cobdry-to-cocycle-map nil :type (or null cochain-map)))

(defstruct (directsum-cochain-cx
	    (:include cochain-cx)
	    (:print-function
	     (lambda (x s k)
	       (declare (type directsum-cochain-cx x)
			(ignore k))
	       (format s "#<~S>"
		 (or (name x)
		     (list
		      'directsum-cochain-cx
		      (cons 'indices
			    (directsum-cochain-cx-indices
			     x))
		      (cons 'summands
			    (map 'list
				 #'(lambda (i)
				     (cons i
					   (get-summand-indexed-by x i)))
				 (directsum-cochain-cx-indices x)))))))))
  (summands (dummy) :type hash-table)
  (indices (dummy) :type simple-vector))

(defstruct (tensor-cochain-cx
	    (:include cochain-cx)
	    (:print-function
	     (lambda (x s k)
	       (declare (type tensor-cochain-cx x)
			(ignore k))
	       (format s "#<~S>"
		       (or (name x)
			   (list 'tensor-cochain-cx
				 (cons 'from1
				       (tensor-cochain-cx-from1 x))
				 (cons 'from2
				       (tensor-cochain-cx-from2 x))))))))
  (from1 (dummy) :type cochain-cx)
  (from2 (dummy) :type cochain-cx))

(defstruct (exterior-algebra
	    (:include cochain-cx)
	    (:constructor really-make-exterior-algebra)
	    ;; Prev. is so there's no conflict with the Sheafhom
	    ;; function make-exterior-algebra.
	    (:print-function
	     (lambda (x s k)
	       (declare (ignore k))
	       (if (name x)
		   (format s "#<~S>" (name x))
		 (format s
			 "#<EXTERIOR-ALGEBRA on ~S>"
			 (exterior-algebra-from x))))))
  "The exterior algebra on a qvsp V.  If E is the algebra, (from E)
returns V.  If V has dim n, E is a cochain-cx with indices 0, 1, ...,
n, inclusive, whose i-th object is wedge^i(V)."
  (from (dummy) :type qvsp)
  (pairing nil :type (or null cochain-map)))

(defstruct (cochain-map (:print-function
			 (lambda (f s k)
			   (declare (type cochain-map f)
				    (ignore k))
			   (format s "#<~S>"
				   (or (name f)
				       (list 'cochain-map
					     (cons 'sou (sou f))
					     (cons 'tar (tar f))
					     (let ((ff-list '()))
					       (maphash
						#'(lambda (i ff)
						    (push
						     (cons i ff)
						     ff-list))
						(mat f))
					       (cons 'mat ff-list))))))))
			       
  (sou (dummy) :type cochain-cx)
  (tar (dummy) :type cochain-cx)
  (mat (empty-hash-table) :type hash-table)
  (ker nil :type (or null cochain-map))
  (coker nil :type (or null cochain-map))
  (ker-retraction nil :type (or null cochain-map))
  (coker-section nil :type (or null cochain-map))
  (map-on-cohomology nil :type (or null cochain-map)))

(deftype directsum ()
"Let V be a directsum object in a certain category C [e.g. C = qvsp,
or cochain-cx].  Then V has two slots, (summands V) and (indices
V), in addition to the slots that objects of C usually have.  (indices
V) is a non-empty vector of distinct Lisp objects that index the
summands forming V.  (summands V) is a hash-table whose keys are
the indices and whose values are the summands.  For example, if i0 is
an index, then (gethash i0 (summands V)) returns the i0-th summand
forming V [though a call to the function (get-summand-indexed-by V i0)
is the recommended way to do this].
   The basis of V is formed by taking the bases of the summands in the
order given by (indices V).
   It is illegal to have a directsum with an empty set of indices."
  '(or directsum-qvsp directsum-cochain-cx))

;;; Tensors.
;;;
;;; Philosophy: We have defined abelian categories of qvsp's and of
;;; graded-object's.  We lay tensors on top of this, perhaps in an
;;; awkward way.  A tensor object is a repository for the two objects
;;; that were tensored.  A tensor product of directsum-qvsps is not
;;; "distributed out".  (In general, an object in Sheafhom has as
;;; little structure as possible.)  (Besides, distributing it out
;;; would change our conventions about row-major order.  But see the
;;; function distribute-tensor-from-left-over-directsum below.)  For a
;;; tensor product of qvsp-morphisms, which has a mat slot and which
;;; is expected not to be destructively altered, we may as well let
;;; make-tensor-morphism fill in the mat slot.

(deftype tensor ()
"Any tensor product is automatically a member of this type.  For more
information, see the documentation for the subtypes tensor-qvsp and
tensor-graded-object.
   Any tensor object v will have slots (from1 v) and (from2 v)."
  '(or tensor-qvsp tensor-cochain-cx))

;;; ------------------------------------------------------------
;;; --------------------- FUNCTIONS ----------------------------
;;; ------------------------------------------------------------

;;; A few basics...

(proclaim '(ftype (function (matrix nnfixnum nnfixnum) integer)
		  matrix-ref))

(proclaim '(ftype (function (qm) qm)
		  fill-out-as-injection fill-out-as-surjection))

#|
;;; An idea from Kelly Murray at Franz Inc., kem@math.ufl.edu .  This
;;; is in the style of Graham's _On Lisp_.  It may not be what you
;;; want if evaluating place has side effects.
(defmacro setf-when-null (place value)
  (let ((old-value (gensym)))
    `(let ((,old-value ,place))
       (when (null ,old-value)
	 (setf ,place ,value)))))
;;; Never useful?  Was used only in the four fill-out-??kerside-?m
;;; functions.
|#

;;; ----------------------------------------

;;; Functions that make it easier to get at the slots of the
;;; structures.  These imitate CLOS's generic functions.

;;; We have to make dim a generic function, because later classes
;;; (like toric-variety) may want it.

(defgeneric dim (x))

(setf (documentation 'dim 'function)
  "The argument is x.  The function returns the dimension of x.  This
number is always a fixnum [a 1-word integer], and is non-negative.
   If x is a qvsp [see doc.], the function returns the dimension of
this vector space.
   Dim may be defined for other classes of x's also, like toric
varieties.")

(defmethod dim (x)
  ;; Handles all the non-CLOS cases.
  (qv-dim x))

(defun sou (f)
  "Whenever f is a morphism, (sou f) gives its source object [also
known as its domain].  You may also type (source f)."
  (etypecase f
    (qvsp-morphism (qvsp-morphism-sou f))
    (cochain-map (cochain-map-sou f))
    (formal-inverse-qvsp-morphism
     (tar (formal-inverse-qvsp-morphism-is-inverse-of f)))
    (quiver-rep-map (quiver-rep-map-sou f))
    (sheaf-morphism (sheaf-morphism-sou f))))

(defsetf sou (f) (val)
  `(etypecase ,f
     (qvsp-morphism (setf (qvsp-morphism-sou ,f) ,val))
     (cochain-map (setf (cochain-map-sou ,f) ,val))
     ))

(defun tar (f)
  "Whenever f is a morphism, (tar f) gives its target object [also
known as its range or codomain].  You may also type (target f)."
  (etypecase f
    (qvsp-morphism (qvsp-morphism-tar f))
    (cochain-map (cochain-map-tar f))
    (formal-inverse-qvsp-morphism
     (sou (formal-inverse-qvsp-morphism-is-inverse-of f)))
    (quiver-rep-map (quiver-rep-map-tar f))
    (sheaf-morphism (sheaf-morphism-tar f))))

(defsetf tar (f) (val)
  `(etypecase ,f
     (qvsp-morphism (setf (qvsp-morphism-tar ,f) ,val))
     (cochain-map (setf (cochain-map-tar ,f) ,val))
     ))

(defmacro source (f)
  "Whenever f is a morphism, (sou f) gives its source object [also
called its domain].    The present macro lets you type (source f)
instead of (sou f), if you prefer."
  `(sou ,f))

(defmacro target (f)
  "Whenever f is a morphism, (tar f) gives its target object [also
known as its range or codomain].    The present macro lets you type
(target f) instead of (tar f), if you prefer."
  `(tar ,f))

(defun mat (f)
  "Argument: f.  If f is a qvsp-morphism, (mat f) is the matrix stored
within it that realizes f with respect to the standard bases of the
source and target.  Otherwise, mat is an appropriate data structure
storing, for each i, the map from the ith term of the source to the
ith term of the target."
  (etypecase f
    (zero-qvsp-morphism
     (make-matrix (dim (tar f)) (dim (sou f))))
    (id-qvsp-morphism
     (make-id-matrix (dim (sou f))))
    (qm
     (qm-mat f))
    (cochain-map
     (cochain-map-mat f))
    (quiver-rep-map
     (quiver-rep-map-mat f))
    (sheaf-morphism
     (sheaf-morphism-mat f))))

(defsetf mat (f) (val)
  `(etypecase ,f
     (qm (setf (qm-mat ,f) ,val))
     ))

(defun ker (f)
  "Argument is a morphism f.  Let V be the source of f.  Output is an
injective morphism from a new `kernel object' K to V.  If you want K
itself, use (sou (ker f)).
   For a splitting of (ker f), use (ker-retraction f).  For an
overview of these ideas, see the documentation on qvsp-morphism."
  (etypecase f
    (qm
     (let ((ans (qm-ker f)))
       (cond ((null ans)
	      (fill-out-kerside-qm f)
	      (qm-ker f))
	     (t ans))))
    (zero-qvsp-morphism
     (make-id-morphism (zero-qvsp-morphism-sou f)
		       (zero-qvsp-morphism-sou f)))
    (id-qvsp-morphism
     (make-zero-morphism *zero-qvsp* (id-qvsp-morphism-sou f)))
    (cochain-map
     (let ((ans (cochain-map-ker f)))
       (cond ((null ans)
	      (fill-out-kerside-cochain-map f)
	      (cochain-map-ker f))
	     (t ans))))))

(defmacro kernel (f)
  "An alias for ker.  See the documentation for ker."
  `(ker ,f))

(defun coker (f)
  "Argument is a morphism f.  Let W be the target of f.  Output is a
surjective morphism from W to a new `cokernel object' C.  If you want
C itself, use (tar (coker f)).
   For a splitting of (coker f), use (coker-section f).  For an
overview of these ideas, see the documentation on qvsp-morphism."
  (etypecase f
    (qm
     (let ((ans (qm-coker f)))
       (cond ((null ans)
	      (fill-out-cokerside-qm f)
	      (qm-coker f))
	     (t ans))))
    (zero-qvsp-morphism
     (make-id-morphism (zero-qvsp-morphism-tar f)
		       (zero-qvsp-morphism-tar f)))
    (id-qvsp-morphism
     (make-zero-morphism (id-qvsp-morphism-tar f) *zero-qvsp*))
    (cochain-map
     (let ((ans (cochain-map-coker f)))
       (cond ((null ans)
	      (fill-out-cokerside-cochain-map f)
	      (cochain-map-coker f))
	     (t ans))))))

(defmacro cokernel (f)
  "An alias for coker.  See the documentation for coker."
  `(coker ,f))

(defun ker-retraction (f)
  "Argument is a morphism f.  Let V be the source of f, and let K be the
source of (ker f), so that (ker f) maps K --> V injectively.  The
value of (ker-retraction f) is a surjective morphism s : V --> K such
that (compose s (ker f)) is the identity on K."
  (etypecase f
    (qm
     (let ((ans (qm-ker-retraction f)))
       (cond ((null ans)
	      (fill-out-kerside-qm f)
	      (qm-ker-retraction f))
	     (t ans))))
    (zero-qvsp-morphism
     (make-id-morphism (zero-qvsp-morphism-sou f)
		       (zero-qvsp-morphism-sou f)))
    (id-qvsp-morphism
     (make-zero-morphism (id-qvsp-morphism-sou f) *zero-qvsp*))
    (cochain-map
     (break
"Gives a degree-wise retraction, but is not a cochain map. Continue if
this is okay with you.")
     (let ((ans (cochain-map-ker-retraction f)))
       (cond ((null ans)
	      (fill-out-kerside-cochain-map f)
	      (cochain-map-ker-retraction f))
	     (t ans))))))

(defun coker-section (f)
  "Argument is a morphism f.  Let W be the target of f, and let C be the
target of (coker f), so that (coker f) maps W --> C surjectively.  The
value of (coker-section f) is an injective morphism s : C --> W such
that (compose (coker f) s) is the identity on C."
  (etypecase f
    (qm
     (let ((ans (qm-coker-section f)))
       (cond ((null ans)
	      (fill-out-cokerside-qm f)
	      (qm-coker-section f))
	     (t ans))))
    (zero-qvsp-morphism
     (make-id-morphism (zero-qvsp-morphism-tar f)
		       (zero-qvsp-morphism-tar f)))
    (id-qvsp-morphism
     (make-zero-morphism *zero-qvsp* (id-qvsp-morphism-tar f)))
    (cochain-map
     (break
"Gives a degree-wise section, but is not a cochain map. Continue if
this is okay with you.")
     (let ((ans (cochain-map-coker-section f)))
       (cond ((null ans)
	      (fill-out-cokerside-cochain-map f)
	      (cochain-map-coker-section f))
	     (t ans))))))

(defun im (f)
  "Argument is a morphism f.  Output is an injective morphism from an image
object into (tar f).  The image is the kernel of the cokernel of f.
For an overview of these ideas, see the documentation on
qvsp-morphism."
  (ker (coker f)))

(defmacro image (f)
  "An alias for im.  See the documentation for im."
  `(im ,f))

(defun coim (f)
  "Argument is a morphism f.  Output is a surjective morphism from
(sou f) onto a coimage object.  The coimage is the cokernel of the
kernel of f.  For an overview of these ideas, see the documentation on
qvsp-morphism."
  (coker (ker f)))

(defmacro coimage (f)
  "An alias for coim.  See the documentation for coim."
  `(coim ,f))

(defmacro objects (x)
  `(cochain-cx-objects ,x))

(defmacro maps (x)
  `(cochain-cx-maps ,x))

(defun d (i x)
  (declare (type cochain-cx x)
	   (fixnum i))
  "(d i x) gives the map d^i in the cochain-cx x."
  (let ((di (gethash i (maps x))))
    (if (null di)
	(make-zero-morphism (term x i)
			    (term x (1+ i)))
      di)))

(defun map-to-cohomology (x)
  (declare (type cochain-cx x))
  "Only for cochain-cx's.  [Was formerly a slot value.]  Is the coker
of what's in the cobdry-to-cocycle-map slot."
  (coker (cobdry-to-cocycle-map x)))

(defgeneric cohomology (x))

(setf (documentation 'cohomology 'function)
  "There is one argument x.  If x is a cochain-cx, this function
returns the cohomology of x as a cochain-cx with zero differentials.
See the documentation for the type cochain-cx for more information.
   If x is a sheaf, this function returns the global cohomology of x
[the hypercohomology, also accessible as (hypercoh x)].  If you
specified a perversity other than top perversity, it will return the
global intersection cohomology of x.")

(defmethod cohomology (x) ; the non-CLOS case
  (etypecase x
    (cochain-cx
     (tar (map-to-cohomology x)))))

(defun map-on-cohomology (f)
  (etypecase f
    (cochain-map
     (let ((ans (cochain-map-map-on-cohomology f)))
       (cond ((null ans)
	      (fill-out-cochain-map f)
	      (cochain-map-map-on-cohomology f))
	     (t ans))))))

(defmacro map-objects (lam x)
  ;; x is a cochain-cx.  lam is a function of two variables, say i and
  ;; v.  This macro calls lam once for each pair i,v occuring in
  ;; (objects x).
  `(maphash ,lam (cochain-cx-objects ,x)))

(defmacro map-maps (lam x)
  `(maphash ,lam (cochain-cx-maps ,x)))

(defmacro map-mat (lam f)
  `(maphash ,lam (mat ,f)))

(defun test-cochain-cx (x)
  "Returns x if dd = 0 everywhere in x.  Signals an error otherwise."
  (map-maps #'(lambda (i di)
		(unless (composition-zerop (d (1+ i) x) di)
			(error "dd is not zero.")))
	    x)
  x)

(defun test-cochain-map (f)
  "Returns f if all squares commute.  Signals error otherwise."
  (let ((a (sou f))
	(b (tar f)))
    (map-mat #'(lambda (i fi)
		 (unless (zero-morphism-p
			  (add
			   (compose (d i b) fi)
			   (scmult -1
				   (compose (term (1+ i) f)
					    (d i a)))))
			 (error "A square does not commute.")))
	     f)
    f))

(defun map-cochain-map (op f)
  "Makes a new cochain-map whose mat field is the result of applying
op, a function of one argument ff, to each member of (mat f).  Assumes
(sou f) and (tar f) do not change."
  (make-cochain-map
   :sou (cochain-map-sou f)
   :tar (cochain-map-tar f)
   :mat (with-eql-ans
	 (map-mat #'(lambda (i ff)
		      (setf (gethash i ans)
			    (funcall op ff)))
                  f))))

(defun summands (x)
  "See the documentation on directsum and partially-known-qvsp."
  (etypecase x
    (directsum-qvsp
     (directsum-qvsp-summands x))
    (directsum-cochain-cx
     (directsum-cochain-cx-summands x))
    ))

(proclaim '(ftype (function (directsum) simple-vector) indices))

(defun indices (x)
  (declare (type directsum x))
  "See the documentation on directsum."
  (etypecase x
    (directsum-qvsp
     (directsum-qvsp-indices x))
    (directsum-cochain-cx
     (directsum-cochain-cx-indices x))
    ))

(defun from1 (x)
  (declare (type tensor x))
  "See the documentation on tensor-qvsp and tensor-cochain-cx."
  (etypecase x
    (tensor-qvsp
     (tensor-qvsp-from1 x))
    (tensor-cochain-cx
     (tensor-cochain-cx-from1 x))))

(defun from2 (x)
  (declare (type tensor x))
  "See the documentation on tensor-qvsp and tensor-cochain-cx."
  (etypecase x
    (tensor-qvsp
     (tensor-qvsp-from2 x))
    (tensor-cochain-cx
     (tensor-cochain-cx-from2 x))))

(defun from (x)
  "See the documentation on wedge-qvsp and exterior-algebra."
  (etypecase x
    (wedge-qvsp
     (wedge-qvsp-from x))
    (exterior-algebra
     (exterior-algebra-from x))))

(defmacro deg (x)
  "If V is the k-th exterior power Wedge-k(V0) of a space V0, then
(deg V) returns k."
  `(wedge-qvsp-deg ,x))

(defmacro is-inverse-of (f)
  "See the documentation on formal-inverse-qvsp-morphism."
  `(formal-inverse-qvsp-morphism-is-inverse-of ,f))

(defmacro mat+ (f)
  "See the documentation on formal-inverse-qvsp-morphism."
  `(formal-inverse-qvsp-morphism-mat+ ,f))

;;; ----------------------------------------
;;; ---------- FUNCTIONS proper ------------
;;; ----------------------------------------

(defun make-qvsp (d &key name)
  (declare (type nnfixnum d))
  "Makes a new qvsp.  If the dimension is 0, it returns the canonical
0-dimensional space *zero-qvsp*.  For hints on usage, see the
documentation for qvsp."
  (with-names
  (if (zerop d)
      *zero-qvsp*
    (make-qv :dim d))))

(defun make-qvsp-morphism (so ta matr &key name)
  (declare (type qvsp so ta)
	   (type matrix matr))
"Makes a qvsp-morphism.  The three arguments are resp. the source [a
qvsp], the target [a qvsp], and a matrix giving the morphism with
respect to standard coordinates.
  The function outputs a zero-qvsp-morphism or an id-qvsp-morphism
whenever possible."
  (with-names
  (cond ((or (eql so *zero-qvsp*)
	     (eql ta *zero-qvsp*)
	     (matrix-zerop matr))
	 (make-zero-qvsp-morphism :sou so :tar ta))
	((id-matrix-p matr)
	 (make-id-qvsp-morphism :sou so :tar ta))
	(t
	 (make-qm :sou so :tar ta :mat matr)))))

(defun copy-qvsp-morphism (f new-so new-ta)
  "Arguments f, new-so, new-ta.  Makes a new qvsp-morphism with the same
matrix as f, but with source new-so and target new-ta.  This is for
when (sou f) and new-so [resp. (tar f) and new-ta] are isomorphic but
different [non-`eq'], and you need to make replacements.  Whenever
possible, f itself [not a copy] is returned, for efficiency reasons."
  ;; If the copy would have all slots eq to those of f, just
  ;; return f.  This happens, e.g., if f is the result of
  ;; composing something with an identity morphism i, i.e. when
  ;; (sou i) is eq to (tar i).
  (when (and (eq (sou f) new-so) (eq (tar f) new-ta))
	(return-from copy-qvsp-morphism f))
  (assert (= (dim (sou f)) (dim new-so)) ()
	  "The old and new sources have different dimensions.")
  (assert (= (dim (tar f)) (dim new-ta)) ()
	  "The old and new targets have different dimensions.")
  (etypecase f
    (qm
     ;; Efficiency problem: this throws away any LLL info that has
     ;; been computed.
     (make-qm :sou new-so :tar new-ta :mat (qm-mat f)))
    (zero-qvsp-morphism
     (make-zero-qvsp-morphism :sou new-so :tar new-ta))
    (id-qvsp-morphism
     (make-id-qvsp-morphism :sou new-so :tar new-ta))
    ))

(defun zero-object-of-type-of (x)
  "Argument x.  Returns a zero object of the same type.  If x is a qvsp,
the result is *zero-qvsp*.  If x is a cochain-cx, the result is a
cochain-cx with one zero object in degree 0."
  (etypecase x
    (qvsp
      *zero-qvsp*)
    (cochain-cx
      ;; This version trusts that every entry of (objects x) has the
      ;; same type, or at least compatible types in some sense.  It
      ;; also assumes (objects x) is not empty.
     (make-cochain-cx
      :objects (with-eql-ans
		(setf (gethash 0 ans)
		      (zero-object-of-type-of
		       (get-one-hash-value (objects x)))))
      :maps (empty-hash-table)))
    ))

(defun zero-object-p (x)
  "Returns true if and only if its argument x is a zero object in its
category."
  (etypecase x
    (qvsp
     (zerop (the nnfixnum (dim x))))
    (cochain-cx
     (map-objects #'(lambda (i v)
		      (declare (ignore i))
		      (unless (zero-object-p v)
			      (return-from zero-object-p
					   nil)))
		  x)
     t)
    ))

(defun zero-maps-p (x)
  (declare (type cochain-cx x))
  "Returns true iff its argument is a complex with all differentials
zero."
  (map-maps #'(lambda (i ff)
		(declare (ignore i))
		(unless (zero-morphism-p ff)
			(return-from zero-maps-p
				     nil)))
	    x)
  t)

(defun zero-morphism-p (f)
  "Returns `true' [Lisp's `t'] if and only if its argument f is a zero
morphism in its category."
  (etypecase f
    (zero-qvsp-morphism
     t)
    (id-qvsp-morphism
     (zero-object-p (sou f)))
    (qm
     (matrix-zerop (mat f)))
    (cochain-map
     (map-mat #'(lambda (i ff)
		  (declare (ignore i))
		  (unless (zero-morphism-p ff)
			  (return-from zero-morphism-p nil)))
	      f)
     t)
    ))

(defun composition-zerop (f g)
  "Arguments: f, g.  Returns true if and only if the composition fg is a
zero morphism in its category.  It checks this without storing the
composition."
  (etypecase g
    (qvsp-morphism
      (etypecase f
	(qvsp-morphism
	 (cond ((or (typep f 'zero-qvsp-morphism)
		    (typep g 'zero-qvsp-morphism))
		t)
	       ((typep f 'id-qvsp-morphism)
		(zero-morphism-p g))
	       ((typep g 'id-qvsp-morphism)
		(zero-morphism-p f))
	       (t
		(matrix-product-zerop (mat f) (mat g)))))
	))
    (cochain-map
     (etypecase f
       (cochain-map
	(map-mat #'(lambda (i ff)
		     (unless (composition-zerop ff (term g i))
			     (return-from composition-zerop nil)))
		 f)
	t)))))

(defun injective-p (f)
  "Argument f.  Returns true if and only if the morphism f is injective."
  (etypecase f
    (zero-qvsp-morphism
     (zerop (dim (sou f))))
    (id-qvsp-morphism
     t)
    (qm
     (= (rank f) (dim (sou f))))
    (formal-inverse-qvsp-morphism
     ;; If one of these has been made, then we're already supposed to
     ;; have checked that the map is a Q-isomorphism.
     t)
    (cochain-map
     (map-mat #'(lambda (i ff)
		  (declare (ignore i))
		  (unless (injective-p ff)
			  (return-from injective-p nil)))
	      f)
     t)))

(defun surjective-p (f)
  "Argument f.  Returns true if and only if the morphism f is surjective."
  (etypecase f
    (zero-qvsp-morphism
     (zerop (dim (tar f))))
    (id-qvsp-morphism
     t)
    (qm
     (= (rank f) (dim (tar f))))
    (formal-inverse-qvsp-morphism
     ;; If one of these has been made, then we're already supposed to
     ;; have checked that the map is a Q-isomorphism.
     t)
    (cochain-map
     (map-mat #'(lambda (i ff)
		  (declare (ignore i))
		  (unless (surjective-p ff)
			  (return-from surjective-p nil)))
	      f)
     t)))

(defun dual (f)
 "If f is a qvsp-morphism from V to W with matrix A, then (dual f) is a
qvsp-morphism from W to V with matrix A-transpose.  Strictly speaking,
this is not correct: (dual f) should be a morphism from W^* to V^*,
where the starred objects are formal dual spaces, different from W and
V.  However, if we identify V with V^* and W with W^* under the
standard dot product, then these definitions are correct."
  (etypecase f
    (qm
     (make-qm 
      :sou (qm-tar f) :tar (qm-sou f)	; backwards
      :mat (transpose (qm-mat f))))
    (zero-qvsp-morphism
     (make-zero-qvsp-morphism :sou (zero-qvsp-morphism-tar f)
			      :tar (zero-qvsp-morphism-sou f)))
    (id-qvsp-morphism
     (make-id-qvsp-morphism :sou (id-qvsp-morphism-tar f)
			    :tar (id-qvsp-morphism-sou f)))))

(defun inverse (f)
  "If f is an invertible morphism, this gives the inverse.  However, if
the representation of the inverse would involve non-integer matrices,
this function is supported very poorly.  Currently it is meant to be
used only inside the (compose-two ...) form when it is known in advance
that the _composition_ can be represented by an integer matrix.  See the
documentation for qvsp-morphism and ismith."
  (etypecase f
   (qm
    (assert (= (dim (qm-sou f)) (dim (qm-tar f)))
	    ()
	    "I was asked to invert a morphism when the source and target have
different dimensions:
~S" f)
    (let* ((mm+ (ismith (qm-mat f)))
	   (detf (det mm+)))
      (declare (type matrix+ mm+)
	       (integer detf))
      (assert (not (zerop detf))
	      () "The argument is not of full rank:
~S" f)
      (cond ((= 1 (abs detf))
	     ;; Here f has an inverse over Z.  Great!
	     ;; id = P * (mat f) * Q, so (mat f)^{-1} = Q * P.
	     (make-qm
	      :sou (qm-tar f) :tar (qm-sou f) ; backwards
	      :mat (mult (matrix+-q mm+) (matrix+-p mm+))))
	    (t
	     (make-formal-inverse-qvsp-morphism
	      :is-inverse-of f
	      :mat+ mm+)))))
   (id-qvsp-morphism
    (make-id-qvsp-morphism :sou (id-qvsp-morphism-tar f)
			   :tar (id-qvsp-morphism-sou f)))
   (zero-qvsp-morphism
    (if (and (= 0 (dim (zero-qvsp-morphism-sou f)))
	     (= 0 (dim (zero-qvsp-morphism-tar f))))
	(make-zero-qvsp-morphism :sou (zero-qvsp-morphism-tar f)
				 :tar (zero-qvsp-morphism-sou f))
      (error
       "I was asked to invert the following zero morphism.
~S" f)))
   (cochain-map
    ;; Note that the inverse is still a cochain-map, i.e. the squares
    ;; still commute.
    (make-cochain-map
     :sou (tar f)
     :tar (sou f)
     :mat (with-eql-ans
	   (map-mat #'(lambda (i ff)
			(setf (gethash i ans)
			      (inverse ff)))
		    f))))))

(defmacro compose (&rest args)
  "Composes one or more morphisms.  These must be in the same
category, and must have compatible sources and targets.  For instance,
(compose f g h) gives the morphism

   D <--f-- C <--g-- B <--h-- A,

whose source is A = (sou h) and whose target is D = (tar f)."
  (cond ((null args)
         (error
"You have called compose with no arguments.  I was expecting
(compose f g), or (compose f g h), etc."))
        ((null (cdr args)) ; one argument
         (car args))
        (t
         (labels ((compose-aux (lyst)
                    (if (null (cddr lyst))
                        `(compose-two ,(car lyst) ,(cadr lyst))
                      (list 'compose-two
                            (car lyst)
                            (compose-aux (cdr lyst))))))
           (compose-aux args)))))

(defun compose-two (f g)
  "The composition of two morphisms.  These must be in the same category,
and (tar g) must be the same as (sou f) [Lisp's `eq'].  See the
documentation for compose."
  ;; Ultimately, the caller of compose-two should check for this if it
  ;; cares, and otherwise we shouldn't call this, to save time.  For
  ;; now, leave it in as a safety net.
  (assert (eq (sou f) (tar g)) ()
"I can't compose C <--f-- B <--g-- A, because the target of g is not
the same as the source of f.")
  (cond ((or (typep f 'zero-qvsp-morphism)
	     (typep g 'zero-qvsp-morphism))
	 (make-zero-morphism (sou g) (tar f)))
	((typep f 'id-qvsp-morphism)
	 (copy-qvsp-morphism g (sou g) (tar f)))
	((typep g 'id-qvsp-morphism)
	 (copy-qvsp-morphism f (sou g) (tar f)))
	((and (typep f 'qm)
	      (typep g 'qm))
	 (make-qvsp-morphism
	  (sou g) (tar f) (mult (mat f) (mat g))))
	((and (typep f 'formal-inverse-qvsp-morphism)
	      (typep g 'formal-inverse-qvsp-morphism))
	 ;; If F^{-1} = f, and G^{-1} = g, then fg = (GF)^{-1}.
	 (inverse (compose-two (is-inverse-of g) (is-inverse-of f))))
	((and (typep f 'formal-inverse-qvsp-morphism)
	      (typep g 'qvsp-morphism))
	 ;; Let f = F^{-1}.  Let A be the matrix of F, and A' the
	 ;; matrix of g.  A^{-1} probably doesn't have entries in Z.
	 ;; However, D = P A Q, where P and Q are GL(Z) matrices
	 ;; produced by ismith.  This implies A^{-1} A' = Q (D^{-1})
	 ;; (P A') .  The function computes the latter product.  If
	 ;; the result doesn't have integer entries, it gives up.  If
	 ;; it does have integer entries, the result is returned as a
	 ;; qvsp-morphism.
	 (let ((n (dim (sou f)))	; A is n-by-n.  A' is n-by-m.
	       (m (dim (sou g)))
	       (ans (mult (matrix+-p (mat+ f))
			  (mat g))))
	   (declare (fixnum m n))
	   (dotimes (i n)
	     (declare (fixnum i))
	     (let ((divisor (matrix-ref (matrix+-d (mat+ f)) i i)))
	       ;; not efficient for csparses
	       (declare (integer divisor))
	       (unless (= divisor 1)
		 (dotimes (j m)
		   (declare (fixnum j))
		   (multiple-value-bind (quo rem)
					(floor (matrix-ref ans i j)
					       divisor)
		     (declare (integer quo rem))
		     (if (zerop rem)
			 (unless (zerop quo)
			   ;; quo = 0 iff original entry = 0 iff no change is
			   ;; needed
			   (matrix-set ans i j quo))
		       (error
"Sorry, but the composition fg of the following two maps f and g will
not have integer entries:
~S
~S" f g)))))))
	   ;; If you get here, fg is defined over Z.
	   (make-qvsp-morphism
	    (sou g) (tar f)
	    (mult (matrix+-q (mat+ f))
		  ans))))
	((and (typep f 'qvsp-morphism)
	      (typep g 'formal-inverse-qvsp-morphism))
         ;; Let g = G^{-1}.  Let A be the matrix of G, and A' that of
	 ;; f.  A^{-1} probably doesn't have entries in Z.  However, D
	 ;; = P A Q [see the documentation on ismith].  This implies
	 ;; A' A^{-1} = A' Q D^{-1} P.  The function computes the
	 ;; latter product.  If the result doesn't have integer
	 ;; entries, it gives up.  Else, the result returned is a
	 ;; qvsp-morphism.
	 (let ((n (dim (tar g)))
	       (m (dim (tar f)))	; A is n-by-n.  A' is m-by-n.
	       (ans (mult (mat f)
			  (matrix+-q (mat+ g)))))
	   (declare (fixnum m n))
	   (dotimes (j n)
	     (declare (fixnum j))
	     (let ((divisor (matrix-ref (matrix+-d (mat+ g)) j j)))
	       ;; not efficient for csparses
	       (declare (integer divisor))
	       (unless (= divisor 1)
		 (dotimes (i m)
		   (declare (fixnum i))
		   (multiple-value-bind (quo rem)
					(floor (matrix-ref ans i j)
					       divisor)
		     (declare (integer quo rem))
		     (if (zerop rem)
			 (unless (zerop quo) ; see comment in previous
			   (matrix-set ans i j quo))
		       (error
"Sorry, but the composition fg of the following two maps f and g will
not have integer entries:
~S
~S" f g)))))))
	   (make-qvsp-morphism
	    (sou g) (tar f)
	    (mult ans
		  (matrix+-p (mat+ g))))))
	((and (typep f 'cochain-map)
	      (typep g 'cochain-map))
	 (make-cochain-map
	  :sou (sou g)
	  :tar (tar f)
	  :mat (with-eql-ans
		(map-mat #'(lambda (i gg)
			     (setf (gethash i ans)
				   (compose-two
				    (term f i)
				    gg)))
			 g))))
	(t
	 (error "Here are two types COMPOSE-TWO can't compose:
~S and ~S." (type-of f) (type-of g)))))

(defun add (f g)
  "To add morphisms f and g [which must have equal sources and equal
targets], use (add f g).  To add matrices, see add-matrix."
  (assert (and (eq (sou f) (sou g)) (eq (tar f) (tar g))) ()
	  "Sources and/or targets are incompatible.")
  (cond ((and (typep f 'qvsp-morphism)
	      (typep g 'qvsp-morphism))
	 ;; Inefficient, but it's not used very often.
	 (let ((ans (add-list (list f g))))
	   (if (null ans)
	       (make-zero-morphism (sou f) (tar f))
	     ans)))
	((and (typep f 'cochain-map)
	      (typep g 'cochain-map))
	 (make-cochain-map
	  :sou (sou f)
	  :tar (tar f)
	  :mat (with-eql-ans
		(let ((inds '()))
		  (maphash #'(lambda (i v)
			       (declare (ignore v))
			       (push i inds))
			   (mat f))
		  (maphash #'(lambda (i v)
			       (declare (ignore v))
			       (push i inds))
			   (mat g))
		  (map nil #'(lambda (i)
			       (setf (gethash i ans)
				     (add (term f i)
					  (term g i))))
		       (remove-duplicates inds :test #'=))))))
	(t
	 (error "Here are two things I can't add:
~S
~S"
		f g))))

(defun scmult (c f)
  (declare (integer c))
  "Scalar multiplication facility for lots of different types.  Use
(scmult c f), where c is an integer and f is a morphism.  For scalar
multiplication involving matrices, see scmult-matrix."
  (etypecase f
    (qvsp-morphism
     (cond ((typep f 'zero-qvsp-morphism)
	    f) ; not a copy--see two clauses down
	   ((zerop c)
	    (make-zero-morphism (sou f) (tar f)))
	   ((= c 1)
	    ;; I believe it's safe to return the identical
	    ;; object and not a copy.  We are never supposed to
	    ;; destructively alter the slots of a qvsp-morphism.
	    f)
	   ((typep f 'id-qvsp-morphism)
	    (let* ((n (dim (sou f)))
		   (ans-m (make-matrix n n)))
	      (declare (fixnum n))
	      (dotimes (j n)
		(declare (fixnum j))
		(matrix-set ans-m j j c))
	      (make-qvsp-morphism (sou f) (tar f) ans-m)))
	   (t
	    (make-qvsp-morphism
	     (sou f)
	     (tar f)
	     (scmult-matrix c (mat f))))))
    (cochain-map
     (cond ((zerop c)
	    (make-zero-morphism (sou f) (tar f)))
	;; If c = 1, we do want to make a new morphism, since
	;; I haven't figured out yet how mutable those (mat f)
	;; hash-tables are going to be.
	   (t
	    (map-cochain-map #'(lambda (ff) (scmult c ff)) f))))))

(defun copy-hash-table (h)
  "Argument is a hash-table h.  Returns a new hash-table with the same
equality test and the same key-value pairs."
  (let ((ans (make-hash-table
               :test (hash-table-test h)
               :size (hash-table-size h))))
    (maphash #'(lambda (key val)
                 (setf (gethash key ans) val))
             h)
    ans))

(defun inclusion-of-subspaces (f g)
  "Arguments are f and g.  Here f : A --> C and g : B --> C, with g
injective, and with the proviso that the image subspace of f is
contained in the image subspace of g.  The function returns the unique
morphism h : A --> B such that gh = f.
   Currently, this will fail if the image of f, as a Z-lattice, is not
contained in the image of g as a Z-lattice--for then h would have to
have entries in Q.  In this case, use choice-of-pullback-up-to-scalar
[see doc.]"
  (cond ((and (typep f 'zero-qvsp-morphism)
	      (typep g 'qvsp-morphism))
	 (make-zero-morphism (zero-qvsp-morphism-sou f)
			     (sou g)))
	((and (typep f 'qvsp-morphism)
	      (typep g 'id-qvsp-morphism))
	 (copy-qvsp-morphism f (sou f) (id-qvsp-morphism-sou g)))
	((and (typep f 'qvsp-morphism)
	      (typep g 'qvsp-morphism))
	 (assert (composition-zerop (coker g) f)
		 () "The image of f does not lie in the image of g.")
	 (assert (injective-p g)
		 () "g is not injective.")
	 ;; Now we do the job.  Remark: the ker/coker/im/coim slots of f are
	 ;; never needed.  Only the cokerside slots of g are needed.
	 ;;   The present algorithm is correct whenever pullback-to-ker is
	 ;; correctly defined, as is shown by a simple diagram chase.  In
	 ;; particular, we don't have to use the specific implementation of
	 ;; pullback-to-ker given below.
	 ;;   Better method(??)--just solve the linear system of equations 
	 ;; (mat g) * X = (mat f).  Will this involve integer blow-up?  If
	 ;; so, the current LLL methods may actually be better for memory,
	 ;; though perhaps slower.
	 (compose-two (inverse (pullback-to-ker g (coker g)))
		      (pullback-to-ker f (coker g))))
	((and (typep f 'cochain-map)
	      (typep g 'cochain-map))
	 (make-cochain-map
	  :sou (sou f)
	  :tar (sou g)
	  :mat (with-eql-ans
		(map-mat #'(lambda (j ff)
			     (setf (gethash j ans)
				   (inclusion-of-subspaces
				    ff (term j g))))
			 f))))
	(t (error "Fell through end."))))

(defun projection-of-quotients (f g)
  "Arguments are f and g.  Here f : C --> A and g : C --> B, with g
surjective, and with (compose f (ker g)) = 0.  The function returns
the unique morphism h : B --> A such that hg = f.
   Currently, this will fail if h would have to have entries in Q.
   This is the dual of inclusion-of-subspaces [see doc.]"
  (cond ((and (typep f 'zero-qvsp-morphism)
	      (typep g 'qvsp-morphism))
	 (make-zero-morphism (tar g) (zero-qvsp-morphism-tar f)))
	((and (typep f 'qvsp-morphism)
	      (typep g 'id-qvsp-morphism))
	 (copy-qvsp-morphism f (id-qvsp-morphism-tar g) (tar f)))
	((and (typep f 'qvsp-morphism)
	      (typep g 'qvsp-morphism))
	 (assert (composition-zerop f (ker g))
		 () "The arrow would not be well-defined.")
	 (assert (surjective-p g)
		 () "g is not surjective.")
	 ;; Now we do the job.  Remark: the ker/coker/im/coim slots of f are
	 ;; never needed.  Only the kerside slots of g are needed.
	 ;; See comments on correctness and efficiency of the algorithm in
	 ;; inclusion-of-subspaces.
	 (compose-two
	  (pushforward-to-coker f (ker g))
	  (inverse (pushforward-to-coker g (ker g)))))
	((and (typep f 'cochain-map)
	      (typep g 'cochain-map))
	 (make-cochain-map
	  :sou (tar g)
	  :tar (tar f)
	  :mat (with-eql-ans
		(map-mat #'(lambda (j ff)
			     (setf (gethash j ans)
				   (projection-of-quotients
				    ff (term j g))))
			 f))))
	(t (error "Fell through end."))))

;;; There should be a function inclusion-of-subspace-into-kernel, for
;;; use like (inclusion-of-subspaces f g) when g is the kernel of some
;;; other map gg.  Since (ker (coker (ker gg))) may be [and currently
;;; is] taken to be EQUAL (eq) to (ker gg), the inclusion of subspaces
;;; is merely (compose (ker-retraction gg) f).  Of course, for
;;; idiot-proofing, we should also check that (compose (coker g)
;;; f) is zero, as we currently do check.

;;; I used to have functions split-surjection and split-injection
;;; here.  E.g., if f was a surjection, (split-surjection f) would
;;; give a splitting of f.  I removed this, however, because the
;;; splitting might not be defined over Z.  Example: let f be
;;; given by any square matrix of det > 1.
;;;    For the injections provided by ker, and the surjections
;;; provided by coker, the functions ker-retraction and coker-section do
;;; this job already.
;;;    The algorithm for doing general splittings over Q is on p. 14
;;; of my 9/93 notes.
;;;    The following is a "better" version of what I was aiming for
;;; with the functions split-<...>.

(defun image-and-complement (f)
  "The argument is a morphism f.  The result is a pair (g . h) s.t. g
is the image morphism of f, and h is an injective morphism whose image
subspace is the orthocomplement in (tar f) of the image subspace of f
[w.r.t. the standard dot product on the columns]."
  (cons (im f) (dual (coker f))))

(defun choice-of-pullback-up-to-scalar (f g)
  "Arguments f, g.  These are maps f : A --> T and g : B --> T.  The
image of f should lie in the image of g.  We choose an `arbitrary' map
h : A --> B such that gh = c f for some positive integer c, and return
h."
  (cond ((and (typep f 'qvsp-morphism)
	      (typep g 'qvsp-morphism))
	 (cond ((typep f 'zero-qvsp-morphism)
		(make-zero-morphism (zero-qvsp-morphism-sou f)
				    (sou g)))
	       ((typep g 'zero-qvsp-morphism)
		;; Here f is not 0.
		(error
"The image of f doesn't lie in the image of g, because g is a zero
morphism and f is not."))
	       ((typep g 'id-qvsp-morphism)
                (copy-qvsp-morphism f (sou f) (sou g)))
	       (t
		(assert (eq (tar f) (tar g)) ()
			"The two maps must have the same target.")
		;; Is the next check necessary?
		(assert (composition-zerop (coker g) f)
			() "The image of f does not lie in the image of g.")
		;; This method is inefficient.  Besides, it does a
		;; full row/col-reduction on A, when this has already
		;; been done (by LLL) on at least the cokerside!  This
		;; method should be replaced with something from Cohen
		;; Ch. 2.  This method is not efficient for sparse's,
		;; either.
		(let* ((A (mat g))
		       (B (mat f))
		       (n (matrix-dim B 1))
		       (m (matrix-dim A 1))
		       (A0 (ismith A))
		       (D (matrix+-d A0))
		       (PB (mult (matrix+-p A0) B))
		       (Y (make-matrix m n))
		       (min-D-index (min m (matrix-dim D 0)))
		       (max-Dii-and-index
			(dotimes (i min-D-index
				    ;; if you get here, no Dii was zero
				    (let ((i0 (1- min-D-index)))
				      (cons (matrix-ref D i0 i0)
					    min-D-index)))
			  (declare (fixnum i))
			  (when (zerop (matrix-ref D i i))
				(return
				 (cons (matrix-ref D (1- i) (1- i))
				       i)))))
		       (max-pivot (car max-Dii-and-index))
		       (max-pivot-index (cdr max-Dii-and-index)))
		  (declare (type matrix A B D PB Y)
			   (type matrix+ A0)
			   (fixnum min-D-index m n max-pivot-index)
			   (integer max-pivot))
		  ;; We must solve DY = PB.  The final answer will be
		  ;; QY.
		  (do ((i (1- max-pivot-index) (1- i)))
		      ((< i 0))
		    (declare (fixnum i))
		    (let ((recip-pivot (floor max-pivot
					      (the integer
						   (matrix-ref D i i)))))
		      (declare (integer recip-pivot))
		      (dotimes (j n)
			(declare (fixnum j))
			;; Slow, because it iterates over i and j
			;; [i.e. is not sparse].
			(matrix-set Y i j
				    (* (the integer (matrix-ref PB i j))
				       recip-pivot)))))
#|
    (let* ((QY (mult (q A0) Y))
	   (ans (make-qvsp-morphism (sou f) (sou g) QY))
	   ;; Let's check the work.  But remove this soon, once it's
	   ;; debugged, since it's slow.
	   (should-equal-f (compose g ans)))
      (declare (type matrix QY))
      (dotimes (i (dim (tar f)))
	(declare (fixnum i))
	(dotimes (j n)
          (declare (fixnum j))
	  (unless (= (the integer (matrix-ref (mat should-equal-f) i j))
		     (* max-pivot
			(the integer (matrix-ref (mat f) i j))))
	    (error "The answer is not correct."))))
      ;; Final result:
      ans)))
|#
		  ;; Final result:
		  (make-qvsp-morphism (sou f)
				      (sou g)
				      (mult (matrix+-q A0) Y))))))))

;;; The main det function is in csparsez.lisp.

(proclaim '(ftype (function (t) integer) det-qvsp-morphism))

(defun det-qvsp-morphism (x)
  (declare (type qvsp-morphism x))
  "Input: x, which is a qvsp-morphism.  Output: the determinant of x.
Do not call this function directly; it is appendage to the function
det [see doc.]"
  (unless (= (dim (sou x)) (dim (tar x)))
	  (error
	   "I was asked to take the determinant of a qvsp-morphism, but its
source and target have different dimensions."))
  (unless (eq (sou x) (tar x))
	  (break
	   "I'm being asked to take the determinant of the qvsp-morphism
~S
The source and target have equal dimensions, but they are not equal.
If you don't think this is a problem, continue out of this breakpoint,
and I'll go on as if nothing had happened." x))
  (etypecase x
    (zero-qvsp-morphism
     0)
    (id-qvsp-morphism
     1)
    (qm
     (det (mat x)))))

(proclaim '(ftype (function (t) integer)
		  math-trace))

(defun math-trace (x)
  "Input: x, which is a matrix.  Output: the trace of x.
Alternatively, x can be a qvsp-morphism, and the function will return
the trace of (mat x).  We can't give the function the name `trace'
because that's already taken by an important debugging function."
  (etypecase x
    (matrix
     (let ((m (matrix-dim x 0))
	   (ans 0))
       (declare (type nnfixnum m)
		(integer ans))
       (assert (= m (matrix-dim x 1)) ()
	       "Can't take the trace of a non-square matrix.")
       (dotimes (i m
		   ans)
         (declare (type nnfixnum i))
	 (incf ans (matrix-ref x i i)))))
    (zero-qvsp-morphism
     0)
    (id-qvsp-morphism
     (matrix-dim x 0))
    (qm
     (math-trace (mat x)))))

;;;    THE PRESENT PARAGRAPH HAS BEEN SUPERSEDED
;;; In the next function, we can't store things like (ker (ker f))
;;; --too many 0 or id morphisms taking up space.  But we do want to
;;; arrange for (ker (coim f)) to be identical to (ker f), etc.  The
;;; current rule is that we store a morphism iff computing it twice
;;; would create two source [resp. target] spaces that should be eq
;;; but aren't.

;;; It is probably better to store all the explicit 0's and id's.
;;; E.g., if g is (im f), it might take a complicated LLL computation
;;; to show that (ker g) is zero, whereas we actually know it
;;; already.  The function fill-out-as-injection, resp. -surjection,
;;; have the job of storing these 0's and id's.

(defmacro setf-unless-not-qm (form val)
  "For use with the fill-out-[co]kerside-... family."
  (with-gensyms (g)
    `(let ((,g ,(cadr form)))
       (when (typep ,g 'qm)
	 (setf (,(car form) ,g) ,val)))))

(defun fill-out-kerside-qm (f)
  (declare (type qm f))
  "When f is a qvsp-morphism [of type qm, technically], this fills out
the ker and ker-retraction slots of f, together with the coker and
coker-section slots of (ker f).  See the documentation on
qvsp-morphism for what these maps are.  For details on how they are
computed, see the documentation on kernel-etc-LLL."
  ;; [If you ever destructively modify (qm-mat f) for a qm f, you will
  ;; have to set the ker, coker, ker-retraction and coker-section
  ;; slots to nil.  Currently the program is designed so that such
  ;; destructive modifications are never made to qvsp-morphisms.
  (let* ((ma (qm-mat f))
	 (m (matrix-dim ma 0))
	 (n (matrix-dim ma 1)))
    (declare (type matrix ma)
	     (type nnfixnum m n))
    (destructuring-bind (left-H right-H top-Hinv bottom-Hinv kerdim)
			(kernel-etc-LLL ; comput'lly, the main step
			 (copy-of-matrix ma 0 m 0 n))
      (declare (type (or null matrix)
		     left-H right-H top-Hinv bottom-Hinv)
	       (type nnfixnum kerdim))
      (let ((r (- n kerdim))) ; = rank f
	(declare (type nnfixnum r))
	;; Quick check for surjectivity.
	(when (= r m)
	  (fill-out-as-surjection f))
	;; If r = 0, the CLOS version changes the type of f to
	;; zero-qvsp-morphism at this point.  We can't do this with
	;; structures.
	(if (zerop kerdim)
	    ;; Here f is injective.
	    (fill-out-as-injection f)
	  (let ((ker-sp (make-qvsp kerdim))
		(coim-sp (make-qvsp r)))
	    (declare (type qvsp ker-sp coim-sp))
	    (setf (qm-ker f)
	     (fill-out-as-injection
	      (make-qvsp-morphism ker-sp (qm-sou f) left-H)))
	    (setf (qm-ker-retraction f)
	     (fill-out-as-surjection
	      (make-qvsp-morphism (qm-sou f) ker-sp top-Hinv)))
	    (setf (qm-coker (qm-ker f))
	     (fill-out-as-surjection
	      (make-qvsp-morphism (qm-sou f) coim-sp bottom-Hinv)))
	    (setf (qm-coker-section (qm-ker f))
	     (fill-out-as-injection
	      (make-qvsp-morphism coim-sp (qm-sou f) right-H)))
	    (setf-unless-not-qm
	     (qm-ker (qm-ker-retraction f))
	     (qm-coker-section (qm-ker f)))
	    (setf-unless-not-qm
	     (qm-ker-retraction (qm-ker-retraction f))
	     (qm-coker (qm-ker f)))
	    (setf-unless-not-qm
	     (qm-ker (qm-coker (qm-ker f)))
	     (qm-ker f))
	    (setf-unless-not-qm
	     (qm-ker-retraction (qm-coker (qm-ker f)))
	     (qm-ker-retraction f))
	    (setf-unless-not-qm
	     (qm-coker (qm-coker-section (qm-ker f)))
	     (qm-ker-retraction f))
	    (setf-unless-not-qm
	     (qm-coker-section (qm-coker-section (qm-ker f)))
	     (qm-ker f))
	    ;; Return the filled-out f as the final result.
	    f))))))

(defun fill-out-cokerside-qm (f)
  (declare (type qm f))
  "When f is a qvsp-morphism [of type qm, technically], this fills out
the coker and coker-section slots of f, together with the ker and
ker-retraction slots of (coker f).  See the documentation on
qvsp-morphism for what these maps are.  For details on how they are
computed, see the documentation on kernel-etc-LLL."
  ;; See comments on fill-out-kerside-qm on not setf's the mat slot
  ;; after it's first set.
  (destructuring-bind (left-H right-H top-Hinv bottom-Hinv cokerdim)
		      (kernel-etc-LLL (transpose (mat f)))
    (declare (type (or null matrix)
		   left-H right-H top-Hinv bottom-Hinv)
	     (fixnum cokerdim))
    (let ((r (- (dim (qm-tar f)) cokerdim))) ; = rank f
      (declare (type nnfixnum r))
      ;; Quick check for injectivity.
      (when (= r (dim (qm-sou f)))
	(fill-out-as-injection f))
      ;; If r = 0, the CLOS version changes the type of f to
      ;; zero-qvsp-morphism at this point.  We can't do this with
      ;; structures.
      (if (zerop cokerdim)
	  ;; Here f is surjective.
	  (fill-out-as-surjection f)
	(let ((coker-sp (make-qvsp cokerdim))
	      (im-sp (make-qvsp r)))
	  (declare (type qvsp coker-sp im-sp))
	  (setf (qm-coker f)
	   (fill-out-as-surjection
	    (make-qvsp-morphism (qm-tar f) coker-sp
				(transpose left-H))))
	  (setf (qm-coker-section f)
	   (fill-out-as-injection
	    (make-qvsp-morphism coker-sp (qm-tar f)
				(transpose top-Hinv))))
	  (setf (qm-ker (qm-coker f))
	   (fill-out-as-injection
	    (make-qvsp-morphism im-sp (qm-tar f)
				(transpose bottom-Hinv))))
	  (setf (qm-ker-retraction (qm-coker f))
	   (fill-out-as-surjection
	    (make-qvsp-morphism (qm-tar f) im-sp
				(transpose right-H))))
	  (setf-unless-not-qm
	   (qm-coker (qm-coker-section f))
	   (qm-ker-retraction (qm-coker f)))
	  (setf-unless-not-qm
	   (qm-coker-section (qm-coker-section f))
	   (qm-ker (qm-coker f)))
	  (setf-unless-not-qm
	   (qm-coker (qm-ker (qm-coker f)))
	   (qm-coker f))
	  (setf-unless-not-qm
	   (qm-coker-section (qm-ker (qm-coker f)))
	   (qm-coker-section f))
	  (setf-unless-not-qm
	   (qm-ker (qm-ker-retraction (qm-coker f)))
	   (qm-coker-section f))
	  (setf-unless-not-qm
	   (qm-ker-retraction (qm-ker-retraction (qm-coker f)))
	   (qm-coker f))
	  ;; Return the filled-out f as the final result.
	  f)))))

(defun fill-out-as-injection (f)
  (declare (type qm f))
  "Input is a qvsp-morphism f [technically, a qm] which is injective.
This function sets its kernel to the zero morphism, and makes the other
appropriate changes on the `kerside'.  The value returned is the
filled-out f."
  (setf (qm-ker f)
	(make-zero-qvsp-morphism :sou *zero-qvsp* :tar (qm-sou f)))
  (setf (qm-ker-retraction f)
	(make-zero-qvsp-morphism :sou (qm-sou f) :tar *zero-qvsp*))
  (setf-unless-not-qm
   (qm-coker (qm-ker f))
	(make-id-qvsp-morphism :sou (qm-sou f) :tar (qm-sou f)))
  (setf-unless-not-qm
   (qm-coker-section (qm-ker f))
	(make-id-qvsp-morphism :sou (qm-sou f) :tar (qm-sou f)))
  f)

(defun fill-out-as-surjection (f)
  (declare (type qm f))
  "Input is a qvsp-morphism f [technically, a qm] which is surjective.
This function sets its cokernel to the zero morphism, and makes the
other appropriate changes on the `cokerside'.  The value returned is
the filled-out f."
  (setf (qm-coker f)
	(make-zero-qvsp-morphism :sou (qm-tar f) :tar *zero-qvsp*))
  (setf (qm-coker-section f)
	(make-zero-qvsp-morphism :sou *zero-qvsp* :tar (qm-tar f)))
  (setf-unless-not-qm
   (qm-ker (qm-coker f))
	(make-id-qvsp-morphism :sou (qm-tar f) :tar (qm-tar f)))
  (setf-unless-not-qm
   (qm-ker-retraction (qm-coker f))
	(make-id-qvsp-morphism :sou (qm-tar f) :tar (qm-tar f)))
  f)

(defun fill-out-kerside-cochain-map (f)
  (declare (type cochain-map f))
  "When f is a cochain-map, this fills out the ker and ker-retraction
slots of f, together with the coker and coker-section slots of (ker f)."
  (let ((ker-cx
	 (let ((o (empty-hash-table))
	       (m (empty-hash-table)))
	   (map-objects #'(lambda (i v)
			    (declare (ignore v))
			    (setf (gethash i o)
				  (sou (ker (term f i)))))
			(sou f))
	   (maphash #'(lambda (i k)
			(declare (ignore k))
			(setf (gethash i m)
			      (pullback-to-ker
			       (compose (d i (sou f))
					(ker (term f i)))
			       (term f (1+ i)))))
		    o)
	   (make-cochain-cx :objects o :maps m))))
    (setf (cochain-map-ker f)
	  (make-cochain-map
	   :sou ker-cx
	   :tar (sou f)
	   :mat (with-eql-ans
		 (map-objects #'(lambda (i v)
				  (declare (ignore v))
				  (setf (gethash i ans)
					(ker (term f i))))
			      (sou f)))))
    (setf (cochain-map-ker-retraction f)
	  (make-cochain-map
	   :sou (sou f)
	   :tar ker-cx
	   :mat (with-eql-ans
		 (map-objects #'(lambda (i v)
				  (declare (ignore v))
				  (setf (gethash i ans)
					(ker-retraction (term f
							      i))))
			      (sou f)))))
    (let ((coim-cx
	   (let ((o (empty-hash-table))
		 (m (empty-hash-table)))
	     (map-objects #'(lambda (i v)
			      (declare (ignore v))
			      (setf (gethash i o)
				    (tar (coker
					  (term (cochain-map-ker f)
						i)))))
			  (sou f))
	     (maphash #'(lambda (i v)
			  (declare (ignore v))
			  (setf (gethash i m)
				(pushforward-to-coker
				 (compose
				  (coker
				   (term (cochain-map-ker f) (1+ i)))
				  (d i (sou f)))
				 (term (cochain-map-ker f) i))))
		      o)
	     (make-cochain-cx :objects o :maps m))))
      (setf (cochain-map-coker (cochain-map-ker f))
	    (make-cochain-map
	     :sou (sou f)
	     :tar coim-cx
	     :mat (with-eql-ans
		   (map-objects #'(lambda (i v)
				    (declare (ignore v))
				    (setf (gethash i ans)
					  (coker
					   (term (cochain-map-ker f)
						 i))))
				(sou f)))))
      (setf (cochain-map-coker-section (cochain-map-ker f))
	    (make-cochain-map
	     :sou coim-cx
	     :tar (sou f)
	     :mat (with-eql-ans
		   (map-objects #'(lambda (i v)
				    (declare (ignore v))
				    (setf (gethash i ans)
					  (coker-section
					   (term (cochain-map-ker f)
						 i))))
				(sou f)))))
      (setf (cochain-map-ker (cochain-map-ker-retraction f))
       (cochain-map-coker-section (cochain-map-ker f)))
      (setf (cochain-map-ker-retraction (cochain-map-ker-retraction f))
       (cochain-map-coker (cochain-map-ker f)))
      (setf (cochain-map-ker (cochain-map-coker (cochain-map-ker f)))
       (cochain-map-ker f))
      (setf (cochain-map-ker-retraction (cochain-map-coker
					 (cochain-map-ker f)))
       (cochain-map-ker-retraction f))
      (setf (cochain-map-coker (cochain-map-coker-section (cochain-map-ker f)))
       (cochain-map-ker-retraction f))
      (setf (cochain-map-coker-section (cochain-map-coker-section
					(cochain-map-ker f)))
       (cochain-map-ker f))
      ;; Return the filled-out f as the final result.
      f)))

(defun fill-out-cokerside-cochain-map (f)
  (declare (type cochain-map f))
  "When f is a cochain-map, this fills out the coker and coker-section
slots of f, together with the ker and ker-retraction slots of (coker f)."
  (let ((coker-cx
	 (let ((o (empty-hash-table))
	       (m (empty-hash-table)))
	   (map-objects #'(lambda (i v)
			    (declare (ignore v))
			    (setf (gethash i o)
				  (tar (coker (term f i)))))
			(tar f))
	   (maphash #'(lambda (i k)
			(declare (ignore k))
			(setf (gethash i m)
			      (pushforward-to-coker
			       (compose (coker (term f (1+ i)))
					(d i (tar f)))
			       (term f i))))
		    o)
	   (make-cochain-cx :objects o :maps m))))
    (setf (cochain-map-coker f)
	  (make-cochain-map
	   :sou (tar f)
	   :tar coker-cx
	   :mat (with-eql-ans
		 (map-objects #'(lambda (i v)
				  (declare (ignore v))
				  (setf (gethash i ans)
					(coker (term f i))))
			      (tar f)))))
    (setf (cochain-map-coker-section f)
	  (make-cochain-map
	   :sou coker-cx
	   :tar (tar f)
	   :mat (with-eql-ans
		 (map-objects #'(lambda (i v)
				  (declare (ignore v))
				  (setf (gethash i ans)
					(coker-section (term f i))))
			      (tar f)))))
    (let ((im-cx
	   (let ((o (empty-hash-table))
		 (m (empty-hash-table)))
	     (map-objects #'(lambda (i v)
			      (declare (ignore v))
			      (setf (gethash i o)
				    (sou (ker
					  (term (cochain-map-coker f)
						i)))))
			  (tar f))
	     (maphash #'(lambda (i v)
			  (declare (ignore v))
			  (setf (gethash i m)
				(pullback-to-ker
				 (compose
				  (d i (tar f))
				  (ker
				   (term (cochain-map-coker f) i)))
				 (term (cochain-map-coker f) (1+ i)))))
		      o)
	     (make-cochain-cx :objects o :maps m))))
      (setf (cochain-map-ker (cochain-map-coker f))
	    (make-cochain-map
	     :sou im-cx
	     :tar (tar f)
	     :mat (with-eql-ans
		   (map-objects #'(lambda (i v)
				    (declare (ignore v))
				    (setf (gethash i ans)
					  (ker
					   (term (cochain-map-coker f)
						 i))))
				(tar f)))))
      (setf (cochain-map-ker-retraction (cochain-map-coker f))
	    (make-cochain-map
	     :sou (tar f)
	     :tar im-cx
	     :mat (with-eql-ans
		   (map-objects #'(lambda (i v)
				    (declare (ignore v))
				    (setf (gethash i ans)
					  (ker-retraction
					   (term (cochain-map-coker f)
						 i))))
				(tar f)))))
      (setf (cochain-map-coker (cochain-map-coker-section f))
       (cochain-map-ker-retraction (cochain-map-coker f)))
      (setf (cochain-map-coker-section (cochain-map-coker-section f))
       (cochain-map-ker (cochain-map-coker f)))
      (setf (cochain-map-coker (cochain-map-ker (cochain-map-coker f)))
       (cochain-map-coker f))
      (setf (cochain-map-coker-section (cochain-map-ker (cochain-map-coker f)))
       (cochain-map-coker-section f))
      (setf (cochain-map-ker (cochain-map-ker-retraction
			      (cochain-map-coker f)))
       (cochain-map-coker-section f))
      (setf (cochain-map-ker-retraction (cochain-map-ker-retraction
					 (cochain-map-coker f)))
       (cochain-map-coker f))
      ;; Return the filled-out f as the final result.
      f)))

(defun rank (f)
  "Argument is f.  Currently f can be a qvsp-morphism or a matrix+.  The
function returns the rank of f."
  (etypecase f
    (matrix+
     (matrix+-rank f))
    (zero-qvsp-morphism
     0)
    (id-qvsp-morphism
     (dim (id-qvsp-morphism-sou f)))
    (qm
     (cond ((not (null (qm-ker f)))
	    ;; i.e. if ker and therefore coim has been computed
	    (dim (tar (coim f))))
	   ((not (null (qm-coker f)))
	    ;; i.e. if coker and therefore im has been computed
	    (dim (sou (im f))))
	   ;; If neither has been computed, let's compute the one
	   ;; whose change-of-basis matrices will be smallest.
	   ((<= (the fixnum (dim (qm-tar f)))
		(the fixnum (dim (qm-sou f))))
	    ;; Forces a call to LLL to fill out f on the kerside.
	    (dim (sou (im f))))
	   (t
	    ;; Forces a call to LLL to fill out f on the cokerside.
	    (dim (tar (coim f))))))))

(defun make-zero-morphism (so ta &key name)
  "Makes a zero morphism between its two arguments, which are assumed
to be objects in the same category."
  (with-names
  (cond ((and (typep so 'qvsp)
	      (typep ta 'qvsp))
	 (make-zero-qvsp-morphism :sou so :tar ta))
	((and (typep so 'cochain-cx)
	      (typep ta 'cochain-cx))
	 (make-cochain-map
	  :sou so :tar ta
	  :mat (empty-hash-table)))
	(t
	 (error "Can't make a zero morphism between objects of type
~S and ~S." (type-of so) (type-of ta))))))

(defun make-id-morphism (so ta &key name)
  "Makes an identity morphism between its two arguments, which are
assumed to be objects in the same category.  See the documentation on
id-qvsp-morphism."
  (with-names
  (cond ((and (typep so 'qvsp)
	      (typep ta 'qvsp))
	 (assert (= (dim so) (dim ta)) ()
"You can't make an id-qvsp-morphism between spaces of different dimension.")
	 (make-id-qvsp-morphism :sou so :tar ta))
	((and (typep so 'cochain-cx)
	      (typep ta 'cochain-cx))
	 (make-cochain-map
	  :sou so :tar ta
	  :mat (with-eql-ans
		;; We could define this just by iterating over the
		;; objects of so.  The reason for using indices from
		;; both sou and tar is error-checking: we want to be
		;; sure there is an object of degree i in so iff there
		;; is one of degree i in ta [and the subcase of
		;; make-id-morphism with check to make sure these are,
		;; say, of the same dim].
		(let ((inds '()))
		  (map-objects #'(lambda (i v)
				   (declare (ignore v))
				   (push i inds))
			       so)
		  (map-objects #'(lambda (i v)
				   (declare (ignore v))
				   (push i inds))
			       ta)
		  (map nil #'(lambda (i)
			       (setf (gethash i ans)
				     (make-id-morphism
				      (term so i)
				      (term ta i))))
		       (remove-duplicates inds :test #'=))))))
	(t (error "Can't make an identity morphism between
~S
~S"
		  so ta)))))

(defun coim-to-im (f)
  "The argument f is a morphism.  Value returned is the canonical
isomorphism from its coimage object to its image object.    For an
overview of these ideas, see the documentation on qvsp-morphism."
  (etypecase f
    (qvsp-morphism
     (pushforward-to-coker (pullback-to-ker f (coker f)) (ker f)))
    (cochain-map
     (make-cochain-map
      :sou (tar (coim f))
      :tar (sou (im f))
      :mat (with-eql-ans
	    (map-objects #'(lambda (i v)
			     (declare (ignore v))
			     (setf (gethash i ans)
				   (coim-to-im (term f i))))
			 (tar (coim f))))))))

;;; It is not immediately obvious that the next two methods work.  For
;;; while (compose (ker-retraction f) (ker f)) is by definition the
;;; identity on the ker-object, (compose (ker f) (ker-retraction f))
;;; is not the identity on (source f).  So why does (compose (ker f)
;;; (ker-retraction f) g) equal g?
;;;    Nevertheless, the methods are correct, at least with ker and
;;; ker-retraction constructed the way we do.  Let A be the matrix of
;;; f, and B that of g.  Let H be an invertible matrix s.t. AH = (0 | M),
;;; where M has lin indep columns.  We have been letting left(H) be
;;; the matrix for ker, top(Hinv) be the matrix for ker-retraction,
;;; etc.  Let B be the matrix for g.  Then one of the givens is AB =
;;; 0.  Let P ("pullback") be   top(Hinv)B  .  What we have to prove
;;; is that left(H)P = B.
;;;    Proof: we want to prove
;;;      left(H) top(Hinv) B = B
;;; Let I/0 be the matrix with I in the upper-left corner and 0's
;;; elsewhere.  Multiply both sides of the equation on the left by
;;; Hinv, to get
;;;      I/0 top(Hinv) B = Hinv B.
;;; Observe that I/0 top(X) = X for any X.  So the previous equation
;;; is equivalent to
;;;      top(Hinv) B = Hinv B.
;;; Rewrite top(Hinv) B as top(Hinv) H Hinv B, to get the equiv form
;;;      I/0 Hinv B = Hinv B
;;; This holds iff Hinv B is of the form
;;;      ( * )
;;;      ( 0 )
;;; We must prove that Hinv B has the latter form.  Write it as
;;;      ( C )
;;;      ( D )
;;; But 0 = AB = (AH)(Hinv B) = (0 | M) (Hinv B) = 0 C + M D.
;;; Hence 0 = M D.  Since M has lin indep cols, it is
;;; left-cancellable.  Hence D = 0.  QED.
;;;    The proof for pushforward-to-coker is dual.

(defun pullback-to-ker (g f)
"In the call (pullback-to-ker g f), the arguments f, g are morphisms,
with fg = 0 .  Output is a morphism h from (sou g) to (sou (ker f))
such that (compose (ker f) h) is the same as g.  Mnemonic: `pull g back
to the kernel of f.'
   The program does not check whether fg is zero."
  (compose (ker-retraction f) g))

(defun pushforward-to-coker (g f)
  "In the call (pushforward-to-coker g f), the arguments f, g are
morphisms, with gf = 0 .  Output is a morphism h from (tar (coker f))
to (tar g) such that (compose h (coker f)) is the same as g.
Mnemonic: `push g forward to the cokernel of f.'
   The program does not check whether gf is zero."
  (compose g (coker-section f)))

(defun intersection-of-images (f g)
  "Arguments f,g, which are two morphisms with the same target T.
Output is an injective morphism from a new object I_0 to T whose image
subspace is exactly the intersection of the image subspaces of f and
g."
  (assert (eq (tar f) (tar g)) ()
	  "The maps don't have the same target.")
  (let ((h (compose (coker g) f)))
    (compose f (ker h))))
  ;; In future: let this interchange the roles of f and g if that's
  ;; simpler?

(defun term (x i)
  "This gives the i-th term of x, where x is a graded-object,
partially-known-chain-cx or graded-morphism.  If x is an object, the
value returned is the i-th piece of x.  If x is a morphism, the value
returned is the morphism of i-th pieces.
   Using either (term x i) or (term i x) should work, so you don't
have to remember whether to put the i first or second. :-)"
  (if (numberp x)
      (if (numberp i) ; infinite loop danger!
	  (error
	   "I can't take the term of a number.~&You asked for (term ~D ~D)."
	   x i)
	(term i x)) ; default is object first, index second.
    (locally (declare (type nnfixnum i))
      (etypecase x
	(cochain-cx
	 (let ((ans (gethash i (cochain-cx-objects x))))
	   (if (not (null ans))
	       ans
	     (zero-object-of-type-of
	      (get-one-hash-value (cochain-cx-objects x))))))
	(cochain-map
	 (let ((ans (gethash i (cochain-map-mat x))))
	   (if (not (null ans))
	       ans
	     (make-zero-morphism
	      (term (sou x) i)
	      (term (tar x) i)))))
	(simple-vector
	 (svref x i))))))

(defun add-list (lyst)
  (declare (list lyst))
  "Argument is a list of qvsp-morphisms, all with the same source and
the same target.  The function returns their sum.  [Exception: the function
returns nil if the sum is the zero-morphism.]"
  (let ((lyst0 (remove nil lyst)))
    (declare (list lyst0))
    (when (null lyst0)
	  ;; nil signals the make-p.-k. family that the sum is a
	  ;; zero-morphism
	  (return-from add-list nil))
    (assert (= 1 (length (the list (remove-duplicates
				    (mapcar #'sou lyst0))))) ()
      "The members of the argument list do not all have the same source.")
    (assert (= 1 (length (the list (remove-duplicates
				    (mapcar #'tar lyst0))))) ()
      "The members of the argument list do not all have the same target.")
    (let ((so (sou (car lyst0)))
	  (ta (tar (car lyst0)))
	  ;; The previous two must be qvsp's, not partially-known-qvsps.
	  (lyst1 (remove-if #'zero-morphism-p lyst0)))
      (declare (type qvsp so ta)
	       (list lyst1))
      (cond ((null lyst1)
	     ;; all were zero morphisms
	     nil)
	    ((= 1 (length lyst1))
	     ;; just one.  Return it.  [Not a copy of it...okay?]
	     (car lyst1))
	    (t
	     (let ((m (dim ta))
		   (n (dim so)))
	       (declare (fixnum m n))
	       (cond ((= m n)
		      ;; If m = n, there might be id-qvsp-morphisms on
		      ;; lyst1.  Let's sift them out.
		      (let ((lyst2 '())
			    (count-id 0))
			(declare (fixnum count-id))
			(dolist (f lyst1)
			  (typecase f
			    (id-qvsp-morphism
			     (incf count-id))
			    (t
			     (push f lyst2))))
			(make-qvsp-morphism
			 so ta
			 (add-matrix-list (mapcar #'mat lyst2)
					  m n count-id))))
		     (t
		      (make-qvsp-morphism
		       so ta
		       (add-matrix-list (mapcar #'mat lyst1)
					m n))))))))))

;;; Functions for directsum's.

(defun make-directsum-object (su ind &key name)
  (declare (type simple-vector su ind))
  "This function takes two arguments, which are non-empty vectors of
the same length.  The first vector contains the direct summands, and
the second contains some things which index the summands.  Currently,
the summands must be either (a) qvsp's or (b) cochain complexes where
we know how to apply make-directsum-object to each entry.
 Example: if you have defined objects U, V, W, and you want to form
their direct sum with indices 0, 1, 2 resp., call
   (make-directsum-object (vector U V W) (vector 0 1 2)).
 Note: when this function creates a directsum of cochain-cx's, the maps
in the answer are just the directsums of the maps of the inputs.  For
one example of how to construct more complicated directsum-cochain-cx's,
see the function directsum-inclusion-of-several."
  (with-names
  (assert (= (length su) (length ind)) ()
    "Arguments have different lengths.")
  (assert (> (length su) 0) ()
"You can't make a directsum with an empty vector of summands.")
  (let ((su-hash (make-hash-table
		  :test #'equal
		  :size (length su))))
    (map nil
	 #'(lambda (key val)
	     (setf (gethash key su-hash) val))
	 ind su)
    (typecase (svref su 0)
      (qvsp
       (make-directsum-qvsp
	:summands su-hash
	:indices ind
	:dim (reduce #'+ su :key #'dim)))
      (cochain-cx
       (let ((degs (let ((lyst '()))
		     (map nil
			  #'(lambda (s)
			      (maphash #'(lambda (k v)
					   (declare (ignore v))
					   (push k lyst))
				       (objects s)))
			  su)
		     (remove-duplicates lyst :test #'=)))
	     (o (empty-hash-table))
	     (m (empty-hash-table)))
	 (map nil #'(lambda (i)
		      (setf (gethash i o)
			    (make-directsum-object
			     (map 'vector #'(lambda (v)
					      (term v i))
				  su)
			     ind)))
	      degs)
	 (map nil #'(lambda (i)
		      (when (and (not (null (gethash i o)))
				 (not (null (gethash (1+ i) o))))
			(setf (gethash i m)
			  (make-directsum-morphism
			   (gethash i o)
			   (gethash (1+ i) o)
			   #'(lambda (indj indi)
			       (if (equal indj indi)
				   (d i (gethash indj su-hash))
				 0))))))
	      degs)
	 (make-directsum-cochain-cx
	  :objects o :maps m :summands su-hash :indices ind)))
      (t
       (error
	"I can only take directsums of qvsp's, or of graded-objects in which
each entry is something I know how to work with recursively."))))))

(defun make-directsum-morphism (x y f &key name)
  (declare (type directsum x y))
  "Arguments x, y, f.  Here x and y are directsum objects.  The former
is the source, the latter the target.  f is a function of two
variables, j and i, where j is an index for x and i is an index for y.
Calling the function f with inputs j and i should return a morphism
g_{ji} : x_j --> y_i between the corresponding summands.  The output
of (make-directsum-morphism ...) is a morphism (in the appropriate
category) from x to y which is the directsum of all the g_{ji}'s.
   There are two exceptions to the rule that f returns a morphism.  If
f returns 0, it means g_{ji} is a zero morphism.  If f returns 1, it
means g_{ji} is an identity morphism.
   If you want to treat x as a singleton space--i.e. f will be
returning maps [ _all of_ x ] ---> [i-th summand of y] --then use
make-directsum-morphism-from-singleton.  Ditto, dually, for
make-directsum-morphism-to-singleton.  The f variables for these two
functions take only the one relevant argument."
  (with-names
  ;;    Beware: a function that calls make-directsum-morphism should
  ;; not use the letter f as a variable.  When Lisp is inside the call
  ;; to make-directsum-morphism, it will not be able to see the value
  ;; of f in the calling program.
  ;;    Split into two versions for profiling.
  (cond ((and (typep x 'directsum-qvsp)
	      (typep y 'directsum-qvsp))
	 (make-directsum-morphism-qvsp x y f))
	((and (typep x 'directsum-cochain-cx)
	      (typep y 'directsum-cochain-cx))
         (let ((f-values (make-hash-table :test #'equal)))
	   (make-cochain-map
	    :sou x
	    :tar y
	    :mat (with-eql-ans
		  (map-objects
		   #'(lambda (i xi)
		       (assert (typep xi 'directsum)
			       () "Directsum structure lost in x's terms.")
		       (let ((yi (term y i)))
			 (cond ((typep yi 'directsum)
				(setf (gethash i ans)
				      (make-directsum-morphism
				       xi
				       yi
				       #'(lambda (jj ii)
					   (let ((g (gethash
						     (cons jj ii)
						     f-values)))
					     (when (null g)
					       (setq g
						 (setf (gethash
							(cons jj ii)
							f-values)
						       (funcall f jj ii))))
					     (if (or (equal g 0) (equal g 1))
						 g
					       (term g i)))))))
			       ((zero-object-p yi)
				nil)	; do nothing
			       (t
				(error "Directsum structure lost in y's terms.")))))
		   x)))))
	(t (error "No method in make-directsum-morphism.")))))

(defun make-directsum-morphism-qvsp (dirj diri f)
  (declare (type directsum-qvsp dirj diri))
  "The version of make-directsum-morphism for qvsp's."
  (let ((ans (make-matrix (dim diri) (dim dirj)))
	(numj (length (directsum-qvsp-indices dirj)))
	(numi (length (directsum-qvsp-indices diri))))
    (declare (fixnum numj numi))
    (do ((i0 0 (1+ i0))
	 (offseti 0 (+ offseti (dim (get-summand-indexed-by
				     diri
				     (svref (directsum-qvsp-indices diri)
					    i0))))))
	((= i0 numi)
	 ;; Here is the final answer.
	 (make-qvsp-morphism
	  dirj diri ans))
      (declare (type nnfixnum i0 offseti))
      (let ((indi (svref (directsum-qvsp-indices diri) i0)))
        (do ((j0 0 (1+ j0))
	     (offsetj 0 (+ offsetj (dim (get-summand-indexed-by
					 dirj
					 (svref
					  (directsum-qvsp-indices dirj)
					  j0))))))
	    ((= j0 numj))
	  (declare (type nnfixnum j0 offsetj))
	  (let ((indj (svref (directsum-qvsp-indices dirj) j0)))
	    (let ((z (funcall f indj indi)))
	      (cond ((or (eql z 0)
			 (typep z 'zero-qvsp-morphism))
		     ) ; for zero qvsp-morphisms, do nothing
		    ((or (eql z 1)
			 (typep z 'id-qvsp-morphism))
		     (copy-identity-matrix-to-matrix
		      (dim (get-summand-indexed-by dirj indj))
		      ans offseti offsetj))
		    ((typep z 'qvsp-morphism)
		     (copy-matrix-to-matrix (mat z) ans offseti offsetj))
		    (t
		     (error 
	  "I don't know how to handle this map between summands."))))))))))

(defun make-directsum-morphism-from-singleton (x y f &key name)
  (declare (type directsum y))
  "Arguments x, y, f.  Here y is a directsum object, and x is an object
in the same category.  x is the source and y is the target.  f is a
function of one variable i, where i is an index for y.  Calling the
function f with input i should return a morphism g_{i} : x --> y_i .
The output is the unique morphism (in the appropriate category) from x
to y which is the directsum of all the g_{i}'s.  The existence and
uniqueness of this morphism characterizes y as the **direct product**
of its summands.
   There are two exceptions to the rule that f returns a morphism.  If
f returns 0, it means g_{i} is a zero morphism.  If f returns 1, it
means g_{i} is an identity morphism."
  (with-names
  ;; See comment on f after make-directsum-morphism.
  (cond ((and (typep x 'qvsp)
	      (typep y 'directsum-qvsp))
	 (let* ((diri y)
		(dirj x)
		(ans (make-matrix (dim diri) (dim dirj)))
	        (numi (length (indices diri))))
	   (declare (type qvsp dirj)
		    (type directsum-qvsp diri)
		    (type nnfixnum numi))
	   (do ((i0 0 (1+ i0))
		(offseti 0 (+ offseti (dim (get-summand-indexed-by
					    diri
					    (svref (indices diri) i0))))))
	       ((= i0 numi)
		;; Here is the final answer.
		(make-qvsp-morphism
		 dirj diri ans))
	     (declare (type nnfixnum i0 offseti))
	     (let ((z (funcall f (svref (indices diri) i0))))
	       ;; z should be a qvsp-morphism, or the integer 0 or 1
	       (cond ((or (eql z 0)
			  (typep z 'zero-qvsp-morphism))
		      )			; for zero qvsp-morphisms, do nothing
		     ((or (eql z 1)
			  (typep z 'id-qvsp-morphism))
		      (copy-identity-matrix-to-matrix (dim dirj)
						      ans offseti 0))
		     ((typep z 'qvsp-morphism)
		      (copy-matrix-to-matrix (mat z) ans offseti 0))
		     (t
		      (error 
"I don't know how to handle this map between summands.")))))))
	((and (typep x 'cochain-cx)
	      (typep y 'directsum-cochain-cx))
	 (let ((f-values (make-hash-table :test #'equal)))
	 (make-cochain-map
	  :sou x
	  :tar y
	  :mat (with-eql-ans
		(map-objects
		 #'(lambda (i xi)
		     (let ((yi (term y i)))
		       (cond ((typep yi 'directsum)
			      (setf (gethash i ans)
				(make-directsum-morphism-from-singleton
				 xi
				 (term y i)
				 #'(lambda (ii)
				     (let ((g (gethash ii f-values)))
				       (when (null g)
					 (setq g
					   (setf (gethash ii f-values)
					     (funcall f ii))))
				       ;; Not = in next, since g
				       ;; may not be a number.
				       (if (or (equal g 0) (equal g 1))
					   g
					 (term g i)))))))
			     ((zero-object-p yi)
			      nil)	; do nothing
			     (t
			      (error "Recursive directsum structure lost.")))))
		 x)))))
	(t
	 (error "Can't handle this case.")))))

(defun make-directsum-morphism-to-singleton (x y f &key name)
  (declare (type directsum x))
  "Arguments x, y, f.  Here x is a directsum object, and y is an object
in the same category.  x is the source and y is the target.  f is a
function of one variable j, where j is an index for y.  Calling the
function f with input j should return a morphism g_{j} : x_j --> y .
The output is the unique morphism (in the appropriate category) from x
to y which is the directsum of all the g_{j}'s.  The existence and
uniqueness of this morphism characterizes y as the **coproduct** of
its summands.
   There are two exceptions to the rule that f returns a morphism.  If
f returns 0, it means g_{j} is a zero morphism.  If f returns 1, it
means g_{j} is an identity morphism."
  (with-names
  ;; See comment on f after make-directsum-morphism.
  (cond ((and (typep x 'directsum-qvsp)
	      (typep y 'qvsp))
	 (let* ((diri y)
		(dirj x)
		(ans (make-matrix (dim diri) (dim dirj)))
	        (numj (length (indices dirj))))
	   (declare (type directsum-qvsp dirj)
		    (type qvsp diri)
		    (type nnfixnum numj))
	   (do ((j0 0 (1+ j0))
		(offsetj 0 (+ offsetj (dim (get-summand-indexed-by
					    dirj
					    (svref (indices dirj) j0))))))
	       ((= j0 numj)
		;; Here is the final answer.
		(make-qvsp-morphism
		 dirj diri ans))
	     (declare (fixnum j0 offsetj))
	     (let ((z (funcall f (svref (indices dirj) j0))))
	       ;; z should be a qvsp-morphism, or the integer 0 or 1
	       (cond ((or (eql z 0)
			  (typep z 'zero-qvsp-morphism))
		      )			; for zero qvsp-morphisms, do nothing
		     ((or (eql z 1)
			  (typep z 'id-qvsp-morphism))
		      (copy-identity-matrix-to-matrix (dim diri)
						      ans 0 offsetj))
		     ((typep z 'qvsp-morphism)
		      (copy-matrix-to-matrix (mat z) ans 0 offsetj))
		     (t
		      (error 
"I don't know how to handle this map between summands.")))))))
	((and (typep x 'directsum-cochain-cx)
	      (typep y 'cochain-cx))
	 (let ((f-values (make-hash-table :test #'equal)))
	 (make-cochain-map
	  :sou x
	  :tar y
	  :mat (with-eql-ans
		(map-objects
		 #'(lambda (i yi)
		     (let ((xi (term x i)))
		       (cond ((typep xi 'directsum)
			      (setf (gethash i ans)
				(make-directsum-morphism-to-singleton
				 (term x i)
				 yi
				 #'(lambda (jj)
				     (let ((g (gethash jj f-values)))
				       (when (null g)
					 (setq g
					   (setf (gethash jj f-values)
					     (funcall f jj))))
				       ;; Not = in next, since g
				       ;; may not be a number.
				       (if (or (equal g 0) (equal g 1))
					   g
					 (term g i)))))))
			     ((zero-object-p xi)
			      nil) ; do nothing
			     (t
			      (error "Recursive directsum structure lost.")))))
			     y)))))
	(t
	 (error "Can't do this case.")))))

;;; IDEA: define restrict-to-summand-in-source, resp. -target,
;;; resp. -both.  This will avoid forms like
;;;    (compose f (directsum-inclusion i (sou f))),
;;; which use matrix multiplication rather than the more efficient
;;; copy-of-matrix.

(defun directsum-inclusion (ind obj)
  (declare (type directsum obj))
  "Arguments ind, obj.  Here obj is a directsum object.  The function
returns the canonical map from the ind-th summand
(get-summand-indexed-by obj ind) to obj."
  (let ((v (get-summand-indexed-by obj ind)))
    (make-directsum-morphism-from-singleton
     v
     obj
     #'(lambda (i)
	 (if (equal ind i)
	     1
	   0)))))

(defun directsum-projection (ind obj)
  (declare (type directsum obj))
  "Arguments ind, obj.  Here obj is a directsum object.  The function
returns the canonical map from obj to the ind-th summand
(get-summand-indexed-by obj ind)."
  (let ((v (get-summand-indexed-by obj ind)))
    (make-directsum-morphism-to-singleton
     obj
     v
     #'(lambda (i)
	 (if (equal ind i)
	     1
	   0)))))

(defun make-directsum-of-two (a b &key name)
   "This has two arguments, a and b, which are objects in the same
category.  It returns their direct sum.  The indices in the directsum
are 0 for a and 1 for b.
   This is a simpler version of the function make-directsum-object."
  (with-names
   (make-directsum-object
    (vector a b)
    (vector 0 1))))


;;; ---------- Methods for cohomology of cochain complexes ----------

(defun fill-out-cochain-cx (x)
  (declare (type cochain-cx x))
  "Input: cochain-cx x.  Output: x, with all its cohomology-related
slots filled out."
  (let ((inds '()))
    (map-objects #'(lambda (i v)
		     (declare (ignore v))
		     (push i inds))
		 x)
    (setq inds (sort inds #'>))
    ;; now inds is a list of the indices of all the terms in x, in
    ;; decreasing order
    (let ((z-o (empty-hash-table))
	  (z-mat (empty-hash-table))
	  (z-retr (empty-hash-table))
	  (b-o (empty-hash-table))
	  (b-mat (empty-hash-table)))
      (dolist (i inds)
	(let ((bz-i+1 (gethash (1+ i) b-mat)))
	  (cond ((null bz-i+1)
		 ;; here (gethash (1+ i) b-mat) is zero
		 (setf (gethash i z-o) (term x i))
		 (setf (gethash i z-mat)
		       (make-id-morphism (gethash i z-o) (term x i)))
		 (setf (gethash i z-retr)
		       (make-id-morphism (term x i) (gethash i z-o)))
		 (setf (gethash i b-o) 
		       (term x (- i 1)))
		 (setf (gethash i b-mat)
		       (d (- i 1) x)))
		(t
		 (setf (gethash i z-o)
		       (sou (ker bz-i+1)))
		 (setf (gethash i z-mat)
		       (ker bz-i+1))
		 (setf (gethash i z-retr)
		       (ker-retraction bz-i+1))
		 (setf (gethash i b-o)
		       (term x (- i 1)))
		 (setf (gethash i b-mat)
		       (compose (ker-retraction bz-i+1)
				(d (- i 1) x)))))))
      (setf (cochain-cx-cocycle-inclusion-map x)
	    (make-cochain-map
	     :sou (make-cochain-cx
		   :objects z-o :maps (empty-hash-table))
	     :tar x
	     :mat z-mat))
      (setf (cochain-cx-cocycle-retraction x)
	    (make-cochain-map
	     :sou x
	     :tar (sou (cochain-cx-cocycle-inclusion-map x))
	     :mat z-retr))
      (setf (cochain-cx-cobdry-to-cocycle-map x)
	    (make-cochain-map
	     :sou (make-cochain-cx
		   :objects b-o :maps (empty-hash-table))
	     :tar (sou (cochain-cx-cocycle-inclusion-map x))
	     :mat b-mat))
      ;; return the filled-out x
      x)))
	    
(defun cocycle-inclusion-map (x)
  (let ((ans (cochain-cx-cocycle-inclusion-map x)))
    (cond ((null ans)
	   (fill-out-cochain-cx x)
	   (cochain-cx-cocycle-inclusion-map x))
	  (t ans))))

(defun cobdry-to-cocycle-map (x)
  (let ((ans (cochain-cx-cobdry-to-cocycle-map x)))
    (cond ((null ans)
	   (fill-out-cochain-cx x)
	   (cochain-cx-cobdry-to-cocycle-map x))
	  (t ans))))

(defun fill-out-cochain-map (f)
  (declare (type cochain-map f))
  "Input: cochain-map f.  Output: the filled-out f.  This makes,
stores and returns the induced map from (cohomology (sou f)) to
(cohomology (tar f)), which is then accessible by (map-on-cohomology f)." 
  (let ((x (sou f))
	(y (tar f)))
    (setf (cochain-map-map-on-cohomology f)
      (projection-of-quotients
       (compose
	(coker (cobdry-to-cocycle-map y))
	(inclusion-of-subspaces
	 (compose f
		  (cocycle-inclusion-map x))
	 (cocycle-inclusion-map y)))
       (coker (cobdry-to-cocycle-map x))))))

(defun dims (x)
  (declare (type cochain-cx x))
  (let ((inds '()))
    (map-objects #'(lambda (i v)
		     (declare (ignore v))
		     (push i inds))
		 x)
    (let ((imin (apply #'min inds))
	  (imax (apply #'max inds)))
      (declare (fixnum imin imax))
      (unless (zerop imin)
	(format t "~&[lowest degree = ~D]~&" imin))
      (do ((i imax (1- i))
	   (ans '()))
	  ((< i imin)
	   ans)
	(push (dim (term x i)) ans)))))

(defun ranks (x)
  (declare (type cochain-map x))
  (let ((inds '()))
    (map-objects #'(lambda (i v)
		     (declare (ignore v))
		     (push i inds))
		 (sou x))
    (map-objects #'(lambda (i v)
		     (declare (ignore v))
		     (push i inds))
		 (tar x))
    (let ((imin (apply #'min inds))
	  (imax (apply #'max inds)))
      (declare (fixnum imin imax))
      (unless (zerop imin)
	(format t "~&[lowest degree = ~D]~&" imin))
      (do ((i imax (1- i))
	   (ans '()))
	  ((< i imin)
	   ans)
	(push (rank (term x i)) ans)))))

(defmethod betti-numbers (x)
  ;; The generic case, when x is a cochain-cx.  Returns the dims of
  ;; the cohomology of x.
  (dims (cohomology x)))

(defun map-to-cohomology-zero-d (x)
  (declare (type cochain-cx x))
  "x should be a cochain-cx with all differentials zero.  Returns the
canonical isomorphism from x to (cohomology x)."
  (assert (zero-maps-p x) () "Not all differentials are zero.")
  (make-id-morphism x (cohomology x)))

;;; ---------------Methods for TENSORS ---------------

(defun make-tensor-object (x y &key name)
  "Makes the tensor product of the two objects given as arguments.  The
result will be in the same category, but will be in the subtype
tensor-<name of the category>.  For more information, read the
documentation on tensor-qvsp and tensor-graded-object."
  (with-names
  (cond ((and (typep x 'qvsp)
	      (typep y 'qvsp))
	 (make-tensor-qvsp
	  :dim (the fixnum (* (dim x) (dim y)))
	  :from1 x :from2 y))
	((and (typep x 'cochain-cx)
	      (typep y 'cochain-cx))
	 ;; Result is a cochain-cx, obtained by directsumming the
	 ;; double graded object along the diagonals in an unspecified
	 ;; order.
	 ;;
	 ;; \ \ \
	 ;;  \ \
	 ;; \ \ \
	 ;;
	 ;; The indices in the directsums are dotted pairs (i . j) of
	 ;; degrees.
	 (let ((indx (let ((ans '()))
		       (map-objects #'(lambda (i v)
					(declare (ignore v))
					(push i ans))
				    x)
		       ans))
	       (indy (let ((ans '()))
		       (map-objects #'(lambda (i v)
					(declare (ignore v))
					(push i ans))
				    y)
		       ans))
	       (sorter (empty-hash-table))
	       (o (empty-hash-table))
	       (m (empty-hash-table)))
	   (dolist (i indx)
             (dolist (j indy)
               (push (cons i j) (gethash (+ i j) sorter))))
	   (maphash #'(lambda (k lyst)
			(let ((v (coerce lyst 'vector)))
			  (setf (gethash k o)
				(make-directsum-object
				 (map 'vector #'(lambda (pair)
						  (make-tensor-object
						   (term x (car pair))
						   (term y (cdr pair))))
				      v)
				 v))))
		    sorter)
	   (maphash #'(lambda (k a)
			(let ((b (gethash (1+ k) o)))
			  (unless (null b)
			    (setf (gethash k m)
			      (make-directsum-morphism
			       ;; need map from
			       ;; sum_{i+j=k} x^i tensor y^j    to
			       ;; sum_{i+j=k+1} x^alpha tensor y^beta
			       a
			       b
			       #'(lambda (jj ii)
				   (let ((so (get-summand-indexed-by a
								     jj))
					 (ta (get-summand-indexed-by b
								     ii)))
				     (cond ((equal (car jj) (car ii))
					    (make-tensor-morphism
					     (make-id-morphism
					      (from1 so) (from1 ta))
					     (d (cdr jj) y)
					     so
					     ta))
					   ((equal (cdr jj) (cdr ii))
					    (make-tensor-morphism
					     (scmult
					      (if (evenp (cdr jj)) 1
						-1)
					      (d (car jj) x))
					     (make-id-morphism
					      (from2 so) (from2 ta))
					     so
					     ta))
					   (t 0)))))))))
		    o)
	   (make-tensor-cochain-cx
	    :objects o :maps m :from1 x :from2 y)))
	(t (error "I don't know how to tensor the arguments.")))))

(defun make-tensor-morphism (f g so ta &key name)
  (declare (type tensor so ta))
  "The arguments are f g so ta.  Here f and g are morphisms in the same
category.  Output is the induced morphism from (sou f)-tensor-(sou g)
to (tar f)-tensor-(tar g).  The latter two tensor-product spaces must be
provided as the arguments so and ta --if you need to make these
objects, use make-tensor-object."
  (with-names
  (assert (and (eq (sou f) (from1 so))
	       (eq (sou g) (from2 so))) ()
"The third argument is not the tensor product of the sources of the
given morphisms (first and second arguments).")
  (assert (and (eq (tar f) (from1 ta))
	       (eq (tar g) (from2 ta))) ()
"The fourth argument is not the tensor product of the targets of the
given morphisms (first and second arguments).")
  (cond ((or (typep f 'zero-qvsp-morphism)
	     (typep g 'zero-qvsp-morphism))
	 (make-zero-morphism so ta))
	((and (typep f 'id-qvsp-morphism)
	      (typep g 'id-qvsp-morphism))
	 (make-id-morphism so ta))
	((and (typep f 'id-qvsp-morphism)
              (typep g 'qvsp-morphism))
	 (let* ((B (mat g))
		(n1 (dim (sou f)))	; = m1
		(m2 (matrix-dim B 0))
		(n2 (matrix-dim B 1))
		(ans (make-matrix (the fixnum (dim ta))
				  (the fixnum (dim so)))))
	   (declare (type csparse B ans)
		    (fixnum n1 m2 n2))
	   (dotimes (j1 n1)
	     (declare (fixnum j1))
	     (let ((j1n2 (* j1 n2)))
	       (declare (fixnum j1n2))
	       (let* ((i1m2 (* j1 m2))) ; i1 = j1 and Ai1j1 = 1
		 (declare (fixnum i1m2))
		 (dotimes (j2 n2)
		   (declare (fixnum j2))
		   (dolist (pair (svref (csparse-cols B) j2))
		     (let ((i2 (car pair))
			   (Bi2j2 (cdr pair)))
		       (declare (fixnum i2)
				(integer Bi2j2))
		       (push (cons (the fixnum (+ i1m2 i2))
				   Bi2j2)
			     (svref (csparse-cols ans)
				    (the fixnum (+ j1n2 j2))))))))))
	   ;; Now we're done, except that ans's columns must be reversed.
	   (let ((ansc (csparse-cols ans)))
	     (declare (simple-vector ansc))
	     (dotimes (j (matrix-dim ans 1))
	       (declare (fixnum j))
	       (setf-self (svref ansc j) (nreverse :self))))
	   ;; Final answer:
	   (make-qvsp-morphism so ta ans)))
	((and (typep f 'qvsp-morphism)
	      (typep g 'id-qvsp-morphism))
	 (let* ((A (mat f))
		;; m1 never used
		(n1 (matrix-dim A 1))
		(n2 (dim (sou g)))	; = m2
		(ans (make-matrix (the fixnum (dim ta))
				  (the fixnum (dim so)))))
	   (declare (type csparse A ans)
		    (fixnum n1 n2))
	   (dotimes (j1 n1)
	     (declare (fixnum j1))
	     (let ((j1n2 (* j1 n2)))
	       (declare (fixnum j1n2))
	       (dolist (pair (svref (csparse-cols A) j1))
		 (let* ((i1 (car pair))
			(i1m2 (* i1 n2))
			(Ai1j1 (cdr pair)))
		   (declare (fixnum i1 i1m2)
			    (integer Ai1j1))
		   (dotimes (j2 n2)
		     (declare (fixnum j2))
		     ;; i2 = j2.  Bi2j2 = 1.
		     (push (cons (the fixnum (+ i1m2 j2))
				 Ai1j1)
			   (svref (csparse-cols ans)
				  (the fixnum (+ j1n2 j2)))))))))
	   ;; Now we're done, except that ans's columns must be reversed.
	   (let ((ansc (csparse-cols ans)))
	     (declare (simple-vector ansc))
	     (dotimes (j (matrix-dim ans 1))
	       (declare (fixnum j))
	       (setf-self (svref ansc j) (nreverse :self))))
	   ;; Final answer:
	   (make-qvsp-morphism so ta ans)))
	((and (typep f 'qvsp-morphism)
	      (typep g 'qvsp-morphism))
	 ;; The most general version for qvsp-morphisms.
	 (let* ((A (mat f))
		(B (mat g))
		;; m1 never used
		(n1 (matrix-dim A 1))
		(m2 (matrix-dim B 0))
		(n2 (matrix-dim B 1))
		(ans (make-matrix (the fixnum (dim ta))
				  (the fixnum (dim so)))))
	   (declare (type csparse A B ans)
		    (fixnum n1 m2 n2))
	   (dotimes (j1 n1)
	     (declare (fixnum j1))
	     (let ((j1n2 (* j1 n2)))
	       (declare (fixnum j1n2))
	       (dolist (pair (svref (csparse-cols A) j1))
		 (let* ((i1 (car pair))
			(i1m2 (* i1 m2))
			(Ai1j1 (cdr pair)))
		   (declare (fixnum i1 i1m2)
			    (integer Ai1j1))
		   (dotimes (j2 n2)
		     (declare (fixnum j2))
		     (dolist (pair (svref (csparse-cols B) j2))
		       (let ((i2 (car pair))
			     (Bi2j2 (cdr pair)))
			 (declare (fixnum i2)
				  (integer Bi2j2))
			 (push (cons (the fixnum (+ i1m2 i2))
				     (* Ai1j1 Bi2j2))
			       (svref (csparse-cols ans)
				      (the fixnum (+ j1n2 j2)))))))))))
	   ;; Now we're done, except that ans's columns must be reversed.
	   (let ((ansc (csparse-cols ans)))
	     (declare (simple-vector ansc))
	     (dotimes (j (matrix-dim ans 1)) ; = (* n1 n2)
	       (declare (fixnum j))
	       (setf-self (svref ansc j) (nreverse :self))))
	   ;; Final answer:
	   (make-qvsp-morphism so ta ans)))
	((and (typep f 'cochain-map)
	      (typep g 'cochain-map))
	 (make-tensor-morphism-gr f g so ta))
	(t (error "Can't handle this case.")))))

(defun make-tensor-morphism-gr (f0 g0 so ta)
  (declare (type cochain-map f0 g0)
	   (type cochain-cx so ta))
  "The version of make-tensor-morphism for cochain-cx's."
   ;; Note: books like Brown, _Coh. of Groups_, Springer GTM vol. 87,
   ;; p. 10 suggest that we need a sign of (-1)^(j_j * i_i) on the
   ;; make-tensor-morphism below.  However, the result is still a
   ;; chain map even without these conventions.
  (make-cochain-map
    :sou so
    :tar ta
    :mat (with-eql-ans
	  (map-objects #'(lambda (i so-i)
			   (let ((ta-i (term ta i)))
			     (unless (zero-object-p ta-i)
			       (setf (gethash i ans)
				     (make-directsum-morphism
				      so-i
				      ta-i
				      #'(lambda (indj indi)
					  (if (equal indj indi)
					      (make-tensor-morphism
					       ;; Have to use f0, not
					       ;; f, because of
					       ;; enclosing
					       ;; make-directsum-morphism.
					       (term f0 (car indj))
					       (term g0 (cdr indj))
					       (get-summand-indexed-by
						so-i indj)
					       (get-summand-indexed-by
						ta-i indi))
					    0)))))))
		       so))))

(defun tensor-associate-to-left (x y)
  (declare (type tensor x y))
  "Let A, B, C be objects in a certain category.  The two arguments of
this function are x = A-tensor-[B-tensor-C] and y =
[A-tensor-B]-tensor-C.  The output is the canonical isomorphism from x
to y.  For how to make objects like x and y, see the documentation on
make-tensor-object."
  (assert (and (eq (from1 x) (from1 (from1 y)))
	       (eq (from1 (from2 x)) (from2 (from1 y)))
	       (eq (from2 (from2 x)) (from2 y)))
	  () "Pieces of the tensor objects don't agree.")
  (cond ((and (typep x 'tensor-qvsp)
              (typep y 'tensor-qvsp))
         ;; Since row-major order is "associative", the following works:
	 (make-id-morphism x y))
        ((and (typep x 'tensor-cochain-cx)
              (typep y 'tensor-cochain-cx))
	 (make-cochain-map
	  :sou x
	  :tar y
	  :mat (with-eql-ans
		(map-objects
		 #'(lambda (a xa)
		     (let ((ya (term y a)))
		       (setf (gethash a ans)
			     (make-directsum-morphism
			      xa
			      ya
			      #'(lambda (indsou indtar)
				  (let* ((i (car indsou))
					 (k (cdr indtar))
					 (j (- a (the fixnum (+ i k))))
					 (left (get-summand-indexed-by
						xa indsou))
					 (right (get-summand-indexed-by
						 ya indtar))
					 (bc (get-summand-indexed-by
					      (from2 left)
					      (cons j k))))
				    (declare (fixnum i j k))
				    (if (null bc)
					0
				      (let ((ab (get-summand-indexed-by
						 (from1 right)
						 (cons i j))))
					(if (null ab)
					    0
					  (let ((a-bc (make-tensor-object
						       (from1 left)
						       bc))
						(ab-c (make-tensor-object
						       ab
						       (from2 right))))
					    (compose
					     (make-tensor-morphism
					      (directsum-inclusion
					       (cons i j)
					       (from1 right))
					      (make-id-morphism
					       (from2 ab-c)
					       (from2 right))
					      ab-c
					      right)
					     (tensor-associate-to-left
					      a-bc ab-c)
					     (make-tensor-morphism
					      (make-id-morphism
					       (from1 left)
					       (from1 a-bc))
					      (directsum-projection
					       (cons j k)
					       (from2 left))
					      left
					      a-bc))))))))))))
		 x))))
	(t (error "Can't handle this case."))))

(defun distribute-tensor-from-left-over-directsum (x y)
  (declare (type tensor x)
	   (type directsum y))
  "Arguments X, Y.  We assume X = A tensor (directsum_i B^(i)), and
Y = directsum_i (A tensor B^(i)).  The function returns the canonical
isomorphism X --> Y.  Here A and the B^(i) are in either the category
of qvsps or the category of graded-objects."
  (cond ((and (typep x 'tensor-qvsp)
	      (typep y 'directsum-qvsp))
	 (assert (every #'(lambda (ind)
			    (eq (from1 x)
				(from1 (get-summand-indexed-by y ind))))
			(indices y)) ()
"The A's don't match.  See the documentation for this function.")
	 (assert (every #'equal
			(indices (from2 x))
			(indices y)) ()
"The indices vectors do not contain the same elements in the same order.")
	 (assert (every #'(lambda (ind)
			    (eq (get-summand-indexed-by (from2 x) ind)
				(from2 (get-summand-indexed-by y ind))))
			(indices y)) ()
"The B's don't all match.  See the documentation for this function.")
	 (let ((id-on-A (make-id-morphism (from1 x) (from1 x))))
	   (make-directsum-morphism-from-singleton
	    x y
	    #'(lambda (i)
		(make-tensor-morphism
		 id-on-A
		 (directsum-projection i (from2 x))
		 x
		 (get-summand-indexed-by y i))))))
	((and (typep x 'tensor-cochain-cx)
	      (typep y 'directsum-cochain-cx))
	 (assert (every #'(lambda (ind)
			    (eq (from1 x)
				(from1 (get-summand-indexed-by y ind))))
			(indices y)) ()
"The A's don't match.  See the documentation for this function.")
	 ;; Are the next two assert's unnecessary, because the tensor
	 ;; functions will do the same checking?
	 (assert (eq (indices (from2 x)) (indices y)) ()
"The sums have different indices vectors.  See the documentation for
this function.")
	 (assert (every #'(lambda (ind)
			    (eq (get-summand-indexed-by (from2 x) ind)
				(from2 (get-summand-indexed-by y ind))))
			(indices y)) ()
"The B's don't all match.  See the documentation for this function.")
	 (let ((id-on-A (make-id-morphism (from1 x) (from1 x))))
	   (make-directsum-morphism-from-singleton
	    x y
	    #'(lambda (i)
		(make-tensor-morphism
		 id-on-A
		 (directsum-projection i (from2 x))
		 x
		 (get-summand-indexed-by y i))))))
	(t (error "Can't handle this case."))))		 

(defun extract-f-from-id-tensor-f (g)
  (declare (type cochain-map g))
  "g is a cochain-map (A^* tensor B^*) ----> (A^* tensor C^*) .  It is
assumed that g is the tensor product of two maps, and that the map on
the A^* part is the identity.  We also assume that A^0 has dimension
1.  The map B_* --> C^* is called f.  The function returns f."
  (let ((a (from1 (sou g)))
	(b (from2 (sou g)))
	(c (from2 (tar g))))
    (assert (eq a (from1 (tar g))) ()
	    "A^* parts are not the same in source and target.")
    (assert (= 1 (dim (term a 0))) ()
	    "A^0 is not of dimension 1.")
    (let* ((a0 (make-cochain-cx
		;; A cx with just A^0 in degree 0.
		:objects (with-eql-ans
			  (setf (gethash 0 ans) (term a 0)))
		:maps (empty-hash-table)))
	   (a0-in (make-cochain-map
		   :sou a0
		   :tar a
		   :mat (with-eql-ans
			 (setf (gethash 0 ans)
			       (make-id-morphism (term a0 0)
						 (term a 0))))))
	   (a0-out (make-cochain-map
		    :sou a
		    :tar a0
		    :mat (with-eql-ans
			  (setf (gethash 0 ans)
				(make-id-morphism (term a 0)
						  (term a0 0))))))
	   (a0b (make-tensor-object a0 b))
	   (a0c (make-tensor-object a0 c))
	   (b-a0b (make-cochain-map
		   :sou b
		   :tar a0b
		   :mat (with-eql-ans
			 (map-objects
			  #'(lambda (i bi)
			      (setf (gethash i ans)
				    (make-id-morphism
				     ;; This assumes the map on A^0
				     ;; really _is_ the id!
				     bi
				     (term a0b i))))
			  b))))
	   (a0c-c (make-cochain-map
		   :sou a0c
		   :tar c
		   :mat (with-eql-ans
			 (map-objects
			  #'(lambda (i ci)
			      (setf (gethash i ans)
				    (make-id-morphism
				     ;; dual comment to previous
				     (term a0c i)
				     ci)))
			  c)))))
      (declare (type cochain-cx a0)
	       (type tensor-cochain-cx a0b a0c)
	       (type cochain-map a0-in a0-out b-a0b a0c-c))
      ;; The answer:
      (compose
       a0c-c
       (make-tensor-morphism
	a0-out
	(make-id-morphism c c)
	(tar g)
	a0c)
       g
       (make-tensor-morphism
	a0-in
	(make-id-morphism b b)
	a0b
	(sou g))
       b-a0b))))

(defun include-from2 (b ab)
  (declare (type tensor-cochain-cx ab)
	   (type cochain-cx b))
  "A partial converse to extract-f-from-id-tensor-f.  Here
ab = A^* tensor B^* with A^0 of dim 1, and b = B^*.  The function
returns the map from b to ab obtained by tensoring with `1'
everywhere."
  (assert (eq b (from2 ab)) ()
	  "b and (from2 ab) don't match.")
  (assert (= 1 (dim (term (from1 ab) 0))) ()
	  "A^0 is not of degree 1.")
  (make-cochain-map
   :sou b
   :tar ab
   :mat (with-equal-ans
	 (map-objects
	  #'(lambda (j bj)
	      (setf (gethash j ans)
		(make-directsum-morphism-from-singleton
		 bj
		 (term ab j)
		 #'(lambda (ii)
		     (cond ((zerop (car ii)) ; A^0 tensor B^j
			    (assert (= (cdr ii) j) () "Degree mixup.")
			    1)
			   (t 0))))))
	  b))))
		 
(defun kunneth-map-left-trivial (a-hb a-b)
  "A^* has zero differential.  Input: A^* tensor H^*(B) and
A^* tensor B^*.  Returns the canonical isomorphism from the former to
the cohomology of the latter."
  (let ((a (from1 a-b))
	(b (from2 a-b)))
    (assert (zero-maps-p a) ()
	    "A^* does not have zero differentials.")
    (assert (eq a (from1 a-hb)) ()
	    "The two A^* factors are not the same.")
    (assert (eq (from2 a-hb) (cohomology b)) ()
	    "The second factor of a-hb is not the cohomology of B.")
    (let* ((ia
	    (make-id-morphism a a))
	   (zmap
	    (inclusion-of-subspaces
	     ;; the next one is really itself composed with the
	     ;; identity on a-b
	     (make-tensor-morphism
	      ia
	      (cocycle-inclusion-map b)
	      (make-tensor-object a (sou (cocycle-inclusion-map b)))
	      a-b)
	     (cocycle-inclusion-map a-b))))
      ;; answer:
      (projection-of-quotients
       (compose
	(map-to-cohomology a-b)
	zmap)
       (make-tensor-morphism
	ia
	(map-to-cohomology b)
	(sou zmap)
	a-hb)))))

;;; --------------- Functions for WEDGES. ---------------

(defun fetchequal (key a-list)
  "Arguments key, a-list.  An a-list is a list of ordered pairs
((k1 . v1) (k2 . v2) ...)  The key should be one of the k's.  The
function returns the corresponding one of the v's.  An error is
signaled if the key is not found.  The test for equality is Lisp's
equal."
  (let ((x (assoc key a-list :test #'equal)))
    (if (null x)
	(error "FETCHEQUAL couldnt find the key ~S in the association list
~S"
	       key a-list)
      (cdr x))))

(defun binom (n k)
  (declare (fixnum n k))
  "The arguments n and k are nonnegative integers [fixnums].  The
function gives the binomial coefficient n-choose-k = n!/(k!(n-k)!).
If k > n, it returns 0."
  ;; If the answer is too big to be a fixnum, expect problems.  Note
  ;; that (binom 30 15) < 2^29 < (binom 32 16).  Thus, e.g., the toric
  ;; variety code will not run into problems with this until we have
  ;; varieties of complex dimension 16 or more.
  (assert (and (<= 0 k) (<= 0 n)) ()
	  "Arguments should both be nonnegative integers [fixnums].")
  (cond ((zerop k) 1)
	;; Table look-up when 0 <= k <= n <= 5.
        ((<= k n 5)
         (svref (vector 1
			1 1
			1 2  1
			1 3  3  1
			1 4  6  4 1
			1 5 10 10 5 1)
		(the fixnum
		     (+ (the fixnum (svref (vector 0 1 3 6 10 15) n))
			k))))
	((> k n) 0)
	((= k 1) n)
	((> (the fixnum (+ k k)) n)
	 (binom n (the fixnum (- n k))))
	(t
	 ;; We now compute (n-k+1)(n-k+2)...(n)/(1)(2)...(k) by
	 ;; taking the products of the fractions (n-k+i)/i for
	 ;; i = 1,...,k.  Each partial product is an integer, because
	 ;; it equals (n-k+i) choose (n-k).
	 (do ((numerator (1+ (- n k)) (1+ numerator))
	      (i 1 (1+ i))
	      (ans 1 (floor (* ans numerator) i)))
	     ((> i k)
	      ans)
	   (declare (fixnum i)
		    (integer numerator ans))))))

(defvar *odom-ind2tuple* '() "See the documentation on wedge-qvsp.")
(defvar *odom-tuple2ind* '() "See the documentation on wedge-qvsp.")
(defvar *odom-lists*     '() "See the documentation on wedge-qvsp.")

;;; *odom-lists* is unnecessary.
;;; (fetchequal '(a b) *odom-lists*) is the same as
;;; (coerce (fetchequal '(a b) *odom-ind2tuple*)) 'list).
;;; Which method is faster?

(defun iota (n)
  (declare (fixnum n))
  "The argument is a non-negative integer n.  The output is the list
(0 1 ... n-1).  The name comes from APL."
  (do ((i (1- n) (1- i))
       (ans '() (cons i ans)))
      ((< i 0) ans)
    (declare (fixnum i))))

;;; Changes in the next functions made on 5/9/96 are labelled 5/9.

(defun update-odom (d a)
  (declare (fixnum d a))
  "This function adds to the ODOM- lists the material needed for
wedge-a of a d-dimensional space.  For how the ODOM- lists are
structured, see the documentation in the source code near the
definition of wedge-qvsp."
  (assert (<= 0 (min a d)) ; 5/9 was (<= 0 a d)
	  () "In UPDATE-ODOM, bad input: ~D choose ~D" d a)
  (cond ((not (null (assoc (list d a) *odom-lists* :test #'equal))))
	;; If d-choose-a is already present, do nothing.
	((> a d) ; 5/9 this whole cond clause added
	 (setq *odom-ind2tuple*
	       (acons (list d a)
		      (make-array '(0))
		      *odom-ind2tuple*))
	 (setq *odom-tuple2ind*
	       (acons (list d a)
		      '()
		      *odom-tuple2ind*))
	 (setq *odom-lists*
	       (acons (list d a)
		      '()
		      *odom-lists*)))
	((zerop a)
	 (setq *odom-ind2tuple*
	       (acons (list d 0)
		      (vector '()) ; used to be (vector '(()))
		      *odom-ind2tuple*))
	 (setq *odom-tuple2ind*
	       (acons (list d 0)
		      (pairlis '(()) '(0))
		      *odom-tuple2ind*))
	 (setq *odom-lists*
	       (acons (list d 0)
		      '(())
		      *odom-lists*)))
	((= d a)
	 (let ((iotad (iota d)))
	   (declare (list iotad))
	   (setq *odom-ind2tuple*
		 (acons (list d a)
			(make-array '(1) :initial-element iotad)
			*odom-ind2tuple*))
	   (setq *odom-tuple2ind*
		 (acons (list d a)
			(pairlis
			 (list iotad)
			 '(0))
			*odom-tuple2ind*))
	   (setq *odom-lists*
		 (acons (list d a)
			(list iotad)
			*odom-lists*))))
	(t
	 ;; The main idea is that the tuples for (d a) are
	 ;;    the tuples for (d-1 a), and
	 ;;    the tuples for (d-1 a-1), with d-1 appended to the end.
	 ;; This produces reverse-lex order.
	 (update-odom (the fixnum (1- d)) a)
	 (update-odom (the fixnum (1- d)) (the fixnum (1- a)))
	 (let* ((tuples (append
			 (the list
			   (fetchequal (list (the fixnum (1- d)) a)
				       *odom-lists*))
			 (mapcar
			  #'(lambda (x)
			      (let ((c (list (the fixnum (1- d)))))
				;; Puts d-1 at the end of list x.
				(append x c)))
			  (the list
			    (fetchequal (list (the fixnum (1- d))
					      (the fixnum (1- a)))
					*odom-lists*)))))
		(nn (binom d a)) ; = (length tuples)
		(i2t-vec (make-array (list nn)))
		(t2i-a-list '()))
	   (declare (list tuples t2i-a-list)
		    (fixnum nn)
		    (simple-vector i2t-vec))
	   (do ((i 0 (1+ i))
		(a tuples (cdr a)))
	       ((= i nn))
	     (declare (fixnum i))
	     (setf (aref i2t-vec i) (car a))
	     (setq t2i-a-list (acons (car a) i t2i-a-list)))
	   (setq *odom-ind2tuple*
		 (acons (list d a) i2t-vec *odom-ind2tuple*))
	   (setq *odom-tuple2ind*
		 (acons (list d a) (reverse t2i-a-list) *odom-tuple2ind*))
	   (setq *odom-lists*
		 (acons (list d a) tuples *odom-lists*))))))

(defun wedge-aux (a b &optional (kount 0) (tuple '()))
  (declare (fixnum kount)
	   (list a b tuple))
  "The arguments A and B are lists of distinct fixnums, each sorted
by <.  This function outputs a two-element list (sign C), where C is
(union A B), but sorted, and SIGN is the sign of the permutation
needed to do the sorting.  If A and B share an element, then SIGN = 0,
and C is set to nil as a signal."
  ;; I will assume the elements of A and B are fixnums.
  (labels ((minus1-to-k (k)
	     (declare (fixnum k))
	     (if (evenp k) +1 -1)))
    (cond ((null a)
	   (list (minus1-to-k kount) (append (nreverse tuple) b)))
	  ((null b)
	   (list (minus1-to-k kount) (append (nreverse tuple) a)))
	  (t
	   (let ((a1 (car a))
		 (b1 (car b)))
	     (declare (fixnum a1 b1))
	     (cond ((= a1 b1)
		    (list 0 '()))
		   ((< a1 b1)
		    (wedge-aux (cdr a) b kount (cons (car a) tuple)))
		   (t
		    (wedge-aux a
			       (cdr b)
			       (the fixnum
				    ;; because b1 moves past (length a)
				    ;; things
				    (+ kount (length a)))
			       (cons (car b) tuple)))))))))

(defun make-wedge-object (x d &key name)
  (declare (type qvsp x)
	   (type nnfixnum d))
  "Arguments V, d.  Makes wedge^d(V).  Is only defined when V is a qvsp
and d is a non-negative integer.
   To make the whole exterior algebra of V, where the result is a
cochain-cx indexed by i = 0,1,2,..., use the function
make-exterior-algebra."
  (with-names
  (make-wedge-qvsp
    :dim (binom (dim x) d)
    :from x
    :deg d)))

(defun make-exterior-algebra (v &key name)
  (declare (type qvsp v))
  "Input is a qvsp V.  Output is an exterior-algebra whose i-th entry
is wedge^i(V)."
  (with-names
   (let ((o (empty-hash-table)))
     (dotimes (i (1+ (dim v)))
       (declare (fixnum i))
       (setf (gethash i o)
	     (make-wedge-object v i)))
     (really-make-exterior-algebra
      :objects o
      :maps (empty-hash-table)
      :from v))))

(defun pairing (e)
  (declare (type exterior-algebra e))
  "Returns the pairing morphism (e tns e) --> e."
  (let ((p (exterior-algebra-pairing e)))
    (if (not (null p))
	p
      ;; compute, store, and return the pairing
      (let ((ee (make-tensor-object e e))
	    (p-mat (empty-hash-table))
	    (n1 (1+ (dim (from e)))))
	(declare (type tensor-cochain-cx ee)
		 (hash-table p-mat)
		 (type nnfixnum n1))
	(dotimes (i n1)
	  (declare (fixnum i))
          (setf (gethash i p-mat)
		(make-directsum-morphism-to-singleton
		 (term ee i)
		 (term e i)
		 #'(lambda (jj)
		     (wedge-product
		      (get-summand-indexed-by (term ee i) jj)
		      (term e i))))))
	(setf (exterior-algebra-pairing e)
	      (make-cochain-map :sou ee :tar e :mat p-mat))
	(exterior-algebra-pairing e)))))

(defun wedge-product (wa-tens-wb wc)
  (declare (type tensor-qvsp wa-tens-wb)
	   (type qvsp wc))
  "The two inputs are Wedge^a(V)-tensor-Wedge^b(V) and
Wedge^{a+b}(V), where V is a qvsp.  Output is the canonical
wedge-product morphism between them."
  (let ((wa (from1 wa-tens-wb))
	(wb (from2 wa-tens-wb))
	(ans-m (make-matrix (dim wc) (dim wa-tens-wb))))
    (let ((v (from wa))
	  (a (deg wa))
	  (b (deg wb))
	  (c (deg wc)))
      (declare (fixnum a b c))
      (assert (and (eq v (from wb))
		   (eq v (from wc))
		   (= c (the fixnum (+ a b))))
	      () "Bad inputs.")
      (let ((n (dim v)))
	(declare (fixnum n))
	(update-odom n a)
	(update-odom n b)
	(update-odom n c)
	(let ((oita (fetchequal (list n a) *odom-ind2tuple*))
	      (oitb (fetchequal (list n b) *odom-ind2tuple*))
	      (otic (fetchequal (list n c) *odom-tuple2ind*))
	      (count -1))
	  (declare (fixnum count))
	  (dotimes (i (dim wa))
	    (declare (fixnum i))
	    (dotimes (j (dim wb))
              (declare (fixnum j))
	      ;; The double loop in this order realizes our def'n of a
	      ;; basis of the tensor product.
	      (incf count)
	      (let ((product (wedge-aux (svref oita i)
					(svref oitb j))))
		(unless (zerop (the (integer -1 1) (car product)))
                  (matrix-set
		   ans-m
		   (fetchequal (cadr product) otic)
		   count
		   (car product))))))
	  ;; Result
	  (make-qvsp-morphism wa-tens-wb wc ans-m))))))

(defun make-wedge-morphism (f a so ta &key name)
  (declare (type nnfixnum a)
	   (type wedge-qvsp so ta))
  "The arguments are f a so ta.  Here f is a qvsp-morphism, and a is a
non-negative integer.  Output is the induced morphism wedge^a(sou f)
--> wedge^a(tar f).  The latter two wedge-power spaces must be
provided by the arguments so and ta.  If you need to make these
objects, use make-wedge-object.
  For more information, see the documentation for the type
wedge-qvsp.  To lift f to the corresponding morphism on exterior
algebras, use lift-morphism-to-exterior-algebra."
  (with-names
  (assert (and (eq (from so) (sou f))
	       (= a (deg so))) ()
"The arguments are f, a, so, ta.  The trouble is that so is not
wedge^a(sou f).") 
  (assert (and (eq (from ta) (tar f))
	       (= a (deg ta))) ()
"The arguments are f, a, so, ta.  The trouble is that ta is not
wedge^a(tar f).")
  (etypecase f
    (zero-qvsp-morphism
     (if (zerop a)
	 (make-id-morphism so ta)
       (make-zero-morphism so ta)))
    (id-qvsp-morphism
     (make-id-morphism so ta))
    (qm
     (let* ((x (qm-mat f))
	    (m (matrix-dim x 0)) ; (dim (tar f))
	    (n (matrix-dim x 1)) ; (dim (sou f))
	    ;; We don't have to stop and check here that 0 <= a <= (min m n),
	    ;; since otherwise so and ta could not have been created by
	    ;; make-wedge-object.
	    (m1 (binom m a))
	    (n1 (binom n a))
	    (ans (make-matrix m1 n1)))
       (declare (type nnfixnum m n m1 n1)
		(type matrix x ans))
       (update-odom m a)
       (update-odom n a)
       (let ((ma-ind2tuple (fetchequal (list m a) *odom-ind2tuple*))
	     (na-ind2tuple (fetchequal (list n a) *odom-ind2tuple*)))
	 (declare (simple-vector ma-ind2tuple na-ind2tuple))
	 (do ((i (1- m1) (1- i)))
	     ((< i 0))
	   (declare (fixnum i))
	   (dotimes (j n1)
	     (declare (fixnum j))
	     ;; Someday, write special methods for a = 0, a = 1.
	     (matrix-set ans i j
			 (minor-det x 
				    (svref ma-ind2tuple i)
				    (svref na-ind2tuple j))))))
       (make-qvsp-morphism so ta ans))))))

(defun lift-morphism-to-exterior-algebra (f e1 e2)
  (declare (type qvsp-morphism f)
	   (type exterior-algebra e1 e2))
  "Input: f, e1, e2.  Here f is a qvsp-morphism.  If S and T are
resp. the source and target of f, then e1 must be a copy of the
exterior-algebra on S, and e2 a copy of the exterior-algebra on T.
Output: a cochain-map from e1 to e2 giving the lift of f to the
exterior powers.  This basically just calls make-wedge-morphism [see
its doc.] repeatedly."
  (assert (and (eq (from e1) (sou f))
	       (eq (from e2) (tar f))) ()
"Let f be the first argument.  The second argument is not the exterior
algebra of (sou f), and/or the third argument is not the exterior
algebra of (tar f).")
  (assert (= (dim (sou f)) (dim (tar f))) ()
"I don't yet know how to handle it when the first argument is not a
morphism between spaces of the same dimension.")
  (make-cochain-map
    :sou e1 :tar e2
    :mat (with-eql-ans
	  (dotimes (i (1+ (dim (sou f))))
	    (declare (fixnum i))
            (setf (gethash i ans)
		  (make-wedge-morphism
		   f i (term e1 i) (term e2 i)))))))

(defun distribute-tensor-over-exterior-algebra-of-directsum
       (wa-tens-wb wab)
  (declare (type tensor-cochain-cx wa-tens-wb)
	   (type exterior-algebra wab))
  "The first argument is wedge^*(A) tensor wedge^*(B), and the second is 
wedge^*(A-directsum-B), for some qvsp's A and B.  Output: the
canonical isomorphism (a cochain-map) between them."
  (assert (and (eq (from (from1 wa-tens-wb))
		   (get-summand-indexed-by (from wab) 0))
	       (eq (from (from2 wa-tens-wb))
		   (get-summand-indexed-by (from wab) 1)))
	  ()
"Inputs are not of the right form.  See the documentation for this
function for more details.  The inputs were:
~S
~S" wa-tens-wb wab)
  (let ((d (dim (from wab)))
	 (da (dim (from (from1 wa-tens-wb))))
	 (db (dim (from (from2 wa-tens-wb))))
	 (ans-m (empty-hash-table)))
    (declare (fixnum d da db))
    (do ((i d (1- i)))
	((< i 0))
      (declare (fixnum i))
;; Let f0 be $\bigoplus_{k+j=i} (\bigwedge^k(A) \otimes \bigwedge^j(B))
;;              \to \bigwedge^i(A \oplus B)$
      (let ((f0 (make-directsum-morphism-to-singleton
		 (term wa-tens-wb i)
		 (term wab i)
		 #'(lambda (indsou)
		     ;; Output the map $\bigwedge^j(A) \otimes
		     ;; \bigwedge^{i-j}(B) \to \bigwedge^i(A\oplus B)$,
		     ;; where indsou is (j . i-j)
		     (let* ((sou0 (get-summand-indexed-by
				   (term wa-tens-wb i)
				   indsou))
			    (tar0 (term wab i))
			    (wa (from1 sou0)) ; wedge^j(A)
			    (wb (from2 sou0)) ; wedge^{i-j}(B)
			    ;; the map we're making is sou0 --> tar0,
			    ;; which is the same as
			    ;; wa tensor wb --> tar0
			    (ans0 (make-matrix (dim tar0) (dim sou0)))
			    (j (car indsou)))
		       (declare (fixnum j)
				(type qvsp sou0 tar0 wa wb))
		       (update-odom da j)
		       (update-odom db (the fixnum (- i j)))
		       (update-odom d i)
		       (dotimes (k1 (dim wa))
			 (declare (fixnum k1))
			 (dotimes (k2 (dim wb))
			   (declare (fixnum k2))
			   (matrix-set ans0
				       ;; the row number
				       (the fixnum
					 (fetchequal
					  (append
					   (svref
					    (the simple-vector
					      (fetchequal
					       (list da j)
					       *odom-ind2tuple*))
					    k1)
					   (mapcar
					    #'(lambda (z)
						;; 5/9 (+ da z))
						(declare (fixnum z))
						(the fixnum
						  (+
						   (the fixnum
						     (dim
						      (from
						       (from1
							wa-tens-wb))))
						   z)))
					    (svref
					     (the simple-vector
					       (fetchequal
						(list db
						      (the fixnum (- i j)))
						*odom-ind2tuple*))
					     k2)))
					  (the list ; really a-list
					    (fetchequal
					     (list d i)
					     *odom-tuple2ind*))))
				       ;; the col number
				       (the fixnum
					 (+ (the fixnum
					      (* k1
						 (the fixnum (dim wb))))
					    k2))
				       ;; the value
				       1)))
		       (make-qvsp-morphism sou0 tar0 ans0))))))
	(setf (gethash i ans-m) f0)))
    (make-cochain-map
      :sou wa-tens-wb
      :tar wab
      :mat ans-m)))

;;; ----------------------------------------

;;; Truncation

(defun truncation-zeroes-high (x d)
  (declare (type cochain-cx x)
	   (fixnum d))
  (let ((o (empty-hash-table))
	(m (empty-hash-table))
	(dmin1 (- d 1)))
    (declare (fixnum dmin1))
    (map-objects
     #'(lambda (i v)
	 (cond ((< i dmin1)
		(setf (gethash i o) v)
		(setf (gethash i m) (d i x)))
	       ((= i dmin1)
		(setf (gethash i o) v)
		(setf (gethash i m) (pullback-to-ker
				     (d dmin1 x) (d d x))))
	       ((= i d)
		(setf (gethash d o) (sou (ker (d d x)))))
	       (t nil)))
     x)
    (make-cochain-cx
     :objects o :maps m)))

(defun morphism-from-truncation-zeroes-high (tr x d)
  (declare (type cochain-cx tr x)
	   (type fixnum d))
  "Arguments are tr, x, d.  Here x is a cochain-cx, and tr is the
truncation of x obtained from the function
[truncation-zeroes-high x d].  The present function returns the
obvious cochain-map from tr to x."
  (make-cochain-map
    :sou tr
    :tar x
    :mat (with-eql-ans
           (map-objects
	    #'(lambda (i v)
		(cond ((< i d)
		       (setf (gethash i ans)
			     (make-id-morphism v (term x i))))
		      ((= i d)
		       (setf (gethash d ans)
			     (ker (d d x))))
		      (t nil)))
	    tr))))

;;; --------------- USER INTERFACE ---------------

(defun user-input-qvsp (dim &key name)
  "This is the same function as make-qvsp.  See the documentation on
make-qvsp."
  (with-names
   (assert (typep dim 'nnfixnum) ()
	   "~S is not a nonnegative integer [fixnum]." dim)
   (make-qvsp dim)))

;;; The function user-input-matrix should be defined wherever matrices
;;; are defined.  It takes one argument, a matrix, and overwrites it
;;; with the user's input.
;;;    NOTE: The doc for user-input-matrix should tell the user about
;;; (make-array (list m n)), which they may want for some purpose.

(defun user-input-qvsp-morphism (so ta &optional (mm nil) &key name)
   "This function takes two arguments, so and ta, which are resp. the
source and the target of a qvsp-morphism.  [To make these objects,
call (make-qvsp <dim>).]  The function also takes an optional third
argument mm.  The idea is that, if (dim so) is n and (dim ta) is m, the
function will set up an m-by-n matrix of integers representing your
morphism with respect to the standard bases.  There are basically three
ways the matrix can be set up:
(.) If mm is not provided or is nil, the system will create a new
    matrix and prompt you interactively for its entries.
(.) If mm is a matrix, the system will prompt you interactively for
    its entries and write these into mm, destroying any previous
    values.
(.) If mm is a list, either nested [like '((5 6 7) (8 9 10))] or not
    nested [like '(5 6 7 8)], then the matrix will be set up in the
    same way that user-input-matrix handles these cases [see doc.]
   See the documentation on qvsp-morphism, matrix, and
user-input-matrix for more information.
   Example: If you have vector spaces v and w defined, and you want
to define a morphism from v to w, type (user-input-qvsp-morphism v w).
   Example: If you want to define a qvsp-morphism without having
defined the vector spaces first, try something like
(user-input-qvsp-morphism (make-qvsp 5) (make-qvsp 3))
This amounts to creating two qvsp's on the fly, and then setting up a 
map between them given by a 3-by-5 matrix.
   Example: If you want to create a qvsp-morphism as in the previous
example, and the matrix you want it to have is
1 1 1 1 1
2 3 4 5 6
0 1 9 8 7
then type
(user-input-qvsp-morphism (make-qvsp 5) (make-qvsp 3)
     '((1 1 1 1 1) (2 3 4 5 6) (0 1 9 8 7)))
or, perhaps more simply,
(user-input-qvsp-morphism (make-qvsp 5) (make-qvsp 3)
     '(1 1 1 1 1 2 3 4 5 6 0 1 9 8 7))."
  (with-names
   (assert (typep so 'qvsp)
	   ()
"The first argument was supposed to be in the type qvsp, but instead
it is
~S" so)
   (assert (typep ta 'qvsp)
	   ()
"The second argument was supposed to be in the type qvsp, but instead
it is
~S" ta)
   (let ((m (dim ta))
	 (n (dim so)))
     (declare (type nnfixnum m n))
     (cond ((null mm)
            (make-qvsp-morphism
	     so ta
	     (user-input-matrix (make-matrix m n))))
           ((typep mm 'matrix)
            (make-qvsp-morphism so ta (user-input-matrix mm)))
           ((typep mm 'list)
            (when (and (not (null mm))
                       (listp (car mm)))
              (assert (= m (length (the list mm))) ()
                "You are trying to enter a matrix with ~D rows,
your third argument
~S
has ~D entries, not ~D." m mm m)
              (assert (every
                        #'(lambda (y) (= n (length (the list y)))) mm)
                ()
                "You are trying to enter a matrix with ~D columns,
but your third argument
~S
is a list of sublists, and not every sublist has length ~D." n mm n))
            (make-qvsp-morphism so ta (user-input-matrix m n mm)))
           (t
            (error "I couldn't recognize the third argument:
~S
Type (doc 'user-input-qvsp-morphism) at a Lisp prompt, or see the
tutorial file, for more information." mm))))))

(defun user-input-cochain-cx (objects-v &key name)
  "The input should be a vector, each of whose entries is a qvsp.  To
make a vector with entries a0, a1, ..., an, use (vector a0 a1 ... an).
Example:
  (user-input-cochain-cx (vector (make-qvsp 4) (make-qvsp 6) (make-qvsp 4)))
makes a cochain-cx with objects of dimension 4, 6, and 4 in degrees 0,
1, and 2 respectively.  The user will be prompted to enter the
boundary morphisms interactively."
  (with-names
  (assert (and (typep objects-v 'vector)
	       (every #'(lambda (x)
			  (typep x 'qvsp))
		      objects-v))
	  ()
"The input should be a vector, each of whose entries is a qvsp.
Instead, you have given the argument
~S
To make a vector with entries a0, a1, ..., an, use (vector a0 a1 ... an).
Example:
  (user-input-cochain-cx (vector (make-qvsp 4) (make-qvsp 6) (make-qvsp 4)))
makes a cochain-cx with objects of dimension 4, 6, and 4 in degrees 0,
1, and 2 respectively.  The user will be prompted to enter the
boundary morphisms interactively." objects-v)
  (let ((n (length objects-v)) ; from 0 to n-1 inclusive
        (o (empty-hash-table))
	(m (empty-hash-table)))
    (declare (fixnum n))
    (dotimes (i n)
      (declare (fixnum i))
      (setf (gethash i o)
	    (svref objects-v i)))
    (dotimes (i (1- n))
      (declare (fixnum i))
      (princ (format () "

For the map from degree ~R to ~R:" i (1+ i)))
      (setf (gethash i m)
        (user-input-qvsp-morphism (svref objects-v i)
                                  (svref objects-v (1+ i)))))
    (let ((ans (make-cochain-cx :objects o :maps m)))
      (test-cochain-cx ans)
      (format t "
As a check on your input, I have verified that the composition of any two
consecutive maps is zero.")
      ans))))

(defun user-input-cochain-map (so ta &key name)
  "The input should be two cochain-cx's, which are to be the source and
the target of the cochain map.  You will be prompted to enter the maps
between the cochain-cx's in each degree."
  (with-names
  (assert (typep so 'cochain-cx) ()
	  "The first argument is not a cochain-cx.")
  (assert (typep ta 'cochain-cx) ()
	  "The second argument is not a cochain-cx.")
  (let ((ans-m (empty-hash-table)))
    (map-objects
     #'(lambda (i v)
	 (format t "~&For the map in degree ~R:" i)
	 (setf (gethash i ans-m)
	       (user-input-qvsp-morphism v (term ta i))))
     so)
    (let ((ans (make-cochain-map :sou so :tar ta :mat ans-m)))
      (test-cochain-map ans)
      (format t "~&As a check on your input, I have verified that all
the squares in the cochain map commute.")
      ans))))

;;; --------------- Documentation ---------------

(defun doc (symb)
  "(doc x) prints the documentation string for the function, class,
etc. named by x.  It is an abbreviation for the Lisp function
(documentation x ...).
   When you use (doc ...), don't forget the single quote mark on the
argument.  For instance, use (doc 'qvsp), not (doc qvsp)."
  (format t "~A"
    (or (documentation symb 'function)
	(documentation symb 'type)
	(documentation symb 'variable)
	(documentation symb 'setf)
	(documentation symb 'structure))))

(defun help (symb)
  "(help x) prints the documentation string for the function, class,
etc. named by x.  It is an alias for (doc x), and is an abbreviation
for the Lisp function (documentation x ...).
   When you use (help ...), don't forget the single quote mark on the
argument.  For instance, use (help 'qvsp), not (help qvsp)."
  (doc symb))

(defun print-time (&optional (start-string "~&") (print-flag t))
  "Prints out the current date and time.
   Has two optional arguments, a string to print at the start [the
default is to move to the beginning of a new line], and a flag
[default `true'].  A calling program can give nil as the second
argument to turn print-time off." 
  (when print-flag
    (multiple-value-bind (sec min hr date month yr weekday dst zone)
			 (get-decoded-time)
      (declare (ignore date month yr weekday dst zone)
	       (fixnum sec min hr))
      (format t (concatenate 'string
			     start-string
			     " ~2,'0D:~2,'0D:~2,'0D")
	        hr min sec))))

