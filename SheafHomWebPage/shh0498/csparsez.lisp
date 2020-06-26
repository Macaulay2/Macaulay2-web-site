;;; Here are matrix routines for Sheafhom using sparse integer
;;; matrices.  The data structure "sparse", which stores the structure
;;; of both rows and columns, comes from summer 1987.  Work on it for
;;; Sheafhom dates from 2/94--2/97, the main improvement being the
;;; change in the summer of '96 to LLL, away from exclusive reliance on
;;; Smith normal form [jac].  The current version, with "csparse"s
;;; (which store only the columns for LLL), dates from summer 1997.

;;; ----------------------------------------

(proclaim '(optimize speed))

(proclaim '(freeze-type
	    nnfixnum sparsev csparse dense-matrix matrix sparse mpdq codedv
	    codedv-elt rowv matrix+))
  ;; Freeze-type is only used by CMU CL, and is not part of Common
  ;; Lisp in general.  Delete this form if you are trying to compile
  ;; the code with a Lisp other than CMU CL.

(deftype nnfixnum ()
  "The type of a non-negative fixnum [single-precision integer]."
  `(integer 0 ,most-positive-fixnum))

;;; We begin with a few basic things.

(defmacro setf-self (place body)
  (let ((newbody (subst place ':self body)))
    `(setf ,place ,newbody)))

(defun dummy ()
  "When something [e.g. a defstruct slot] requires you to include a
default argument, you can give it (dummy).  This will not conflict with
any type declarations you specify."
  (error "A required argument was not specified."))

(proclaim '(inline fixnum=))

(defun fixnum= (x y)
  (declare (fixnum x y))
  "A version of = for use when the two arguments are fixnums."
  (= x y))

;;; Here are macros a la Graham, _On Lisp_.

(defmacro while (test &body body)
  "In (while test body), the body will be evaluated over and over
until the test returns false."
  `(do () ((not ,test)) ,@body))

(defmacro till (test &body body)
  "In (till test body), the body will be evaluated over and over until
the test returns true."
  `(do () (,test) ,@body))

(defmacro do-till-explicit-return (&body body)
  "Loops forever, doing the body, until the body calls an explicit
return."
  `(do () (nil) ,@body))

(defconstant *max-n* (1- (round (sqrt (* 2 (1+ most-positive-fixnum)))))
  "If an m-by-n matrix has n greater than this value, then LLL may
fail, because the values of lam-index will go outside the fixnum
range.  In CMU CL, the value is 2^{15} - 1 = 32767.  A later version
could avoid this limitation.")

(proclaim '(fixnum *max-n*))

;;;             Material involving CSPARSEs
;;;
;;; (or that works equally well for csparse's and sparse's).

(deftype sparsev ()
  "A sparsev is a list  (... (j . n) ...) of pairs, where j is an
index and n an entry.  This represents a sparse integer vector.  For
instance, ((0 . 5) (3 . 7) ...) is the vector 5,0,0,7,...  The
j's are fixnums, and the pairs occur in order of increasing j.  The
n's are integers, and must be non-zero.  See also codedv."
  'list)

;;; Rule about shared list structure:  A sparsev will never share
;;; top-level list structure with another sparsev.  They may share
;;; pairs--e.g., in
;;; (setq a '(0 . 10)
;;;       b (list a)
;;;       c (list a))
;;; b and c do not violate the rule.  Never delve into one of those
;;; dotted pairs and call rplaca or rplacd.  But we may use the
;;; destructive functions to replace a whole pair with another whole
;;; pair, as long as a new top-level cons in used to point to the pair.
;;;    Exception: the overwrite and copy functions cause w and `new w'
;;; to share structure.  This is okay because w is about to be thrown
;;; to the garbage collector anyway.

(defstruct (csparse
	    (:print-function (lambda (x stream depth)
			       (declare (type csparse x)
					(ignore depth))
			       (if (or (> (csparse-m-val x) 100)
				       (> (csparse-n-val x) 25))
				   (format stream
				     "[~D by ~D, too big to print]"
				     (csparse-m-val x)
				     (csparse-n-val x))
				 (format stream "~A"
					 (mat-prn-to-string
					  (csparse-to-array x)))))))
"This is the more space-efficient of the two current data structures
for a sparse 2-dimensional array of integers [the other is `sparse'].
The array is m by n, where m and n are respectively given by the slots
m-val and n-val.  The cols slot holds a vector whose j-th entry is a
sparsev [see doc.] representing the j-th column of the matrix.  These
slots are accessed using the functions csparse-cols, csparse-m-val, etc.
   The user should not have to manipulate csparse's directly.  See the
functions like make-matrix, user-input-matrix [for interactively
filling a matrix's entries], add-matrix, mult, and so forth."
  (cols  (dummy) :type (simple-vector *))
  (m-val (dummy) :type nnfixnum)
  (n-val (dummy) :type nnfixnum))

(deftype dense-matrix ()
  "The type of an ordinary two-dimensional array of integers.  Compare
csparse and sparse."
  '(simple-array integer (* *)))

(defun make-csparse-zero (m n)
  (declare (type nnfixnum m n))
  "Makes an m-by-n csparse of 0's.  Example:
(setf x (make-csparse-zero 4 5)).  The user would normally use the more
general functions make-matrix, make-qvsp-morphism, or the interactive
user-input-qvsp-morphism [see their doc's]."
  (make-csparse
    :cols (make-array (list n) :element-type 'sparsev
                               :initial-element '()) ; entries init. 0
    :m-val m
    :n-val n))

(defun make-csparse-id (n)
  (declare (type nnfixnum n))
  "Makes an n-by-n identity csparse.  Example:
(setf x (make-csparse-id 4)).  The user would normally use
make-id-matrix [see doc.]"
  (let ((col1 (make-array (list n) :element-type 'sparsev
                                   :initial-element '())))
    (declare (type (simple-vector *) col1))
    (dotimes (j n)
      (declare (type nnfixnum j))
      (push (cons j 1) (svref col1 j)))
    (make-csparse
      :cols col1 :m-val n :n-val n)))

(defun add-sparsev (a b)
  (declare (type sparsev a b))
  "The arguments a and b are sparsev's.  Result is their sum, as a
sparsev."
  ;; The sum will never share top-level list structure with a or b,
  ;; though the pairs (j . n) forming the entries of the sparsev's may
  ;; be shared.
  (let ((ans '()))
    (declare (type sparsev ans))
    (do-till-explicit-return
      (cond ((null a)
	     (return (nreconc ans (copy-list b))))
	    ((null b)
	     (return (nreconc ans (copy-list a))))
	    (t
	     (let ((a1 (caar a))
		   (b1 (caar b)))
	       (declare (type nnfixnum a1 b1))
	       (cond ((< a1 b1)
		      (push (pop a) ans))
		     ((> a1 b1)
		      (push (pop b) ans))
		     (t	; here a1 = b1
		      (let ((sum (+ (the integer (cdr (pop a)))
				    (the integer (cdr (pop b))))))
			(declare (integer sum))
			(unless (zerop sum)
				(push (cons a1 sum) ans)))))))))))

(defun scalar-times-sparsev (s v)
  (declare (integer s)
	   (type sparsev v))
  "Arguments s, v.  s is an integer.  v, and the result, are
sparsev's.  The function performs scalar multiplication s times v."
  ;; The result will never share top-level list structure with v.
  (cond ((zerop s) '())
	((= s 1) (copy-list v))
	(t (mapcar #'(lambda (x)
		       ;; x = (j . n)
		       (cons (car x)
			     (* s (the integer (cdr x)))))
		   v))))

(defun csparse-aset (mm i j val)
  (declare (type csparse mm)
	   (type nnfixnum i j)
	   (integer val))
  "Sets the i,j entry of the csparse mm to the value val.  This
destructively alters mm.  Value returned is val."
  (assert (and (<= 0 i) (< i (csparse-m-val mm))) ()
	  "Index i out of bounds.")
  (symbol-macrolet ((col-j (svref (csparse-cols mm) j)))
    (labels ((csparse-aset-aux0 (prev-head curr-head i)
	       ;; Tears down a sparsev, looking to set the i-th entry
	       ;; to 0.
	       (declare (type sparsev prev-head curr-head)
			(type nnfixnum i))
	       (unless (null curr-head)
		       ;; If curr-head null, do nothing
		 (let ((curr-i (caar curr-head)))
		   (declare (type nnfixnum curr-i))
		   ;; By previous work, (caar prev-head) < i.
		   (cond ((< i curr-i)) ; do nothing
			 ((= i curr-i)
			  ;; need to remove an entry
			  (rplacd prev-head (cdr curr-head)))
			 (t
			  ;; recurse
			  (csparse-aset-aux0
			   (cdr prev-head) (cdr curr-head) i))))))
	     (csparse-aset-aux1 (prev-head curr-head i val)
	       ;; Tears down a sparsev, looking to set the i-th entry
	       ;; to val.
	       (declare (type sparsev prev-head curr-head)
			(type nnfixnum i)
			(integer val))
	       (if (null curr-head)
		   (nconc prev-head (list (cons i val)))
		 (let ((curr-i (caar curr-head)))
		   (declare (type nnfixnum curr-i))
		   (cond ((< i curr-i)
			  (rplacd prev-head
				  (cons (cons i val)
					curr-head)))
			 ((= i curr-i)
			  (rplacd prev-head
				  (cons (cons i val)
					(cdr curr-head))))
			 (t
			  (csparse-aset-aux1
			   (cdr prev-head) (cdr curr-head) i
			   val)))))))
      (let ((v col-j))
	(declare (type sparsev v))
	(cond ((zerop val)
	       (cond ((null v)) ; do nothing
		     ((null (cdr v)) ; v has one elt
		      (let ((i0 (caar v)))
			(declare (type nnfixnum i0))
			(when (= i0 i)
			      ;; The [unique] elt must be removed
			      (setf col-j '())
			  ;; else, do nothing
			      )))
		     (t ; v has at least 2 elts
		      (let ((i0 (caar v)))
			(declare (type nnfixnum i0))
			(cond ((< i i0))
			      ((= i i0)
			       (setf col-j (cdr col-j)))
			      (t
			       (csparse-aset-aux0
				v (cdr v) i)))))))
	      (t ; val is non-zero
	       (cond ((null v)
		      (setf col-j (list (cons i val))))
		     ((null (cdr v)) ; v has one elt
		      (let ((i0 (caar v)))
			(declare (type nnfixnum i0))
			(cond ((> i0 i)
			       (setf col-j
				     (push (cons i val) col-j)))
			      ((= i0 i)
			       (setf col-j (list (cons i val))))
			      (t
			       (setf col-j
				     (nconc col-j
					    (list (cons i val))))))))
		     (t
		      (let ((i0 (caar v)))
			(declare (type nnfixnum i0))
			(cond ((< i i0)
			       (setf col-j (push (cons i val) col-j)))
			      ((= i i0)
			       (setf col-j (push (cons i val)
						 (cdr col-j))))
			      (t
			       (csparse-aset-aux1
				v (cdr v) i val)))))))))))
  ;; final value
  val)

;;; IN THE FUTURE, rewrite the next one to do destructive operations
;;; on the j1-th column, like csparse-aset or overwrite-sparsev-...

(defun csparse-col-alter (aa j1 factor j2)
  (declare (type csparse aa)
	   (type nnfixnum j1 j2)
	   (integer factor))
  "Arguments aa j1 factor j2.  Alters column j1 of the csparse aa by
adding factor * (column j2) to it.  Overwrites the result onto aa by
destructively altering the j1-th column."
  (when (zerop factor)
    ;; immediate exit, doing nothing
    (return-from csparse-col-alter nil))
  (let* ((c (csparse-cols aa))
	 (a (svref c j1))
	 (b (svref c j2))
	 (ans '()))
    (declare (type (simple-vector *) c)
	     (type sparsev a b ans))
    (setf (svref c j1) 
      (do-till-explicit-return
        (cond ((null a)
	       (return (nconc (nreverse ans)
			      (scalar-times-sparsev factor b))))
	      ((null b)
	       (return (nconc (nreverse ans) (copy-list a))))
	      (t
	       (let ((a1 (caar a))
		     (b1 (caar b)))
		 (declare (type nnfixnum a1 b1))
		 (cond ((< a1 b1)
			(push (pop a) ans))
		       ((> a1 b1)
			(push (cons b1
				    (* factor
				       (the integer (cdr (pop b)))))
			      ans))
		       (t ; here a1 = b1
			(let ((sum (+ (the integer (cdr (pop a)))
				      (the integer
					   (* factor
					      (the integer (cdr (pop b))))))))
			  (declare (integer sum))
			  (unless (zerop sum)
				  (push (cons a1 sum) ans))))))))))
    aa))

(defun csparse-col-swap (z j1 j2)
  (declare (type csparse z)
	   (type nnfixnum j1 j2))
  "Arguments z, j1, j2.  Permutes columns j1 and j2 of the csparse z.
Overwrites the result onto z."
  (let ((c (csparse-cols z)))
    (declare (type (simple-vector *) c))
    (psetf (svref c j1) (svref c j2)
	   (svref c j2) (svref c j1))
    z))

(defun copy-vector-contents (x y)
  (declare (simple-vector x y))
  "x and y are 1-dim'l arrays [simple-vector's] with indices starting
from 0.  The length of y must be >= that of x.  Result: x is
overwritten onto y."
  (let ((n (array-dimension x 0)))
    (declare (type nnfixnum n))
    (dotimes (i n)
      (declare (type nnfixnum i))
      (setf (svref y i)
	    (svref x i)))))

;;; Input/output.

(defun csparse-to-array (x)
  (declare (type csparse x))
  "Dumps the argument x, a csparse, into an ordinary 2-dimensional
array of integers.  Value returned is the array.  This is mostly for
printing out small matrices."
  (let* ((n (csparse-n-val x))
	 (c (csparse-cols x))
         (z (make-array (list (csparse-m-val x) n)
			:initial-element 0)))
    (declare (type nnfixnum n)
	     (type (simple-vector *) c)
	     (type (simple-array integer (* *)) z))
    (dotimes (j n)
      (declare (type nnfixnum j))
      (dolist (a (svref c j))
	;; a = (i . n)
	(setf (aref z (the nnfixnum (car a)) j)
	      (cdr a))))
    z))

;;; ---------- Methods for MATRIX's, using csparse's. ----------

(deftype matrix ()
  "The program provides a type representing m-by-n matrices over  the
integers.  Here are some matrix functions that are supported  [type
(doc 'make-matrix) , and so on, to see what they do]:
   (make-matrix m n)
   (make-id-matrix n)
   (user-input-matrix x)
   (matrix-dim x i)
   (matrix-ref x i j)
   (matrix-set x i j val)
   (matrix-zerop x)
   (id-matrix-p x)
   (minor-det x lyst1 lyst2)
   (det x)  ; det of a matrix+ is faster if latter is already computed
   (math-trace x) ; as opposed to the debugging tool `trace'
   (inverse x)
   add-matrix
   add-matrix-list
   mult
   scmult-matrix
   copy-of-matrix
   copy-matrix-to-matrix
   copy-identity-matrix-to-matrix
   ismith
A matrix will always be initialized to 0.  The indices i,j always
start at 0.
  In the present version, matrices are implemented as sparse data
structures whose entries are arbitrary-precision integers.  The
structures are called csparse's--see the documentation on  csparse .
The first implementations of Sheafhom used ordinary arrays of integers
[see the file plainzmat.lisp].  The implementations in January-April
1997 used sparse's [see def].  Other implementations could use
matrices over finite fields, or functions that compute things only as
needed (streams? lazy evaluation?)
  Vectors are thought of as *column* vectors throughout.  If V has
dimension n and W has dimension m, then a map from V to W is
represented by an m-by-n matrix A.  If x is an n-dimensional column
vector giving a point of V with respect to standard coordinates, then
the coordinates of its image are given by the m-dimensional column
vector Ax."
  'csparse)

(defun make-matrix (m n)
  (declare (type nnfixnum m n))
  "Inputs: m n.  Output: a new m-by-n matrix of integers, initialized to 0."
  (make-csparse-zero m n))

(defun make-id-matrix (n)
  (declare (type nnfixnum n))
  "Input: n.  Output: a new n-by-n matrix of integers, initialized to
the identity matrix."
  (make-csparse-id n))

(proclaim '(inline make-matrix make-id-matrix))

(defmacro matrix-dim (mm i)
  (declare (type nnfixnum i))
  "Inputs: mm, i.  mm is a matrix, and i = 0 or 1.  Output: the i-th
dimension of mm.  For instance, if mm is a 5-by-3 matrix, then
(matrix-dim mm 0) is 5 and (matrix-dim mm 1) is 3."
  (cond ((= i 0) `(csparse-m-val ,mm))
        ((= i 1) `(csparse-n-val ,mm))
        (t (error "Second argument must be 0 or 1."))))

(proclaim '(ftype (function (matrix nnfixnum nnfixnum) integer)
		  matrix-ref))

(defun matrix-ref (a i j)
  (declare (type csparse a)
	   (type nnfixnum i j))
  "Input: a i j.  Output: the (i,j)-th entry of the matrix a.  The
indices i,j start at 0."
  (assert (and (<= 0 i) (< i (csparse-m-val a))) ()
	  "Index i out of bounds.")
  (let ((v (svref (csparse-cols a) j)))
    (declare (type sparsev v))
    (dolist (pair v
		  0) ; value if i > (max index in v)
      (let ((i0 (car pair)))
	(declare (type nnfixnum i0))
	(cond ((= i i0)
	       (return (cdr pair)))
	      ((> i0 i)
	       (return 0)))))))

(defun matrix-set (A i j val)
  (declare (type csparse A)
	   (type nnfixnum i j)
	   (integer val))
  "Input: A i j val.  This sets the (i,j)-entry of matrix A to the
value val, discarding the old value.  It returns val."
  (csparse-aset A i j val))

(proclaim '(inline matrix-set))

(defun user-input-matrix (&rest rest)
  (declare (list rest))
  "This can be used in several ways:
(.) If the argument is a matrix, the user will be prompted to type in
    values, and these will be overwritten into the matrix.
(.) If the arguments are two non-negative integers, the function will
    create a matrix, and prompt the user to fill it in as in the first
    case.
(.) If the arguments are two non-negative integers and a list of
    integers, say m n L, then the function will create an m-by-n
    matrix and fill it with the entries of L in row-major order.  If L
    runs out, it will start over again with L from the beginning [like
    APL's `ravel' function, the one denoted rho].  Examples:
    (.) (user-input-matrix 2 2 '(5 6 7 8)) produces the matrix
       5 6
       7 8
    (.) (user-input-matrix 3 3 '(1 0 0 0)) produces the 3-by-3
        identity matrix
(.) If the argument is a nested list L like '((5 6 7) (8 9 10)), the
    program produces the 2-by-3 matrix
       5 6  7
       8 9 10
(.) If there are three arguments m n L, with L a nested list as in the
    last case, the program ignores the m and n and proceeds as in the
    previous case."
  (cond ((null rest)
	 (break
"You have to give user-input-matrix at least one argument.  Type
(doc 'user-input-matrix) at a Lisp prompt to see the various
options."))
        ((= 1 (length rest))
         (let ((x (car rest)))
           (etypecase x
             (matrix
	      (let* ((m (matrix-dim x 0))
		     (n (matrix-dim x 1))
		     (z (make-array (list m n) :element-type 'integer)))
		(declare (type nnfixnum m n)
			 (type (simple-array integer (* *)) z))
		(mat-in z)
		(dotimes (i m)
		  (declare (type nnfixnum i))
		  (dotimes (j n)
		    (declare (type nnfixnum j))
		    (csparse-aset x i j (aref z i j))))
		x))
             (list
               ;; List-of-lists case
               (locally
                 (declare (list x))
                 (let ((m (length x)))
                   (declare (type nnfixnum m))
                   (assert (listp (car x)) ()
"The argument
~S
should be a list of lists.  It is a list of something's, but
the first element is not a list." x)
                   (let ((n (length (car x))))
                     (declare (type nnfixnum n))
                     (assert (every #'(lambda (y)
                                        (and (listp y)
                                             (= n (length y))))
                                    (cdr x)) ()
"The argument
~S
should be a list of lists.  It is a list of something's, but not all
of its members are lists having the same length as the first member."
x)
                     (let ((ans (make-matrix m n)))
                       (declare (type matrix ans))
                       (do ((i 0 (1+ i))
                            (a x (cdr a)))
                           ((= i m)
                            ans) ; final value
                         (declare (type nnfixnum i))
                         (do ((j 0 (1+ j))
                              (b (car a) (cdr b)))
                             ((= j n))
                           (declare (type nnfixnum j))
                           (assert (integerp (car b)) ()
"In your list-of-lists argument
~S
the entry ~S is not an integer." x (car b))
                           (matrix-set ans i j (car b))))))))))))
        ((= 2 (length rest))
         (let ((m (car rest))
               (n (cadr rest)))
           (assert (and (typep m 'nnfixnum) (typep n 'nnfixnum)) ()
"You gave two arguments to the function
m = ~S
n = ~S
These should have been non-negative integers, stating that you want to
create an m-by-n matrix.  Type (doc 'user-input-matrix) at a Lisp
prompt for more information." m n)
           (user-input-matrix (make-matrix m n)))) ; recurse
        ((= 3 (length rest))
         (let ((x (third rest)))
           (assert (listp x) () "Third argument is not a list.")
           (if (listp (car x))
               (user-input-matrix x) ; recurse to nested-list case
             ;; Here is the ravel case
             (let ((m (first rest))
                   (n (second rest)))
               (declare (type nnfixnum m n))
               (let ((ans (make-matrix m n))
                     (circ (copy-list x)))
                 ;; Make circ circular.
                 (rplacd (last circ) circ)
                 (dotimes (i m)
                   (declare (type nnfixnum i))
                   (dotimes (j n)
                     (declare (type nnfixnum j))
                     (matrix-set ans i j (pop circ))))
                 ans)))))
        (t
         (error "Too many arguments.  Type (doc 'user-input-matrix) at
a Lisp prompt for more information."))))

(defun matrix-zerop (x)
  (declare (type csparse x))
  "Input: a matrix x.  Output is true if and only if x is the zero matrix."
  (every #'null (csparse-cols x)))

(defun id-matrix-p (x)
  (declare (type csparse x))
  "Input: a matrix x.  Output is true if and only if x is an identity matrix."
  (let ((m (csparse-m-val x))
	(n (csparse-n-val x)))
    (declare (type nnfixnum m n))
    (unless (= m n)
	    (return-from id-matrix-p nil))
    (let ((c (csparse-cols x)))
      (declare (type simple-vector c))
      (dotimes (j n
		  t)
	(declare (type nnfixnum j))
	(let ((col (svref c j)))
	  (declare (type sparsev col))
	  (unless (and (not (null col))
		       (null (cdr col))
		       (let ((pair (car col)))
			 (and (= j (the nnfixnum (car pair)))
			      (= 1 (the integer (cdr pair))))))
		  (return nil)))))))

(defmacro aset (mat i j val)
  "For dense-matrices.  (aset mat i j val) sets the i,j-th entry of
the dense-matrix mat to the value val."
  `(setf (aref ,mat ,i ,j) ,val))

#|
(defun extended-gcd (a b)
  (declare (integer a b))
  "Input: integers a,b.  Output: a list (u v d) of integers such that
d is the non-negative gcd of a and b, and u*a + v*b = d.  The program
jumps to a breakpoint if a and b are both 0.  If a divides b, then
v will be 0.
   We use Algorithm 1.3.6 in Henri Cohen, _A Course in Computational
Number Theory_ (Springer GTM 138), 1st edition."
  ;; SOMETIME: DOCUMENT THE CONDITIONS ON |U| AND |V|.
  (labels ((extended-gcd-step1 (a b)
	     (declare (type (integer 0 *) a b))
	     (cond ((zerop a)
		    (when (zerop b)
			  (break
"I am being asked to take the gcd of 0 and 0.  If you don't think this
is a problem, continue out of this break loop, and I will return the
answer (0 1 0), which means 0*0 + 1*0 = 0."))
		    (list 0 1 b))
		   ((zerop (mod b a))
		    ;; This is Cohen's "important remark" on p. 68.
		    ;; If a|b, it is important to have v = 0.
		    ;; Conversely, if a doesn't divide b, both |u| and
		    ;; |v| will be in the right range in the end.
		    (list 1 0 a))
		   (t
		    (extended-gcd-step2 a b 1 a 0 b))))
	   (extended-gcd-step2 (a b u d v1 v3)
	     (declare (type (integer 0 *) a b d)
		      (integer u v1 v3))
	     (cond ((zerop v3)
		    (list u
			  (the integer
			       (floor (- d (* a u)) b)) ; exact division
			  d))
		   (t
		    (extended-gcd-step3 a b u d v1 v3))))
	   (extended-gcd-step3 (a b u d v1 v3)
	     (declare (type (integer 0 *) a b d)
		      (integer u v1 v3))
	     (multiple-value-bind (q t3) (floor d v3)
	       (declare (integer q t3))
	       (extended-gcd-step2 a b v1 v3 (- u (* q v1)) t3))))
    (let ((ans (extended-gcd-step1 (abs a) (abs b))))
      ;; This is a stupid patch over the fact that Cohen's inputs are
      ;; nonnegative.
      (when (< a 0)
	    (setf (first ans) (- (the integer (first ans)))))
      (when (< b 0)
	    (setf (second ans) (- (the integer (second ans)))))
      ans)))
|#

;;; Input/output.

(defun mat-in (x)
  (declare (type dense-matrix x)) 
  "Input: a two-dimensional array [dense-matrix] of integers.  The
function prompts the user interactively for entries and puts them into
the array, overwriting any previous values."
  (princ
   (format () "
Please enter your ~D by ~D integer matrix row by row.  After each
entry, press <space>.  At the end of each row, press return."
	  (array-dimension x 0)
	  (array-dimension x 1)))
  (dotimes (i (array-dimension x 0)
	      x) ; final value returned
    (declare (type nnfixnum i))
    (princ "
Row ")
    (princ i)
    (princ "?")
    (dotimes (j (array-dimension x 1))
      (declare (type nnfixnum j))
      (aset x i j
	(let ((val (read)))
	  (assert (integerp val)
		  (val) "Your input was not an integer.")
	  val)))))

(proclaim '(ftype (function (dense-matrix) string)
		  mat-prn-to-string))

(defun mat-prn-to-string (x)
  (declare (type dense-matrix x))
  "Input: a two-dimensional array [dense-matrix] of integers x.
Produces a string whose printed version will be a visual
representation of x.  Current version is for integer matrices, and can
be slow."
  (when (member 0 (array-dimensions x) :test #'=)
    ;; No one wants to see the printed representation of a 100-by-0 matrix.
    (return-from mat-prn-to-string "
[empty]"))
  ;; First, find the longest-printing entry in each column.
  (let ((maxes (make-array (list (array-dimension x 1))
			   :element-type 'fixnum
			   :initial-element 0))
	(string-ans ""))
    (declare (simple-vector maxes)
	     (string string-ans))
    (dotimes (i (array-dimension x 0))
      (declare (type nnfixnum i))
      (dotimes (j (array-dimension x 1))
        (declare (type nnfixnum j))
	(let ((a (integer-print-length (aref x i j))))
	  (declare (fixnum a))
          (cond ((> a (the fixnum (aref maxes j)))
		 (setf (aref maxes j) a))))))
    (dotimes (i (array-dimension x 0))
      (declare (type nnfixnum i))
      (setq string-ans (concatenate 'string string-ans "
"))
      (dotimes (j (array-dimension x 1))
	(declare (type nnfixnum j))
        (dotimes (k (- (the fixnum (aref maxes j))
                       (the fixnum (integer-print-length (aref x i j)))))
	  (declare (fixnum k))
          (setq string-ans
		(concatenate 'string string-ans " ")))
        (setq string-ans
	      (concatenate 'string
			   string-ans
			   (the simple-base-string
                             (format nil "~D " (aref x i j)))))))
    ;; Final value returned:
    string-ans))

(defun two-mat-prn-to-string (x y)
  (declare (type dense-matrix x y))
  "Input: two two-dimensional arrays [dense-matrix's] of integers x,y,
of the same dimensions.  Produces a string whose printed version, in
its i,j-th entry, will contain the i,j-th entry of x, the symbol - ,
and the i,j-th entry of y.  Current version is for integer matrices,
and can be slow."
  (when (member 0 (array-dimensions x) :test #'=)
    ;; No one wants to see the printed representation of a 100-by-0 matrix.
    (return-from two-mat-prn-to-string "
[empty]"))
  ;; First, find the longest-printing entry in each column.
  (let ((maxes (make-array (list (array-dimension x 1))
			   :element-type 'fixnum
			   :initial-element 0))
	(string-ans ""))
    (declare (simple-vector maxes)
	     (string string-ans))
    (dotimes (i (array-dimension x 0))
      (declare (type nnfixnum i))
      (dotimes (j (array-dimension x 1))
        (declare (type nnfixnum j))
	(let ((a (the fixnum (+ (integer-print-length (aref x i j))
				(integer-print-length (aref y i j))))))
	  (declare (fixnum a))
          (cond ((> a (the fixnum (aref maxes j)))
		 (setf (aref maxes j) a))))))
    (dotimes (i (array-dimension x 0))
      (declare (type nnfixnum i))
      (setq string-ans (concatenate 'string string-ans "
"))
      (dotimes (j (array-dimension x 1))
	(declare (type nnfixnum j))
        (dotimes (k (- (the fixnum (aref maxes j))
                       (the fixnum (+ (integer-print-length (aref x i j))
				      (integer-print-length (aref y i j))))))
	  (declare (fixnum k))
          (setq string-ans
		(concatenate 'string string-ans " ")))
        (setq string-ans
	      (concatenate 'string
			   string-ans
			   (format nil "~D-~D "
				   (aref x i j)
				   (aref y i j))))))
    ;; Final value returned:
    string-ans))

(proclaim '(ftype (function (integer) fixnum)
		  integer-print-length))

(defun integer-print-length (n)
  (declare (integer n))
  "Input is an integer.  Output is the number of ASCII characters
required to print it."
  (labels ((integer-print-length-aux (n ans)
	     (declare (integer n)
		      (fixnum ans))
	     (cond ((minusp n)
		    (integer-print-length-aux (- n)
					      (the fixnum (1+ ans))))
		   ((<= n 9)
		    (the fixnum (1+ ans)))
		   (t
		    (integer-print-length-aux (floor n 10)
					      (the fixnum
						(1+ ans)))))))
    (integer-print-length-aux n 0)))

(defun sparsev-dot (v w)
  (declare (type sparsev v w))
  "Input: v and w, which are sparsev's.  Output: the dot product of v and w."
  (labels ((sparsev-dot-aux (v w ans)
	     (declare (type sparsev v w)
		      (integer ans))
	     (cond ((null v) ans)
		   ((null w) ans)
		   (t
		    (let ((j0 (caar v))
			  (j1 (caar w)))
		      (declare (type nnfixnum j0 j1))
		      (cond ((< j0 j1)
			     (sparsev-dot-aux (cdr v) w ans))
			    ((> j0 j1)
			     (sparsev-dot-aux v (cdr w) ans))
			    (t
			     (sparsev-dot-aux
			      (cdr v)
			      (cdr w)
			      (+ ans
				 (* (the integer (cdar v))
				    (the integer (cdar w))))))))))))
    (sparsev-dot-aux v w 0)))

(defun mult (a b)
  "Input: matrix's A and B.  Output: the matrix product AB.  For scalar
multiplication, see scmult-matrix.  For composing morphisms, see
compose."
  ;; If we ever need to multiply two things other than matrices, I'll
  ;; have to define a new version.
  (csparse-mult a b))

(defun csparse-mult (a b)
  (declare (type csparse a b))
  "Arguments: csparse's A and B.  Output: the matrix product AB, as a
csparse.  The arguments are not destructively affected."
  (let* ((m (csparse-m-val a))
	 (n (csparse-n-val a))
	 (nmin1 (1- n))
	 (p (csparse-n-val b))
	 (bb (csparse-cols b))
	 (c (make-csparse-zero m p))
	 (ptr (make-array (list n) :element-type 'sparsev)))
    (declare (type nnfixnum m n p)
	     (fixnum nmin1)
	     (type csparse c)
	     (type (simple-vector *) bb ptr))
    (assert (= n (the nnfixnum (csparse-m-val b))) ()
	    "Inner dimensions incompatible.")
    (copy-vector-contents (csparse-cols a) ptr)
    ;; Now, for each i, compute the i-th row as a sparsev.  Keep a
    ;; pointer to the top of each column, to find the rows more
    ;; efficiently.
    (dotimes (i m)
      (declare (type nnfixnum i))
      (let ((row-i '()))
	(declare (type sparsev row-i))
	(do ((j nmin1 (1- j)))
	    ((< j 0))
	  (declare (fixnum j))
	  (let ((col-head (svref ptr j)))
	    ;; pair = (i0 . n) for i0 >= i
	    (when (and (not (null col-head))
		       (= i (the nnfixnum (caar col-head))))
	      (push (cons j (cdar col-head)) row-i)
	      (pop (svref ptr j)))))
	;; For each k, find the product of row-i and the k-th col, and
	;; put it into the answer.
        (unless (null row-i)
          (dotimes (k p)
	    (let ((entry (sparsev-dot row-i (svref bb k))))
	      (declare (integer entry))
	      (when (not (zerop entry))
		    (push (cons i entry)
			  (svref (csparse-cols c) k))))))))
    ;; Now done, except that each column of c is reversed [i.e. it
    ;; has the i's in decreasing order].
    (dotimes (k p)
      (setf-self (svref (csparse-cols c) k)
		 (nreverse :self)))
    c))

(defun matrix-product-zerop (a b)
  (declare (type csparse a b))
  "Arguments: two matrix's, A and B.  The output is true iff the
matrix product A*B is zero.  The product itself is not stored."
  (let* ((n (csparse-n-val a))
	 (nmin1 (1- n))
	 (p (csparse-n-val b))
	 (bb (csparse-cols b))
	 (ptr (make-array (list n) :element-type 'sparsev)))
    (declare (type nnfixnum n p)
	     (fixnum nmin1)
	     (type (simple-vector *) bb ptr))
    (assert (= n (the nnfixnum (csparse-m-val b))) ()
	    "Inner dimensions incompatible.")
    (copy-vector-contents (csparse-cols a) ptr)
    ;; Now, for each i, compute the i-th row as a sparsev.  Keep a
    ;; pointer to the top of each column, to find the rows more
    ;; efficiently.
    (dotimes (i (csparse-m-val a)
		t) ; final value if you never hit a zero entry in product
      (declare (type nnfixnum i))
      (let ((row-i '()))
	(declare (type sparsev row-i))
	(do ((j nmin1 (1- j)))
	    ((< j 0))
	  (declare (fixnum j))
	  (let ((col-head (svref ptr j)))
	    ;; pair = (i0 . n) for i0 >= i
	    (when (and (not (null col-head))
		       (= i (the nnfixnum (caar col-head))))
	      (push (cons j (cdar col-head)) row-i)
	      (pop (svref ptr j)))))
	;; For each k, find the product of row-i and the k-th col.
	;; Stop immediately if the product is non-zero, but keep going
	;; if it is zero.
        (unless (null row-i)
          (dotimes (k p)
	    (declare (type nnfixnum k))
	    (unless (zerop (the integer
				(sparsev-dot row-i (svref bb k))))
		    (return-from matrix-product-zerop nil))))))))

(defun transpose (x)
  (declare (type csparse x))
  "Input: an m-by-n matrix x.  The function creates a new n-by-m
matrix, copies x-transpose into it, and returns it."
  (let* ((m (csparse-m-val x))
	 (n (csparse-n-val x))
	 (ans (make-csparse-zero n m)))
    (declare (type nnfixnum m n)
	     (type csparse ans))
    (do ((j (1- n) (1- j)))
	((< j 0))
      (declare (fixnum j))
      (dolist (pair (svref (csparse-cols x) j))
	(push (cons j (cdr pair))
	      (svref (csparse-cols ans) (the nnfixnum (car pair))))))
    ans))

(proclaim '(ftype (function (sparse) csparse)
		  convert-sparse-to-csparse))
(proclaim '(ftype (function (csparse) sparse)
		  convert-csparse-to-sparse))

(defun convert-sparse-to-csparse (x)
  (declare (type sparse x))
  "Input: a sparse x.  Output: the same matrix, as a csparse."
  (let* ((m (sparse-m-val x))
	 (n (sparse-n-val x))
	 (ans (make-csparse-zero m n)))
    (declare (type nnfixnum m n)
	     (type csparse ans))
    (do ((i (the fixnum (1- m)) (1- i)))
	((< i 0))
      (declare (fixnum i))
      (dolist (ce (svref (sparse-mat x) i))
        (declare (type codedv-elt ce))
	(let ((j (codedv-col-index ce))
	      (val (codedv-value ce)))
	  (declare (type nnfixnum j)
		   (integer val))
	  (push (cons i val)
		(svref (csparse-cols ans) j)))))
    ans))

(defun convert-csparse-to-sparse (x)
  (declare (type csparse x))
  "Input: a csparse x.  Output: the same matrix, as a sparse."
  (let* ((m (csparse-m-val x))
	 (n (csparse-n-val x))
	 (mat-ans (make-array (list m) :element-type 'codedv
			               :initial-element '()))
	 (cols-ans (map 'vector
			#'(lambda (lyst)
			    (declare (type sparsev lyst))
			    (mapcar #'car lyst))
			(csparse-cols x))))
    (declare (type nnfixnum m n)
	     (type (simple-vector *) mat-ans cols-ans))
    (do ((j (the fixnum (1- n)) (1- j)))
	((< j 0))
      (declare (fixnum j))
      (dolist (pair (svref (csparse-cols x) j))
        ;; pair = (i . val)
	(push (codedv-encode-jn j (the integer (cdr pair)))
	      (svref mat-ans (the nnfixnum (car pair))))))
    (make-sparse
     :mat mat-ans
     :cols cols-ans
     :row-lengths (map 'vector #'length mat-ans)
     :col-lengths (map 'vector #'length cols-ans)
     :m-val m
     :n-val n)))

;;; The interface between jac and matrix+'s:

;;; Current policy on ismith: it alone has the job of converting
;;; csparse's to sparse's, and converting the answers back.  Only
;;; csparse's can be fed to ismith.  Only sparse's can be fed to jac.

(defstruct (matrix+
	    (:predicate nil)) ; so that matrix+-p gives the p slot
  "Matrix+ is merely the type of the output of the function
(ismith ...).  See the documentation for  ismith ."
  (mat nil :type (or null matrix))
  (d nil :type (or null matrix))
  (p nil :type (or null matrix))
  (q nil :type (or null matrix))
  (pinv nil :type (or null matrix))
  (qinv nil :type (or null matrix))
  (rank 0 :type (integer 0 *))
  (det-parity 1 :type (integer -1 1))) ; really +/- 1

(defun ismith (mm)
  (declare (type csparse mm))
  "The input is a matrix M.   The function gives the Smith normal form
decomposition D = P M Q.  Here P, Q are square matrices of determinant
+/- 1.  D is a diagonal matrix diag( d_1, d_2, d_3, ...) of
non-negative integers (the rightmost 0 or more of the d_i may be 0's),
with d_1 | d_2 and d_2 | d_3 etc. [here | denotes `divides'].
   The output is of the type MATRIX+ .  If the output of (ismith M) is
stored in a variable m0, say, then (matrix+-mat m0) recovers M.
Calling (matrix+-d m0), (matrix+-p m0), and (matrix+-q m0) gives the
matrices D, P, Q respectively.  (matrix+-pinv m0) and (matrix+-qinv m0)
give P^(-1) and Q^(-1).  All of these are csparse's.  (matrix+-rank m0)
gives the rank of M.
   When M is square, the determinant of M is given by [determinant of
(matrix+-d m0)]*(matrix+-det-parity m0) --and to get this determinant,
call (det m0).
   If you want the det of M, but don't need its Smith normal form, it
is faster to call (det M), which uses Gauss-Bareiss.  Call
(det (copy-of-matrix M)) if you don't want M to be destroyed--
Gauss-Bareiss destroys its argument.
   The function ismith (and its workhorse jac) are not efficient for
large matrices, because they allow integers to blow up too fast during
the intermediate calculations.  The current version of Sheafhom uses
LLL methods, unless Smith normal form is needed for a specific
purpose.  See the documentation on kernel-etc-LLL for details."
  (let ((ans
	 (jac (convert-csparse-to-sparse mm)
              :p :q :pinv :qinv)))
    ;; We leave off the non-destructive option, since we can destroy
    ;; the sparse copy of the csparse mm.
    (declare (type mpdq ans))
    (make-matrix+
     :mat mm
     :d (convert-sparse-to-csparse (mpdq-d ans))
	;; We have to invert p <-> pinv, and q <-> qinv, because jac
	;; does M = P D Q and Sheafhom wants D = P M Q.
     :p (convert-sparse-to-csparse (mpdq-pinv ans))
     :pinv (convert-sparse-to-csparse (mpdq-pp ans))
     :q (convert-sparse-to-csparse (mpdq-qinv ans))
     :qinv (convert-sparse-to-csparse (mpdq-q ans))
     :rank (mpdq-rank ans)
     :det-parity (mpdq-det-parity ans))))

;;; Other useful functions.

(defun overwrite-sparsev-onto-sparsev (v w start end)
  (declare (type sparsev v w)
	   (fixnum start end))
  "Input: v w start end.  Returns a sparsev which has v overwritten
onto w in positions start <= i < end.  The 0-th entry of v is written
onto the start-th entry of w, etc.  If v has non-zero entries in the
(end-start)'th position or later, they are ignored."
  ;; See comments near the deftype of sparsev about sharing list
  ;; structure.
  (assert (<= start end) ()
	  "It is required that start <= end.")
  (labels ((get-last-cons-before-start (w start)
	     (declare (type sparsev w)
		      (fixnum start))
	     (if (null w)
		 nil
	       (let ((i (caar w)))
		 (declare (type nnfixnum i))
		 (if (>= i start)
		     nil
		   (if (null (cdr w))
		       w
		     (get-last-cons-before-start-aux
		      w (cdr w) start))))))
	   (get-last-cons-before-start-aux (prev curr start)
	     (declare (type sparsev prev curr)
		      (fixnum start))
	     (if (null curr)
		 prev
	       (let ((i (caar curr)))
		 (declare (type nnfixnum i))
		 (if (>= i start)
		     prev
		   (get-last-cons-before-start-aux
		    (cdr prev) (cdr curr) start)))))
	   (get-first-cons->=-end (w end)
	     (declare (type sparsev w)
		      (fixnum end))
	     (if (null w)
		 nil
	       (let ((i (caar w)))
		 (declare (type nnfixnum i))
		 (if (>= i end)
		     w
		   (get-first-cons->=-end (cdr w) end))))))
    (let* ((last-cons-left-w
	    ;; nil if there is no last cons strictly before start.
	    ;; Otherwise, is the appropriate cdd...ddr of w
	    (get-last-cons-before-start w start))
	   (v-active-length (- end start))
	   (shifted-v '())
	   (first-cons-right-w
	    (get-first-cons->=-end
	     (if (null last-cons-left-w)
		 w
	       last-cons-left-w)
	     end)))
      (declare (type sparsev last-cons-left-w first-cons-right-w
		     shifted-v)
	       (fixnum v-active-length))
      (dolist (pair v)
	(let ((i (car pair)))
	  (declare (type nnfixnum i))
	  (if (>= i v-active-length)
	      (return nil) ; from the dolist
	    (push (cons (the fixnum (+ start i))
			(cdr pair))
		  shifted-v))))
      ;; Result
      (cond ((null last-cons-left-w) ; no left section
	     ;; return this nreconc
	     (nreconc shifted-v first-cons-right-w))
	    (t
	     (rplacd ; alter w...
	      last-cons-left-w
	      (nreconc shifted-v first-cons-right-w))
	     ;; ...and now return w
	     w)))))

(defun copy-matrix-to-matrix (aa bb offset-i offset-j)
  (declare (type csparse aa bb)
	   (fixnum offset-i offset-j))
  "Arguments aa, bb, offset-i, offset-j.  The first two are matrix's.
Overwrites aa onto bb starting at the position (offset-i,offset-j) in
bb."
  (let* ((maa (csparse-m-val aa))
	 (naa (csparse-n-val aa))
	 (end-i (+ offset-i maa)))
    (declare (fixnum maa naa end-i))
    (assert (>= offset-i 0) ()
	    "Third argument negative.")
    (assert (>= offset-j 0) ()
	    "Fourth argument negative.")
    (assert (<= end-i (the nnfixnum (csparse-m-val bb))) ()
	    "Attempt to exceed (matrix-dim bb 0).  Type
(doc 'copy-matrix-to-matrix) for more information.")
    (assert (<= (the fixnum (+ offset-j naa))
                (the nnfixnum (csparse-n-val bb))) ()
            "Attempt to exceed (matrix-dim bb 1).  Type
(doc 'copy-matrix-to-matrix) for more information.")
    (do ((j 0 (1+ j))
	 (oj offset-j (1+ oj)))
	((= j naa)
	 bb) ; final answer
      (declare (fixnum j oj))
      (setf-self (svref (csparse-cols bb) oj)
		 (overwrite-sparsev-onto-sparsev
		  (svref (csparse-cols aa) j)
		  :self
		  offset-i
		  end-i)))))

(defun copy-identity-matrix-to-matrix (n bb offset-i offset-j)
  (declare (type csparse bb)
	   (fixnum n offset-i offset-j))
  "Arguments n, bb, offset-i, offset-j.  Overwrites the n-by-n identity
matrix onto bb starting at the position (offset-i,offset-j) in bb."
  (assert (>= offset-i 0) ()
	  "Third argument negative.")
  (assert (>= offset-j 0) ()
	  "Fourth argument negative.")
  (assert (<= (the fixnum (+ offset-i n))
	      (the nnfixnum (csparse-m-val bb))) ()
	  "Attempt to exceed (matrix-dim bb 0).  Type
(doc 'copy-matrix-to-matrix) for more information.")
  (assert (<= (the fixnum (+ offset-j n))
	      (the nnfixnum (csparse-n-val bb))) ()
	  "Attempt to exceed (matrix-dim bb 1).  Type
(doc 'copy-matrix-to-matrix) for more information.")
  (let ((end-i (+ offset-i n)))
    (declare (fixnum end-i))
    (do ((j 0 (1+ j))
	 (oj offset-j (1+ oj)))
	((= j n)
	 bb) ; final answer
      (declare (fixnum j oj))
      (setf-self (svref (csparse-cols bb) oj)
                 (overwrite-sparsev-onto-sparsev
		  (list (cons j 1)) ; fake sparsev for j-th col of id matrix
		  :self
		  offset-i
		  end-i)))))

(defun copy-of-matrix (aa &optional (i0 0) (i1 (csparse-m-val aa))
			            (j0 0) (j1 (csparse-n-val aa)))
  (declare (type csparse aa)
	   (fixnum i0 i1 j0 j1))
  "Arguments aa and [optionally] i0, i1, j0, j1.  Creates a new matrix
bb, of size i1-i0 by j1-j0, and copies into bb the entries of aa that
have i0 <= i < i1 and  j0 <= j < j1.  Value returned is bb.  If only
one argument aa is provided, the result is a copy of the whole of aa.
   Use (copy-of-matrix aa) when a function [like jac or
kernel-etc-LLL] will overwrite aa and you don't want to lose its
values."
  (let* ((ans (make-csparse-zero (the fixnum (- i1 i0))
				 (the fixnum (- j1 j0))))
	 (n (csparse-n-val ans)))
    (declare (type csparse ans)
	     (type nnfixnum n))
    (assert (and (<= 0 i0) (<= i1 (the nnfixnum (csparse-m-val aa)))
		 (<= 0 j0) (<= j1 (the nnfixnum (csparse-n-val aa)))) ()
	    "One of the numerical arguments is out of bounds.")
    (do ((j (the fixnum (1- n)) (1- j)) ; the to matrix
	 (oj (- j1 1) (1- oj))) ; the from matrix
	((< j 0))
      (declare (fixnum j oj))
      (let ((v (svref (csparse-cols aa) oj)))
	(declare (type sparsev v))
	;; Remove part of v above i0
	(till (or (null v)
		  (<= i0 (the nnfixnum (caar v))))
	  (pop v))
	(dolist (pair v)
	  (let ((ii (car pair))) ; in from matrix
	    (declare (type nnfixnum ii))
	    (if (>= ii i1)
		(return nil) ; loops back to the next j
	      ;; Add new entry to ans
	      (let ((iii (- ii i0))) ; in to matrix
		(declare (fixnum iii))
		(push (cons iii (cdr pair))
		      (svref (csparse-cols ans) j))))))))
    ;; Need to reverse the sparsev's in (csparse-cols ans).
    (dotimes (j n)
      (declare (type nnfixnum j))
      (setf-self (svref (csparse-cols ans) j)
		 (nreverse :self)))
    ans))

;;; Next was formerly (defmethod add (a b) ...)

(defun add-matrix (a b)
  "Inputs: A B, which are matrix's of the same dimensions.  Output:
the matrix A+B."
  (declare (type csparse a b))
  (let* ((m (csparse-m-val a))
	 (n (csparse-n-val a))
	 (c (make-csparse-zero m n)) ; holds the answer
	 (aa (csparse-cols a))
	 (bb (csparse-cols b))
	 (cc (csparse-cols c)))
    (declare (type csparse c)
	     (type (simple-vector *) aa bb cc)
	     (type nnfixnum m n))
    (assert (and (= m (the nnfixnum (csparse-m-val b)))
		 (= n (the nnfixnum (csparse-n-val b))))
	    () "Matrices are of different sizes.")
    (dotimes (j n
		c) ; final value
      (declare (type nnfixnum j))
      (setf (svref cc j)
	    (add-sparsev (svref aa j) (svref bb j))))))

(defun add-matrix-list (lyst m n &optional (count-id 0))
  (declare (fixnum m n count-id))
  "Arguments lyst, m, n, and optionally count-id.  Lyst is a list of 0
or more m-by-n matrix's.  The function adds the matrices and returns
their sum.  If count-id is non-zero, it adds on to the result that
many copies of the identity matrix.  [Obviously count-id can be
non-zero only if m = n.]"
;; This function doesn't have to check that every entry of lyst is
;; really m-by-n.  The caller will have done that.
  (assert (or (= m n) (zerop count-id)) ()
"Count-id is non-zero, indicating that we need to include some
identity matrices in the sum.  Yet m <> n.")
  (let ((ans (make-matrix m n)))
    (declare (type matrix ans)) ; either csparse or sparse
    (unless (zerop count-id)
      (dotimes (i m) ; m = n
	(declare (type nnfixnum i))
	(matrix-set ans i i count-id)))
    (dolist (a lyst)
      (declare (type matrix a))
      (setq ans (add-matrix ans a)))
    ans))

;;; Next one was formerly (defmethod scmult ((c integer) a) ...)

(defun scmult-matrix (c a)
  "Input: c A, where c is an integer and A is a matrix.  Output: the
matrix c times A."
  (declare (integer c)
	   (type csparse a))
  (let ((m (csparse-m-val a))
	(n (csparse-n-val a)))
    (declare (type nnfixnum m n))
    (cond ((zerop c)
	   (make-csparse-zero m n))
	  ((= c 1)
	   (make-csparse
	    :cols (map 'vector #'copy-list (csparse-cols a))
	    :m-val m
	    :n-val n))
	  (t
	   (make-csparse
	    :cols (map 'vector
		       #'(lambda (v)
			   (declare (type sparsev v))
			   (scalar-times-sparsev c v))
		       (csparse-cols a))
	    :m-val m
	    :n-val n)))))

(defun minor-det (x lysti lystj)
  (declare (type matrix x)
	   (list lysti lystj))
  "The arguments are x, lysti, lystj.  x is a matrix.  lysti is a list
of indices of certain of x's rows, and lystj a list of indices of
certain of x's columns.  The function returns the determinant of the
submatrix that includes only the rows and columns indicated in the
lyst's."
  (let ((k (length lysti)))
    (declare (type nnfixnum k))
    (assert (= k (length lystj))
	    () "Asked to take det of a non-square minor.")
    (let ((ans (make-array (list k k) :element-type 'integer)))
      (declare (type dense-matrix ans))
      (do ((a lysti (cdr a))
	   (i 0 (1+ i)))
	  ((null a))
	(declare (type nnfixnum i))
	(do ((b lystj (cdr b))
	     (j 0 (1+ j)))
	    ((null b))
	  (declare (type nnfixnum j))
	  (aset ans i j
		(matrix-ref x
			    (the nnfixnum (car a))
			    (the nnfixnum (car b))))))
      ;; Final value is det of the dense matrix:
      (det ans))))

(proclaim '(ftype (function (t) integer)
		  det))

(proclaim '(ftype (function (sparse nnfixnum nnfixnum) integer)
		  get-entry))

(defun det (x)
  "Input: x.  Output: the determinant of x.  Here x can be a matrix,
matrix+, qvsp-morphism, or dense-matrix.
  If x is a dense-matrix, (det x) will destroy x.  If x is of the
other types, det will not alter it."
  (cond ((typep x 'dense-matrix)
	 ;; Gauss-Bareiss is faster than ismith if all you need is the
	 ;; determinant.  The dense-matrix will be overwritten.
	 (gauss-bareiss x))
	((typep x 'csparse)
	 ;; Can't use Gauss-Bareiss, since we may have to use a sparse
	 ;; algorithm.
	 (let ((n (csparse-n-val x))
	       (y (jac ; no p's or q's.  Destroys the sparse copy.
		   (convert-csparse-to-sparse x))))
	   (declare (type nnfixnum n)
		    (type mpdq y))
	   (assert (= n (csparse-m-val x)) ()
		   "You asked for the determinant of a non-square matrix.")
	   (when (zerop n)
		 ;; I guess it's 1.
		 (return-from det 1))
	   (when (< (the nnfixnum (mpdq-rank y)) n)
		 (return-from det 0))
	   (let ((d-mat (mpdq-d y))
		 (ans 1))
	     (declare (type sparse d-mat)
		      (integer ans))
	     (dotimes (i n
			 ;; final result
			 (* (the (integer -1 1) (mpdq-det-parity y))
			    ans))
	       (declare (type nnfixnum i))
	       (setq ans (* ans (get-entry d-mat i i)))))))
	((typep x 'matrix+)
	 (let ((n (csparse-m-val (matrix+-mat x))))
	   (declare (type nnfixnum n))
	   (assert (= n (csparse-n-val (matrix+-mat x)))
		   () "You asked for the determinant of a non-square matrix.")
	   (when (zerop n)
		 ;; Well, I guess the det of a 0-by-0 is 1.
		 (return-from det 1))
	   (when (< (matrix+-rank x) n)
		 0)
	   (let ((d-mat (matrix+-d x))
		 (ans 1))
	     (declare (type matrix d-mat)
		      (integer ans))
	     (dotimes (i n
			 ;; final result:
			 (* (the (integer -1 1) (matrix+-det-parity x))
			    ans))
	       (declare (fixnum i))
	       (let ((dii (matrix-ref d-mat i i)))
		 (declare (integer dii))
		   (setq ans (* dii ans)))))))
	((typep x 'qvsp-morphism)
	 (det-qvsp-morphism x)) ; defined in homolalg.lisp
	 (t
	  (error "I can't find the determinant of an object of type ~A."
		 (type-of x)))))

(defun gauss-bareiss (mm)
  (declare (type dense-matrix mm))
  "Input: an n-by-n dense matrix mm of integers.  Output: the
determinant of mm.  We use Algorithm 2.2.6 in Cohen, _A Course in
Computational Algebraic Number Theory_, Springer GTM 138.  You could
find the determinant using (ismith mm), but Gauss-Bareiss is faster if
the determinant is all you want.
  WARNING: mm will be overwritten."
  (labels ((gauss-bareiss-step2 (mm n k c s)
	     (declare (type dense-matrix mm)
		      (fixnum n k)
		      (integer c s))
	     (if (= (the fixnum (1+ k))
		    n)
		 (* s (the integer (aref mm k k))) ; the final value!
	       (let ((p (aref mm k k)))
		 (declare (integer p))
		 (if (not (zerop p))
		     (gauss-bareiss-step4 mm n k c s p)
		   (let ((i (do ((i0 (1+ k) (1+ i0)))
				((= i0 n)
				 (return-from gauss-bareiss-step2 0))
			      (declare (fixnum i0))
			      (when (not (zerop
					  (the integer (aref mm i0 k))))
				    (return ; this binds i
				     i0)))))
		     (declare (fixnum i))
		     (do ((j k (1+ j)))
			 ((= j n))
		       (declare (fixnum j))
		       (psetf (aref mm i j) (aref mm k j)
			      (aref mm k j) (aref mm i j)))
		     (gauss-bareiss-step4 mm n k c (- s)
					  (aref mm k k)))))))
           (gauss-bareiss-step4 (mm n k c s p)
             (declare (type dense-matrix mm)
		      (fixnum n k)
		      (integer c s p))
             (do ((i (1+ k) (1+ i)))
		 ((= i n))
	       (declare (fixnum i))
	       (do ((j (1+ k) (1+ j)))
		   ((= j n))
		 (declare (fixnum j))
		 (let ((tt (- (* p (aref mm i j))
			      (* (aref mm i k)
				 (aref mm k j)))))
		   (declare (integer tt))
		   (aset mm i j (the integer (floor tt c)))))) ; exact division
             (gauss-bareiss-step2 mm n (1+ k) p s)))
  (let ((n (array-dimension mm 0)))
    (declare (type nnfixnum n))
    (assert (= n (the nnfixnum (array-dimension mm 1)))
	    () "You asked for the determinant of a non-square matrix.")
    (when (zerop n)
      ;; I guess it's 1.
      (return-from gauss-bareiss 1))
    (gauss-bareiss-step2 mm n 0 1 1))))

;;; ---------- LLL routines for finding ker/coker, etc. ----------

;;; BIG QUESTION HERE: lam stores what is ess'ly the set of dot
;;; products of all pairs of columns.  If the matrix being LLL'd is
;;; big, lam could take up too much storage.  We will store lam in a
;;; vector.

;;; It is surely possible to run LLL and recompute the lam's as
;;; needed, without storing them.  Does it save time, though?

(defun col-dot (a j0 j1)
  (declare (type csparse a)
	   (type nnfixnum j0 j1))
  "Arguments A, j0, j1.  Computes the dot product of the j0-th and
j1-st columns of the matrix A."
  (sparsev-dot (svref (csparse-cols a) j0)
	       (svref (csparse-cols a) j1)))

;;; Note: rowv-dot was an alias for sparsev-dot.

(defmacro svref-d (j)
  "For use with kernel-etc-LLL."
  `(if (= ,j -1)
       1
     (aref d ,j)))

;;; lam will be vector.  It is created by the LLL
;;; function.  The next few macros allow us to work with it.

(locally
 (declare (optimize (safety 0)))
 ;; This allows the compiler to believe that the product below really
 ;; will be an nnfixnum.  The material involving *max-n* in
 ;; kernel-etc-LLL will ensure that it is a fixnum.
 (defun lam-index (k l)
   "Input: k, l with k > l >= 0.  [k,l] is the index of an entry in a
strictly-lower-triangular matrix.  The output names the corresponding
entry using row-major order in the triangle.  Thus [1,0] -> 0, [2,0] -> 1,
[2,1] -> 2, [3,0] -> 3, etc."
   (declare (type nnfixnum k l))
   (the nnfixnum
	(+ (the nnfixnum
		(floor (the nnfixnum
			    (* k (the nnfixnum (1- k))))
		       2))
	   l))))
  
(proclaim '(ftype (function (nnfixnum nnfixnum) nnfixnum)
		  lam-index))

(defmacro get-lam (k l)
  `(the integer (svref lam (lam-index ,k ,l))))

(defmacro set-lam (k l val)
  `(the integer (setf (svref lam (lam-index ,k ,l)) ,val)))

(defmacro decf-lam (k l decrement)
  `(decf (the integer (svref lam (lam-index ,k ,l)))
	 (the integer ,decrement)))

(defmacro redi (l)
  ;; Cohen's sub-algorithm REDI(k,l).
  "For use with kernel-etc-LLL."
  `(unless (<= (the integer (abs (* 2 (the integer (get-lam k ,l)))))
	       (the integer (svref-d ,l)))
     (let* ((q (round (the integer (get-lam k ,l))
		      (the integer (svref-d ,l))))
	    (negative-q (- q)))
       (declare (integer q negative-q))
       (csparse-col-alter H k negative-q ,l)
       (csparse-col-alter Hinvtr ,l q k)
       (csparse-col-alter A k negative-q ,l)
       (decf-lam k ,l (* q (the integer (svref-d ,l))))
       (dotimes (i ,l) ; 0 <= i <= l-1
	 (declare (type nnfixnum i))
	 (decf-lam k i (* q (the integer (get-lam ,l i))))))))

(defmacro redi2 (l)
  ;; Same as REDI above, but omits reference to H and Hinv, and works
  ;; on Ainvtr as well as A.
  "For use with kernel-etc-LLL."
  `(unless (<= (the integer (abs (* 2 (the integer (get-lam k ,l)))))
	       (the integer (svref-d ,l)))
     (let* ((q (round (the integer (get-lam k ,l))
		      (the integer (svref-d ,l)))))
       (declare (integer q))
       (csparse-col-alter A k (- q) ,l)
       (csparse-col-alter Ainvtr ,l q k)
       (decf-lam k ,l (* q (the integer (svref-d ,l))))
       (dotimes (i ,l) ; 0 <= i <= l-1
	 (declare (type nnfixnum i))
	 (decf-lam k i (* q (the integer (get-lam ,l i))))))))

(defun kernel-etc-LLL (A)
  (declare (type matrix A))
  "Input: a matrix A.  Output: a 5-element list

        (left(H) right(H) top(Hinv) bottom(Hinv) kerdim).

If A is m-by-n, then H is n-by-n and invertible over Z [i.e. has det
+/- 1], and Hinv is the inverse of H.  The product A*H = (0 | B),
where the number of columns in the left block is by definition kerdim,
and B has linearly independent columns.  Thus the leftmost kerdim
columns of H, which by definition are the matrix left(H), form a basis
for the kernel of A.  Let right(H) be the matrix formed by the other
columns of H.  Let top(Hinv) be the matrix given by the topmost kerdim
rows of Hinv, and bottom(Hinv) the matrix given by the other rows.
These have related interpretations--for instance, top(Hinv) is a
retraction for the kernel [see doc. on ker-retraction].  An exception
is that the four values left(H), etc. will be nil in the trivial cases
where the kernel subspace is either zero or all of the source.
   left(H) will be LLL-reduced.  right(H) will be LLL-reduced as much
as it can be, given that the subspace spanned by the left(H) cannot be
disturbed.  To learn how to suppress these LLL-reductions of left(H)
and right(H), type (doc '*simplify-matrix-results*) at a Lisp prompt.
   We use Algorithm 2.7.2 from Henri Cohen, _A Course in Computational
Number Theory_ (Springer GTM 138), 1st edition.  At the final stage,
however, we try to make left(H) and right(H) nicer.  To get left(H)
and right(H) LLL-reduced, we will call Algorithm 2.6.7 (modified) from
the same book; the latter is embodied in glnz-LLL-with-subspace.
   WARNING: the matrix A will be OVERWRITTEN.  If you don't want this
to happen, use (kernel-etc-LLL (copy-of-matrix A))."
  ;; Cohen's 2.7.1 makes left(H) and B LLL-reduced, while only making
  ;; right(H) `small'.  Cohen's 2.7.2 pays no attention to either
  ;; right(H) or B.
  ;;    In Cohen, the index j goes from 1 to n.  Here, it's from 0 to n-1.
  ;;    This code should work whether the type matrix is defined to be
  ;; csparse or sparse.
  (labels ((kernel-etc-LLL-end (H Hinvtr n kerdim)
	    (declare (type (or null matrix) H Hinvtr)
		     (type nnfixnum n kerdim))
	    ;; Returns the 5-element list
	    ;;    (left(H) right(H) top(Hinv) bottom(Hinv) kerdim),
            ;; transposing Hinvtr as it goes
	    ;; along.  The notation is as in the documentation on
	    ;; kernel-etc-LLL.    Would it be faster to write a
	    ;; special routine that did the transposing and copy-ing
	    ;; at the same time?
	    (if (or (zerop kerdim) (= n kerdim))
		(list nil nil nil nil kerdim)
	      (list
	       (copy-of-matrix H 0 n 0 kerdim)
	       (copy-of-matrix H 0 n kerdim n)
	       (transpose (copy-of-matrix Hinvtr 0 n 0 kerdim))
	       (transpose (copy-of-matrix Hinvtr 0 n kerdim n))
	       kerdim))))
  (prog* ((n (matrix-dim A 1))
	  (H (make-matrix n n))
	  ;; The next one will hold H-inverse-transpose until the last
	  ;; minute, since csparse's are adapted to column operations.
	  (Hinvtr (make-matrix n n))
	  (f (make-array (list n) :element-type 'bit))
	  (k 1) ; will have 1 <= k < n throughout
	  (kmax 0)
	  (d (make-array (list n) :element-type 'integer))
	  ;; d_{-1}, Cohen's d_0 = 1, will be treated separately with
	  ;; the svref-d function.
	  (lam (make-array (list (the nnfixnum
				      (floor
				       (locally
					(declare
					 (optimize (safety 0)))
					(the nnfixnum
					     (* n (the fixnum (1- n)))))
				       2)))
			   :element-type 'integer))
	  ;; See comments above on whether lam takes too much space.
	  ;; Currently, it is a simple-vector of integers, with an
	  ;; indexing scheme goverened by lam-index.
	  )
    (declare (type nnfixnum n k kmax)
	     (type matrix H Hinvtr)
	     (type simple-vector lam)
	     (bit-vector f)
	     (type (simple-array integer (*)) d))
    (assert (<= n *max-n*) ()
"You are working with a ~D-by~D matrix.  The second value, n, is so
big that the largest possible value of lam-index, n*(n+1)/2 - 1,  will
no longer be a fixnum.  I'm surprised the system hasn't crashed yet,
since it has already allocated a vector of length n*(n+1)/2.  If you
want to go on, remove the `safety 0' and nnfixnum declarations in
lam-index, remove the present assert form in kernel-etc-LLL, recompile
this file, and see what happens."
            (matrix-dim A 0) n)
    ;; H and Hinvtr are initially 0.  Make them = Id.
    (dotimes (i n)
      (declare (type nnfixnum i))
      (matrix-set H i i 1)
      (matrix-set Hinvtr i i 1))
    ;; Quick exit if n = 0 or 1.
    (when (= n 0)
      (return-from kernel-etc-LLL
	(kernel-etc-LLL-end H Hinvtr 0 0)))
    (when (= n 1)
      (return-from kernel-etc-LLL
	(kernel-etc-LLL-end H Hinvtr 1 (if (matrix-zerop A) 1 0))))
    ;; A Barthel speed-up patch.  Moves all zero col's to the left
    ;; right away.  Overwrites A, H, Hinv as it goes along.
    (zeroes-to-left A 0 (1- (csparse-n-val A)) H Hinvtr)
    ;; More initialization.
    (let ((tt (col-dot A 0 0)))
      (declare (integer tt))
      (if (zerop tt)
	  (setf (aref d 0) 1
		(sbit f 0) 0)
	(setf (aref d 0) tt
	      (sbit f 0) 1)))
    ;; At this point, we have finished the initialization phase in
    ;; Step 1.

    step2 ; incremental Gram-Schmidt

    (when (<= k kmax)
      (go step3))
    (setq kmax k)
    (do ((j 0 (1+ j))) ; 0 <= j <= k
	((> j k))
      (declare (type nnfixnum j))
      (cond ((and (< j k)
		  (zerop (sbit f j)))
	     (set-lam k j 0))
	    (t
	     (let ((u (col-dot A k j)))
	       (declare (integer u))
	       (dotimes (i j) ; 0 <= i <= j-1
		 (unless (zerop (sbit f i))
		   (setq u (floor ; Cohen guarantees quotient is in Z
			    (-
			     (* (the integer (svref-d i))
				u)
			     (* (the integer (get-lam k i))
				(the integer (get-lam j i))))
			    (the integer (svref-d (1- i)))))))
	       (cond ((< j k)
		      (set-lam k j u))
		     (t
		      (if (zerop u)
			  (setf (aref d k) (svref-d (1- k))
				(sbit f k) 0)
			(setf (aref d k) u
			      (sbit f k) 1))))))))

    step3 ; test f_k = 0 and f_{k-1} <> 0

    (let ((kmin1 (1- k)))
      (declare (type nnfixnum kmin1))
      (unless (zerop (sbit f kmin1))
	(redi kmin1)) ; see macro above
      (cond ((and (not (zerop (sbit f kmin1)))
		  (zerop (sbit f k)))
	     ;; Cohen's sub-algorithm SWAPK(k)
	     (csparse-col-swap A k kmin1)
	     (csparse-col-swap H k kmin1)
	     (csparse-col-swap Hinvtr k kmin1)
	     (when (> k 1)
	       (dotimes (j kmin1) ; 0 <= j <= k-2
		 (let ((dummy (get-lam k j)))
		   (declare (integer dummy))
		   (set-lam k j (the integer (get-lam kmin1 j)))
		   (set-lam kmin1 j dummy))))
	     (let ((lamc (get-lam k kmin1)))
	       (declare (integer lamc))
	       (cond ((zerop lamc)
		      (setf (aref d kmin1) (svref-d (1- kmin1))
			    (sbit f kmin1) 0
			    (sbit f k) 1)
		      (set-lam k kmin1 0)
		      (do ((i (1+ k) (1+ i)))
			  ((> i kmax))
			(declare (type nnfixnum i))
			(set-lam i k (the integer (get-lam i kmin1)))
			(set-lam i kmin1 0)))
		     (t	; lamc <> 0
		      (do ((i (1+ k) (1+ i)))
			  ((> i kmax))
			(declare (type nnfixnum i))
			(set-lam i kmin1
				    (floor ; quotient will be in Z
				     (* lamc
					(the integer
					  (get-lam i kmin1)))
				     (the integer (svref-d kmin1)))))
		      (let* ((tt (svref-d k)))
			(declare (integer tt))
			(setf (aref d kmin1)
			      (floor
			       (the integer (expt lamc 2))
			       (the integer (svref-d kmin1))))
			(setf (aref d k)
			      (the integer (svref-d kmin1)))
			(do ((j (1+ k) (1+ j)))
			    ((>= j kmax)) ; k+1 <= j <= kmax-1
			  (declare (type nnfixnum j))
			  (do ((i (1+ j) (1+ i)))
			      ((> i kmax)) ; j+1 <= i <= kmax
			    (declare (type nnfixnum i))
			    (set-lam i j
					(floor
					 (* (the integer (get-lam i j))
					    (the integer (svref-d kmin1)))
					 tt))))
			(do ((j (1+ k) (1+ j)))
			    ((> j kmax))
			  (declare (type nnfixnum j))
			  (setf (aref d j)
				(floor
				 (* (the integer (svref-d j))
				    (the integer (svref-d kmin1)))
				 tt)))))))
	     (setq k (max 1 kmin1))
	     (go step3))
      (t
       (do ((l (- k 2) (1- l)))
	   ((< l 0))
	 (declare (fixnum l))
	 (unless (zerop (sbit f l))
		 (redi l)))
       (incf k))))

    step4 ; finished?

    (when (< k n)
      (go step2))
    ;; We're done!  First, let's find the dim of the kernel.
    (let ((kerdim n)) ; if all f's are 0, this will be right
      (declare (type nnfixnum kerdim))
      (dotimes (i n)
	(declare (type nnfixnum i))
	(when (not (zerop (sbit f i)))
	  (return ; from the dotimes
	   (setq kerdim i))))
      ;; Finally, call the lin-indep integer LLL algorithm to make
      ;; left(H) and [as much as possible] right(H) LLL-reduced.
      (return-from kernel-etc-LLL
	(let ((gl-ans (glnz-LLL-with-subspace H Hinvtr kerdim)))
	  (kernel-etc-LLL-end (first gl-ans)
			      (second gl-ans)
			      n
			      (third gl-ans))))))))

(defvar *simplify-matrix-results* nil
  "When Sheafhom computes the kernel [or cokernel, ker-retraction,
image, etc.] of a matrix M, the matrix K representing the kernel map
will often have large integer entries.  If *simplify-matrix-results*
is true, the system will simplify K by making a change of basis in the
kernel space.  In general, it would seem very important to make such
simplifications.  However, our applications seem to run faster if we
don't simplify the K's.  The default value of
*simplify-matrix-results* is therefore nil.  If you want to turn on
the simplification, type (setq *simplify-matrix-results* t) at a Lisp
prompt.")

(defun glnz-LLL-with-subspace (A Ainvtr &optional (fence 0))
  (declare (type matrix A Ainvtr)
	   (type nnfixnum fence))
  "Input: A and Ainvtr, with optional third argument fence (default 0).
Here A is an n-by-n integer matrix of determinant +/- 1, and Ainvtr is
its inverse-transpose.  The function performs the integer LLL
algorithm on the columns of A.  However, the columns j with 0 <= j <
fence are deemed to generate a distinguished subspace V of the column
space, and V is never disturbed.  The result is that left(A) is an
LLL-reduced basis of V, and all of A is as LLL-reduced as it can be
given the previous constraint.  The final value of Ainvtr is the
inverse-transpose of the final value of A.
  The output is a list (A Ainvtr fence).  WARNING: A and Ainvtr will be
OVERWRITTEN as this function goes along.
  The variable *simplify-matrix-results* [see doc.] controls whether
or not this function is called."
  ;; Exit, doing nothing, if it's asked not to simplify matrix
  ;; results.
  (unless *simplify-matrix-results*
    (return-from glnz-LLL-with-subspace
      (list A Ainvtr fence)))
  (prog* ((n (matrix-dim A 0))
	  (k 1)
	  (kmax 0)
	  (d (make-array (list n) :element-type 'integer))
	  (lam (make-array (list (the nnfixnum
				      (floor
				       (locally
					(declare
					 (optimize (safety 0)))
					(the nnfixnum
					     (* n (the fixnum (1- n)))))
				       2)))
			   :element-type 'integer))
	  ;; same comments on lam's wastefulness as above
	  )
    (declare (type nnfixnum n k kmax)
	     (type (simple-array integer (*)) d)
	     (type simple-vector lam))
    ;; d_{-1}, which is Cohen's d_0 = 1, is handled via the macro
    ;; (svref-d i) above.
    (assert (= n (matrix-dim A 1)) ()
"This function only works with square matrices, but it was given a
matrix of size ~D by ~D." n (matrix-dim A 1))
    ;; Quick exit when n < 2.  Here A is already LLL-reduced.
    (when (< n 2)
      (return-from glnz-LLL-with-subspace
	(list A Ainvtr fence)))
    (when (or (zerop fence) (= n fence))
      (return-from glnz-LLL-with-subspace
	(list nil nil fence)))
    ;; More initialization.
    (setf (aref d 0) (col-dot A 0 0))
    ;; This ends the initialization step 1.

    step2 ; incremental Gram-Schmidt

    (when (<= k kmax)
      (go step3))
    (setq kmax k)
    (do ((j 0 (1+ j)))
	((> j k))
      (declare (type nnfixnum j))
      (let ((u (col-dot A k j)))
	(declare (integer u))
	(dotimes (i j) ; 0 <= i <= j-1
	  (declare (type nnfixnum i))
	  (setq u
		(floor
		 (-
		  (* (svref-d i)
		     u)
		  (* (the integer (get-lam k i))
		     (the integer (get-lam j i))))
		 (svref-d (1- i)))))
	(cond ((< j k)
	       (set-lam k j u))
	      (t
	       (setf (aref d k) u)))))
    (assert (not (zerop (svref-d k))) ()
"The columns of the first argument are not linearly independent.")

    step3 ; test the Lovasz condition

    (let ((kmin1 (1- k)))
      (declare (type nnfixnum kmin1))
      (redi2 kmin1)
      (cond ((and
	      ;; If k is to the right of the fence (not in V), and
	      ;; kmin1 is to the left (in V), don't even _try_ to
	      ;; swap.
	      (not (= k fence))
	      ;; the Lovasz condition, with 3/4 replaced by 99/100
	      (< (* 100 (svref-d k) (svref-d (- k 2)))
		 (-
		  (* 99 (the integer (expt (the integer (svref-d kmin1)) 2)))
		  (* 100 (the integer (expt (the integer (get-lam k kmin1)) 2))))))
	     ;; Cohen's SWAPI(k)
	     (csparse-col-swap A k kmin1)
	     (csparse-col-swap Ainvtr k kmin1)
	     (when (> k 1)
	       (dotimes (j kmin1) ; 0 <= j <= k-2
		 (declare (type nnfixnum j))
		 (let ((dummy (get-lam k j)))
		   (declare (integer dummy))
		   (set-lam k j (the integer (get-lam kmin1 j)))
		   (set-lam kmin1 j dummy))))
	     (let* ((lamc (get-lam k kmin1))
		    (bb (floor
			 (+
			  (* (svref-d k) (svref-d (- k 2)))
			  (the integer (expt lamc 2)))
			 (svref-d kmin1))))
	       (declare (integer lamc bb))
	       (do ((i (1+ k) (1+ i)))
		   ((> i kmax))
		 (declare (type nnfixnum i))
		 (let ((tt (get-lam i k)))
		   (declare (integer tt))
		   (set-lam i k
			       (floor
				(-
				 (* (svref-d k)
				    (the integer (get-lam i kmin1)))
				 (* lamc tt))
				(svref-d kmin1)))
		   (set-lam i kmin1
			       (floor
				(+
				 (* bb tt)
				 (* lamc (the integer (get-lam i k))))
				(svref-d k)))))
	       (setf (aref d kmin1) bb))
	     (setq k (max 1 kmin1))
	     (go step3))
	    (t
	     (do ((l (- k 2) (1- l)))
		 ((< l 0))
	       (declare (fixnum l))
	       (redi2 l))
	     (incf k))))

    step4

    (when (< k n)
      (go step2))
    ;; Here you're done!
    (return ; from the prog 
     (list A Ainvtr fence))))

(defun zeroes-to-left (A left right H Hinvtr)
  "This is a subroutine for kernel-etc-LLL.  It moves all the
non-zero columns of A to the left right away.  Does the same column
swaps to H and Hinvtr [see the documentation on kernel-etc-LLL for
what these are].  Call it initially with left = 0 and right = [#
columns of A]-1."
  (declare (type csparse A H Hinvtr)
	   (fixnum left right))
  (if (>= left right)
      nil ; done!  Continue with kernel-etc-LLL.
    (let ((v (csparse-cols A)))
      (declare (simple-vector v))
      ;; Beware: we can't carry this along separately the whole time,
      ;; since (csparse-col-swap A) will be trying to change it.  Well,
      ;; perhaps we could, but I'd rather not risk it.
      (cond ((null (svref v left)) ; left-th col of A already zero
	     (zeroes-to-left A (1+ left) right H Hinvtr))
	    ((not (null (svref v right))) ; right-th col of A non-zero
	     (zeroes-to-left A left (1- right) H Hinvtr))
	    (t
	     ;; If you get here, matrix is
	     ;; (0 0 * ... 0 * *)
             ;;      L     R       <-- denoting left and right
	     ;; We need to do the swap.  Then inc/decrement both
	     ;; indices.
	     (csparse-col-swap A left right)
	     (csparse-col-swap H left right)
	     (csparse-col-swap Hinvtr left right)
	     (zeroes-to-left A (1+ left) (1- right) H Hinvtr))))))

;;; ------------------------------------------------------------
;;;
;;;              Material involving SPARSEs
;;;
;;; (Currently, this is only here so I don't have to rewrite jac.)

;;; Warning: do not say (make-array (list n) :element-type fixnum).
;;; You get a 1-D array of (unsigned-byte 32)'s, and the machine
;;; spends all its time switching from fixnum's to unsigned-byte
;;; 32's.

(defstruct (sparse
	    (:print-function (lambda (x stream depth)
			       (declare (type sparse x)
					(ignore depth))
			       (let ((m (sparse-m-val x))
				     (n (sparse-n-val x)))
				 (declare (type nnfixnum m n))
				 (if (or (> m 100) (> n 25))
				     (format stream
					     "[too big to print]")
				   (format stream "~A"
					   (mat-prn-to-string
					    (sparse-to-array x))))))))
  "This is the less space-efficient of our two data structures for a
sparse 2-dimensional array of integers.  The other structure is
csparse [see doc.]  We use sparse when we must find the Smith normal
form of a matrix [see doc. on ismith], because this calls for both row
and column operations on the matrix, while only column operations are
convenient for csparse's.
   The array is m by n, where m and n are respectively given
by the slots m-val and n-val.  The mat slot holds a vector of length
m, whose i-th row holds a codedv v_i representing the i-th row of the
matrix.  [A codedv is a kind of sparse integer vector--see its
documentation.]  The cols slot holds a vector of length n, whose j-th
column holds a sorted list of all the indices i such that the i,j-th
entry is non-zero.  The row-lengths and col-lengths slots hold vectors
whose k-th entries give the number of non-zeroes in the k-th row
[resp. k-th column].  These six slots are accessed using the functions
sparse-mat, sparse-cols, etc.
   The user should not have to manipulate sparse's directly."
  (mat (dummy) :type (simple-vector *))
  (cols (dummy) :type (simple-vector *))
  (row-lengths (dummy) :type (simple-vector *))
  (col-lengths (dummy) :type (simple-vector *))
  (m-val (dummy) :type nnfixnum)
  (n-val (dummy) :type nnfixnum))

(defstruct (mpdq)
  "Holds the output of the function jac for sparse's.  The user will
be calling ismith, not jac, and so should refer to the documentation
on ismith and matrix+ for more information."
  ;; The defaults will never be evaluated if mpdq's are only created
  ;; by jac.
  (main (make-sparse-zero 0 0)	:type sparse)
  (d (make-sparse-zero 0 0) :type (or null sparse))
  ;; The next one was p, but confusion arose because mpdq-p is
  ;; supposed to be the predicate that recognizes mpdq's.
  (pp (make-sparse-zero 0 0) :type (or null sparse))
  (q (make-sparse-zero 0 0) :type (or null sparse))
  (pinv (make-sparse-zero 0 0) :type (or null sparse))
  (qinv (make-sparse-zero 0 0) :type (or null sparse))
  (rank 0 :type nnfixnum)
  (det-parity 1 :type (integer -1 1)))

(defun make-sparse-zero (m n)
  (declare (type nnfixnum m n))
  "Makes an m-by-n sparse of 0's.  Example: (setf x (make-sparse-zero 4 5))."
  (make-sparse
   :mat (make-array (list m)
		    :element-type 'codedv
		    :initial-element '())
   :cols (make-array (list n)
		     :element-type 'list
		     :initial-element '())
   :m-val m
   :n-val n
   :row-lengths (make-array (list m)
			    ;; :element-type 'nnfixnum
			    :initial-element 0)
   :col-lengths (make-array (list n)
			    ;; :element-type 'nnfixnum
			    :initial-element 0)))

(defun make-sparse-id (n)
  (declare (type nnfixnum n))
  "Makes a sparse n-by-n identity matrix.  Example:
(setf x (make-sparse-id 4)).  The user would normally use the more
general functions make-matrix, make-qvsp-morphism,  or the interactive
function user-input-qvsp-morphism."
  (let ((mat1 (make-array (list n)
			  :element-type 'codedv
			  :initial-element '()))
	(col1 (make-array (list n)
			  :element-type 'list
			  :initial-element '())))
    (declare (type (simple-vector *) mat1 col1))
    (dotimes (i n)
      (declare (type nnfixnum i))
      (setf (svref mat1 i) (list i)) ; codedv
      (setf (svref col1 i) (list i)))
    (make-sparse
     :mat mat1
     :cols col1
     :m-val n
     :n-val n
     :row-lengths (make-array (list n)
			      ;; :element-type 'nnfixnum
			      :initial-element 1)
     :col-lengths (make-array (list n)
			      ;; :element-type 'nnfixnum
			      :initial-element 1))))

#|
;;; In a sparse, the elts of the COLS array don't have to be sorted by
;;; < , but we believe it saves time in COL-PERM if you do.  If you
;;; don't want to sort, replace the following function by CONS.

(defun safe-insert (n l)
  (declare (fixnum n))
  "Arguments n, l.  Inserts integer n into the proper spot in the list
l of strictly increasing integers.  [Here the integers are fixnums.
Doesn't destroy l, though the result may share a tail with l.]"
  (do ((a l (cdr a))
       (b '() (cons (car a) b)))
      ((or (null a) (< n (the fixnum (car a))))
       (append (nreverse b) (cons n a)))))
|#

;;; As long as no sparse(s) share(s) list structure among the entries
;;; of the sparse-mat and sparse-cols slots, the insertions don't have
;;; to be safe, either.  So I'll write:

(defun merge-insert (n l)
  (declare (fixnum n)
	   (list l))
  "Arguments n, l.  Inserts integer n into the proper spot in the list
l of strictly increasing integers.  [Here the integers are fixnums.]
The list l will be destructively altered."
  (merge 'list
	 (the list (list n))
	 l
	 #'(lambda (a b) (declare (fixnum a b)) (< a b))))

(deftype codedv ()
  "A codedv represents a sparse integer vector.  It is a list whose
items are either fixnums or dotted pairs:

          (...  A(i)  A(i+1)  (A(i+2) . B(i+2))  ...)

The Ai and Bi are integers, with (abs Ai) strictly increasing.  The
rules are: each Ai is a fixnum; if an item is a fixnum >= 0, the
vector has a +1 in the Ai position; if an item is a fixnum < 0, the
vector has a -1 in the |Ai| position; the presence of (Ai . Bi) says
there is a Bi in the Ai position.  A non-zero entry is NEVER
stored--i.e. all Bi are non-zero."
  ;; No sparse x should share any part of its list structure with
  ;; another sparse, since we want to be able to do destructive
  ;; operations on sparses.  By the same token, no row should share
  ;; list structure with another row in the same sparse, and ditto for
  ;; columns.  We may do destructive operations on an individual entry
  ;; in (sparse-mat x) or (sparse-cols x), as long as the result is
  ;; stored back in the same place.
  ;;   In fact, two different codedv's can run into trouble even if
  ;; have different top-level conses, but if their items [codedv-elts]
  ;; are equal.  For instance, say two codedv's both have a 3 in the
  ;; 0-th position, and both have their car's equal to the _same_ pair
  ;; (0 . 3).  Then one can never call (setf (cdr ...)) on this pair
  ;; to reset the value, since that will change the value in both
  ;; codedv's.
  ;;   Here is what the codedv type really is:
  'list)

(deftype codedv-elt ()
  "This is the type of an element of a codedv.  See codedv."
  '(or cons fixnum))

(proclaim '(ftype (function (codedv-elt) nnfixnum)
		  codedv-col-index))

(defun codedv-col-index (x)
  (declare (type codedv-elt x))
  "If x is an element of a codedv in the j-th position [called the
|Ai|-th position in the documentation on codedv], this function
returns the fixnum j."
  (if (typep x 'fixnum)
      (the nnfixnum (abs (the fixnum x)))
    (car x)))

(proclaim '(ftype (function (codedv-elt) integer)
		  codedv-value))

(defun codedv-value (x)
  (declare (type codedv-elt x))
  "If x is an element of a codedv, this function returns the value x
represents.  This is +1, -1, or is what is called Bi in the
documentation on codedv."
  (if (typep x 'fixnum)
      (if (>= x 0)
	  1
	-1)
    (cdr x)))

(proclaim '(ftype (function (nnfixnum integer) codedv-elt)
		  codedv-encode-jn))

(defun codedv-encode-jn (j n)
  (declare (type nnfixnum j)
	   (integer n))
  "Here j, n are integers [with j a fixnum], j >= 0 and n <> 0.
Result is the appropriate entry to put into a codedv.  See codedv."
  (cond ((= n 1) j)
	((= n -1)
	 (if (zerop j)
	     '(0 . -1) ; special case
	   (the fixnum (- j))))
	(t (cons j n))))

(deftype rowv ()
  "Rowv is an old name for sparsev."
  'list)

;;; Basic operations on sparsely-stored vectors.

(defun add-codedv-with-census (a b)
  (declare (type codedv a b))
  "The arguments a and b are codedv's.  Result is a list of
four elements:
 the actual sum a + b, as a codedv;
 born = list of indices which have entry =  0 in b but <> 0 in a+b;
 die  = list of indices which have entry <> 0 in b but =  0 in a+b;
 toll = (- (length born) (length die)).
N.B.: born and die aren't sorted.  If they're only ever going to
be used by ALTER-ROWS and ALTER-COLS, they don't need to be."
  (do ((ans '())
       (born '())
       (die '())
       (toll 0))
      (nil) ; exit via explicit returns
    (declare (type codedv ans)
	     (list born die)
	     (fixnum toll))
    (cond ((null a)
	   (return (list (nconc (nreverse ans) (copy-list b))
			 born
			 die
			 toll)))
	  ((null b)
	   (return (list (nconc (nreverse ans) (copy-list a))
			 (nconc born
				(mapcar #'codedv-col-index a))
			 die
			 (the fixnum (+ toll (length a))))))
	  (t
	   (let ((a1 (codedv-col-index (the codedv-elt (car a))))
		 (b1 (codedv-col-index (the codedv-elt (car b)))))
	     (declare (type nnfixnum a1 b1))
	     (cond ((< a1 b1)
		    (push (pop a) ans)
		    (push a1 born)
		    (incf toll))
		   ((> a1 b1)
		    (push (pop b) ans))
		   (t ; here a1 = b1
		    (let ((sum (+ (the integer
				       (codedv-value
					(the codedv-elt (car a))))
				  (the integer
				       (codedv-value
					(the codedv-elt (car b)))))))
		      (declare (integer sum))
		      (cond ((zerop sum)
			     (push a1 die)
			     (decf toll))
			    (t
			     (push (codedv-encode-jn a1 sum) ans)))
		      (pop a)
		      (pop b)))))))))

(defun scalar-times-codedv (s v)
  (declare (integer s)
	   (type codedv v))
  "Arguments s, v.  s is an integer.  v, and the result, are
codedv's.  The function performs scalar multiplication s times v."
  (cond ((zerop s) '())
	((= s 1) (copy-list v))
	((= s -1) (mapcar #'negate-codedv-elt v))
	(t (mapcar #'(lambda (x)
		       (declare (type codedv-elt x))
		       (cons (codedv-col-index x)
			     (* s
				(the integer (codedv-value x)))))
		   v))))

(defun negate-codedv-elt (x)
  (declare (type codedv-elt x))
  "x is a codedv-elt.  Returns the codedv-elt with the same
index and with negative value."
  (cond ((typep x 'fixnum)
	 (if (zerop x)
	     '(0 . -1)
	   (the fixnum (- (the fixnum x)))))
	((and ; '(0 . -1) case
	  (=  0 (the fixnum  (car x)))
	  (= -1 (the integer (cdr x))))
	 0)
	(t
	 (cons (car x) (- (the integer (cdr x)))))))

;;; The 3rd argument to ALTER-COLS should be a codedv, but the 3rd
;;; argument to ALTER-ROWS should be a rowv.

(defun get-col-as-rowv (m j)
  (declare (type sparse m)
	   (type nnfixnum j))
  "Arguments m, j.  Gets the j-th column of the sparse m, outputting
it as a rowv."
  (let ((ans '()))
    (declare (type rowv ans))
    (dolist (i (svref (sparse-cols m) j))
      (declare (type nnfixnum i))
      (let ((val (get-entry m i j)))
	(declare (integer val))
	(assert (not (zerop val))
		() "There is an explicit 0 entry in this sparse.")
	(push (cons i val)
	      ans)))
    (nreverse ans)))

(defun get-entry (m i j)
  (declare (type sparse m)
	   (type nnfixnum i j))
  "Arguments m, i, j.  Gives the i,j-th entry of the sparse m.  It
works even if the entry is 0."
  (dolist (x (svref (sparse-mat m) i)
	     0) ; final result if failure
    (declare (type codedv-elt x))
    (let ((j0 (codedv-col-index x)))
      (declare (type nnfixnum j0))
      (cond ((= j j0)
	     (return-from get-entry (codedv-value x)))
	    ((> j0 j)
	     (return-from get-entry 0))))))

(defun sparse-aset (mm i j val)
  (declare (type sparse mm)
	   (type nnfixnum i j)
	   (integer val))
  "Sets the i,j entry of the sparse mm to the value val, doing the
appropriate bookkeeping.  This destructively alters mm.  Value
returned is val."
  (cond ((zerop val)
	 (let ((x (svref (sparse-mat mm) i))
	       (y '()))
	   (declare (type codedv x y))
	   (do ()
	       ;; if you get stopped at the next termination clause,
	       ;; the i,j-entry was originally 0.  Do nothing.
	       ((null x)
		0)
	     (let ((j0 (codedv-col-index (the codedv-elt (car x)))))
	       (declare (type nnfixnum j0))
	       (cond ((= j j0)
		      ;; There was an old non-zero i,j value.  Delete
		      ;; it, and do appropriate bookkeeping.
		      (setf (svref (sparse-mat mm) i)
			    (nreconc y (cdr x))) ; omits (car x)
		      (setf-self (svref (sparse-cols mm) j)
				 (delete i (the list :self)
					 :test #'fixnum=
					 :count 1))
		      (decf (the nnfixnum (svref (sparse-row-lengths mm) i)))
		      (decf (the nnfixnum (svref (sparse-col-lengths mm) j)))
		      (return-from sparse-aset 0))
		     ((> j0 j)
		      ;; old entry was 0, so do nothing
		      (return-from sparse-aset 0))
		     (t
		      (push (pop x) y)))))))
	(t ; val <> 0
	 (let ((x (svref (sparse-mat mm) i))
	       (y '()))
	   (declare (type codedv x y))
	   (do ()
	       ((null x)
		;; Old value was 0, and new isn't.  Do an insertion.
		(setf-self (svref (sparse-mat mm) i)
			   (nconc :self
				  (list (codedv-encode-jn j val))))
		(setf-self (svref (sparse-cols mm) j)
			   (merge-insert i :self))
		(incf (the nnfixnum (aref (sparse-row-lengths mm) i)))
		(incf (the nnfixnum (aref (sparse-col-lengths mm) j)))
		val)
	     (let ((j0 (codedv-col-index (the codedv-elt (car x)))))
	       (declare (type nnfixnum j0))
	       (cond ((= j j0)
		      ;; There was an old value.  Since both old and
		      ;; new are non-zero, we need only change the
		      ;; value.
		      (setf (svref (sparse-mat mm) i)
			    (nreconc y
				     (cons (codedv-encode-jn j val)
					   (cdr x))))
		      (return-from sparse-aset val))
		     ((> j0 j)
		      ;; Old value was 0.  Again, need an insertion.
		      (setf (svref (sparse-mat mm) i)
			    (nreconc y
				     (cons (codedv-encode-jn j val)
					   x))) ; not (cdr x)
		      (setf-self (svref (sparse-cols mm) j)
				 (merge-insert i :self))
		      (incf (the nnfixnum (aref (sparse-row-lengths mm) i)))
		      (incf (the nnfixnum (aref (sparse-col-lengths mm) j)))
		      (return-from sparse-aset val))
		     (t
		      (push (pop x) y)))))))))

;;; The next several functions do basic row/column operations.
;;; (alter-rows m i1 v k) will add multiples of row i1 to each row
;;; a1, where v = (... (a1 . b1) ...) .   Alter-cols is def'd sim'ly,
;;; though there v is a codedv.  The multiple is by a factor of
;;; (round (/ b1 k)), integer division with abs. least remainder.
;;; Assumes i1 is never among the a1's.

(defun alter-rows (m i1 v k)
  (declare (type sparse m)
	   (type nnfixnum i1)
	   (type rowv v)
	   (integer k))
  "Arguments m, i1, v, k.  This performs several elementary row
operations all at once--it is designed to fit well with the routine
jac.  v is a rowv [see doc.] of the form (... (a1 . b1) ...).  For
each pair (a1 . b1) in v, we let c(a1) be b1/k rounded off to the
nearest integer, and we add c(a1) times row i1 to row a1.  The result
of these row operations is overwritten onto m.  We assume i1 is not
among the a1's."
  (let ((w (aref (sparse-mat m) i1))) ; the altering row, as a codedv
    (declare (type codedv w))
    (dolist (a v
	       m) ; final value
      (declare (cons a))
      (let* ((a1 (car a))
	     (b1 (cdr a))
	     (x (add-codedv-with-census
		 (scalar-times-codedv (round b1 k) w)
		 (aref (sparse-mat m) a1))))
	(declare (fixnum a1)
		 (integer b1))
	(setf (aref (sparse-mat m) a1)
	      (the codedv (first x)))
	(incf (the nnfixnum (aref (sparse-row-lengths m) a1)) ; toll
	      (the fixnum (fourth x)))
	(dolist (b (second x)) ; born
	  (declare (fixnum b))
	  (setf-self (aref (sparse-cols m) b)
		     (merge-insert a1 :self))
	  (incf (the nnfixnum (aref (sparse-col-lengths m) b))))
	(dolist (b (third x)) ; die
	  (declare (fixnum b))
	  (setf-self (aref (sparse-cols m) b)
		     (delete a1 (the list :self) :test #'fixnum= :count 1))
	  (decf (the nnfixnum (aref (sparse-col-lengths m) b))))))))

(defun alter-cols (m j1 v k)
  (declare (type sparse m)
	   (type nnfixnum j1)
	   (type codedv v)
	   (integer k))
  "Arguments m, j1, v, k.  This performs several elementary column
operations all at once--it is designed to fit well with the routine
jac.  v is a codedv [see doc.].  For each pair item e in v, with
index a1 and value b1, we let c(a1) be b1/k rounded off to the nearest
integer, and we add c(a1) times column j1 to column a1.  The result of
these column operations is overwritten onto m.  We assume j1 is not
among the a1's."
  (let ((w (mapcar #'(lambda (x)
		       (declare (type codedv-elt x))
		       (codedv-encode-jn
			(codedv-col-index x)
			(round (the integer (codedv-value x)) k)))
		   v)))
	;; w is a codedv which (up to rounding) equals 1/k * v.
    (declare (type codedv w))
    ;; 1/9/97: since a codedv `should' never have explicit 0's, let's
    ;; remove them
    (setq w (delete-if #'(lambda (z)
			   (declare (type codedv-elt z))
			   (and (consp z)
				(zerop (the integer (cdr z)))))
		       w))
    (dolist (a (get-col-as-rowv m j1)
	       m) ; final value
      ;; Since sparses are stored by rows, we don't add multiples of
      ;; the altering column to m; instead, for each c_{i,j1} in the
      ;; j1-th column we add c_{i,j1} * w to the i-th row of m.  In
      ;; the dolist, a is the pair (i . c_{i,j1}) .
      (declare (cons a))
      (let* ((a1 (car a))
	     (b1 (cdr a))
	     (x (add-codedv-with-census
		 (scalar-times-codedv b1 w)
		 (aref (sparse-mat m) a1))))
	(declare (fixnum a1)
		 (integer b1))
	(setf (aref (sparse-mat m) a1)
	      (the codedv (first x)))
	(incf (the nnfixnum (aref (sparse-row-lengths m) a1)) ; toll
	      (the fixnum (fourth x)))
	(dolist (b (second x)) ; born
	  (declare (fixnum b))
	  (setf-self (aref (sparse-cols m) b)
		     (merge-insert a1 :self))
	  (incf (the nnfixnum (aref (sparse-col-lengths m) b))))
	(dolist (b (third x)) ; die
	  (declare (fixnum b))
	  (setf-self (aref (sparse-cols m) b)
		     (delete a1 (the list :self) :test #'fixnum= :count 1))
	  (decf (the nnfixnum (aref (sparse-col-lengths m) b))))))))

(defun row-perm (z i1 i2)
  (declare (type sparse z)
	   (type nnfixnum i1 i2))
  "Arguments z, i1, i2.  Permutes rows i1 and i2 of the sparse z.
Overwrites the result onto z."
  (cond ((= i1 i2)
	 z) ; do nothing
	((> i1 i2)
	 (row-perm z i2 i1))
	(t
	 (let* ((grew (mapcar #'codedv-col-index
			      (aref (sparse-mat z) i1)))
		(shrunk (mapcar #'codedv-col-index
				(aref (sparse-mat z) i2)))
		(set-diffs
		 (mutual-set-difference grew shrunk '() '())))
	   (declare (list grew shrunk))
	   (psetf (aref (sparse-mat z) i1) (aref (sparse-mat z) i2)
		  (aref (sparse-mat z) i2) (aref (sparse-mat z) i1))
	   (psetf
	    (aref (sparse-row-lengths z) i1)
	      (aref (sparse-row-lengths z) i2)
	    (aref (sparse-row-lengths z) i2)
	      (aref (sparse-row-lengths z) i1))
	   (dolist (b (car set-diffs)) ; grew - shrunk
	     (declare (fixnum b))
	     (setf-self (aref (sparse-cols z) b)
			;; This merge and delete should be one
			;; special-purpose procedure.  Ditto for the
			;; next dolist.
			(merge-insert i2
				      (delete i1 (the list :self)
					      :test #'fixnum=
					      :count 1))))
	   (dolist (b (cdr set-diffs)) ; shrunk - grew
	     (declare (fixnum b))
	     (setf-self (aref (sparse-cols z) b)
		        (merge-insert i1
				      (delete i2 (the list :self)
					      :test #'fixnum=
					      :count 1))))
	   ;; Final value:
	   z))))

(defun col-perm-aux (v i1 i2)
  (declare (type codedv v)
	   (type nnfixnum i1 i2))
  "Arguments v, i1, i2.  Interchanges the i1 and i2 entries of the
codedv v.  It's required that i1 < i2.  Does not destructively alter
v, though the result may share a tail with the original v."
  (do ((a v (cdr a))
       (ein '())
       (zwei '())
       (x1 nil)
       (x2 nil)
       (b 0))
      ((or (null a)
	   ;; b will be set on each pass while a not null.
	   (> (the nnfixnum (setq b (the nnfixnum
					 (codedv-col-index (car a)))))
	      i2))
       ;; Final answer:
       (nconc (nreverse ein)
	      (when x2
		    (list (codedv-encode-jn i1 x2)))
	      (nreverse zwei)
	      (when x1
		    (list (codedv-encode-jn i2 x1)))
	      a))
    (declare (type codedv a ein zwei)
	     (type (or null integer) x1 x2)
	     (type nnfixnum b))
    (cond ((< b i1) (push (car a) ein))
	  ((= b i1) (setq x1 (codedv-value (car a))))
	  ((< b i2) (push (car a) zwei))
	  ((= b i2) (setq x2 (codedv-value (car a)))))))

(defun col-perm (z j1 j2)
  (declare (type sparse z)
	   (type nnfixnum j1 j2))
  "Arguments z, j1, j2.  Permutes columns j1 and j2 of the sparse z.
Overwrites the result onto z."
  (cond ((= j1 j2)
	 z) ; do nothing
	((> j1 j2)
	 (col-perm z j2 j1))
	(t
	 (let* ((x (aref (sparse-cols z) j1)) ; dummy
		(y (aref (sparse-cols z) j2)) ; dummy
		(b (prog ((x1 x) (y1 y) (ans '()))
		      ;; b will = sorted union of x and y
		      begin
		      (cond ((or (null x1) (null y1))
			     (return	;from the prog
			      (nconc (nreverse ans)
				     (copy-list x1)
				     (copy-list y1))))
			    (t
			     (let ((xa (car x1))
				   (ya (car y1)))
			       (declare (fixnum xa ya))
			       (cond ((< xa ya)
				      (push (pop x1) ans))
				     ((> xa ya)
				      (push (pop y1) ans))
				     (t
				      (push (pop x1) ans)
				      (pop y1))))))
		      (go begin))))
	   (declare (list x y b))
	   (setf (aref (sparse-cols z) j1) y
		 (aref (sparse-cols z) j2) x)
	   (psetf
	    (aref (sparse-col-lengths z) j1)
	      (aref (sparse-col-lengths z) j2)
	    (aref (sparse-col-lengths z) j2)
	      (aref (sparse-col-lengths z) j1))
	   (dolist (a b)
	     (declare (fixnum a))
	     (setf-self (aref (sparse-mat z) a)
			(col-perm-aux :self j1 j2)))
	   ;; final value:
	   z))))

(defparameter mwm-markowitz-rows-to-check 10
  "See minlentry-markowitz.")

(defvar mwm-markowitz-bestfew-is-filled nil
  "See minlentry-markowitz.")

(defun minlentry-markowitz (z i0)
  (declare (type sparse z)
	   (type nnfixnum i0))
  "Arguments z, i0.  This is the Markowitz minimal-entry function for
the Smith normal form [jac] routine.  z is a sparse.  The result (i j #)
means that the (i,j)-th entry contains an element # (ideally +/- 1),
in the region i >= i0, such that fill-in on the next step will be
minimized.  The result is nil iff the active area of the matrix is zero.
  Reference: `Data Structures, Algorithms, and Software for Sparse
Matrices', Iain S. Duff, in _Sparsity and its Applications_, ed.
David J. Evans, Camb. U. Press, 1985, pp. 13-15.
  There is a simpler function minlentry, which is used if the
Markowitz version fails.  It finds an entry of minimal absolute value
by a direct search."
    ;; The main function finds singleton rows very well, but we need to
    ;; add some code at the beginning to find singleton columns
    ;; quickly.
    (do ((j i0 (1+ j))
	 (col-ls (sparse-col-lengths z))
	 (n (sparse-n-val z)))
	((= j n)
	 nil) ; if no +/- 1 singleton cols, do clause does nothing
      (declare (type nnfixnum j n)
	       (simple-vector col-ls))
      (when (= 1 (the nnfixnum (svref col-ls j)))
	(let* ((i (car (svref (sparse-cols z) j)))
	       (v (get-entry z i j)))
	  (declare (type nnfixnum i)
		   (integer v))
	  (when (= (the integer (abs v)) 1)
	    (return-from minlentry-markowitz (list i j v))))))
    (let ((bestfew '())
	  ;; will hold ("length" "i") for the shortest few rows
	  ;; (indexed by i) with +/- 1's in the right places.  Sorted
	  ;; by > on car.
	  (m (sparse-m-val z))
	  (row-ls (sparse-row-lengths z))
	  (col-ls (sparse-col-lengths z))
	  (mat-non-0-flag nil)
	  (product 0)
	  (best-product nil)
	  (ans '()))
      (declare (list bestfew ans)
	       (type nnfixnum m product)
	       (type (or null nnfixnum) best-product)
	       (simple-vector row-ls col-ls))
      (setq mwm-markowitz-bestfew-is-filled nil)
      (do ((i i0 (1+ i)))
	  ((= i m))
	(declare (type nnfixnum i))
	(let ((l (aref row-ls i)))
	  (declare (type nnfixnum l))
	  (cond ((zerop l))
		((= l 1)
		 ;; singleton row
		 (setq mat-non-0-flag t)
		 (let* ((dummy (car (svref (sparse-mat z) i)))
			(dummy-val (codedv-value dummy)))
		   (declare (type codedv-elt dummy)
			    (integer dummy-val))
		   (when (= 1 (abs dummy-val))
		     (return-from minlentry-markowitz
				  (list i
					(codedv-col-index dummy)
					dummy-val)))))
		((null bestfew)
		 ;; any non-zero row to start  bestfew .
		 (setq bestfew (list (list l i))))
		((or (< l (the fixnum (caar bestfew)))
		     (and (not mwm-markowitz-bestfew-is-filled)
			  (= l (the fixnum (caar bestfew)))))
		 (setq bestfew
		       (minlentry-markowitz-aux (list l i) bestfew))))))
      (when (and (null bestfew)
		 (null mat-non-0-flag)) ; z = 0
	     (return-from minlentry-markowitz nil))
      (dolist (a (reverse bestfew)) ; a = (length-of-row-i i)
	(dolist (b (svref (sparse-mat z) (the fixnum (second a))))
		;; b in i-th row
	  (declare (type codedv-elt b))
	  (cond ((> (abs (the integer (codedv-value b))) 1))
		(t
		 (setq product
		    (the fixnum
			 (* (the fixnum
				 (1- (the fixnum (car a))))
			    (the fixnum
				 (1- (the fixnum
					  (svref col-ls
						 (the fixnum
						      (codedv-col-index
						       b)))))))))
		 ;; product = # of expected entries of fill-in
		 (cond ((zerop product)
			;; singleton column with entry +/- 1
			(return-from minlentry-markowitz
				     (list (second a)
					   ;; wasteful to recompute
					   (codedv-col-index b)
					   (codedv-value b))))
		       ((or (null best-product)
			    (< product best-product))
			(setq best-product product
			      ans (list (cadr a)
					(codedv-col-index b)
					(codedv-value b)))))))))
      (if (null ans)
	  (minlentry z i0)
	ans)))

(defun minlentry-markowitz-aux (elt lyst)
  (declare (list elt lyst))
  "Arguments elt, lyst.  Puts elt on lyst, sorted by > on car.  Each
element of lyst is a list (l i) of two fixnums.  The function chops
off the highest (leftmost) entry if the global variable
mwm-markowitz-bestfew-is-filled is non-nil."
  (let* ((car-elt (car elt))
	 (ans (do ((a lyst (cdr a))
		   (b '() (cons (car a) b)))
		  ((or (null a)
		       (>= car-elt (the fixnum (caar a))))
		   (nreconc b (cons elt a))))))
    (declare (fixnum car-elt))
    (cond (mwm-markowitz-bestfew-is-filled
	   (cdr ans))
	  (t
	   (when (= (length ans)
                    (the fixnum mwm-markowitz-rows-to-check))
		 (setq mwm-markowitz-bestfew-is-filled t))
	   ans))))

(defun minlentry (z i1)
  (declare (type sparse z)
	   (type nnfixnum i1))
  "Arguments: z, i1.  z is a sparse.  The function finds an element of
minimal non-zero absolute value # in the region i >= i1, and returns
(i j #), where # is in the i,j-th position.  Returns nil iff the i >=
i1 rows of the sparse are zero."
  (do ((i i1 (1+ i))
       (ans nil) ; at end, (i j #) ; during run, (# i j)
       (m (sparse-m-val z)))
      ((or (= i m)
	   (and (not (null ans))
		(= 1 (abs (the integer (car ans))))))
       (list (second ans) (third ans) (first ans)))
    (declare (type nnfixnum i m)
             (list ans))
    (dolist (v (svref (sparse-mat z) i))
      (declare (type codedv-elt v))
      (let ((b (codedv-value v)))
        (declare (integer b))
	(when (or (null ans)
		  (< (abs b) (abs (the integer (car ans)))))
	  (setq ans (list b i (codedv-col-index v)))
	  (when (= 1 (abs (the integer (car ans))))
		(return nil) ; from the dolist
		))))))

(defun jac (m &rest rest)
  (declare (type sparse m))
  "In JAC, which finds Smith normal form, the first argument m is a
sparse.  M = P D Q,  where
M is a given m-by-n sparse of integers, P and Q are square sparses
of determinant +/- 1 [i.e. P and Q are in GL(Z)], and D is a diagonal
sparse diag( d_1, d_2, d_3, ...) of integers, where d_1 | d_2
and d_2 | d_3 etc., with  |  denoting divides, and where all d_i are
non-negative (the last few may be 0's).  The function returns an mpdq
[see doc.] holding D and, if they were computed, P, Q, P^(-1) [called
pinv], and Q^(-1) [called qinv].
   Optional arguments to jac, after the first argument, are
        :p  :q  :pinv  :qinv  :non-destructive
Putting in one of the first four says, Do the work of computing and
remembering this matrix.  The latter says, Don't destroy m (default is
to destroy it).
   Since 1980, the author has called this function JAC because he
learned it as Theorem 3.8 of the celestial _Basic Algebra I_ by Nathan
Jacobson.  The author owns a sequence of nine stuffed-animal panda
bears, the second of which is named Jacobson."
  ;; For general integer matrices, we should use Cohen, GTM 138,
  ;; Algorithm 2.4.14.  E.g., if a matrix is
  ;;   (67 24)
  ;;   ( *  *)
  ;;   ( *  *),
  ;; then in the current version we'll have to do five operations on
  ;; the whole (long!) columns, corresponding to the five steps in the
  ;; Euc. algorithm  67, 24 --> 19, 5, 4, 1, 0 , before the top row is
  ;; cleared.  In Cohen's version, you solve 67 u + 24 v = gcd(67,24)
  ;; first, then do just one pair of column operations. Since most of
  ;; our pivots will +/- 1, it's probably not crucial that we recode
  ;; the algorithm a la Cohen.
  ;;    The :p, etc. are not keywords, since they aren't followed by
  ;; arguments like t or nil.  They're just tokens.
  (let* ((d nil) ; d will be a sparse, the one that's overwritten
	 (pinv nil)
	 (qinv nil)
	 (p-flag (member :p rest))
	 (q-flag (member :q rest))
	 (pinv-flag (or p-flag (member :pinv rest)))
	 (qinv-flag (or q-flag (member :qinv rest)))
	;; One of these flags is non-nil iff you're supposed to
	;; compute the corresponding matrix.
	 (det-parity 1))
    (declare (type (or null sparse) d pinv qinv)
             (type (integer -1 1) det-parity))
    (cond ((not (member :non-destructive rest))
	   ;; m will be overwritten
	   (setq d m))
	  (t
	   ;; let d be a copy of m
	   (setq d (make-sparse-zero (sparse-m-val m) (sparse-n-val m)))
	   (copy-vector-contents (sparse-mat m) (sparse-mat d))
	   (copy-vector-contents (sparse-cols m) (sparse-cols d))
	   (copy-vector-contents (sparse-row-lengths m)
				 (sparse-row-lengths d))
	   (copy-vector-contents (sparse-col-lengths m)
				 (sparse-col-lengths d))))
    (when pinv-flag
	  (setq pinv (make-sparse-id (sparse-m-val m))))
    (when qinv-flag
	  (setq qinv (make-sparse-id (sparse-n-val m))))
    (prog ((corner 0)
	   (b nil)
           (x nil)
           (n 0)
           (rk 0)
	   (dim (min (sparse-m-val d)
		     (sparse-n-val d))))
          (declare (type nnfixnum corner dim rk)
                   (type (or codedv rowv fixnum) x) ; old code!
                   (list b)
		   (integer n))
	  l1
	  ;; Permute a row and column to bring a good pivot into the
	  ;; active diagonal position.
	  (setq b (minlentry-markowitz d corner))
	  ;; b = (i j #)
	  (when (null b) ; <=> d = 0 from corner on down
		(go end))
	  (col-perm d corner (the fixnum (cadr b)))
          (unless (= corner (the fixnum (cadr b)))
            (setq det-parity (- det-parity)))
	  (when qinv-flag
		(col-perm qinv corner (the fixnum (cadr b))))
	  (row-perm d corner (the fixnum (car b)))
          (unless (= corner (the fixnum (car b)))
            (setq det-parity (- det-parity)))
	  (when pinv-flag
		(row-perm pinv corner (the fixnum (car b))))
	  l3
	  ;; Main part of the work, to try to clear out the corner-th
	  ;; row and col by using the corner-th diagonal entry.
	  (setq x (svref (sparse-mat d) corner))
	  ;; x = top active row, as a codedv
	  (alter-cols d
                      corner
                      (the codedv (cdr x))
                      (- (the integer
                           (codedv-value (the codedv-elt (car x))))))
	  (when qinv-flag
	    (alter-cols
	     qinv
	     corner
	     (the codedv (cdr x))
	     (- (the integer (codedv-value (the codedv-elt (car x)))))))
	  (setq x (get-col-as-rowv d corner)
		n (cdar x)) ; pivot entry
	  ;; x = leftmost active column, as a rowv
	  (alter-rows d corner (the rowv (cdr x)) (- n))
	  (when pinv-flag
		(alter-rows pinv corner (the rowv (cdr x)) (- n)))
	  (cond ((= 1 (abs n))
		 ;; if the corner entry was 1, then the corner-th row
		 ;; and col are clear now
		 (go l2))
		((or (not (= 1 (the nnfixnum
                                 (svref (sparse-row-lengths d) corner))))
		     (not (= 1 (the nnfixnum
                                 (svref (sparse-col-lengths d) corner)))))
		 ;; corner-th row and/or col aren't clear
		 (go l1)))
	  (setq x nil) ; x is now a dummy for the next do clause.
	  ;; Is every other active entry divisible by n?
	  (do ((i (1+ corner) (1+ i))
	       (y nil))
	      ((or (not (null x))
		   (= i (sparse-m-val d))))
	    (declare (type nnfixnum i)
		     (type codedv y))
	    (setq y (svref (sparse-mat d) i))
	    (dolist (z y)
	      (declare (type codedv-elt z))
	      (unless (zerop (mod (the integer (codedv-value z)) n))
		      (setq x i)
		      (return nil) ; from the dolist
		      )))
	  (when (null x)
		;; If every entry in active part is divisible by n, go
		;; to l2 to end this induction step.
		(go l2))
	  ;; Add 1 copy of the culprit row (index x) to top active
	  ;; row, and go back to l3.
	  (alter-rows d (the fixnum x) (list (cons corner 1)) 1)
	  (when pinv-flag
	    (alter-rows pinv (the fixnum x) (list (cons corner 1)) 1))
	  (go l3)
	  l2
	  ;; Ready to end the current induction step.
	  (incf corner)
	  (when (< corner dim)
		(go l1))
	  end
	  (do ((i 0 (1+ i)))
	      ((= i dim))
            (declare (type nnfixnum i))
	    (cond ((null (svref (sparse-mat d) i))
		   (return nil))
		   ;; leave this do--you've hit where diagonal becomes 0
		  (t
		   ;; Look for elem. divisors that will give torsion
		   (let ((a (codedv-value
			     (car (svref (sparse-mat d) i)))))
		     (declare (integer a))
		     (when (minusp a)
		       (setf-self (svref (sparse-mat d) i)
				  (scalar-times-codedv -1 :self))
		       (setq det-parity (- det-parity))
		       (when pinv-flag
			 (setf-self (aref (sparse-mat pinv) i)
				    (scalar-times-codedv -1 :self)))))
                   (incf rk))))
	  (return ; from the prog--final answer
	   (make-mpdq
	    :main m
	    ;; the value of MAIN will be either m or d, depending on
	    ;; the setting of :non-destructive.
            :d d
            :rank rk
            :det-parity det-parity
	    :pp (if p-flag
		    (glnz-inverse pinv)
		  nil)
	    :q  (if q-flag
		    (glnz-inverse qinv)
		  nil)
	    :pinv pinv
	    :qinv qinv)))))

(defun glnz-inverse (x)
  (declare (type sparse x))
  "x is a square sparse of determinant 1 or -1.  The output is the
sparse for the inverse of x.  This is designed to be used with the
current implementation of the function jac.  It is not very
efficient.  See the doc. on inverse for a version that's better
integrated with Sheafhom."
  ;; Proof it works: JAC finds pinv and qinv, inverses of p and q
  ;; resp., so that x = p * id * q.  Clearly x-inverse = qinv * pinv.
  (let ((z (jac x :pinv :qinv :non-destructive)))
    ;; not :p or :q, or we get into an infinite loop
    (sparse-mult (mpdq-qinv z) (mpdq-pinv z))))

(defun sparse-to-array (x)
  (declare (type sparse x))
  "Dumps the argument x, a sparse, into an ordinary 2-dimensional
array of integers.  Value returned is the array.  This is mostly for
printing out small matrices."
  (let ((z (make-array (list (sparse-m-val x) (sparse-n-val x))
		       :initial-element 0)))
    (declare (type (simple-array integer (* *)) z))
    (dotimes (i (sparse-m-val x))
      (declare (type nnfixnum i))
      (dolist (a (svref (sparse-mat x) i))
        (declare (type codedv-elt a))
	(setf (aref z i (the nnfixnum (codedv-col-index a)))
	      (codedv-value a))))
    z))

(defun sparse-zerop (a)
  (declare (type sparse a))
  "Input: the sparse a.  Output is true if and only if a is the zero matrix."
  (if (<= (the nnfixnum (sparse-m-val a))
	  (the nnfixnum (sparse-n-val a)))
      (every #'(lambda (row)
		 (declare (type codedv row))
		 (null row))
	     (sparse-mat a))
    (every #'(lambda (col)
	       (declare (list col))
	       (null col))
	   (sparse-cols a))))

;;; Here are some patches for elementary row/column operation
;;; functions on sparses.  The main versions are above, but their syntax
;;; [division by that fourth argument] is adapted to jac and is rather
;;; baroque.

(defmacro row-swap (A i1 i2)
  "Arguments A, i1, i2.  Interchanges two rows of the sparse A.
Overwrites the result onto the sparse."
  `(row-perm ,A ,i1 ,i2))
  
(defmacro col-swap (A j1 j2)
  "Arguments A, j1, j2.  Interchanges two columns of the sparse A.
Overwrites the result onto the sparse."
  `(col-perm ,A ,j1 ,j2))

(defmacro row-alter (A i1 factor i2)
  "Arguments A i1 factor i2.  Alters row i1 of the sparse A by adding
factor * (row i2) to it.  Overwrites the result onto the sparse."
  `(alter-rows ,A ,i2 (list (cons ,i1 ,factor)) 1))

(defmacro col-alter (A j1 factor j2)
  "Arguments A j1 factor j2.  Alters column j1 of the sparse A by
adding factor * (column j2) to it.  Overwrites the result onto the
sparse."
  `(alter-cols ,A ,j2 (list (codedv-encode-jn ,j1 ,factor)) 1))

(defun sparse-mult (a b)
  (declare (type sparse a b))
  "Arguments: two sparse's, A and B.  Output: their matrix product AB,
as a sparse.  The arguments are not destructively affected."
  (let* ((m (sparse-m-val a))
	 (n (sparse-n-val a))
	 (p (sparse-n-val b))
	 (c (make-sparse-zero m p)))
    (declare (type nnfixnum m n p)
	     (type sparse c))
    (assert (= n (the nnfixnum (sparse-m-val b))) ()
	    "Inner dimensions incompatible.")
    (do ((j (1- p) (1- j)))
	((< j 0)
	 c) ; final value
      (declare (fixnum j))
      (let ((w (get-col-as-rowv b j)))
	(declare (type rowv w))
	(do ((i (1- m) (1- i)))
	    ((< i 0))
	  (declare (fixnum i))
	  (let ((val (codedv-dot-rowv (svref (sparse-mat a) i) w 0)))
	    (declare (integer val))
	    (unless (zerop val)
	      (push (codedv-encode-jn j val)
		    (svref (sparse-mat c) i))
	      (push i (svref (sparse-cols c) j))
	      (incf (the nnfixnum (svref (sparse-row-lengths c) i)))
	      (incf (the nnfixnum (svref (sparse-col-lengths c) j))))))))))

(defun codedv-dot-rowv (v w ans)
  (declare (type codedv v)
	   (type rowv w)
	   (integer ans))
  "Arguments: v, w, ans.  Here v and w are sparse integer vectors--v
is a codedv and w is a rowv.  Output: the dot product of v and w.  The
user should call this function with the third argument initially 0."
  (cond ((null v) ans)
	((null w) ans)
	(t
	 (let ((j0 (codedv-col-index (the codedv-elt (car v))))
	       (j1 (caar w)))
	   (declare (type nnfixnum j0 j1))
	   (cond ((< j0 j1)
		  (codedv-dot-rowv (cdr v) w ans))
		 ((> j0 j1)
		  (codedv-dot-rowv v (cdr w) ans))
		 (t
		  (codedv-dot-rowv
		   (cdr v)
		   (cdr w)
		   (+ ans
		      (the integer
			   (*
			    (the integer
				 (codedv-value (the codedv-elt (car v))))
			    (the integer
				 (cdar w))))))))))))

(defun sparse-transpose (a)
  (declare (type sparse a))
  "Input: an m-by-n sparse x.  The function creates a new n-by-m
sparse, copies x-transpose into it, and returns it."
  (let* ((m (sparse-m-val a))
	 (n (sparse-n-val a))
	 (ans (make-sparse-zero n m)))
    (declare (type nnfixnum m n)
	     (type sparse ans))
    (do ((i (1- m) (1- i)))
	((< i 0))
      (declare (fixnum i))
      (dolist (x (svref (sparse-mat a) i))
	(declare (type codedv-elt x))
	(let ((j (codedv-col-index x))
	      (val (codedv-value x)))
	  (declare (type nnfixnum j)
		   (integer val))
	  (push (codedv-encode-jn i val)
		(svref (sparse-mat ans) j))
	  (push j (svref (sparse-cols ans) i))
	  (incf (the nnfixnum (svref (sparse-row-lengths ans) j)))
	  (incf (the nnfixnum (svref (sparse-col-lengths ans) i))))))
    ;; At this point, because the i's were traversed backwards, the
    ;; rows of ans are in the correct order, but the col-index-lists
    ;; need to be reversed.
    (let ((c (sparse-cols ans)))
      (dotimes (j m)
        (declare (type nnfixnum j))
        (setf-self (svref c j) (nreverse :self))))
    ans))

(defun overwrite-codedv-onto-codedv-with-census (v w start end)
  (declare (type codedv v w)
	   (fixnum start end))
  "Arguments: v w start end.  Computes a new codedv which has v
overwritten onto w in positions start <= i < end.  If v has non-zero
entries in the (end-start)'th position or later, they are ignored.
The function returns a list of four items:
  the new codedv
  born, a list of the indices i which were 0 in w but <> 0 in v
  die, a list of the indices i which were <> 0 in w but 0 in v
  toll = (- (length born) (length die))"
  ;; Result may share list structure with w at the end.
  (let ((w-head '())
	(die '())
	(born '())
	(shifted-v '()))
    (declare (list born die)
	     (type codedv w-head shifted-v))
    ;; Next loop strips off the unwanted early part of w, storing it
    ;; in w-head.
    (do ()
	((or (null w)
	     (>= (the nnfixnum (codedv-col-index (the codedv-elt (car w))))
		 start))
	 (setq w-head (nreverse w-head)))
      (push (pop w) w-head))
    ;; Strip off the unwanted middle part of w, storing the indices of
    ;; the elts in die.
    (do ()
	((null w)
	 (setq die (nreverse die)))
      (let ((i (codedv-col-index (the codedv-elt (car w)))))
	(declare (type nnfixnum i))
	(if (>= i end)
	    (return ; exit from this do
	     (setq die (nreverse die)))
	  (progn (push i die) (pop w)))))
    ;; Create the new middle section, and record the indices that are
    ;; born.
    (dolist (x v)
      (declare (type codedv-elt x))
      (let ((j (+ start (the nnfixnum (codedv-col-index x)))))
	(declare (fixnum j))
	(when (>= j end)
	  (return nil)) ; exit this do immediately
	(push j born)
	(push (codedv-encode-jn j (codedv-value x))
	      shifted-v)))
    (setq shifted-v (nreverse shifted-v))
    (setq born (nreverse born))
    ;; Now remove duplicates from born and die.
    (let ((b-d (mutual-set-difference born die '() '())))
      ;; Final value:
      (list
       (nconc w-head shifted-v w)
       (car b-d)
       (cdr b-d)
       (- (length (the list (car b-d)))
	  (length (the list (cdr b-d))))))))

(defun mutual-set-difference (b d b-ans d-ans)
  "Arguments: b d b-ans d-ans.  The first two are lists of distinct
integers [fixnums], sorted by < .  The user should call the function
with the last two arguments to nil.  The function computes b' =
(set-difference b d) and d' = (set-difference d b) simultaneously,
taking advantage of the sorted order.  The output is the pair (b' . d')."
  (cond ((null b)
	 (cons (nreverse b-ans)
	       (nreconc d-ans (copy-list d))))
	((null d)
	 (cons (nreconc b-ans (copy-list b))
	       (nreverse d-ans)))
	(t
	 (let ((b0 (car b))
	       (d0 (car d)))
	   (declare (fixnum b0 d0))
	   (cond ((< b0 d0)
		  (mutual-set-difference
		   (cdr b) d (cons b0 b-ans) d-ans))
		 ((> b0 d0)
		  (mutual-set-difference
		   b (cdr d) b-ans (cons d0 d-ans)))
		 (t ; (= b0 d0)
		  (mutual-set-difference (cdr b) (cdr d) b-ans d-ans)))))))

(defun copy-sparse-to-sparse (aa bb offset-i offset-j)
  (declare (type sparse aa bb)
	   (fixnum offset-i offset-j))
  "Arguments aa, bb, offset-i, offset-j.  The first two are sparses.
Overwrites aa onto bb starting at the position (offset-i,offset-j) in
bb."
  (let* ((maa (sparse-m-val aa))
	 (naa (sparse-n-val aa))
	 (end-j (+ offset-j naa)))
    (declare (fixnum maa naa end-j))
    (do ((i 0 (1+ i))
	 (oi offset-i (1+ oi)))
	((= i maa)
	 bb) ; final answer
      (declare (fixnum i oi))
      (let ((new (overwrite-codedv-onto-codedv-with-census
		  (svref (sparse-mat aa) i)
		  (svref (sparse-mat bb) oi)
		  offset-j
		  end-j)))
	(setf (svref (sparse-mat bb) oi)
	      (first new))
	(dolist (j0 (second new)) ; born
	  (declare (fixnum j0))
	  (setf-self (svref (sparse-cols bb) j0)
		     (merge-insert oi :self))
	  (incf (the fixnum (svref (sparse-col-lengths bb) j0))))
	(dolist (j0 (third new)) ; die
	  (declare (fixnum j0))
	  (setf-self (svref (sparse-cols bb) j0)
		     (delete oi (the list :self) :test #'= :count 1))
	  (decf (the fixnum (svref (sparse-col-lengths bb) j0))))
	(incf (the fixnum (svref (sparse-row-lengths bb) oi)); toll
	      (the fixnum (fourth new)))))))

(defun copy-identity-sparse-to-sparse (n bb offset-i offset-j)
  (declare (type sparse bb)
	   (fixnum n offset-i offset-j))
  "Arguments n, bb, offset-i, offset-j.  Overwrites the n-by-n identity
sparse onto bb starting at the position (offset-i,offset-j) in bb."
  (let ((end-j (+ offset-j n)))
    (declare (fixnum end-j))
    (do ((i 0 (1+ i))
	 (oi offset-i (1+ oi)))
	((= i n)
	 bb) ; final answer
      (declare (fixnum i oi))
      (let ((new (overwrite-codedv-onto-codedv-with-census
		  (list i) ; fake codedv for i-th row of id matrix
		  (svref (sparse-mat bb) oi)
		  offset-j
		  end-j)))
	(setf (svref (sparse-mat bb) oi)
	      (first new))
	(dolist (j0 (second new)) ; born
	  (declare (fixnum j0))
	  (setf-self (svref (sparse-cols bb) j0)
		     (merge-insert oi :self))
	  (incf (the fixnum (svref (sparse-col-lengths bb) j0))))
	(dolist (j0 (third new)) ; die
	  (declare (fixnum j0))
	  (setf-self (svref (sparse-cols bb) j0)
		     (delete oi (the list :self) :test #'= :count 1))
	  (decf (the fixnum (svref (sparse-col-lengths bb) j0))))
	(incf (the fixnum (svref (sparse-row-lengths bb) oi)); toll
	      (the fixnum (fourth new)))))))

(defun copy-of-sparse (aa &optional (i0 0) (i1 (sparse-m-val aa))
			            (j0 0) (j1 (sparse-n-val aa)))
  (declare (type sparse aa)
	   (fixnum i0 i1 j0 j1))
  "Arguments aa and [optionally] i0, i1, j0, j1.  Creates a new sparse
bb, of size i1-i0 by j1-j0, and copies into bb the entries of aa that
have i0 <= i < i1 and  j0 <= j < j1.  Value returned is bb.  If only
one argument aa is provided, the result is a copy of the whole of aa.
   Use (copy-of-sparse aa) when a function [like jac] will overwrite
aa and you don't want to lose its values."
  (let ((ans (make-sparse-zero (the fixnum (- i1 i0))
			       (the fixnum (- j1 j0)))))
    (declare (type sparse ans))
    (do ((i (the fixnum (1- (the fixnum (- i1 i0)))) (1- i)) ; the to matrix
	 (oi (- i1 1) (1- oi))) ; the from matrix
	((< i 0))
      (declare (fixnum i oi))
      (let ((v (svref (sparse-mat aa) oi)))
	(declare (type codedv v))
	;; Remove part of v left of j0
	(do ()
	    ((or (null v)
		 (<= j0 (the fixnum (codedv-col-index
				     (the codedv-elt (car v)))))))
	  (pop v))
	(dolist (x v)
	  (declare (type codedv-elt x))
	  (let ((jj (codedv-col-index x))) ; in from matrix
	    (declare (fixnum jj))
	    (when (>= jj j1)
	      (return nil)) ; loops back to the next i
	    ;; Add new entry to ans
	    (let ((jjj (- jj j0))) ; in to matrix
	      (declare (fixnum jjj))
	      (push (codedv-encode-jn jjj (codedv-value x))
		    (svref (sparse-mat ans) i))
	      (incf (the fixnum (svref (sparse-row-lengths ans) i)))
	      (push i (svref (sparse-cols ans) jjj))
	      (incf (the fixnum (svref (sparse-col-lengths ans) jjj))))))))
    ;; At this point, the lists in (sparse-cols ans) are in the
    ;; correct order, but those in (sparse-mat ans) are reversed.
    (dotimes (i (the fixnum (sparse-m-val ans)))
      (declare (fixnum i))
      (setf-self (svref (sparse-mat ans) i)
		 (nreverse :self)))
    ans))

(defun sparse-incf-entry (x i j addend)
  (declare (type sparse x)
	   (type nnfixnum i j)
	   (integer addend))
  "Arguments: x i j addend.  Adds addend to the i,j-th entry of the
sparse x, overwriting the result onto x."
  (let ((sum (add-codedv-with-census
	      (list (codedv-encode-jn j addend)) ; fake codedv
	      (svref (sparse-mat x) i))))
    (setf (svref (sparse-mat x) i) (first sum)) ; new row
    (incf (the nnfixnum (svref (sparse-row-lengths x) i))
	  (the fixnum (fourth sum))) ; toll
    (dolist (j0 (second sum)) ; born
      (declare (fixnum j0))
      (setf-self (svref (sparse-cols x) j0)
		 (merge-insert i :self))
      (incf (the nnfixnum (svref (sparse-col-lengths x) j0))))
    (dolist (j0 (third sum)) ; die
      (declare (fixnum j0))
      (setf-self (svref (sparse-cols x) j0)
		 (delete i (the list :self) :test #'= :count 1))
      (decf (the fixnum (svref (sparse-col-lengths x) j0))))
    
    x))

(defmacro sparse-decf-entry (x i j addend)
  "Arguments: x i j addend.  Subtracts addend from the i,j-th entry of
the sparse x, overwriting the result onto x."
  `(sparse-incf-entry ,x ,i ,j (- (the integer ,addend))))

