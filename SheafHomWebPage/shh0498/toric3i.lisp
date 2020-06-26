;;;                   The Toric Variety Routines

(proclaim '(optimize speed))

(defvar *load-automatically* t
  "If this is true [the default], then when you load a Sheafhom file,
all its subordinate files will be loaded automatically.  To disable
this feature, type
   (setf *load-automatically* nil)
at a Lisp prompt.")

(when *load-automatically*
  (load "sheaf"))

(proclaim '(function dim (t) nnfixnum))
(proclaim '(ftype (function (fixnum) fixnum)
		  zero-perversity sublogarithmic-perversity
		  middle-perversity logarithmic-perversity
		  top-perversity))

(defclass toric-variety ()
  ((ur :accessor ur :initarg :ur :type qvsp)
   (r-p :accessor r-p :initarg :r-p :type ranked-poset)
   (fan-edges :accessor fan-edges :initarg :fan-edges :type simple-vector)
   (normal-subspaces :accessor normal-subspaces
		     :initform (make-hash-table :test #'equal)
		     :type hash-table)
   (tangential-subspaces :accessor tangential-subspaces
			 :initform (make-hash-table :test #'equal)
			 :type hash-table)
   (tangential-algebras :accessor tangential-algebras
			:initform (make-hash-table :test #'equal)
			:type hash-table)))

(defclass sheaf (toric-variety)
  ((perversity :accessor perversity :initarg :perversity)
   (stalks :accessor stalks :initarg :stalks :type quiver-rep)
#|
   (stalk-to-single-of-adapteds
    :accessor stalk-to-single-of-adapteds 
    :initform (make-hash-table :test #'equal)
    :type hash-table)
   (quiver-adaption-maps
    :accessor quiver-adaption-maps
    :initform (make-hash-table :test #'equal)
    :type hash-table)
   (hcoh
    :accessor hcoh
    :initform nil
    :type (or null quiver-rep))
|#
   ))

#|
(defstruct sheaf-morphism
  (sou (dummy) :type sheaf)
  (tar (dummy) :type sheaf)
  (mat (dummy) :type hash-table)
  (hcoh-map nil :type (or null quiver-rep-map)))
|#

;;; --------------- Methods ---------------

(defmethod dim ((x toric-variety))
  ;; The complex dimension.
  (dim (ur x)))

(defmethod open-star (c (x toric-variety))
  (open-star c (r-p x)))

#|
(defmethod hypercoh :before ((x sheaf))
  (when (null (slot-value x 'hcoh))
    (setf (slot-value x 'hcoh)
	  (compute-section (r-p x) x))))
|#

(defmethod hypercoh ((x sheaf))
  ;; Make sure all the stalks have been created:
  (do-vertices (c (quiver-rep-r-p (stalks x)))
    (get-stalk c x))
  ;; Do the work.
  (cohomology (global-section (stalks x))))

(defmacro hypercohomology (x)
  `(hypercoh ,x))

(defmethod betti-numbers ((x sheaf))
  (dims (hypercoh x)))

(defun make-sheaf (x &optional (p #'top-perversity))
  ;; p = perversity function.  This function just sets up the
  ;; foundation for the recursion.  The quiver-rep in 'stalks' will
  ;; grow later as needed.
  (check-type x toric-variety)
  (let ((ans (make-instance 'sheaf
	       :ur (ur x)
	       :r-p (r-p x)
	       :fan-edges (fan-edges x)
	       :perversity p
	       :stalks (make-quiver-rep
			:r-p (r-p x)
			:vertices (with-equal-ans
				   (setf (gethash '() ans)
					 (make-tensor-object
					  (get-tangential-algebra '() x)
					  (make-cochain-cx
					   :objects (with-eql-ans
						     (setf (gethash 0 ans)
							   *groundfield*))
					   :maps (empty-hash-table)))))
			:edges (with-equal-ans)))))
    (setf (normal-subspaces ans) (normal-subspaces x))
    (setf (tangential-subspaces ans) (tangential-subspaces x))
    (setf (tangential-algebras ans) (tangential-algebras x))
    ans))

(defun get-stalk (c x)
  (declare (type poset-elt c)
	   (type sheaf x))
  (let ((stalk (gethash c (quiver-rep-vertices (stalks x)))))
    (cond
     ((not (null stalk))
      stalk)
     (t
      ;; compute, store, and return it
      (format t "~&~S" c)
      (print-time " ")
      (let* ((U
	      (puncture (open-star c x)))
	     (raw-stalks
	      (make-quiver-rep
	       :r-p U
	       :vertices (with-equal-ans
			  (do-vertices (pe U)
			    (setf (gethash pe ans)
				  (get-stalk pe x))))
	       :edges (with-equal-ans
		       (do-edges (pair U)
			 (setf (gethash pair ans)
			       (get-stalk-map pair x))))))
	     (adaption-mat
	      (with-equal-ans
	       (do-vertices (c1 U)
		 (setf (gethash c1 ans)
		       (adapt-to-Tpr c1 c x)))))
	     (adapted-quiver-rep
	      (make-quiver-rep
	       :r-p U
	       :vertices (with-equal-ans
			  (maphash
			   #'(lambda (pe ff)
			       (setf (gethash pe ans)
				     (sou ff)))
			   adaption-mat))
	       :edges (with-equal-ans
		       (do-edges (pair U)
			 ;; pair = (tau, sigma)
			 (setf (gethash pair ans)
			       (compose
				(inverse (gethash (car pair) adaption-mat))
				(compose
				 (gethash pair (quiver-rep-edges raw-stalks))
				 (gethash (cdr pair)
					  adaption-mat))))))))
#|
	     (adapted-quiver-map
	      (make-quiver-rep-map
	       :sou adapted-quiver-rep
	       :tar raw-stalks
	       :mat adaption-mat))
|#
	     (normal-quiver-rep
	      (make-quiver-rep
	       :r-p U
	       :vertices (with-equal-ans
			  (do-vertices (pe U)
			    (setf (gethash pe ans)
				  (from2
				   (gethash pe
					    (quiver-rep-vertices
					     adapted-quiver-rep))))))
	       :edges (with-equal-ans
		       (do-edges (pair U)
			 (setf (gethash pair ans)
			       (extract-f-from-id-tensor-f
				(gethash pair
					 (quiver-rep-edges
					  adapted-quiver-rep))))))))
	     (global-normal-data
	      (global-section-with-edge-morphisms normal-quiver-rep))
	     (tdeg
#|
              ;; In the present algorithm, this makes things worse,
	      ;; since truncation adds junk.
	      (if (normally-nonsingular-p c x)
		  0
|#
		(funcall (perversity x) (real-codim c x)))
	     (trunc-normal-map
	      (truncation-i (car global-normal-data) tdeg))
	     (trunc-normal-cx
	      (sou trunc-normal-map))
	     (trunc-cx
	      (make-tensor-object
	       (get-tangential-algebra c x)
	       trunc-normal-cx)))
	;; Now create the things we actually want.
	(setf (gethash c (quiver-rep-vertices (stalks x)))
	      trunc-cx)
	(map nil ; over (rest global-normal-data)
	     #'(lambda (pair) ; pair = (tau . morphism)
		 (setf (gethash (cons (car pair) c) ; (tau . sigma)
				(quiver-rep-edges (stalks x)))
		       (compose
			(gethash (car pair) adaption-mat)
			(make-tensor-morphism
			 (make-id-morphism
			  (from1 trunc-cx)
			  (from1 (sou (gethash (car pair) adaption-mat))))
			 (compose
			  (cdr pair)
			  trunc-normal-map)
			 trunc-cx
			 (sou (gethash (car pair) adaption-mat))))))
	     (rest global-normal-data))
	;; Return trunc-cx [the stalk] as value.
	trunc-cx)))))
		       
(defun get-stalk-map (pair x)
  (let ((ans (gethash pair (quiver-rep-edges (stalks x)))))
    (if (null ans)
	(error "Map not found.  Should be there by now.")
      ans)))

(defun adapt-to-Tpr (c1 c x)
 "Arguments are c1, c, x.  Here x is a sheaf on a toric-variety, and
c1 and c are poset-elts in (ranked-poset x), with (preceq c c1) true
and c not equal to c1.  Inside (ur x), let T' be the tangential space
for c, and T that for c1.  The idea is that c is for a small face of
the moment-map polytope, and c1 is for a larger face bordering on c's
face, so that T' is a subset of T.  Say (get-stalk c1 x) is
   ta = Wedge^*(T) tensor B^*
The function outputs the isomorphism
  Wedge^*(T') tensor [Wedge^*(N') tensor B^*]  ---> ta ."
  (let* ((Tpr-inclusion (get-tangential-inclusion c x))
	 (ta (get-stalk c1 x))
	 (Tpr-to-T (inclusion-of-subspaces Tpr-inclusion
					   (get-tangential-inclusion c1 x)))
#|
	 ;; N', N are normal spaces corresponding to T', T resp.
         ;;    ...but that's not how the next two lines use them.
	 (Npr-to-T (coker-section Tpr-to-T))
         (Npr-to-T (cdr (image-and-complement Tpr-to-T)))
|#
         ;; The map called Npr-to-T below should have image lying in
	 ;; the image of (get-normal-inclusion c x).
         ;;    The name suggests that (sou Npr-to-T) should be (sou
	 ;; (get-normal-inclusion c x)).  However, (sou Npr-to-T) is
	 ;; really a subspace of the latter object, isomorphic onto
	 ;; its image, which is the intersection of (sou
	 ;; (get-normal-inclusion c x)) and T.
         (Npr-to-T (inclusion-of-subspaces
		    (intersection-of-images
		     (get-normal-inclusion c x)
		     (get-tangential-inclusion c1 x))
		    (get-tangential-inclusion c1 x)))
	 ;; We have an isomorphism over Q of T' directsum N' --> T.
	 ;; (Is this an isomorphism over Z?)  We will hold this in
	 ;; split-sum-to-T.
	 (split-sum (make-directsum-of-two
		     (sou Tpr-to-T)
		     (sou Npr-to-T)))
	 (split-sum-to-T ; T' directsum N' --isom--> T
	  (make-directsum-morphism-to-singleton
	   split-sum
	   (tar Tpr-to-T)
	   #'(lambda (j)
	       (cond ((= j 0) Tpr-to-T)
		     ((= j 1) Npr-to-T)
		     (t (error
"Directsum-of-two didn't use indices 0 and 1."))))))
	 (wedge-isom
	  ;; the composition
	  ;;    Wedge_*(T') tensor Wedge_*(N')
	  ;;      --isom--> Wedge_*(T' directsum N')
	  ;;          --isom--> Wedge_*(T).
	  (let ((wedge-of-sum (make-exterior-algebra split-sum)))
	    ;; wedge-of-sum is the intermediate object in the above
	    ;; composition.
	    (compose-two
	     (lift-morphism-to-exterior-algebra
	      split-sum-to-T
	      wedge-of-sum
	      ;; See note below desc'g the next function.
	      (get-tangential-algebra c1 x))
	     (distribute-tensor-over-exterior-algebra-of-directsum
	      (make-tensor-object
	       (get-tangential-algebra c x)
	       (make-exterior-algebra (sou Npr-to-T)))
	      wedge-of-sum))))
	 (breaker
	  ;; (Wedge_*(T') tensor Wedge_*(N')) tensor [?]
	  ;;    --isom--> Wedge_*(T) tensor [same?]
	  ;;              := ta
	  (make-tensor-morphism
	   wedge-isom
	   (make-id-morphism (from2 ta)
			     (from2 ta))
	   (make-tensor-object
	    (sou wedge-isom)
	    (from2 ta))
	   ta)))
    ;; Answer
    (compose
     breaker
     (tensor-associate-to-left
      (make-tensor-object
       (get-tangential-algebra c x)
       (make-tensor-object
	(from2 (sou wedge-isom))
	(from2 (sou breaker))))
      (sou breaker)))))

#|
(defun truncation-i (x k)
  (declare (type cochain-cx x)
	   (fixnum k))
  "Returns the morphism

   truncation_{\le k} x --> x

where the LHS is set up by `adding stuff' [a mapping cone for the
{\ge k+1} truncation]."
  (let* ((kplus2 (+ k 2))
	 (maxi 0)
	 (so-o
	  (with-eql-ans
	    (map-objects
	     #'(lambda (i xi)
		 (declare (fixnum i))
		 (setq maxi (max maxi i))
		 (setf (gethash i ans)
		   (make-directsum-of-two
		    xi
		    (cond ((= i kplus2)
			   (tar (coker (d k x))))
			  ((> i kplus2)
			   (term x (1- i)))
			  (t
			   *zero-qvsp*)))))
	     x)
	    (setf (gethash (the fixnum (1+ maxi)) ans)
	      (make-directsum-of-two
	       *zero-qvsp*
	       (cond ((= (1+ maxi) kplus2)
		      (tar (coker (d k x))))
		     ((> (1+ maxi) kplus2)
		      (term x maxi))
		     (t
		      *zero-qvsp*))))))
	 (so
	  (make-cochain-cx
	   :objects so-o
	   :maps (with-eql-ans
		  (maphash
		   #'(lambda (l sum-l)
		       (unless (null (gethash (1+ l) so-o))
			 (setf (gethash l ans)
			   (make-directsum-morphism
			    sum-l
			    (gethash (1+ l) so-o)
			    #'(lambda (j i)
				(cond ((and (= j 0) (= i 0))
				       (d l x))
				      ((and (= j 1) (= i 1))
				       (scmult -1
					 (cond ((= l kplus2)
						(pushforward-to-coker
						 (d (1+ k) x)
						 (d k x)))
					       ((> l kplus2)
						(d (1- l) x))
					       (t
						(make-zero-morphism
						 (get-summand-indexed-by
						  sum-l
						  1)
						 (get-summand-indexed-by
						  (gethash (1+ l) so-o)
						  1))))))
				      ((and (= j 0) (= i 1))
				       (cond ((= l (1+ k))
					      (coker (d k x)))
					     ((> l (1+ k))
					      1)
					     (t
					      0)))
				      (t
				       0)))))))
		   so-o)))))
    (declare (fixnum kplus2 maxi))
    (test-cochain-cx so)
    (test-cochain-map
     ;; Final value returned:
     (make-cochain-map
      :sou so
      :tar x
      :mat (with-eql-ans
	    (map-objects
	     #'(lambda (i so-i)
		 (setf (gethash i ans)
		   (directsum-projection 0 so-i)))
	     so))))))
|#

(defun truncation-i (x k)
  (declare (type cochain-cx x)
	   (fixnum k))
  "Returns the morphism

   truncation_{\le k} x --> x

where the LHS is set up by `adding stuff' [tar's of coker's]."
  (let* ((kplus2 (+ k 2))
	 (maxi 0)
	 (so-o
	  (with-eql-ans
	    (map-objects
	     #'(lambda (i xi)
		 (declare (fixnum i))
		 (setq maxi (max maxi i))
		 (setf (gethash i ans)
		   (make-directsum-of-two
		    xi
		    (cond ((>= i kplus2)
			   (term (the fixnum (1- i)) (cohomology x)))
			  (t
			   *zero-qvsp*)))))
	     x)
	    (setf (gethash (the fixnum (1+ maxi)) ans)
	      (make-directsum-of-two
	       *zero-qvsp*
	       (cond ((>= (the fixnum (1+ maxi)) kplus2)
		      (term maxi (cohomology x)))
		     (t
		      *zero-qvsp*))))))
	 (so
	  (make-cochain-cx
	   :objects so-o
	   :maps (with-eql-ans
		  (maphash
		   #'(lambda (l sum-l)
		       (declare (fixnum l))
		       (unless (null (gethash (the fixnum (1+ l)) so-o))
			 (setf (gethash l ans)
			   (make-directsum-morphism
			    sum-l
			    (gethash (the fixnum (1+ l)) so-o)
			    #'(lambda (j i)
				(declare (fixnum j i))
				(cond ((and (= j 0) (= i 0))
				       (d l x))
				      ((and (= j 1) (= i 1))
				       0)
				      ((and (= j 0) (= i 1))
				       (cond ((>= l (1+ k))
					      (compose
					       (term
						(coker
						 (cochain-cx-cobdry-to-cocycle-map x))
						l)
					       (term
						(cochain-cx-cocycle-retraction x)
						l)))
					     (t
					      0)))
				      (t
				       0)))))))
		   so-o)))))
    (declare (type nnfixnum kplus2 maxi))
    (test-cochain-cx so)
    (test-cochain-map
     ;; Final value returned:
     (make-cochain-map
      :sou so
      :tar x
      :mat (with-eql-ans
	    (map-objects
	     #'(lambda (i so-i)
		 (setf (gethash i ans)
		   (directsum-projection 0 so-i)))
	     so))))))

(defmacro global-section (qu)
  `(car (global-section-with-edge-morphisms ,qu)))

(defmacro gethash-forbidding-nil (key table)
  `(let ((val (gethash ,key ,table)))
     (if (null val)
	 (error "Got a null hash value where you shouldn't.")
       val)))

(defun global-section-with-edge-morphisms (qu)
  (declare (type quiver-rep qu))
  "Let A be the global section cochain-cx of qu.  Let the edge
morphisms be A --> B_tau for the various tau of minimal rank in the
underlying ranked-poset.  The function returns a list of
A, A-->B_tau_1, A-->B_tau_2, ...."
  ;; Quick exit in the case when the punctured star is only one point
  (when (ranked-poset-equal (quiver-rep-r-p qu) '((())) )
    (return-from global-section-with-edge-morphisms
      (let ((ur-stalk (gethash-forbidding-nil '() (quiver-rep-vertices qu))))
	(list ur-stalk
	      (cons nil
		(make-id-morphism ur-stalk ur-stalk))))))
  (let ((U (quiver-rep-r-p qu))
	(v (quiver-rep-vertices qu)))
    (let ((U-minl (car U))
          (U-superminl (cadr U))
	  (U-triples '()))
      ;; At the end of the next loop, U-triples will be a list of
      ;; lists (a b c).  These run over all the pairs a,b of distinct
      ;; poset-elt's in U-minl.  For each pair, c is the unique
      ;; poset-elt such that (star a) intersect (star b) equals (star c).
      (do ((U1 U-minl (cdr U1)))
	  ((null U1))
	(let ((a (car U1)))
	  (dolist (b (cdr U1))
	    (let ((c (poset-elt-intersection a b)))
              (when (member c U-superminl :test #'poset-elt-equal)
#|
	        (push (list a b (apply #'+ (dims (gethash-forbidding-nil
						  c v))))
		      U-triples))))))
      (setq U-triples
	    (mapcar #'(lambda (tri)
			(let ((a (first tri))
			      (b (second tri)))
			  (declare (type poset-elt a b))
			  (list a b (poset-elt-intersection a b))))
		    (minimal-spanning-tree U-triples)))
|#
                (push (list a b c) U-triples))))))
      (let* ((so (make-directsum-object
		  (map 'vector
		       #'(lambda (c) (gethash-forbidding-nil c v))
		       U-minl)
		  (coerce U-minl 'vector)))
	     (ta (make-directsum-object
		  (map 'vector
		       #'(lambda (tri)
			   (gethash-forbidding-nil (third tri) v))
		       U-triples)
		  (coerce U-triples 'vector)))
	     (big-map
	      (make-directsum-morphism
	       so
	       ta
	       #'(lambda (j tri)
		   (cond ((poset-elt-equal j (first tri))
			  (get-quiver-rep-internal-map
			   (third tri) (first tri) qu))
			 ((poset-elt-equal j (second tri))
			  (scmult -1
			    (get-quiver-rep-internal-map
			     (third tri) (second tri) qu)))
			 (t 0)))))
	     (global-section
;;; DEBUG
	      (progn
		(when (<= 4 (length U)) ; 3- and 4-dim'l case
		  (format t "~&Dims sou big-map ~S" (dims (sou big-map)))
		  (format t "~&Dims tar big-map ~S" (dims (tar big-map)))
		  (format t "~&Starting computation of ker big-map."))
;;; ENDDEBUG
	      (sou (ker big-map)))
	      ) ; THIS LINE ALSO part of the debug
	     (edge-morphisms
	      (mapcar #'(lambda (tau)
			  (cons tau
				(compose (directsum-projection tau so)
					 (ker big-map))))
		      (car U))))
	;; The answer:
	(cons global-section edge-morphisms)))))

#|
(defun ker-of-several (lyst)
  (assert (= 1 (length (remove-duplicates
			(mapcar #'sou lyst)
			:test #'eq)))
	  () "Not all sources are the same, or empty argument.")
  (ker-of-several-aux lyst (make-id-morphism (sou (car lyst))
					     (sou (car lyst)))))

(defun ker-of-several-aux (lyst so-far)
  (cond ((null lyst)
	 so-far)
	(t
	 (ker-of-several-aux
	  (cdr lyst)
	  (compose
	   so-far
	   (ker (compose (car lyst) so-far)))))))
|#

(defun get-quiver-rep-internal-map (tau sigma qu)
  (assert (preceq sigma tau)
	  () "Should have ~S preceq ~S." sigma tau)
  (if (poset-elt-equal sigma tau)
      (let ((v0 (gethash-forbidding-nil sigma (quiver-rep-vertices
					       qu))))
	(make-id-morphism v0 v0))
    (let ((sigma-punct-star (puncture (open-star sigma (quiver-rep-r-p
							qu)))))
      (if (member tau (car sigma-punct-star)
		  :test #'poset-elt-equal)
	  (gethash-forbidding-nil (cons tau sigma)
				  (quiver-rep-edges qu))
	;; Recurse
	(let ((upsilon (dolist (up (car sigma-punct-star)
				   (error "Can't fit intermediate in."))
			 (when (preceq up tau)
                           (return up)))))
	  ;; Now sigma prec[codim-one] upsilon prec tau
	  (compose
	   (get-quiver-rep-internal-map tau upsilon qu)
	   (gethash-forbidding-nil (cons upsilon sigma)
				   (quiver-rep-edges qu))))))))

;;; ------------------------------------------------------------

(defun get-normal-inclusion (coatom-list x)
  (declare (type poset-elt coatom-list)
	   (type toric-variety x))
  "See the documentation on toric-variety."
  (let ((val (gethash coatom-list (normal-subspaces x))))
    (cond ((not (null val))
	   ;; There was a value already there.  Return it.
	   val)
	  ;; Here no value was there.  Create one, then return it.
	  (t
	   (update-normal-and-tangential-inclusions coatom-list x)
	   (gethash coatom-list (normal-subspaces x))))))

(defun get-tangential-inclusion (coatom-list x)
  (declare (type poset-elt coatom-list)
	   (type toric-variety x))
  "See the documentation on toric-variety."
  (let ((val (gethash coatom-list (tangential-subspaces x))))
    (cond ((not (null val))
	   val) ; had one already
	  ;; otherwise, make one
	  (t
	   (update-normal-and-tangential-inclusions coatom-list x)
	   (gethash coatom-list (tangential-subspaces x))))))

(defun get-tangential-algebra (coatom-list x)
  (declare (type poset-elt coatom-list)
	   (type toric-variety x))
  "See the documentation on toric-variety."
  (let ((val (gethash coatom-list (tangential-algebras x))))
    (cond ((not (null val))
	   val) ; had one already
	  ;; otherwise, make one
	  (t
	   (update-normal-and-tangential-inclusions coatom-list x)
	   (gethash coatom-list (tangential-algebras x))))))

(defun update-normal-and-tangential-inclusions (coatom-list x)
  (declare (type poset-elt coatom-list)
	   (type toric-variety x))
  "See the documentation on toric-variety."
  (cond ((null coatom-list)
	 ;; Patch to avoid an empty directsum in the main clause.
	 (setf (gethash '() (normal-subspaces x))
	       (make-zero-morphism *zero-qvsp* (ur x)))
	 (setf (gethash '() (tangential-subspaces x))
	       (make-id-morphism (ur x) (ur x)))
	 (setf (gethash '() (tangential-algebras x))
	       (make-exterior-algebra (ur x))))
	(t
	 (let* ((ind (coerce coatom-list 'vector))
		(prim-v-map (make-directsum-morphism-to-singleton
			     (make-directsum-object
			      (map 'vector
				   #'(lambda (i)
				       ;; just returns *groundfield*
				       (sou (svref (fan-edges x) i)))
				   ind)
			      ind)
			     (ur x)
			     #'(lambda (indj)
				 (svref (fan-edges x) indj))))
		(ans (image-and-complement prim-v-map)))
	   (setf (gethash coatom-list (normal-subspaces x))
		 (car ans))
	   (setf (gethash coatom-list (tangential-subspaces x))
		 (cdr ans))
	   (when (null (name (sou (cdr ans))))
	     (setf (name (sou (cdr ans)))
		   (list 'tangent-space coatom-list
			 'dim (dim (sou (cdr ans))))))
	   (setf (gethash coatom-list (tangential-algebras x))
		 (make-exterior-algebra (sou (cdr ans))))))))

;;; I won't define the next two in general, since I don't know what
;;; they'll do when real codimension 2 is not smooth in Q-homology.

(defun sublogarithmic-perversity (k)
  (declare (fixnum k))
  "A perversity is a function p(k) defined for integers k >= 2, with
p(2) = 0 and p(k+1) equal to either p(k) or p(k)+1.  Here k stands for
the real codimension of a singular stratum in a stratified space X.
The intersection homology IH_i(X) is a function of both X and the
choice of perversity.
  The sublogarithmic perversity is (k-4)/2 for even k >= 4, and is 0
for k = 2.  This is the lowest perversity that gives a perverse sheaf
on X."
  (unless (evenp k)
    (break "We're in sublogarithmic perversity, but I was asked about an
odd-codimension stratum."))
  (max 0
       (the fixnum (floor (the fixnum (- k 4)) 2))))

(defun logarithmic-perversity (k)
  (declare (fixnum k))
  "A perversity is a function p(k) defined for integers k >= 2, with
p(2) = 0 and p(k+1) equal to either p(k) or p(k)+1.  Here k stands for
the real codimension of a singular stratum in a stratified space X.
The intersection homology IH_i(X) is a function of both X and the
choice of perversity.
  The logarithmic perversity is k/2 for even k >= 4, and is 0 for k =
2.  This is the highest perversity that gives a perverse sheaf on X."
  (unless (evenp k)
    (break "We're in logarithmic perversity, but I was asked about an
odd-codimension stratum."))
  (if (= k 2)
      0
    (the fixnum (floor k 2))))

(proclaim '(ftype (function (poset-elt toric-variety) nnfixnum)
		  real-codim cx-codim))

(defmethod real-codim (c (x toric-variety))
  (declare (type poset-elt c))
  "Input: c and x, where x is a toric-variety [see the documentation
on this] and c is a poset-elt in (ranked-poset x).  The output is the
real codimension of the stratum for c in the toric variety.  This
output will be 0 if c = () [corresponding to the whole moment map
polytope], will be 2 if c = (i) for some integer i [corresponding to a
facet of the moment-map polytope], etc."
  (the nnfixnum
    (* 2 (dim (sou (get-normal-inclusion c x))))))

(defmethod cx-codim (c (x toric-variety))
  (declare (type poset-elt c))
  "Input: c and x, where x is a toric-variety [see the documentation
on this] and c is a poset-elt in (ranked-poset x).  The output is the
complex codimension of the stratum for c in the toric variety.  This
output will be 0 if c = () [corresponding to the whole moment map
polytope], will be 1 if c = (i) for some integer i [corresponding to a
facet of the moment-map polytope], etc."
  (dim (sou (get-normal-inclusion c x))))

(defmethod normally-nonsingular-p (c (x toric-variety))
  (declare (type poset-elt c))
  "Arguments c, x.  Returns true iff the toric variety x is smooth
along the stratum S corresponding to c.  [Since Sheafhom currently
works over the rational numbers, this means Q-smooth.  Thus x is
smooth along S iff a neighborhood of S has at worst finite quotient
singularities."
  (= (cx-codim c x) (length c)))

;;; --------------- User Interface ---------------

(defun user-input-toric-variety (input &key name)
  "To load a toric variety into Sheafhom, storing it in [say] the
variable x, use your favorite editor to prepare an ordinary text file
like this:

(setf x (user-input-toric-variety '(
fan-edges
0 (1 1 1)
1 (-1 1 1)
2 (1 -1 1)
3 (-1 -1 1)
...
cones
cone-dim 3
(0 1 2 3)
(4 5 6 7)
...
cone-dim 2
(0 1)
(0 2)
...
)))

Call this file something like filename.lisp .  Giving the command
(load <put your filename here within quotation marks>) within Lisp
will set the value of x to a data structure which Sheafhom uses to
represent the toric variety.  See the tutorial file on the Web page
for examples, and for how to compute the intersection cohomology of x.
   Here are the details on what to put in the file.  Its first line
should have the form

(setf [variable-name] (user-input-toric-variety '(

The file should end with three right parentheses that balance the
three left parentheses of the first line.
   You can put whitespace, extra carriage returns, etc. anywhere you like
for the sake of clarity.
   After the first line, the first entry should be the word

  fan-edges

Follow this with the primitive integer vectors which span the edges
[1-dimensional cones] in the fan Sigma that defines the variety.  Each
vector must be given as a list of n integers, separated by spaces and
enclosed within parentheses.  The [complex] dimension of the toric
variety is n.  The system will internally number the vectors k = 0, 1,
2, 3,...  in the order in which you give them.  Let tau_0, tau_1,
... denote the corresponding edges.
  If you include any words or numbers between the first and last lines
that the system doesn't recognize, these words/numbers are treated as
comments and ignored.  For instance, you can number the primitive
integer vectors 0,1,2,3,... as in the example above--if you are making
the file by hand, this may help you keep things clear.  Anything on a
line to the right of a semicolon is also a comment.
  Once the fan-edges are listed, put in the word

  cones

Then, for each j between 1 and n inclusive [but see shortcut below],
put

  cone-dim [j]

where [j] means to put the number j.  The idea is that after cone-dim [j],
you write down all the j-dimensional cones sigma of the fan.  Each
sigma is represented as a list

  (k k' k'' ... )

of non-negative integers, which means that the set of fan-edges lying
on [the closure of] sigma is precisely

  tau_k, tau_{k'}, tau_{k''}, ...

SHORTCUT: if a cone is _simplicial_, i.e. the number of entries in (k
k' k'' ...) equals j, then you do not have to put in any of its proper
faces.  The system will put them in automatically.  Since all
two-dimensional cones are simplicial, your file will never have to
contain cones of dim j < 2.  Do not put in anything for j = 0, or else
you'll get an error."
  (with-names
  (labels ((first-list-after-fan-edges (lyst)
	     ;; Returns the first element that's a list and that comes
	     ;; after the atom `fan-edges'. 
	     (cond ((null lyst)
		    (error "The user's input file defining the toric variety
never had a symbol

   fan-edges

followed by some lists of integers.  See the documentation for the
function user-input-toric-variety, or the tutorial file on toric
varieties in the Web page, for more information."))
		   ((eql (car lyst) 'fan-edges)
		    (first-list-after-fan-edges-aux (cdr lyst)))
		   (t
		    (first-list-after-fan-edges (cdr lyst)))))
           (first-list-after-fan-edges-aux (lyst)
             ;; Returns the first element that's a list.
	     (cond ((null lyst)
		    (error "The user's input file defining the toric variety
contained the symbol fan-edges, but after that there were no lists of
integers representing the primitive vectors of the fan edges."))
		   ((listp (car lyst))
		    (car lyst))		; done
		   (t
		    (first-list-after-fan-edges-aux (cdr lyst))))))
  (let* ((n (length (the list (first-list-after-fan-edges input))))
         (poset-ar (make-array (list (1+ n))
			       :initial-element '()))
	 (ur (make-qvsp n :name 'ur))
	 (x (make-instance 'toric-variety
			   :ur ur))
         (fan-edges-lyst '()))
    (do ((a input (cdr a))
         ;; Next two are flags showing which mode we're in.
	 (fan-edges-mode nil)
	 (cones-mode nil)
	 (cone-dim 0)) ; holds cone-dim
	((null a))
      (cond ((eql (car a) 'fan-edges)
	     (setq fan-edges-mode t
		   cones-mode nil))
	    ((eql (car a) 'cones)
	     (setq cones-mode t
		   fan-edges-mode nil))
	    ((eql (car a) 'cone-dim)
             (assert (typep (cadr a) 'nnfixnum)
                     () "
There is an item ~S coming after one of the words cone-dim.
This is supposed to be a nonnegative integer." (cadr a))
	     (setq cone-dim (cadr a))) ; (cadr a) ignored on next pass
	    ((listp (car a))
	     (cond (fan-edges-mode
		    (push (make-qvsp-morphism
			   *groundfield*
			   ur
			   (let ((mm (make-matrix n 1)))
			     (do ((i 0 (1+ i))
				  (b (car a) (cdr b)))
				 ((= i n)
				  mm)
                               (declare (fixnum i))
			       (matrix-set mm i 0 (car b)))))
			  fan-edges-lyst))
		   (cones-mode
                    (assert (and (fixnump cone-dim)
                                 (<= 1 cone-dim n)) ()
                      "Cone-dim should be an integer between 1 and ~D."
                                   n)
                    (assert (every
                              #'(lambda (i) (typep i 'nnfixnum))
                              (car a)) ()
"Under the heading   cone-dim ~D  , you have included this list:
~S
That list is supposed to contain only distinct non-negative integers."
                     cone-dim (car a))
		    (let ((p-elt (sort (copy-list (car a))
                                       #'<)))
                      (declare (type poset-elt p-elt))
		      (assert (do ((c p-elt (cdr c)))
				       ((null (cdr c))
					t) ; success
                                     (declare (type poset-elt c))
				     (when (= (the fixnum (car  c))
                                              (the fixnum (cadr c)))
				       (return nil)))
                              ;; A list of nnfixnum's is a poset-elt iff
			      ;; it passes this test.
			      () "
Under the heading  cone-dim ~D , you have included this list:
~S
That list is supposed to contain only distinct non-negative integers."
                              cone-dim (car a))
		      (setf (svref poset-ar cone-dim)
			    (lexicog-union
			     (list p-elt)
			     (svref poset-ar cone-dim)))
		      (when (= cone-dim (length p-elt))
			;; Here lyst represents something simplicial.
			;; Put on all its proper sublists.
			(let ((sx (make-simplicial-ranked-poset
				   p-elt)))
                          (declare (type ranked-poset sx))
			  (mapcar #'(lambda (of-one-dim)
				      (let ((d (length
                                                 (the poset-elt
						   (car of-one-dim)))))
                                        (declare (fixnum d))
					(setf (svref poset-ar d)
					      (lexicog-union
					       of-one-dim
					       (svref poset-ar d)))))
				  (puncture sx))))))
		   (t
		    ;; If you get here, there was a list before
		    ;; fan-edges.  Ignore.
		    nil)))
	    (t
	     ;; Found a non-list that's not a recognized keyword.
	     ;; Ignore.
	     nil)))
    ;; Finalize the toric variety.
    (setf (r-p x)
	  (nreverse ; (car (last (ranked-poset x))) should be (())
	   (coerce poset-ar 'list)))
    (setf (fan-edges x)
	  (coerce (nreverse fan-edges-lyst) 'vector))
    x)))) ; final value returned is x

(defun make-simplicial-ranked-poset (c)
  "Argument c is a poset-elt.  The result is the ranked-poset
consisting of all subsets s of c, with the rank function given by
(length s)."
  (assert (typep c 'poset-elt)
	  () "Argument ~S is not of type poset-elt." c)
  (cond ((null c)
         '((())) ) ; a list of lists-of-poset-elts
	(t
	 (let ((a (car c))
	       (sub (make-simplicial-ranked-poset (cdr c))))
	   (ranked-poset-union
	    (append
	     (mapcar #'(lambda (list-of-poset-elts)
			 (mapcar #'(lambda (pe)
				     (cons a pe))
				 list-of-poset-elts))
		     sub)
	     '(())) ; jack rank up by one
	    (ranked-poset-union
	     (list (list (list a))
                   (list '()))
	     sub))))))

#|
;;; ----- The CANONICAL MAP from one perversity to a higher one -----

(defmethod make-canonical-map ((x sheaf) (y sheaf))
  ;; As with make-sheaf, this function sets up the sheaf-morphism with
  ;; just the initial case of the recursion.  The function
  ;; get-canonical-map-on-stalk will be called later as needed.
  (assert (eq (ur x) (ur y)) ()
	  "The sheaves must have the same underlying variety.")
  (assert (do ((i 2 (+ i 2)))
	      ((> i (* 2 (dim x)))
	       t)
	    (declare (fixnum i))
	    (unless (<= (funcall (perversity x) i)
			(funcall (perversity y) i))
	      (return nil)))
	  () "The perversities do not obey p <= q.")
  (let ((ans (make-sheaf-morphism
	      :sou x :tar y :mat (make-hash-table :test #'equal))))
    (setf (gethash '() (mat ans))
	  (make-id-morphism (get-stalk '() x) (get-stalk '() y)))
    ans))

(defun canonical-map-on-hypercoh (rho)
  (declare (type sheaf-morphism rho))
  (when (null (sheaf-morphism-hcoh-map rho))
    (setf (sheaf-morphism-hcoh-map rho)
	  (make-quiver-rep-map
	   :sou (hypercoh (sou rho))
	   :tar (hypercoh (tar rho))
	   :mat (with-equal-ans
		 (do-vertices (c (r-p (sou rho)))
		   (setf (gethash c ans)
			 (get-canonical-map-on-stalk c rho)))))))
  ;; What the user wants:
  (single-cx-map (sheaf-morphism-hcoh-map rho)))

(defun get-canonical-map-on-stalk (c rho)
  (declare (type poset-elt c)
	   (type sheaf-morphism rho))
  (let ((ans (gethash c (mat rho))))
    (cond ((not (null ans))
	   ans)
	  (t
	   (format t "~&~S" c)
	   (print-time " ")
	   (setf (gethash c (mat rho))
	     (let* ((adaption-map-sou
		     (gethash c (quiver-adaption-maps (sou rho))))
		    (adaption-map-tar
		     (gethash c (quiver-adaption-maps (tar rho))))
		    (raw-stalk-map
		     (make-quiver-rep-map
		      :sou (tar adaption-map-sou)
		      :tar (tar adaption-map-tar)
		      :mat (with-equal-ans
			    (do-vertices (c1 (r-p (tar adaption-map-sou)))
			      (get-canonical-map-on-stalk c1 rho)))))
		    (cochain-stalk-map
		     (inclusion-of-subspaces
		      (compose
		       (inverse (single-cx-map adaption-map-tar))
		       (single-cx-map raw-stalk-map)
		       (single-cx-map adaption-map-sou)
		       (gethash c (stalk-to-single-of-adapteds
				   (sou rho))))
		      (gethash c (stalk-to-single-of-adapteds
				  (tar rho))))))
	       ;; Answer:
#|
	       (compose
		(inverse
		 (kunneth-map-left-trivial
		  (get-stalk c (tar rho))
		  (tar cochain-stalk-map)))
		(map-on-cohomology cochain-stalk-map)
		(kunneth-map-left-trivial
		 (get-stalk c (sou rho))
		 (sou cochain-stalk-map)))))))))
|#
               cochain-stalk-map))))))
|#
