;;;                SHEAFHOM TORIC VARIETY TUTORIAL

;;; ---------- Directions ----------

;;; I'll assume you've started up a Lisp, and have installed Sheafhom
;;; by giving the command (load "toric3i") as described in the
;;; installation instructions.  This load command loads the other
;;; three Sheafhom files automatically.  For the more complicated
;;; examples, you will want to have compiled the code; see the
;;; installation instructions.

;;; If you want to use this file [toric_ex3i.lisp] as an example,
;;; type
;;;       (load "toric_ex3i")
;;;
;;; at a Lisp prompt [typing the .lisp extension is optional].  All
;;; the toric varieties given below will be loaded into your system,
;;; although no intersection cohomology groups will be computed until
;;; you ask for them.

;;; Prepare a file like this for the toric variet(ies) you want to
;;; study.  Details on what to put in the file can be found by reading
;;; the documentation on the function user-input-toric-variety.  (For
;;; convenience, the documentation is appended at the end of this
;;; file.)  Once your file is prepared [and called, say, myfile.lisp],
;;; type
;;;
;;;  (load "myfile")
;;;
;;; at the Lisp prompt.  Afterwards, the variables [whose names come
;;; after the setf --in the first example, the variable name is p1]
;;; will have as values a data structure representing the toric
;;; variety.

;;; For the rest of these directions, I will assume the variable that
;;; holds your toric variety is
;;;
;;;        x
;;;
;;; for simplicity.

;;; Now you have to type two lines of Lisp code.  The first will
;;; create an intersection cohomology sheaf on x in the perversity you
;;; choose.  [I'll call this sheaf xx.]  The second line will give you
;;; the Betti numbers of the hypercohomology of xx; more simply, it
;;; will give you the Betti numbers of the intersection cohomology
;;; (IH) of the toric variety, in the perversity you chose.
;;;
;;; (setf xx (make-sheaf x #'top-perversity))
;;; (betti-numbers xx)
;;;
;;; In this example, I chose to compute the top-perversity IH.  This
;;; is basically ordinary homology.  It is dual to the ordinary
;;; cohomology, but comes out in reverse order [what you see printed
;;; is dim H_i for *decreasing* i = 2n,...,0].

;;; If you type #'zero-perversity instead of #'top-perversity, you
;;; will get the ordinary cohomology Betti numbers, in the usual order
;;; dim H^i for i = 0,...,2n.  However, zero perversity is a little
;;; slower than top perversity in this implementation.

;;; If you want to use middle perversity, which is what people usually
;;; mean when they say "intersection cohomology", create a different
;;; sheaf on x [I'll call it xm] and take its Betti numbers.
;;;
;;; (setf xm (make-sheaf x #'middle-perversity))
;;; (betti-numbers xm)
;;;
;;; If you are in middle perversity, then the command this will give
;;; you dim IH_i(x; Q), the "middle-perversity Betti numbers", in
;;; order of increasing i.  When x is smooth, these will be the same
;;; as the ordinary homology Betti numbers, but they will usually
;;; differ when x is singular.  They should be 0 in odd degrees, and
;;; should satisfy Poincare Duality [dim IH_i = dim IH_{2n-i}] and
;;; Hard Lefschetz [the even Betti numbers are strictly increasing up
;;; to the middle degree].

;;; If you do not specify any perversity, as with
;;;
;;; (setf xx (make-sheaf x))
;;;
;;; then top-perversity is the default.

;;; As you've probably noticed, Lisp considers anything on a line to
;;; the right of a semicolon to be a comment.  This may help you in
;;; preparing your file.

;;; By the way, when we apply a function f to [say] two arguments x,
;;; y, we write f(x,y) in mathematics.  In Lisp, however, one writes
;;; (f x y).  Once you're used to this convention, you'll find it
;;; makes things much more flexible.  A call to a function is just
;;; like a piece of data: it is a list with three elements, f, x, and
;;; y.  This puts f on a more equal footing with x and y.

;;; Sheafhom currently works only with rational coefficients.

;;; ---------- Some easy toric varieties ----------

;;; Here is P1, the complex projective line.  [Note: in Lisp, all
;;; names of functions and variables are case-insensitive.  I type p1,
;;; but the system will echo back P1.]

(setf p1 (user-input-toric-variety '(

fan-edges
0 (1)
1 (-1)

cones
cone-dim 1
(0)
(1)

)))

;;; As we saw above, the thing to do after this command is loaded is
;;; to type
;;;
;;; (setf p1t (make-sheaf p1 #'top-perversity))
;;; (betti-numbers p1t)

;;; Here p1t is a variable name I made up to stand for "p1 and top
;;; perversity".  As the betti-numbers command loads, the local
;;; cohomology groups for each stratum [torus orbit] are computed and
;;; stored.  The system tells you it is computing the local IH for (0)
;;; [corresponding to the point at the positive end of the C^*] and
;;; for (1) [the point at the negative end].

;;; The result of the betti-numbers command should be #(1 0 1).

;;; ----------------------------------------

;;; The projective plane.

(setf p2 (user-input-toric-variety '(

fan-edges
0 (1 0)
1 (0 1)
2 (-1 -1)

cones
cone-dim 2
(0 1)
(0 2)
(1 2)

;;; We don't have to list the 1-dimensional cones, because the
;;; 2-dimensional cones are all simplicial.

)))

;;; As before, you now should type
;;;
;;; (setf p2t (make-sheaf p2)) ; top-perversity is default
;;; (betti-numbers p2t)
;;;
;;; The answer should be #(1 0 1 0 1).

;;; ----------------------------------------

;;; Here is P1 cross P1, the quadric surface.

(setf p1-cross-p1 (user-input-toric-variety '(

fan-edges
0 (1 0)
1 (0 1)
2 (-1 0)
3 (0 -1)

cones
cone-dim 2
(0 1)
(1 2)
(2 3)
(3 0)

)))

;;; --------------- Singular toric varieties ---------------

;;; Here is a singular toric variety.  Its image under the moment map
;;; is the regular octahedron.  Its fan arises by taking the cube with
;;; the eight vertices (+/-1, +/-1, +/-1) and forming the cones
;;; through its faces with central vertex at the origin.

(setf octah (user-input-toric-variety '(

fan-edges
0  ( 1  1  1)
1  (-1  1  1)
2  ( 1 -1  1)
3  (-1 -1  1)
4  ( 1  1 -1)
5  (-1  1 -1)
6  ( 1 -1 -1)
7  (-1 -1 -1)

cones
cone-dim 3
(0 1 2 3)
(4 5 6 7)
(0 1 4 5)
(2 3 6 7)
(0 2 4 6)
(1 3 5 7)

;;; Since none of the 3-dimensional cones is simplicial, we must list
;;; all the 2-dimensional cones.  But we won't have to list the
;;; 1-dimensional cones.

cone-dim 2
(0 1)
(0 2)
(0 4)
(1 3)
(1 5)
(2 3)
(2 6)
(3 7)
(4 5)
(4 6)
(5 7)
(6 7)

)))

;;; When you type
;;;
;;; (setf octaht (make-sheaf octah))
;;; (betti-numbers octaht)
;;;
;;; the program says the Betti numbers dim H_0, dim H_1, ... of octah
;;; are #(1 0 1 2 5 0 1).  [Since we used top perversity, these appear
;;; in reverse order: #(1 0 5 2 1 0 1).]  This agrees with the
;;; computation in Danilov's well-known survey article.

;;; Now let's bend one of the eight fan-edges, but leave everything
;;; else the same.

(setf bent-octah (user-input-toric-variety '(

fan-edges
0  ( 2  1  1) ; bent toward the x-axis
1  (-1  1  1)
2  ( 1 -1  1)
3  (-1 -1  1)
4  ( 1  1 -1)
5  (-1  1 -1)
6  ( 1 -1 -1)
7  (-1 -1 -1)

cones
cone-dim 3
(0 1 2 3)
(4 5 6 7)
(0 1 4 5)
(2 3 6 7)
(0 2 4 6)
(1 3 5 7)

cone-dim 2
(0 1)
(0 2)
(0 4)
(1 3)
(1 5)
(2 3)
(2 6)
(3 7)
(4 5)
(4 6)
(5 7)
(6 7)

)))

;;; The bent-octah has Betti numbers #(1 0 0 1 5 0 1).  This was
;;; the first example [M. McConnell, 1985, unpublished] showing that
;;; the ordinary homology Betti numbers are not combinatorial
;;; invariants of the fan.  This is interesting because the
;;; middle-perversity intersection homology Betti numbers _are_
;;; combinatorial invariants, and the ordinary homology Betti numbers
;;; for smooth toric varieties are combinatorial invariants.  The last
;;; fact was central in Stanley's proof of the necessity of McMullen's
;;; conjecture on the numbers of faces of simplicial convex
;;; polytopes.

;;; ---------- Enter Intersection Homology ----------

;;; To get the middle-perversity IH of the variety octah, type
;;;
;;; (setf octahm (make-sheaf octah #'middle-perversity))
;;; (betti-numbers octahm)

;;; This returns #(1 0 5 0 5 0 1), the IH Betti numbers.  This
;;; confirms the results of the beautiful recursive formula for IH
;;; Betti numbers due independently to Bernstein, Khovanskii and
;;; MacPherson.  [The first published proof appears in Karl-Heinz
;;; Fieseler, "Rational Intersection Cohomology of Projective Toric
;;; Varieties", J. Reine Angew. Math. (Crelle) 413 (1991), pp. 88-98;
;;; MR 92m:14028.]  The IH Betti numbers satisfy Poincare duality
;;; (they are palindromic), even though the variety is singular.  The
;;; inequality 1 < 5 reflects the Lefschetz hyperplane theorem for IH.

;;; We won't compute the middle-perversity IH of bent-octah; it would
;;; have the same Betti numbers, because the middle-perversity Betti
;;; numbers are combinatorial invariants of toric varieties.

;;; ---------- More 3-dimensional examples ----------

;;; A cube centered at the origin has the property that its eight
;;; vertices lie on the eight rays [half-lines] spanned by the
;;; fan-edges for octah.  On the other hand, there is no convex
;;; polyhedron in 3-space having six quadrilateral faces meeting
;;; three-at-a-corner and whose vertices lie on the eight rays for
;;; bent-octah.  [The proof is an amusing exercise.  No matter how you
;;; try to arrange the polyhedron, at least one of its six faces must
;;; be curved.]

;;; Another way to say the same thing is that the toric variety for
;;; octah is projective, but the one for bent-octah is not.

;;; It is of interest to find two combinatorially-equivalent
;;; polyhedra with honest flat faces such that the fans to which they
;;; give rise have different Betti numbers.  In other words, we want
;;; two combinatorially-equivalent toric varieties, both projective,
;;; which have different Betti numbers.  Here is such an example,
;;; from M. McConnell, "The rational homology of toric varieties is
;;; not a combinatorial invariant", Proceedings AMS 105, April 1989,
;;; pp. 986-991.  Markus Eikelberg found similar examples
;;; independently, by an elegant method that relates H_2 to the Picard
;;; group and relates the change in H_2 to Minkowski summands of the
;;; original polytope.

(setf rhombododec1 (user-input-toric-variety '(

fan-edges
(1 0 0) ; if you multiply the first six fan-edges by 2, you get
(0 1 0) ; the vertices of the standard rhombododecahedron
(0 0 1)
(-1 0 0)
(0 -1 0)
(0 0 -1)
(1 1 1)
(1 1 -1)
(1 -1 1)
(1 -1 -1)
(-1 1 1)
(-1 1 -1)
(-1 -1 1)
(-1 -1 -1)

cones
cone-dim 3
(3 4 12 13)
(2 3 10 12)
(2 4 8 12)
(3 5 11 13)
(4 5 9 13)
(1 3 10 11)
(0 4 8 9)
(1 2 6 10)
(0 6 8 2)
(1 5 7 11)
(0 7 5 9)
(0 1 6 7)

cone-dim 2
(3 12)
(12 4)
(4 13)
(13 3)
(2 12)
(5 13)
(3 10)
(3 11)
(4 8)
(4 9)
(2 10)
(2 8)
(5 11)
(5 9)
(1 10) 
(1 11) 
(0 8) 
(0 9)
(0 7)
(7 1) 
(1 6) 
(6 0)
(2 6)
(5 7)

)))

(setf rhombododec2 (user-input-toric-variety '(

fan-edges ; a few are bent away from their positions in rhombododec1
(1 0 0)
(0 1 0)
(-1 0 1) ; bent
(-1 0 0)
(0 -1 0)
(0 0 -1)
(2 3 1) ; bent
(1 1 -1)
(2 -3 1) ; bent
(1 -1 -1)
(-2 1 1) ; bent
(-1 1 -1)
(-2 -1 1) ; bent
(-1 -1 -1)

cones ; this all stays the same
cone-dim 3
(3 4 12 13)
(2 3 10 12)
(2 4 8 12)
(3 5 11 13)
(4 5 9 13)
(1 3 10 11)
(0 4 8 9)
(1 2 6 10)
(0 6 8 2)
(1 5 7 11)
(0 7 5 9)
(0 1 6 7)

cone-dim 2
(3 12)
(12 4)
(4 13)
(13 3)
(2 12)
(5 13)
(3 10)
(3 11)
(4 8)
(4 9)
(2 10)
(2 8)
(5 11)
(5 9)
(1 10) 
(1 11) 
(0 8) 
(0 9)
(0 7)
(7 1) 
(1 6) 
(6 0)
(2 6)
(5 7)

)))

;;; The program confirms that the Betti numbers are
;;;
;;;    1, 0, 2, 3, 11, 0, 1   for rhombododec1
;;;    1, 0, 1, 2, 11, 0, 1   for rhombododec2
;;;
;;; [As usual, top perversity gives these in reverse order.]

;;; ---------- A 4-dimensional example ----------

;;; Finally, let's try a four-dimensional singular variety.  This is a
;;; generalization of octah; its image under the moment map is the
;;; cross-polytope with vertices at plus-or-minus e_i for i = 1,...,4.

(setf cross4 (user-input-toric-variety '(

fan-edges
0  ( 1  1  1  1)
1  ( 1  1  1 -1)
2  ( 1  1 -1  1)
3  ( 1  1 -1 -1)
4  ( 1 -1  1  1)
5  ( 1 -1  1 -1)
6  ( 1 -1 -1  1)
7  ( 1 -1 -1 -1)
8  (-1  1  1  1)
9  (-1  1  1 -1)
10 (-1  1 -1  1)
11 (-1  1 -1 -1)
12 (-1 -1  1  1)
13 (-1 -1  1 -1)
14 (-1 -1 -1  1)
15 (-1 -1 -1 -1)

cones
cone-dim 4
(0 1 2 3 4 5 6 7)
(8 9 10 11 12 13 14 15)

(0 1 2 3 8 9 10 11)
(4 5 6 7 12 13 14 15)

(0 1 4 5 8 9 12 13)
(2 3 6 7 10 11 14 15)

(0 2 4 6 8 10 12 14)
(1 3 5 7 9 11 13 15)

;;; The next two lists are computer-generated.

cone-dim 3
(15 11 7 3) (14 10 6 2) (13 9 5 1) (12 8 4 0) (15 13 7 5) (14 12 6 4)
 (11 9 3 1) (10 8 2 0) (15 14 7 6) (13 12 5 4) (11 10 3 2) (9 8 1 0)
 (15 13 11 9) (14 12 10 8) (7 5 3 1) (6 4 2 0) (15 14 11 10) (13 12 9 8)
 (7 6 3 2) (5 4 1 0) (15 14 13 12) (11 10 9 8) (7 6 5 4) (3 2 1 0)

cone-dim 2
(15 7) (14 6) (13 5) (12 4) (11 3) (10 2) (9 1) (8 0) (15 11) (14 10) (13 9)
 (12 8) (7 3) (6 2) (5 1) (4 0) (15 13) (14 12) (11 9) (10 8) (7 5) (6 4) (3 1)
 (2 0) (15 14) (13 12) (11 10) (9 8) (7 6) (5 4) (3 2) (1 0)

)))

;;; The Betti numbers are
;;;
;;;   #(1 0 1 0 4 11 12 0 1)
;;;
;;; [As usual, they come out in reverse order in top perversity.]  The
;;; middle-perversity Betti numbers are
;;;
;;;   #(1 0 12 0 14 0 12 0 1)

;;; Remark: if you try to bend a single one of the 16 fan-edges, you
;;; will be making a mathematical error.  For instance, if you bend
;;; the 0-th fan-edge to (2 1 1 1), then the fan-edges labeled 0,1,2,3
;;; will span a space of dimension 4; since (0 1 2 3) is on the list
;;; of cones of dimension 3, this is a contradiction.

;;; --------------- Documentation ---------------

;;; Finally, here is the documentation on user-input-toric-variety.
;;; You can obtain this text online by typing
;;;
;;;  (doc 'user-input-toric-variety)
;;;
;;; at a Lisp prompt.

#|
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
|#
