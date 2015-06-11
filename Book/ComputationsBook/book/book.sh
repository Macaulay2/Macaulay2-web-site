#! /bin/sh
# a shell script to manufacture book.tex

cat <<'EOF'
% This tex file has been produced automatically, do not edit.
% Title: Computations in algebraic geometry with Macaulay 2
\def\editors{Editors: D. Eisenbud, D. Grayson, M. Stillman, and B. Sturmfels}
\documentclass[runningheads]{lncse}
 \usepackage{makeidx,multicol}\makeindex
 \def\rhpage{\ifodd\value{page}\else\thispagestyle{empty}\null\vfill\eject\fi}
EOF
cat ../inputs/book-macros.tex
cat <<'EOF'
 \begin{document}
  \frontmatter

    % \hypersetup{
    %     pdftitle=Computations in algebraic geometry with Macaulay 2
    %     pdfsubject=Macaulay 2,
    %     pdfkeywords=computations -- syzygies -- Macaulay 2,
    %     pdfauthor=\editors}

    \thispagestyle{empty}\null
	\vskip 2 in
	\centerline{\Large\textbf{Computations in algebraic geometry}}
	\bigskip
	\centerline{\Large\textbf{with \textsl{Macaulay 2}}}
	\bigskip
	\bigskip
	\bigskip
	\centerline{\editors}
    \vfill\eject
    \rhpage
  \pagenumbering{roman}
  \makeatletter \c@page=5 \makeatother
EOF

#############################################################################

for i in preface/chapter
do i=../chapters/$i
   cat <<EOF

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%
%%%%% $i-m2.tex and $i-wrapper.bbl
%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\bgroup
EOF
   cat "$i"-m2.tex 
   cat "$i"-wrapper.bbl
   cat <<'EOF'
\egroup

EOF
done

cat <<EOF
  \vfill\eject
  \rhpage
  \tableofcontents
  \vfill\eject
  \rhpage
EOF
cat addresses.tex
cat <<EOF
  \rhpage
  \pagenumbering{arabic}
  \mainmatter
  \part{Introducing \Mtwo}
EOF

#############################################################################

for i in \
	 varieties/chapter \
         geometry/chapter \
	 programming/chapter \
	 schemes/chapter
do i=../chapters/$i
   cat <<EOF

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%
%%%%% $i-m2.tex and $i-wrapper.bbl
%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  \bgroup
EOF
   cat "$i"-m2.tex 
   cat "$i"-wrapper.bbl
   cat <<'EOF'
  \egroup
 \makeatletter
 \renewcommand\thesection{\@arabic\c@section}
 \makeatother

EOF
done

#############################################################################

cat <<EOF

  \part{Mathematical Computations}

EOF

#############################################################################

for i in monomialIdeals/chapter \
	 solving/solving \
	 completeIntersections/ci \
	 toricHilbertScheme/chapter \
	 exterior-algebra/chapter \
	 constructions/chapter \
	 d-modules/chapter 
do i=../chapters/$i
   cat <<EOF


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%
%%%%% $i
%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\bgroup
EOF
   cat "$i"-m2.tex 
   cat "$i"-wrapper.bbl
   cat <<'EOF'
\egroup
\makeatletter
\renewcommand\thesection{\@arabic\c@section}
\makeatother

EOF
done

#############################################################################

cat <<'EOF'

  \vfill\eject
  \rhpage
  \addtocontents{toc}{\protect\vskip 8pt }
  \addcontentsline{toc}{title}{Index}
  \markboth{Index}{Index}
  \renewcommand{\indexname}{Index}
  \threecolindex
  \printindex
 \end{document}
EOF
