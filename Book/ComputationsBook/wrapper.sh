#!/bin/sh

FOO=

cat <<EOF
    \documentclass[runningheads]{lncse}
    \usepackage{makeidx,multicol}\makeindex
    \input book-macros.tex
    \begin{document}
      \bgroup
      \input $NAME-m2.tex $FOO
      \egroup
EOF

if egrep '^.citation' "$1" >/dev/null 2>&1
then cat <<EOF
      \bibliography{papers}
EOF
fi

cat <<EOF
     \clearpage
     \addtocmark[2]{Index}
     \markboth{Index}{Index}
     \renewcommand{\indexname}{Index}
     \threecolindex
     \printindex
    \end{document}

    % Local$FOO Variables:
    % mode: latex
    % mode: reftex
    % reftex-use-external-file-finders: t
    % reftex-external-file-finders: (("tex" . "make FILE=%f find-tex") ("bib" . "make FILE=%f find-bib"))
    % End:
EOF

    # Local Variables:
    # mode: sh
    # End:
