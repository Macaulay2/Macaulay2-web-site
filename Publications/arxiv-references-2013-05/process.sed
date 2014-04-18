# -*- sh -*-
/^<h1 [^>]*class="title"/ { # this string marks the line before the 1-line title
    s/.*//
    N
    s%\n%%
    s%</h1>$%%
    s%   *% %g
    s%$%, by %
    h
}
/<a href=.*au:/ { # this string marks each author line
    s%^[^>]*>%%
    s%<.*%%
    s%$%, %
    H
}
$ {
    g
    s%, $%; %
    s%\n%%g
    p
}
