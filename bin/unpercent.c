#include <stdio.h>

hex(int c) {
  if ('0' <= c && c <= '9') return c-'0';
  if ('a' <= c && c <= 'f') return c-'a'+10;
  if ('A' <= c && c <= 'F') return c-'A'+10;
  return 0;
}

main () {
  int c;
  while (-1 != (c = getchar())) {
    if (c == '%') {
      int d, e;
      d = getchar();
      e = getchar();
      if (d == -1 || e == -1) return 1;
      putchar(hex(d) * 16 + hex(e));
    }
    else putchar (c);
  }
  return 0;
}
