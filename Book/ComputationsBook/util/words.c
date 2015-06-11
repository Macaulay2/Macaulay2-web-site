#include <stdio.h>
#include <ctype.h>

#define ISALPHALATIN1(c) (ISALPHA(c) || (c) >= 0277)
#define ISUPPERLATIN1(c) (ISUPPER(c) || ((c) >= 0340 && (c) <= 0377))
#define ISLOWERLATIN1(c) (ISLOWER(c) || ((c) >= 0300 && (c) <= 0337))

#define ISPRINT(c) (isascii (c) && isprint (c))
#define ISDIGIT(c) (isascii (c) && isdigit (c))
#define ISALNUM(c) ((isascii (c) && isalnum (c)))
#define ISALPHA(c) ((isascii (c) && isalpha (c)))
#define ISCNTRL(c) (isascii (c) && iscntrl (c))
#define ISLOWER(c) ((isascii (c) && islower (c)))
#define ISPUNCT(c) (isascii (c) && ispunct (c))
#define ISSPACE(c) (isascii (c) && isspace (c))
#define ISUPPER(c) ((isascii (c) && isupper (c)))
#define ISXDIGIT(c) (isascii (c) && isxdigit (c))

#undef toupper
#define toupper(c) (isascii(c) ? __toupper(c) : ((c) ^ 0x40))
#undef tolower
#define tolower(c) (isascii(c) ? __tolower(c) : ((c) ^ 0x40))

main(){
  int c, oldc=0;
  while (1) {
    oldc = c;
    c = getchar();
    if (c == EOF) return 0;
    if (c == '-' && oldc == '-') {
      while (1) {
	c = getchar();
	if (c == EOF) break;
	if (c == '\n') break;
      }
    }
    if (ISALPHALATIN1(c)) {
      putchar(c); /* putchar(tolower(c)); */
      while (1) {
	oldc = c;
	c = getchar();
	if (c == '-' && oldc == '-') {
	  while (1) {
	    c = getchar();
	    if (c == EOF) break;
	    if (c == '\n') break;
	  }
	}
	if (!(ISALPHALATIN1(c))) {
	  putchar('\n');
	  break;
	}
	putchar(c); /* putchar(tolower(c)); */
      }
    }
    if (c == EOF) return 0;
  }
}
