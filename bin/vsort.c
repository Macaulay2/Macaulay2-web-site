#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#define FALSE 0
#define TRUE 1

static int comp00(const char **p, const char **q) {
     const char *sarray = *p;
     const char *tarray = *q;
     int slen = strlen(sarray), tlen = strlen(tarray), i;
     int ret = 0;
     int len = slen < tlen ? slen : tlen;
     for (i=0; i<len; i++) {
	  unsigned char c = sarray[i];
	  unsigned char d = tarray[i];

	  if (isdigit(c) && isdigit(d)) {
	    int sn, tn;
	    sn=i+1; while(sn<slen && isdigit(sarray[sn])) sn++;
	    tn=i+1; while(tn<tlen && isdigit(tarray[tn])) tn++;
	    if (sn > tn) return 1;
	    if (sn < tn) return -1;
	    do {
	      if (c > d) return 1;
	      if (c < d) return -1;
	      i++;
	      if (!(i<len)) break;
	      c = sarray[i];
	      d = tarray[i];
	    } while (isdigit(c) && isdigit(d));
	    if (!(i<len)) break;
	    if (c != d) {
	      if (c == '.') return 1;
	      if (d == '.') return -1;
	    }
	  }

	  if (isalnum(c)) {
	       if (isalnum(d)) {
		    unsigned char C = toupper(c), D = toupper(d);
		    if (C < D) return -1;
		    if (C > D) return 1;
		    if (ret == 0) {
			 if (c < d) ret = -1;
			 if (c > d) ret = 1;
		    }
	       }
	       else return 1;
	  }
	  else {
	       if (isalnum(d)) return -1;
	       else {
		    if (c < d) return -1;
		    if (c > d) return 1;
	       }
	  }
     }
     if (slen > tlen) return 1;
     if (slen < tlen) return -1;
     return ret;
}

#define reverse TRUE

static int comp0(const char **p, const char **q) {
     return reverse ? comp00(q,p) : comp00(p,q);
     }

#if 0
static int comp(const char **p, const char **q) {
  int r = comp0(p,q);
  printf("\np = %s\nq = %s\ncomp(p,q) = %d\n",*p,*q,r);
  return r;
}
#endif

static char *getaline() {
  int n = 1024, i=0, c;
  char *p = malloc(n);
  while (TRUE) {
    c = getchar();
    if (c == EOF) return i==0 ? NULL : p;
    if (!(i<n)) {
      n = 2*n;
      p = realloc(p,n);
    }
    if (c == '\n') {
      p[i] = 0;
      return p;
    }
    p[i++] = c;
  }
  return p;
}

static char **inputlines(int *numlines) {
  int n = 10, i=0;
  char **p = malloc(n * sizeof(char *));
  while (TRUE) {
    char *line = getaline();
    if (line == NULL) {
      *numlines = i;
      return p;
    }
    if (!(i<n)) {
      n = 2*n;
      p = realloc(p,n * sizeof(char *));
    }
    p[i++] = line;
  }
}

int main () {
  int numlines = 0, i;
  char **lines = inputlines(&numlines);
  qsort(lines,numlines,sizeof(lines[0]),(int(*)(const void *, const void *))comp0);
  for (i=0; i<numlines; i++) puts(lines[i]);
  return 0;
}
