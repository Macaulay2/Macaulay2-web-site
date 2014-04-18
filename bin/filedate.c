#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#define ERROR (-1)
#define FALSE 0
#define TRUE 1

static int gmt = TRUE;
static int rfc822 = FALSE;

char *month[] = {
     "January","February","March","April","May","June","July",
     "August","September","October","November","December"
     };

int less(struct tm *t, struct tm *u) {
     return (
	  t->tm_year < u->tm_year || 
	  (t->tm_year == u->tm_year && (
	       t->tm_mon < u->tm_mon ||
	       (t->tm_mon == u->tm_mon && t->tm_mday < u->tm_mday) ||
	       (t->tm_mon == u->tm_mon 
		    && t->tm_mday == u->tm_mday
		    && t->tm_hour < u->tm_hour
		    ) ||
	       (t->tm_mon == u->tm_mon 
		    && t->tm_mday == u->tm_mday
		    && t->tm_hour == u->tm_hour
		    && t->tm_min < u->tm_min
		    )
	       ))
	  );
     }

int main (int argc, char **argv) {
     struct stat buf;
     struct tm *t;
     static struct tm min;
     int i;
     int printtime = FALSE, numericdate = FALSE, reverse = FALSE;
     if (argc <= 1) {
	  fprintf(stderr,"usage: %s [-t] [-n] filename ...\n", argv[0]);
	  fprintf(stderr,"       prints earliest date of files given\n");
	  fprintf(stderr,"       -r      print latest date instead\n");
	  fprintf(stderr,"       -l      give local time instead of GMT\n");
	  fprintf(stderr,"       -n      give date in format YYYY-MM-DD\n");
	  fprintf(stderr,"       -t      give time of day, too\n");
	  fprintf(stderr,"       -rfc822 give date and time in rfc822 format\n");
	  exit(1);
	  }
     while (TRUE) {
       if (0 == strcmp(argv[1],"-rfc822")) {
	 argv++, argc--;
	 rfc822 = TRUE;
	 continue;
       }
       if (0 == strcmp(argv[1],"-t")) {
	 argv++, argc--;
	 printtime = TRUE;
	 continue;
       }
       if (0 == strcmp(argv[1],"-r")) {
	 argv++, argc--;
	 reverse = TRUE;
	 continue;
       }
       if (0 == strcmp(argv[1],"-l")) {
	 argv++, argc--;
	 gmt = FALSE;
	 continue;
       }
       if (0 == strcmp(argv[1],"-n")) {
	 argv++, argc--;
	 numericdate = TRUE;
	 continue;
       }
       break;
     }
     for (i=1; i < argc; i++) {
     	  if (ERROR == stat(argv[i],&buf)) {
	       char buf[100];
	       sprintf(buf,"error: trying to examine file '%s'", argv[i]);
	       perror(buf);
	       exit(1);
	       }
     	  t = (gmt ? gmtime : localtime)(&buf.st_mtime);
	  if (i == 1) min = *t;
	  else if (reverse ? less(&min,t) : less(t,&min)) min = *t;
	  }
     if (rfc822) {
	  char buf[100];
	  strftime(buf,sizeof(buf),"%a, %d %b %Y %H:%M:%S %z\n",&min);
	  printf("%s",buf);
     }
     else if (printtime) {
	  if (numericdate) printf("%d-%02d-%02d-%02d:%02d:%02d\n", 1900 + min.tm_year, 1 + min.tm_mon, min.tm_mday, min.tm_hour, min.tm_min, min.tm_sec);
	  else printf("%d %s %d %d:%02d:%02d %s\n", 1900 + min.tm_year, month[min.tm_mon], min.tm_mday, min.tm_hour, min.tm_min, min.tm_sec, gmt ? "GMT" : "");
     }
     else {
	  if (numericdate) printf("%d-%02d-%02d\n", 1900 + min.tm_year, 1 + min.tm_mon, min.tm_mday);
	  else printf("%s %d, %d\n", month[min.tm_mon], min.tm_mday, 1900 + min.tm_year);
     }
     return 0;
}
