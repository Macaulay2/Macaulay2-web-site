BEGIN { i = 0; 
  printf("%s","var doc = new Array();\n");
  printf("%s","function documentation() { document.write(doc[ran(doc.length - 1)]); }\n");
}
{printf("doc[%d] = '<a href=\"/%s\">%s</a>'\n", i, $1, $2); i = i+1; }
