BEGIN { i = 0; 
  printf("%s","var cit = new Array();\n");
  printf("%s","function ran(n) { return Math.floor(Math.random() * (n + 1)); }\n");
  printf("%s","function citation() { document.write(cit[ran(cit.length - 1)]); }\n");
}
{printf("cit[%d] = '%s'\n", i, $0); i = i+1; }
