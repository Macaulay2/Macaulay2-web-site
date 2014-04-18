tab = "\t"
outfile = openOut "new-references.tmp"
needsPackage "XML"
needsPackage "Text"					    -- html, TEX
XMLnode _ ZZ := (x,n) -> x.children#n
length XMLnode := x -> length x.children;
keylist = apply(separate_"\n" get "search-results-key",x -> separate(tab,x));
scan(keylist, k -> (
	  if k === {""} then return;
	  century := k#0;
	  num := k#1;
	  assert( k#2 == "arxiv" );
	  section := k#3;
	  x = parse get concatenate("xml/",century,"-",num,"-",section,".xml");
	  x = x_-1;
	  url = first apply(select(x.children,y->y.tag == "id"),y->y_0);
	  title = first apply(select(x.children,y->y.tag == "title"),y->y_0);
	  title = replace("\n"," ",title);
	  title = html TEX title;
	  journalrefs  = apply(select(x.children,y->y.tag == "journal_ref"),y->y_0);
	  authors = apply(select(x.children,y->y.tag == "author"),y->y_0_0);
	  if #authors > 2 then authors = between(", ",authors);
	  if #authors == 2 then authors = between(" ",authors);
	  if #authors > 1 then authors = insert(-2,"and ",authors);
	  authors = concatenate authors;
	  authors = html TEX authors;
	  outfile
	  << century << tab << num << tab << "arxiv" << tab << section << tab
	  << title
	  << ", by "<< authors << "; "
	  << "<a href=\"" << url << "\">arXiv:" << replace(".*/abs/","",url) << "</a>";
	  if #journalrefs > 0 then outfile << "; appeared in: "
	  << replace("\\.$","",
	       replace("--","-",
		    replace("\n"," ",
			 concatenate between(", ", journalrefs))));
	  outfile << "." << endl;
	  ))
outfile << close
moveFile("new-references.tmp","new-references")
