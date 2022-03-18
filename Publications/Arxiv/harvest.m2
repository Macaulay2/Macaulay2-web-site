slowGetWWW = x -> (
     << "-- " << flush;
     sleep (10 + random 5);
     << "fetching " << x << endl;
     getWWW x)

query = "Macaulay2"
-- query = "%22Macaulay+2%22"
-- query = "%22Cohen+Macaulay+2%22"
queryURL = concatenate ("https://search.arxiv.org/?query=",query,"&in=&byDate=1")

harvest = () -> (
     count = 0;
     x = slowGetWWW queryURL;
     p = true;
     while p list (
	  m = lines x;
	  u = apply(select(m,l -> match("class=.url.",l)),s -> replace(".*url.>(http:[^<]*).*","\\1",s));
	  if #u == 0 then error "no url found on this page";
	  print select(m,l -> match("Displaying",l));
	  print stack u;
	  count = count + #u;
	  << "-- count = " << count << endl;
	  u)
     do (
	  n = select(m,l -> match(">Next &gt;&gt;",l));
	  if #n == 0 then break;
	  url = "http://search.arxiv.org:8081/" | replace(".*(.query=[^\"]*).*","\\1",n#0);
	  url = replace("&amp;","&",url);
	  x = slowGetWWW url;
	  ))
