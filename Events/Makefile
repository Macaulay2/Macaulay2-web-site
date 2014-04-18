index-new all:
	~/to/Macaulay2/Scripts/index-new-html -t ~/to/Macaulay2/Style/trailer.html
	for i in * ;\
	do if [ -d "$$i" ] ;\
	   then ( cd "$$i";\
		  ~/to/Macaulay2/Scripts/index-new-html -t ~/to/Macaulay2/Style/trailer.html ;\
		); \
	   fi ;\
	done
local:
	~/to/Macaulay2/Scripts/index-new-html -l -t ~/to/Macaulay2/Style/trailer.html