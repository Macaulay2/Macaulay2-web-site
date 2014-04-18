#! /bin/sh
# /*-*- css -*-*/
function ar () { echo "scale=2;" "$@" |bc; }
function pct () { echo `ar "scale=0; 100 * $1"`%; }
barwid=180
barun="px"
limargin=`ar ".027 * $barwid"`
lipadding=`ar ".027 * $barwid"`
lilimargin=`ar ".054 * $barwid"`
inputmargin=`ar ".0 * $barwid"`
inpwid=`ar "$barwid - 2 * $limargin - 2 * $lipadding - 2 * $lilimargin - 2 * $inputmargin"`
inpshr=1.00			# was .7
inpshrpct=`pct $inpshr`

cat <<EOF
/*-*- css -*-*/
body { color: black; background-color: #d8ffff; /* margin : 0; */ }
h1   { text-align: center; font-size : 125%; }
h2   { font-size : 110%; }
h3   { font-size : 105%; }
h4   { font-size : 100%; margin-left: 8pt; }
ul   { clear: both; } 
a:link { background-color: transparent; color: #0000ee; }
a:visited { background-color: transparent; color: #52188b; }
a:hover { background-color: transparent; color: #009999; }
a:active { background-color: transparent; color: #ee0000; }
table.buttons td { text-align: center; }
table.examples { 
     margin-left: 2em;
     width: 25%;
     background-color: #c0ffff;
     border-style: solid;
     border-top-color: #c0ffff;
     border-bottom-color: #50b0b0;
     border-left-color: #a0ffff;
     border-right-color: #50b0b0;
     border-width: 3px;
     border-spacing: 0px;
     font-size: 75%;

     /* 122% seems to be the default, and 100% leaves almost no space between lines */
     /* but 110% provokes scroll bars everywhere, chrome */
     /* line-height: 110%; */

}
table.examples td {
     border-style: solid;
     border-top-color: #50b0b0;
     border-bottom-color: #c0ffff;
     border-left-color: #50b0b0;
     border-right-color: #a0ffff;
     padding: 1ex;
     border-width: 1px;
}
table.examples pre { margin: 0; padding: 0; }
div.waystouse { font-size: 75%; line-height: 110%; }
dl.element { display: table-row; }
dt.heading {
  display: table-caption;
  vertical-align: top;
  margin-left: 0;
  margin-top: 0;
  margin-bottom: 0;
  margin-right: 1em;
}
dd.value { display: table-cell; vertical-align: top; }

/* new web page design */

#content {
  font-family: Arial, sans-serif;
  background-color: transparent;
  padding: 0.5em 0.6em;
  margin: 0.5em $barwid$barun 0 $barwid$barun;
  border: 0px solid #000000;
}

.sidebar ul { padding-left: 0; margin-top: 0; margin-left: 0 /* for Windows IE */ ; }
#content ul { padding-left: .6em; }
#content ul li { margin-left: 1em; /* list-style-type: none; */ }
#content ol li { margin-left: 1em; }
#content p { text-align: justify; }
#content pre { overflow: auto; }

.margins li { margin-top: 0.5em; margin-bottom: 0.5em; }

a img { border: 0; }
div#content a { text-decoration: none; color: blue; }
div#content a:hover { text-decoration: underline; }

.sidebar { 
    position: absolute; 
    top: 0.2em; 
    width: $barwid$barun;
    font-family:Verdana,sans-serif;
    font-size:80%;
    line-height:1.2em;
}
#left_sidebar { left: 0; }
#right_sidebar { right: 0; padding-right: .2em; }
.sidebar a { text-decoration: none; color: blue; }
.sidebar a:hover { text-decoration: underline; }


li.extra_space_below { margin-bottom: 1em; }


.sidebar li { 
    list-style-type: none; 
    margin: 0.45em $limargin$barun 0.25em $limargin$barun;
    padding-bottom: .2em;
    padding-left: $lipadding$barun;
    padding-top: .15em;
}
.sidebar li li { 
    font-size: 85%; 
    line-height: 1.2em;
    margin: 0.25em 0 0.35em $lilimargin$barun; 
    padding-bottom: 0;
    padding-left: 0;
    padding-top: 0;
}

.sidebar input {
    font-size: $inpshrpct;
    width: $inpwid$barun;
    margin: 3px $inputmargin$barun 0 0;
    border: 1px solid #999999;
    color: #666666;
    height: 16px;
    padding: 2px 2px 0 2px;
}


.bbox {
    background-color: #ffffff;
    border-style: solid;
    border-top-color: #90e0ff;
    border-bottom-color: #20aaaa;
    border-left-color: #80aaaa;
    border-right-color: #20aaaa;
    border-width: 5px 1px 1px 1px;
    -moz-border-radius-bottomleft:4px;
    -moz-border-radius-bottomright:4px;
    -moz-border-radius-topleft:4px;
    -moz-border-radius-topright:4px;
}

div#logo a:link { color: orange; }
div#logo a:visited { color: orange; }
div#logo { 
    font-size: 150%;
    margin-bottom: 8pt;
}
div.tiny { font-size: 66%; }
.red { color: red; }
.orange { color: orange; }
.green { color: green; }

.center { text-align: center; }
.bigger { font-size: 120%; }
.smaller { font-size: 83.33%; }
div.extra_space_below { margin-bottom: 1em; }
div.extra_space_above { margin-top: 1em; }

EOF
