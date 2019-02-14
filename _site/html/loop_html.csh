foreach file(`ls *.html`)
   set base=`basename $file .html`
   echo 'converting ', $base, 'to makrdown'
   pandoc -f html -t markdown $base.html -o $base.md
end
