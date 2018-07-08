mkdir slides_as_html
find ./ -name '*.html' -exec cp -prv '{}' 'slides_as_html' ';'
zip -r slides_as_html.zip slides_as_html
rm -rf slides_as_html