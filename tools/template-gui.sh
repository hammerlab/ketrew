#! /bin/sh

set -e

OUTPUT_HTML=$1
CLIENT_JS=$2
CSS=$3
MORE_SERVERS=$4
DEBUG_LEVEL=$5

cat <<EOBLOB > $OUTPUT_HTML
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
EOBLOB
echo "<!-- Generated on `date` -->" >> $OUTPUT_HTML
cat <<EOBLOB >> $OUTPUT_HTML
    <style>
EOBLOB
cat $CSS >> $OUTPUT_HTML
cat <<EOBLOB >> $OUTPUT_HTML

ul.inline-items-separated {
    padding: 0;
    margin: 2px;
}
.inline-items-separated>li {
    padding: 0;
    margin: 0;
}
.inline-items-separated>li+li:before {
    content: ", ";
}
    </style>
    <title>Ketrew's Mighty GUI</title>
  </head>
  <body>
    <div class="container-fluid" id="ketrew-gui"></div>
    <script>
      window.ketrew_debug_level = "$DEBUG_LEVEL";
      window.ketrew_connections = [
EOBLOB
if [ -f "$MORE_SERVERS" ]; then
    echo "Adding $MORE_SERVERS"
    cat $MORE_SERVERS >> $OUTPUT_HTML
else
    echo "/* No more servers ($MORE_SERVERS) */" >> $OUTPUT_HTML
fi
cat <<EOBLOB >> $OUTPUT_HTML
      ];
EOBLOB
cat $CLIENT_JS >> $OUTPUT_HTML
cat <<EOBLOB >> $OUTPUT_HTML
</script></body></html>
EOBLOB
