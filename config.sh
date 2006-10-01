#!/bin/sh

PROJECT_NAME=$(cat NAME)
VERSION_STRING=$(cat VERSION)
VERSION_BASE=$(mtn automate get_base_revision_id)
VERSION_CURRENT=$(mtn automate get_current_revision_id)
VERSION_STATUS=$(mtn st)

function get_pkg_version()
{
    ocamlfind query -l ${1} | grep ^version: | sed -e 's/version: *\(.*\)/\1/'
}

BUILD_OCAML=$(ocamlc -version)
BUILD_GETOPT=$(get_pkg_version getopt)
BUILD_CAIRO=$(get_pkg_version cairo)
BUILD_LABLGTK=$(get_pkg_version lablgtk2)

rm -f config.ml.tmp
echo "let project_name = \"${PROJECT_NAME}\"" >> config.ml.tmp
echo "let version_string = \"${VERSION_STRING}\"" >> config.ml.tmp
echo "let version_base = \"${VERSION_BASE}\"" >> config.ml.tmp
echo "let version_current = \"${VERSION_CURRENT}\"" >> config.ml.tmp
echo "let version_status = \"${VERSION_STATUS}\"" >> config.ml.tmp
echo "let build_ocaml = \"${BUILD_OCAML}\"" >> config.ml.tmp
echo "let build_getopt = \"${BUILD_GETOPT}\"" >> config.ml.tmp
echo "let build_lablgtk = \"${BUILD_LABLGTK}\"" >> config.ml.tmp
echo "let conf_path = Sys.getenv \"HOME\" ^ \"/.lin/\"" >> config.ml.tmp


if cmp -s config.ml config.ml.tmp
then rm config.ml.tmp
else mv config.ml.tmp config.ml
fi

echo ${VERSION_STRING}

