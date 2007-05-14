#!/bin/sh

if which mtn > /dev/null; then
VERSION_BASE=$(mtn automate get_base_revision_id)
VERSION_CURRENT=$(mtn automate get_current_revision_id)
VERSION_STATUS=$(mtn st)
else
VERSION_BASE="unknown"
VERSION_CURRENT="unknown"
VERSION_STATUS="unknown"
fi

BUILD_LABLGTK=$(${PKG_VERSION} lablgtk2)

cat <<EOF
let project_name = "${PROJECT_NAME}"
let version_string = "${VERSION_STRING}"
let version_base = "${VERSION_BASE}"
let version_current = "${VERSION_CURRENT}"
let version_status = "${VERSION_STATUS}"
let build_ocaml = "${BUILD_OCAML}"
let build_lablgtk = "${BUILD_LABLGTK}"
let conf_path = Sys.getenv "HOME" ^ "/.${PROJECT_NAME}/"
EOF
