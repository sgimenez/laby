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
BUILD_LABLGTKSOURCEVIEW=$(${PKG_VERSION} lablgtksourceview)

cat <<EOF
let project_name = "${PROJECT_NAME}"
let version_string = "${PROJECT_VERSION}"
let version_base = "${VERSION_BASE}"
let version_current = "${VERSION_CURRENT}"
let version_status = "${VERSION_STATUS}"

let build_ocaml = "${OCAML_VERSION}"
let build_lablgtk = "${BUILD_LABLGTK}"
let build_lablgtksourceview = "${BUILD_LABLGTKSOURCEVIEW}"
let conf_path = Sys.getenv "HOME" ^ "/.config/${PROJECT_NAME}/"
let sys_data_path = "/usr/share/${PROJECT_NAME}/"
EOF
