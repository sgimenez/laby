#!/bin/sh

if [ -d ".git" ]
then
VERSION_BASE=$(git log origin/master | head -n 1 | cut -d' ' -f 2)
VERSION_CURRENT=$(git log | head -n 1 | cut -d' ' -f 2)
VERSION_STATUS=$(git diff --stat origin/master)
else
VERSION_BASE="unknown"
VERSION_CURRENT="unknown"
VERSION_STATUS="unknown"
fi

BUILD_LABLGTK=$(${PKG_VERSION} lablgtk3)
BUILD_LABLGTKSV=$(${PKG_VERSION} lablgtk3-sourceview3)

[ -n "${THREADS}" ] && echo "let threads = T_thread.main"

cat <<EOF
let project_name = "${PROJECT_NAME}"
let version_string = "${PROJECT_VERSION}"
let version_base = "${VERSION_BASE}"
let version_current = "${VERSION_CURRENT}"
let version_status = "${VERSION_STATUS}"

let build_system = "${OCAML_SYSTEM}"
let build_arch = "${OCAML_ARCH}"
let build_ocaml = "${OCAML_VERSION}"
let build_lablgtk = "${BUILD_LABLGTK}"
let build_lablgtk_sourceview = "${BUILD_LABLGTKSV}"
let _ = Res.sys_data_dir := "${DATADIR}"
EOF
