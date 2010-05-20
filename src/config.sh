#!/bin/sh

if [ -d "_MTN" ] && mtn automate get_workspace_root 2> /dev/null > /dev/null
then
VERSION_BASE=$(mtn automate get_base_revision_id)
VERSION_CURRENT=$(mtn automate get_current_revision_id)
VERSION_STATUS=$(mtn diff | grep ^# | sed 's/"/\\"/g')
else
VERSION_BASE="unknown"
VERSION_CURRENT="unknown"
VERSION_STATUS="unknown"
fi


BUILD_LABLGTK=$(${PKG_VERSION} lablgtk2)

[ "${THREADS}" = "yes" ] && echo "let threads = T_thread.main"

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
let _ = Res.sys_data_dir := "${SYSDATADIR}"
let _ = Res.sys_tmp_dir := "${SYSTMPDIR}"
EOF
