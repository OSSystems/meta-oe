DESCRIPTION = "The Illume Windowing Environment -- install this task to get the Enlightenment Window Manager + the Illume environment."
SECTION = "x11/wm"
LICENSE = "MIT"
LIC_FILES_CHKSUM = "file://${TOPDIR}/meta-openembedded/meta-efl/COPYING.MIT;md5=3da9cfbcb788c80a0384361b4de20420"
PV = "1.0"
PR = "r5"

inherit packagegroup allarch

# Default theme and config
ETHEME ?= "e-wm-theme-default"
ECONFIG ?= "e-wm-config-mobile"

RPROVIDES_${PN} += "task-x11-illume"
RREPLACES_${PN} += "task-x11-illume"
RCONFLICTS_${PN} += "task-x11-illume"
RDEPENDS_${PN} = "\
  packagegroup-core-x11-xserver \
  packagegroup-core-x11-utils \
  \
  e-wm \
  ${ECONFIG} \
  ${ETHEME} \
"
