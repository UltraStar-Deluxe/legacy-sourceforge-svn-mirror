# Copyright 1999-2008 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header $

inherit cmake-utils games

SONGS_PKG=USDX-SongPackage
SONGS_VER=01

DESCRIPTION="An open-source karaoke game"
HOMEPAGE="http://www.ultrastardeluxe.org/"
SRC_URI="mirror://sourceforge/${PN}/${P}-src.tar.gz
	songs? ( mirror://sourceforge/${PN}/${SONGS_PKG}-${SONGS_VER}.zip )"

LICENSE="GPL-2
	songs? (
		CCPL-Attribution-ShareAlike-NonCommercial-2.5
		CCPL-Attribution-NonCommercial-NoDerivs-2.5
	)"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE="projectm debug songs plugins"

RDEPEND="virtual/opengl
	virtual/glu
	media-libs/libsdl
	media-libs/sdl-image
	media-libs/freetype
	media-libs/libpng
	=media-libs/portaudio-19*
	media-video/ffmpeg
	dev-db/sqlite
	projectm? ( media-libs/libprojectm )"
DEPEND="${RDEPEND}
	dev-util/pkgconfig
	>=dev-lang/fpc-2.2.2"

S=${WORKDIR}/${P}-src

pkg_setup() {
	games_pkg_setup
	built_with_use media-libs/libsdl opengl \
		|| die "You need to compile media-libs/libsdl with USE=opengl."
}

src_compile() {
	# set build-type as the "Gentoo" build-type does not work with
	# Pascal (no optimization, etc.)
	if use debug ; then
		CMAKE_BUILD_TYPE="Debug"
	else
		CMAKE_BUILD_TYPE="Release"
	fi

	# set PREFIX so that CMAKE_INSTALL_PREFIX will be set to GAMES_PREFIX
	PREFIX="${GAMES_PREFIX}"

	# configure flags
	local mycmakeargs="$(cmake-utils_use_enable projectm PROJECTM) \
		$(cmake-utils_use_build plugins PLUGINS) \
		-DBINDIR=${GAMES_BINDIR} \
		-DDATADIR=${GAMES_DATADIR}/${PN}"

	cmake-utils_src_compile
}

src_install() {
	cmake-utils_src_install

	if use songs; then
		insinto "${GAMES_DATADIR}"/${PN}/songs
		doins -r ${WORKDIR}/Songs/* || die "doins songs failed"
	fi

	dodoc AUTHORS.txt ChangeLog.GERMAN.txt ChangeLog.txt README.txt

	doicon icons/${PN}-icon.svg
	make_desktop_entry ${PN} "UltraStar Deluxe"

	prepgamesdirs
}
