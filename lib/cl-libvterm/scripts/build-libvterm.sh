#!/usr/bin/env sh
# Build libvterm (pinned release) plus the cl-libvterm C shim.
#
#   scripts/build-libvterm.sh          # build if needed
#   scripts/build-libvterm.sh --force  # rebuild from scratch
#
# Outputs (relative to the cl-libvterm directory):
#   build/lib/libvterm.{dylib,so}         libvterm itself
#   build/lib/libvterm-cl-shim.{dylib,so} the small C shim (src/shim.c)
#   build/include/vterm.h, vterm_keycodes.h
# Downloads and source trees live in .deps/.  Both directories are gitignored.
#
# The sources are compiled directly with $CC instead of going through
# libvterm's libtool-based Makefile, so neither libtool nor perl is needed
# (the release tarball ships the generated encoding/*.inc and fullwidth.inc).
set -eu

VERSION=0.3.3
# Recorded on first fetch of the tarball (2026-10-15).
SHA256=09156f43dd2128bd347cbeebe50d9a571d32c64e0cf18d211197946aff7226e0
URLS="https://launchpad.net/libvterm/trunk/v0.3/+download/libvterm-$VERSION.tar.gz
https://www.leonerd.org.uk/code/libvterm/libvterm-$VERSION.tar.gz"

root=$(cd "$(dirname "$0")/.." && pwd)
deps="$root/.deps"
build="$root/build"
src="$deps/libvterm-$VERSION"
tarball="$deps/libvterm-$VERSION.tar.gz"
: "${CC:=cc}"
: "${CFLAGS:=-O2}"

case "$(uname -s)" in
  Darwin) ext=dylib ;;
  *)      ext=so ;;
esac
lib="$build/lib/libvterm.$ext"
shim="$build/lib/libvterm-cl-shim.$ext"

if [ "${1:-}" = "--force" ]; then
  rm -rf "$build" "$src" "$deps/obj"
fi

sha256() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | cut -d' ' -f1
  else shasum -a 256 "$1" | cut -d' ' -f1; fi
}

fetch() {
  mkdir -p "$deps"
  if [ -f "$tarball" ] && [ "$(sha256 "$tarball")" = "$SHA256" ]; then return 0; fi
  rm -f "$tarball"
  for url in $URLS; do
    echo "fetching $url"
    if curl -fsSL --retry 2 -o "$tarball.part" "$url"; then
      got=$(sha256 "$tarball.part")
      if [ "$got" = "$SHA256" ]; then mv "$tarball.part" "$tarball"; return 0; fi
      echo "sha256 mismatch for $url: got $got, want $SHA256" >&2
    fi
    rm -f "$tarball.part"
  done
  echo "could not download libvterm $VERSION" >&2
  exit 1
}

build_libvterm() {
  if [ ! -f "$src/include/vterm.h" ]; then
    fetch
    rm -rf "$src"
    (cd "$deps" && tar xzf "$tarball")
  fi
  mkdir -p "$build/lib" "$build/include" "$deps/obj"
  objs=""
  for c in "$src"/src/*.c; do
    o="$deps/obj/$(basename "$c" .c).o"
    echo "CC $(basename "$c")"
    # shellcheck disable=SC2086
    $CC $CFLAGS -std=c99 -fPIC -Wall -I"$src/include" -c "$c" -o "$o"
    objs="$objs $o"
  done
  echo "LINK $(basename "$lib")"
  if [ "$ext" = dylib ]; then
    # shellcheck disable=SC2086
    $CC -dynamiclib -install_name @rpath/libvterm.dylib \
        -compatibility_version 0.3 -current_version $VERSION -o "$lib" $objs
  else
    # shellcheck disable=SC2086
    $CC -shared -Wl,-soname,libvterm.so.0 -o "$lib" $objs
  fi
  cp "$src/include/vterm.h" "$src/include/vterm_keycodes.h" "$build/include/"
}

build_shim() {
  echo "CC shim.c"
  # The shim does not link libvterm itself: its vterm_* references resolve
  # against whichever libvterm the Lisp side loaded first (RTLD_GLOBAL).
  if [ "$ext" = dylib ]; then
    # shellcheck disable=SC2086
    $CC $CFLAGS -std=c99 -fPIC -Wall -I"$build/include" -dynamiclib \
        -undefined dynamic_lookup -install_name @rpath/libvterm-cl-shim.dylib \
        -o "$shim" "$root/src/shim.c"
  else
    # shellcheck disable=SC2086
    $CC $CFLAGS -std=c99 -fPIC -Wall -I"$build/include" -shared \
        -o "$shim" "$root/src/shim.c"
  fi
}

if [ ! -f "$lib" ] || [ ! -f "$build/include/vterm.h" ]; then
  build_libvterm
fi
if [ ! -f "$shim" ] || [ "$root/src/shim.c" -nt "$shim" ]; then
  build_shim
fi
echo "ok: $lib"
echo "ok: $shim"
