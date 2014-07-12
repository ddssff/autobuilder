#!/bin/bash

function warning () {
    echo "W: " $*  1>&2
}

function info () {
    echo "I: " $*  1>&2
}

function error () {
    echo "E: " $*  1>&2
}


if [ "$UID" != "0" ] ; then
    error "You must be root to run this script."
    exit 1
fi

DIST=lucid

WOOT=$(mktemp -d tmpDebootStrap_XXXXXX)
chmod og+rx ${WOOT}

set -x
debootstrap --include=perl-base --variant=buildd --components=main,restricted,universe,multiverse ${DIST} ${WOOT} http://mirror.anl.gov/ubuntu/ 

(cat ${WOOT}/etc/apt/sources.list | sed -e 's/^deb /deb-src /') >>${WOOT}/etc/apt/sources.list

cat <<EOF >> ${WOOT}/etc/apt/sources.list
deb http://mirror.anl.gov/ubuntu ${DIST}-updates main restricted universe multiverse
deb-src http://mirror.anl.gov/ubuntu ${DIST}-updates main restricted universe multiverse
deb http://mirror.anl.gov/ubuntu ${DIST}-backports main restricted universe multiverse
deb-src http://mirror.anl.gov/ubuntu ${DIST}-backports main restricted universe multiverse
deb http://mirror.anl.gov/ubuntu ${DIST}-security main restricted universe multiverse
deb-src http://mirror.anl.gov/ubuntu ${DIST}-security main restricted universe multiverse
deb http://deb.seereason.com/ubuntu ${DIST}-seereason main
deb-src http://deb.seereason.com/ubuntu ${DIST}-seereason main
deb ssh://upload@deb.seereason.com/srv/deb-private/ubuntu ${DIST}-seereason-private main
deb-src ssh://upload@deb.seereason.com/srv/deb-private/ubuntu ${DIST}-seereason-private main
deb file:///work/localpool ${DIST}-seereason main
deb-src file:///work/localpool ${DIST}-seereason main
EOF

chroot ${WOOT} apt-get update


info "Replacing ~/.autobuilder/dists/${DIST}-seereason/clean-Moderate"
rm -rf ~/.autobuilder/dists/${DIST}-seereason/clean-Moderate
mkdir -p ~/.autobuilder/dists/${DIST}-seereason/
mv ${WOOT} ~/.autobuilder/dists/${DIST}-seereason/clean-Moderate

cat <<EOF >> ~/.autobuilder/dists/${DIST}-seereason/sources
deb http://deb.seereason.com/ubuntu ${DIST}-seereason main
deb-src http://deb.seereason.com/ubuntu ${DIST}-seereason main
deb http://mirror.anl.gov/ubuntu ${DIST}-updates main restricted universe multiverse
deb-src http://mirror.anl.gov/ubuntu ${DIST}-updates main restricted universe multiverse
deb http://mirror.anl.gov/ubuntu ${DIST}-backports main restricted universe multiverse
deb-src http://mirror.anl.gov/ubuntu ${DIST}-backports main restricted universe multiverse
deb http://mirror.anl.gov/ubuntu ${DIST}-security main restricted universe multiverse
deb-src http://mirror.anl.gov/ubuntu ${DIST}-security main restricted universe multiverse
EOF

#info 'Starting autobuilder with new root'
#autobuilder ${DIST}-seereason --force haskell-devscripts --target haskell-devscripts

exit 0