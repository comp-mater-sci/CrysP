#!/bin/bash
#
# $Id$
#
## Global configuration section
UTILDIR="${HMS_ROOT}"
BINDIR="$UTILDIR/bin"
SCRIPTDIR="$UTILDIR/scripts"
DATADIR="$UTILDIR/data"

YLPEVOLCMD="${BINDIR}/facetpar"
FNGPOSTCMD="${BINDIR}/fngPost"
CUB2SMTCMD="${BINDIR}/cub2smt" 
SNAPPREFIX="snap_"

DEFFILE="defdata.dat"
RESULTFILE="elem.Q00"
MMMFILE="elem.MMM"
TEXFILE="texout.cub"

# Configuration of Snapshot
SNAPFILELIST="${DEFFILE} ${TEXFILE} ${RESULTFILE} ${MMMFILE}"

# Number of processors for multilevel facet calculations (default 2)
NPROC=2

export PATH=$BINDIR:$PATH

