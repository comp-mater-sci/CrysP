#!/bin/bash
## Global configuration section
UTILDIR="$HOME/TEXEVOL"
BINDIR="$UTILDIR/bin"
SCRIPTDIR="$UTILDIR/scripts"
DATADIR="$UTILDIR/data"

YLPEVOLCMD="${BINDIR}/facetpar"
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

