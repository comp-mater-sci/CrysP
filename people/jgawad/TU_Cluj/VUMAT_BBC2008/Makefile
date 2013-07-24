#
# $Id$
#
SRCS = \
vumat_bbc_2008.for

LIBOBJS=$(patsubst %.for,$(OBJDIR)/%.o,$(SRCS))

FCFLAGS += -I../libHMSIntegration/$(Configuration)/include

TargetName=libvumat_bbc2008
SLIB=$(TargetDir)$(TargetName).a
LIB=$(TargetDir)$(TargetName).so

include ../Makefile.inc

