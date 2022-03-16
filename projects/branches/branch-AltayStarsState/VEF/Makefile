# $Id: Makefile 3235 2018-01-18 16:02:03Z jgawad $
#
#

# Set list of CMakeList files in project directories
CMAKELISTS=CMakeLists.txt  alamDMC/CMakeLists.txt  libAlamDMC/CMakeLists.txt  libAlamYLP/CMakeLists.txt

include ../build/cmake-common/bootstrap.makefile

doc: user_manual

.PHONY: user_manual
user_manual:
	$(CD) manual && $(DOXYGEN)
