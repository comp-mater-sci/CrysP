=====
build
=====

:Author: Jerzy Gawad <jerzy.gawad@cs.kuleuven.be>
:Date: $Date$
:Revision: $Revision$
:Copyright: KU Leuven

Overview
========

This directory contains components of the build system.

cmake-common
  Common components of CMake-based build system
  
  .. list-table:: Common components of CMake-based build system
     :header-rows: 1

     * - File
       - Purpose
     * - basic-fortran-project.cmake
       - Skeleton of a Fortran project to be included__ in project's CMakeLists.txt
     * - bootstrap.makefile
       - Generic Makefile for `CMake based build system`_
     * - fortran-generic.cmake
       - Pre-defined configuration for a Fortran-based projects
     * - basic.cmake
       - Pre-defined configuration for projects with simple Debug and Release 
         configuration
     * - python-bytecode.cmake
       - Helper for generating bytecode in projects with Python components

make-common
  Common components of Make-based build system

.. _cmake_include: https://cmake.org/cmake/help/latest/command/include.html
__ cmake_include_
  
CMake based build system
========================

All new projects should be built using CMake. Direct use of Make is deprecated. However,
it is still convenient that projects include a Makefile that invokes CMake. 
In most projects it is a very simple ``Makefile`` that just includes a generic
one, for instance: ::

  include ../build/cmake-common/bootstrap.makefile

Such ``Makefile`` is available as ``build/make-common/Makefile.bootstrap``.
  
The generic ``Makefile`` offers two basic configurations: ``debug`` and ``release``. It 
provides generic targets:

.. list-table:: Generic targets
   :header-rows: 1

   * - Target name
     - Purpose
   * - install
     - Build the code and install it (default)
   * - all
     - Perform all compilation and linking steps
   * - clean
     - Clean the build tree
   * - mrproper
     - Remove the build tree
   * - doc
     - Build all documentation
   * - info
     - Print help information

Customizing the ``Makefile``
----------------------------

If the generic ``Makefile`` is not sufficient to handle the build requirements of a
project, it is still possible to reuse it. Consider the following example: 

.. code:: makefile

  # Path to the top-level
  ROOT_DIR=..

  # Optional custom build
  custom : all
  	actions-to-be-taken-after-all

  # Optional set of CMake input files
  CMAKELISTS= src/CMakeLists.txt test/CMakeLists.txt

  include $(ROOT_DIR)/build/cmake-common/bootstrap.makefile

  # Custom documentation build
  doc : user_manual
  .PHONY : user_manual

  user_manual :
  	actions-to-be-taken-to-build-user_manual

Customizing ``CMakeLists.txt``
------------------------------

CMake processes its directive files called ``CMakeLists.txt``. Please read 
`documentation of CMake`_ for details.

.. _documentation of CMake: https://cmake.org/cmake/help/latest/index.html
