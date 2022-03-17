============
Introduction
============

:Author: Jerzy Gawad <jerzy.gawad@cs.kuleuven.be>
:Copyright: KU Leuven

.. contents::

Overview of the repository contents
===================================

This directory contains top-level projects. Each project has a separate directory where
its content resides. On this level, the list of projects include:

`AlTay`_
  Source code of AlTay. This includes altay stand-alone program and libaltay library

`build`_
  Collection of commonly used utilities that support building the software.

`FCRI`_
  Fortran library that provides a number of utility components. FCRI is the corner stone
  of almost all Fortran projects in this repository.

`fopt`_
  Collection of optimization procedures written in Fortran

`VEF`_
  Container for sub-projects related to the Virtual Experimentation Framework

Documentation guidelines
========================

There are few general guidelines how to produce, structure and maintain the documentation 
of the projects.

#. Readme in every high-level directory

#. Readme as reStructuredText

#. Each project in C/C++ or Fortran should be ready for being processed with Doxygen_ 

.. _Doxygen: https://www.stack.nl/~dimitri/doxygen
.. _reStructuredText: http://docutils.sourceforge.net/rst.html
.. _reStructuredText webpage: reStructuredText_

Building the software
=====================

In general, the software can be build either on Linux or Windows platform. All components
that need compilation are supposed to compile cleanly.

Requirements and dependencies
-----------------------------

The machine where the software is built must fulfill a set of requirements with respect
to the installed software tools:

- `GNU Make`_ >= 3.8 and CMake_ >= 3.7 (Linux platform)
- Intel Fortran >= 2015
- Intel C/C++ >= 2015 (Linux platform)
- Doxygen_ >= 1.8.13
- Python 2.7.X and packages:

  * docutils
  * pandoc

.. _GNU Make: https://www.gnu.org/software/make/
.. _CMake: https://cmake.org/

The code depends on certain external libraries:

- Intel® Math Kernel Library (MKL_) >= 11.0 (note: it is shipped with Intel compilers)

.. _MKL: https://software.intel.com/en-us/intel-mkl

The build process and the software itself makes use of external programs/tools. 
These include:

Linux platform
--------------

The simplest way to build the software is to simply issue 'make' command from the 
top-level directory. It will build all the projects in the right order, i.e. it would 
respect the dependencies between the top-level projects.

To get more information about the available build options, you may run: ::

  make info

If you want to build a debug variant of the software, just add option DEBUG=1 to the 
command line: ::

  make DEBUG=1


It is also possible to build just selected top-level targets. For instance, to build the
AlTay, you may use: ::

  make altay

This will first build dependencies of altay and then altay itself.

It is also possible to build each project individually. To do so, enter the directory of
the project and use ``make`` command.

----------

.. include:: build/readme.rst

.. include:: AlTay/readme.rst

.. include:: FCRI/readme.rst

.. include:: fopt/readme.rst

.. include:: VEF/readme.rst

