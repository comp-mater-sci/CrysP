============
Introduction
============

:Author: Jerzy Gawad <jerzy.gawad@cs.kuleuven.be>
:Date: $Date$
:Revision: $Revision$
:Copyright: KU Leuven

.. contents::

Overview of the repository contents
===================================

This directory contains top-level projects. Each project has a separate directory where
its content resides. On this level, the list of projects include:

`abaqus`
  Abaqus-related code: VUMATs, Abaqus Python scripts and utilities

`AlTay`_
  Source code of AlTay. This includes altay stand-alone program and libaltay library

`build`_
  Collection of commonly used utilities that support building the software.

`FCRI`_
  Fortran library that provides a number of utility components. FCRI is the corner stone
  of almost all Fortran projects in this repository.

`fng`_
  Implementation of Facet plastic potential.

`fopt`_
  Collection of optimization procedures written in Fortran

`hms`_
  Container for sub-projects related to the Hierarchical Multi-Scale software

`MTM-FHMport`_
  Portable MTM-FHM programs and Python wrappers

simulations
  Collection of Abaqus simulations that are used in verification of the HMS
  
  .. note:: This directory would most likely disappear in the future, since its contents 
     is either project-specific (HMS) or not generic enough to be kept as a top-level
     project.
  
`slis`_
  Simple licensing component that protects the software from unauthorized use

`VEF`_
  Container for sub-projects related to the Virtual Experimentation Framework

Documentation guidelines
========================

There are few general guidelines how to produce, structure and maintain the documentation 
of the projects.

#. Readme in every high-level directory

   **Motivation**
   
   It is useful to have a compact and concise overview of the directory content.

#. Readme as reStructuredText

   .. epigraph::

      reStructuredText is an easy-to-read, what-you-see-is-what-you-get plaintext markup 
      syntax and parser system
      
      -- `reStructuredText webpage`_ 

   **Motivation**

   reStructuredText has several advantages:
   
   - it can be easily read by humans, since it does not contain *too obtrusive* elements
     (as HTML does)
   - it can be transformed into a variety of formats: HTML, PDF, DOC, ...
   - it is structured and well-suited for preparing technical documentation 
   - it is much more standardized than Markdown
   
#. Each project in C/C++ or Fortran should be ready for being processed with Doxygen_ 

   **Motivation**
   
   Doxygen can extract documentation directly from the source code and present it in 
   a searcheable form. Doxygen also produces useful graphs that summarize the code, such 
   as class graphs, caller/callee graphs etc.
   
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
- Microsoft® Visual Studio >= 2013 (Windows platform)
- Intel® Fortran >= 2015
- Intel® C/C++ >= 2015 (Linux platform)
- Doxygen_ >= 1.8.13
- Python 2.7.X and packages:

  * docutils
  * pandoc

.. _GNU Make: https://www.gnu.org/software/make/
.. _CMake: https://cmake.org/

The code depends on certain external libraries:

- Boost_ >= 1.60
- Intel® Math Kernel Library (MKL_) >= 11.0 (note: it is shipped with Intel compilers)

.. _Boost: https://www.boost.org/
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


Windows platform
----------------

There is no unified and automated build procedure on Windows platform. However, all 
projects that need to be compiled contain a Visual Studio solution file and a set of 
Visual Studio projects.

----------

.. include:: build/readme.rst

.. include:: abaqus/readme.rst

.. include:: AlTay/readme.rst

.. include:: fng/readme.rst

.. include:: FCRI/readme.rst

.. include:: fopt/readme.rst

.. include:: MTM-FHMport/readme.rst

.. include:: slis/readme.rst

.. include:: VEF/readme.rst

.. include:: hms/readme.rst

