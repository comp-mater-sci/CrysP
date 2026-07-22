============
Introduction
============

:Copyright: KU Leuven

.. contents::

Overview of the repository contents
===================================

This directory contains top-level projects. Each project has a separate directory where
its content resides. On this level, the list of projects include:

`AlTay`_
  Source code of the AlTay library.

`VEF`_
  Virtual Experimentation Framework

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

- CMake
- Intel oneAPI (Fortran compiler, Intel Math Kernel Library)
- Doxygen


Build process
-------------

t.b.d. (should become standard CMake)

----------

.. include:: AlTay/readme.rst

.. include:: VEF/readme.rst

