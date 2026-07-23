---
project: CrysP-CLI
author: Stijn Schildermans
---

# Introduction
CrysP-CLI is an open-source, minimal command-line interface layer built on top of *libCrysP* to allow academic/expert users to easily work with *libCrysP*. It consists of a collection of *modules*, which each entail a common simulation template which can be configured to the user's needs. These templates and their configuration options are intentionally kept minimal, so that they remain intuitive and to allow the user full control over how data is processed.

# Documentation
The source code is documented in the same manner as *libCrysP* Refer to the [libCrysP README](../libcrysp/README.md) for details.

Additionally, *Crysp-CLI* provides a detailed [user manual](/manual), based on *doxygen*. It contains a detailed overview of how to work with *CrysP-CLI*. It can be generated as follows:

    cd manual
    doxygen

*Doxygen* generates a collection of inter-linked HTML files. Open the manual as follows:

    xdg-open manual/user_manual/html/index.html

This command opens the contents page of the manual in your browser. From here, it should be self-explanatory.

# Installation
*CrysP=CLI* is meant to be installed as part of the *CrysP* framework, so it should not be built stand-alone. Refer to the [top-level README file](../README.md) for instructions on installing *CrysP*.

# Contributing
Refer to the [top-level README file](../README.md).

