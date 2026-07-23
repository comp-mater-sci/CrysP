# Introduction
CrysP is an extensible toolkit for multilevel Crystal Plasticity Modeling. It is developed and maintained by KU Leuven, motivated by the need for and lack of up-to-date comparable frameworks. It is aimed towards academic/expert users who seek a modern, open-source, minimalistic and well-documented framework which automates both strain- and stress-driven crystal plasticity simulations while still providing the user with maximum control over the simulation process. It supports a variety of hardening models, slip systems and crystal plasticity models. It is designed specifically to be highly extensible, allowing users to contribute new models (or other features). Such contributions are highly encouraged.

For industrial/non-expert users, KU Leuven offers a (licensed) web-based graphical user interface for CrysP. Please contact the [KU Leuven NUMA research group](https://numa.cs.kuleuven.be/) for more information.

# Repository Contents
Crysp consists of 2 major components:
- **libCrysP**: this library contains the core framework functionality. It defines a concise interface and is designed to be embedded in user applications.
- **CrysP-CLI**: provides a minimalistic command-line interface to perform common crystal plasticity simulations using *libCrysP*.

Both components of CrysP can be seen as separate projects and are documented independently. Therefore, this README file is limited to a global overview of CrysP. It provides no details on the interface of *libCrysP* or the usage of *CrysP-CLI*. These are documented in their respective subdirectories. Refer to the [libCrysP README](/libcrysp/README.md) and [crysp-cli README](/crysp-cli/README.md) to get started.

Each of these subdirectories contains a README file as well. Refer to these for more details.

Additionally, the repository contains a build script for easy installation and a test suite providing integration tests, which compare a wide variety of simulation outputs to a user-provided reference output.

# Requirements and dependencies
At present, the software is only distributed in source form, and must be built by the user. The only currently supported operating system in Linux. Moreover, the following dependencies must be installed before building CrysP:

- CMake
- Fortran compiler: IFX or gfortran. IFX is preferred.
- C compiler: gcc
- Intel MKL

Additionally, the project has the following optional dependencies:

- Doxygen: for compiling the *CrysP-CLI* manual
- Ford: for generating structured documentation files
- Python/pip: for running the test suite

# Building the software
The project root directory contains a shell script named *build.sh*. Make sure this script is executable and run it from the project root:

    ./build.sh

This will compile and install both *libCrysP* and *CrysP-CLI*, using the IFX compiler. If IFX is not installed on your system, you must specify the gfortran compiler explicitly using the '-c' option:

    ./build.sh -c gfortran

The build script contains several other useful options. These are documented within the script itself.

Upon successful completion of the build script, a static library *libcrysp.a* will be located at *libcrysp/lib* and an executable *crysp* will be located at *crysp-cli/bin*. Refer to the [libCrysP README](/libcrysp/README.md) and [CrysP-CLI README](/crysp-cli/README.md) for details on their usage.
