---
project: CrysP
author: Stijn Schildermans
---

# Introduction
CrysP is an extensible toolkit for multilevel Crystal Plasticity Modeling. Its name simply stands for Crystal Plasticity, mostly due to a complete lack of inspiration from the authors. It is developed and maintained by KU Leuven, motivated by the need for and lack of up-to-date comparable frameworks. It is aimed towards academic/expert users who seek a modern, open-source, minimalistic and well-documented framework which automates both strain- and stress-driven crystal plasticity simulations while still providing the user with maximum control over the simulation process. It supports a variety of hardening models, slip systems and crystal plasticity models. It is designed specifically to be highly extensible, allowing users to contribute new models (or other features). Such contributions are highly encouraged.

For industrial/non-expert users, KU Leuven offers a (licensed) web-based graphical user interface for CrysP. Please contact the [KU Leuven NUMA research group](https://numa.cs.kuleuven.be/) for more information.

# Repository Contents
Crysp consists of 2 major components:
- **libCrysP**: this library contains the core framework functionality. It defines a concise interface and is designed to be embedded in user applications.
- **CrysP-CLI**: provides a minimalistic command-line interface to perform common crystal plasticity simulations using *libCrysP*.

Both components of CrysP can be seen as separate projects and are documented independently. Therefore, this README file is limited to a global overview of CrysP. It provides no details on the interface of *libCrysP* or the usage of *CrysP-CLI*. These are documented in their respective subdirectories. Refer to the [libCrysP README](/libcrysp/README.md) and [CrysP-CLI README](/crysp-cli/README.md) to get started.

Each of these subdirectories contains a README file as well. Refer to these for more details.

Additionally, the repository contains a build script for easy installation and a test suite providing integration tests, which compare a wide variety of simulation outputs to a user-provided reference output.

# Requirements and dependencies
At present, the software is only distributed in source form, and must be built by the user. The only currently supported operating system in Linux. Moreover, the following dependencies must be installed before building CrysP:

- CMake
- Fortran compiler: IFX or gfortran. IFX is preferred.
- C compiler: gcc
- Intel MKL
- OpenMP

Additionally, the project has the following optional dependencies:

- Doxygen: for compiling the *CrysP-CLI* manual
- Python/pip: for running the test suite and installing the Ford documentation generator
- Ford: for generating structured documentation files
- Pytest: testing framework used by the test suite

# Installation
The project root directory contains a shell script named *build.sh*. Make sure this script is executable and run it from the project root:

    ./build.sh

This will compile and install both *libCrysP* and *CrysP-CLI*, using the IFX compiler. If IFX is not installed on your system, you must specify the gfortran compiler explicitly using the '-c' option:

    ./build.sh -c gfortran

The build script contains several other useful options. These are documented within the script itself.

Upon successful completion of the build script, a static library *libcrysp.a* will be located at *libcrysp/lib* and an executable *crysp* will be located at *crysp-cli/bin*. Refer to the [libCrysP README](/libcrysp/README.md) and [CrysP-CLI README](/crysp-cli/README.md) for details on their usage.

# Contributing
Community contributions to *CrysP* are highly engcouraged, especially in the form of new constitutive/crystal plasticity models. To contribute new code, fork a new branch from master with your changes and create a merge request when appropriate. It will be reviewed as soon as possible by a project maintainer.

## Conventions
When writing contributions, please keep the following conventions in mind:

- All procedures are pure. They should not depend on any state outside of their input arguments and should not alter any state outside their scope. They only interact with the outside world through their arguments and return value.
- All procedures at the *meso* and *micro* level should be thread-safe.
- Names of procedures, arguments, constants, classes, fields, enumerators, interfaces or modules should have meaningful names and not contain abbreviations, with the exception of the accepted abbreviations listed below.
- All procedures, arguments, constants, classes, fields, enumerators, interfaces and modules should be documented.
- Procedure documentation should state all checks on the input parameters that are performed directly withing the procedure as well as how the procedure behaves when any of those checks fail. Any constraints on the input parameters that are not explicitly mentioned are not checked for and may result in undefined behavior. These implicit constraints should however always be obvious (e.g. when a procedure expects a velocity gradient, its norm should not be 0, it should not contain NaNs, etc.).
- Top-level *libCrysP* routines should perform full input sanitization. All other routines should keep sanitization to a minimum. It is up to the caller to make sure arguments do not contain trivial errors (e.g. NaN values). This provides a healthy balance between correctness and performance.
- On error condition, all procedures should call *log_error* with meaningful arguments.
- Local variables may be abbreviated but their meaning should be documented if not obvious.
- Names of procedures, arguments, local variables, fields and interfaces follow snake case convention.
- Constants follow screaming snake case convention.
- Class names follow Pascal case convention.
- Each new model should be contained in a singular module, located in the *models* folder of the corresponding abstraction layer.

## Testing
The [test](/test) directory contains an integration test suite based on *pytest*. It generates a large number of simulations, and compares the results to some reference results. The user must first generate these reference results from the current state of the master branch:

    git checkout master
    git pull
    ./build.sh
    cd test
    pytest -m integration --update=out

The reference results are stored in *test/reference*. Once this is done, you can test your local version as folows:

    git checkout [my_patch]
    ./build.sh
    cd test
    pytest -m integration

The integration tests will pass if none of the results in your working branch deviate by more than 1% from the reference results. The default marging of 1% may be overriden through the *--margin* option. For a 2% margin:

    pytest -m integration --margin=2

Adding or modifying test configurations may be done by altering the *test/vef_config.py* and *test/test_vef.py* files appropriately.

## Accepted abbreviations
Below an exhaustive list of acceptable abbreviations for use within procedure, argument, field, constant, interface or class names:

- **crss**: critical resolved shear stress
- **rss**: resolved shear stress
- **fcc**: face-centered cubic
- **bcc**: body-centered cubic
