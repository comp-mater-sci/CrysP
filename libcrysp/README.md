---
project: CrysP
author: Stijn Schildermans
---

# Introduction
LibCrysP is the core of the CrysP framework. It is a library exposing various high-level routines for crystal plasticity modeling. It may be built as part of the CrysP framework or as a stand-alone statically linked archive. Users are free to incorporate this archive into their own software, given they adhere to the CrysP [license](../LICENSE). This document provides useful information to get started with this process. Users purely interested in *libCrysP* as part of the *CrysP* framework can safely ignore this file and instead read the [CrysP-CLI README](../crysp-cli/README.md).

# General Outline
LibCrysP is highly modular in nature. It consists of layers, which represent the 3 levels of abstraction in the multiscale crystal plasticiy modeling process:

- **micro**: Models intra-grain processes. This includes the definition of the slip systems and the hardening behavior of the material. It is described by a *constitutive model*.
- **meso**: Models localized stresses and strains for small clusters of grains. It is described by a *crystal plasticity model*.
- **macro**: Models global material behavior. It assimilates the results from the *meso* and *micro* layers and optionally calibrates a *yield_model* based on them.

The top-level module of each layer lists the supported models. The layers are designed such that adding new models is as easy as possible. At each layer, a module containing an abstract class definition describing the interface of a model for that respective layer is present. To add a new model, simply extend the abstract class and implement the entire interface. Then, add a unique numerical ID for the new model in the top-level module of the layer.

Details on which models are supported at each layer, as well as the interfaces of the models and the routines exposed by *libCrysP* as a whole, are described in the source documentation. See below.

# Documentation
The source documentation provides a full reference of all procedures, interfaces, classes (including models) and modules within *libCrysP*. The documentation follows the [FORD](https://forddocs.readthedocs.io/en/stable/) syntax. Therefore, Ford can be used to generate independent documentation files directly from the source code. Refer to the Ford documentation for details.

Note that the Ford documentation file for the top-level [libcrysp](/src/libcrysp.f90) serves as the formal *libCrysP* API reference.

# Installation
To build *libCrysP* stand-alone, execute the following commands from the *libCrysP* directory:

    cmake -B build
    cmake --build build --parallel --target install

Upon sucessful completion of the cmake commands, the *libcrysp/lib* directory should contain the *libcrysp.a* archive, which can be statically linked into your project.

By default, the project is configured to build a highly optimized release configuration using the default Fortran compiler of the system. It is possible to switch compilers by setting the *FC* environment variable. To build in debug mode rather than release, add the appropriate CMAKE option:

    export FC="[ifx/gfortran]"
    cmake -B build -DCMAKE_BUILD_TYPE=[release/debug]
    cmake --build build --parallel --target install

# Contributing
Community contributions to *libCrysP* are highly engcouraged, especially in the form of new models. To contribute new code, feel free to create a merge request containing your contributions. It will be reviewed as soon as possible by a project maintainer.


## Conventions
When writing contributions to *libCrysP*, please keep the following conventions in mind:

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

## Accepted abbreviations
Below an exhaustive list of acceptable abbreviations for use within procedure, argument, field, constant, interface or class names:

- **crss**: critical resolved shear stress
- **rss**: resolved shear stress
- **fcc**: face-centered cubic
- **bcc**: body-centered cubic
