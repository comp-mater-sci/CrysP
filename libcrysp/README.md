---
project: libCrysP
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
Refer to the [top-level README file](../README.md).
