---
project: VEF
author: Stijn Schildermans
---

VEF is a multiscale framework for crystal plasticity modeling. It supports the following models

Deformation Mechanisms:

- FCC12
- BCC24
- BCC48

Hardening Models:

- No Hardening
- Voce
- SWIFT
- Dislocation Substructural Hardening (edge)
- Dislocation Substructural Hardening (screw)
- Dislocation Substructural Hardening (loop)

Mesoscale Models:

- Full Constraints Taylor
- ALAMEL

##General conventions
The documentation states all checks on the input parameters that are performed within the routine as well as how the routine behaves when any of those checks fail. Any constraints on the input parameters that are not explicitly mentioned are not checked for and may result in undefined behavior. These implicit constraints should however always be obvious (e.g. when a procedure expects a velocity gradient, its norm should not be 0, it should not contain NaNs, etc.). This decision was made to avoid having to write endless input checks and to improve performance. Instead, we expect input sanitization to be performed at the UI level.
