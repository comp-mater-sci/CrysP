Common conventions used in the VEF     {#page_conventions}
==================================

This section describes the notation convention commonly used throughout the VEF software. 
This includes:
- naming convention for variables and computed quantities,
- convention for reference frames,
- convention for inputs and output,
- order of terms in compound entities such as tensors, vectors etc.


Naming convention for variables
-------------------------------

The manual generally adheres to the scientific notation of variables that is
commonly used in the field of mechanics. More specifically, the following conventions
are followed:

- Deviatoric tensors are denoted with capitalized boldface roman letters. 
  Examples: \f$ \mathbf{S} \f$ is deviatoric stress and \f$ \mathbf{A} \f$ is plastic 
  (deviatoric) strain rate mode.
- Second-rank tensors for stress and strain are denoted as \f$ \boldsymbol\sigma \f$ and 
  \f$ \boldsymbol\varepsilon \f$, respectively. 
- All other second-rank tensors are denoted with boldfaced uppercase letters.
- Vectors are denoted with boldface lowercase letters.
- Scalars are denoted with lowerase letters.  

Exceptions from these rules are clearly indicated where appropriate.

Note that strain tensors (and derived quantities) mentioned in the VEF are the 
__plastic strains__ unless stated otherwise. Likewise, the stresses calculated and 
reported by the VEF are usually the __deviatoric stresses__.

### Naming of variables in input and output files

In the input and output files the typesetting decorations (boldface etc.) are obviously 
dropped. Greek letters are replaced by their full names or abbreviations, of example: 
`sigma`, `rho`, `eps`.


Notation of quantities that depend on reference frame 
-----------------------------------------------------

The VEF uses two reference frames:

1. **The material reference frame**: the reference frame in which the material data 
   are given.

   To denote the components of second-order tensors in the material reference frame, 
   numeral indices are used. 
   
   For instance: \f$ \boldsymbol{\sigma}_{11} \f$, \f$ \mathbf{D}_{22} \f$,
   \f$ \mathbf{S}_{13} \f$.  

2. **The sample reference frame**: a local reference frame that is attached to 
   the virtual sample 

    Components of second-order tensors in the sample reference frame are indicated by  
    using the letters x,y and z to denote the axes. Repeated axes can be omitted.

    Examples: \f$ \sigma_x \f$ (or equivalent: \f$ \sigma_{xx} \f$), \f$ \mathbf{D}_y \f$, 
    \f$ \mathbf{S}_{xz} \f$


Notation for other quantities
-----------------------------

Apart from the notation related to the reference frames, the VEF also uses other indices. 
These usually indicate:
- a direction that characterizes a specific sample (e.g. \f$ S_0 \f$ for stress 
in sample at 0 degrees), 
- equivalent quantities (e.g. \f$ \varepsilon_{vM} \f$ for Von Mises equivalent strain).
