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

- Tensors are denoted with capitalized boldface Latin letters, or with boldface Greek 
  letters.  
  Examples: \f$ \mathbf{S} \f$ is deviatoric stress, \f$ \mathbf{A} \f$ is deviatoric 
  strain rate mode,   \f$ \boldsymbol\sigma \f$ is the total stress and 
  \f$ \boldsymbol\epsilon \f$ the plastic strain.
- Vectors are denoted with lowercase boldface Latin letters. Examples: \f$ \mathbf{n} \f$ 
  is a plane normal vector.
- Scalars, including tensor components and vector components, are denoted with lower- or 
  uppercase, Latin or Greek letters. They are never boldface.
  Examples: \f$ W \f$ is plastic work, \f$ \sigma_h \f$ is the hydrostatic stress and 
  \f$ \sigma_{13} \f$ is a component of tensor \f$ \boldsymbol\sigma \f$.   

Exceptions from these rules are clearly indicated where appropriate.

Note that strain tensors (and derived quantities) mentioned in the VEF are the 
__plastic strains__ unless stated otherwise. Likewise, the stresses calculated and 
reported by the VEF are usually the __deviatoric stresses__.

### Naming of variables in input and output files

In the input and output files the typesetting decorations (boldface etc.) are obviously 
dropped. Greek letters are replaced by their full names or abbreviations, of example: 
`sigma`, `rho`, `eps`.
One or more indices are preceded by underscore symbol (`_`), for example: `eps_11`.


Reference frames, and components of vectors and tensors expressed therein 
-------------------------------------------------------------------------

The VEF uses two reference frames:

1. **The material reference frame**: the reference frame in which the material data 
   are given.

   To denote the components of second-order tensors in the material reference frame, 
   numeral indices are used. 
   
   For instance: \f$ \sigma_{11} \f$, \f$ D_{22} \f$,
   \f$ S_{13} \f$.  

2. **The sample reference frame**: a local reference frame that is attached to 
   the virtual sample 

    Components of second-order tensors in the sample reference frame are indicated by  
    using the letters x,y and z to denote the axes. Repeated axes can be omitted.

    Examples: \f$ \sigma_x \f$ (or equivalent: \f$ \sigma_{xx} \f$), \f$ D_y \f$, 
    \f$ S_{xz} \f$


Notation for other quantities
-----------------------------

Apart from the notation related to the reference frames, the VEF also uses other indices. 
These usually indicate:
- a direction that characterizes a specific sample (e.g. \f$ S_0 \f$ for stress 
in sample at 0 degrees), 
- equivalent quantities (e.g. \f$ \epsilon_{vM} \f$ for Von Mises equivalent plastic 
  strain).
