Notation conventions     {#page_conventions}
===============================
This page describes the notation conventions throughout this manual.

[TOC]

Physical quantities {#convention_physicalquantities}
-------------------
The manual generally adheres to the scientific notation of physical quantities that is
commonly used in the field of mechanics. More specifically, the following conventions
are followed:

- Tensors are denoted with capitalized boldface Latin letters, or with boldface Greek
  letters.

  \egs \f$ \mathbf{S} \f$ is deviatoric stress, \f$ \mathbf{A} \f$ is deviatoric
  strain rate mode,   \f$ \boldsymbol\sigma \f$ is the total stress and
  \f$ \boldsymbol\epsilon \f$ the plastic strain.

- Vectors are denoted with lowercase boldface Latin letters.

  \eg \f$ \mathbf{n} \f$ is a plane normal vector.

- Scalars, including tensor components and vector components, are denoted with lower- or
  uppercase, Latin or Greek letters. They are never boldface.

  \egs \f$ W \f$ is plastic work, \f$ \sigma_h \f$ is the hydrostatic stress and
  \f$ \sigma_{13} \f$ is a component of tensor \f$ \boldsymbol\sigma \f$.

Exceptions from these rules are clearly indicated where appropriate.

Note that strain tensors (and derived quantities) mentioned in the VEF are the
__plastic strains__ unless stated otherwise. Likewise, the stresses calculated and
reported by the VEF are usually the __deviatoric stresses__.

Index notation   {#convention_indexnotation}
--------------
Throughout this manual, indices are used to specify various quantities:

- Components of vectors are specified by a single latin numeral, with 1 refering to the first element.

    \eg \f$ \mathbf{n}_1 \f$ is the first element of the vector \f$ \mathbf{n} \f$.

- Components of second order tensors are specified by 2 latin numerals, the first being the row index and the second the column index. The index 1 refers to the first row or column, respectively.

    \eg \f$ \mathbf{S}_{21} \f$ is the element in the second row and first column of the second order tensor \f$ \mathbf{S} \f$.

- Arbitrary 2-dimensional sections of stress space or strain rate space

  The **section reference frame** is defined by two arbitrary axes within the
  6-dimensional stress space. The two axes defining the section are generally denoted
  by the (capitalized) letters \f$ X \f$ and \f$ Y \f$.

  \egs \f$ \sigma_X \f$ and \f$ \sigma_Y \f$

- A direction that characterizes a specific sample

  \eg \f$ S_0 \f$ for stress in sample at 0 degrees

- Equivalent quantities

  \eg \f$ \epsilon_{vM} \f$ is the Von Mises equivalent plastic strain

- Slip system-specific quantitites (usually denoted by index \f$ s \f$)

  \eg \f$ \dot\gamma_s \f$ is the slip rate on slip system \f$ s \f$


Mathematical operators   {#convention_operators}
----------------------
Notations of some common mathematical operators are:

<table>
<caption id="operators_table"></caption>
<tr><th>Operator  <th>Meaning and/or definition
<tr><td> \f$ |x| \f$ <td> Absolute value of scalar \f$ x \f$
<tr><td> \f$ \|\mathbf{x}\| \f$ <td> Euclidean norm of vector \f$ \mathbf{x} \f$
<tr><td> \f$ \|\mathbf{X}\| \f$ <td> Euclidean norm of tensor \f$ \mathbf{X} \f$
<tr><td> \f$ \dot{x} \f$ <td> 1st time derivative of \f$ x \f$
<tr><td> \f$ \overline{x} \f$ <td> Volume-average of (local variable) \f$ x \f$
</table>
