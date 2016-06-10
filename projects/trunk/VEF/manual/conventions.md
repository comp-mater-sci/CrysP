VEF common notation conventions     {#page_conventions}
===============================

VEF common notation conventions     {#conventions}
===============================

This section describes the notation conventions commonly used throughout the VEF software. 

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

### Naming of physical quantities in input and output files

Different conventions apply to physical quantities in input and output files:

- The typesetting decorations (boldface etc.) are obviously dropped.

- Greek letters are replaced by their full names or abbreviations.

  \egs `sigma`, `rho` and `eps`

- One or more indices are preceded by underscore symbol (`_`).

  \eg `eps_11`


Reference frames   {#convention_reference_frame}
----------------

The VEF uses two different reference frames in the physical, 3-dimensional space:

1. **The material reference frame**: the reference frame in which the material data 
   are given

   To denote the components of second-order tensors in the material reference frame, 
   numeral indices are used. Repeated axes can be omitted.
   
   \egs \f$ \sigma_{11} \f$, \f$ D_{22} \f$, \f$ S_{13} \f$.  

2. **The sample reference frame**: a local reference frame that is attached to 
   the virtual sample 

    Components of second-order tensors in the sample reference frame are indicated by  
    using the (lowercase) letters \f$x\f$, \f$y\f$ and \f$z\f$ to denote the axes. Repeated axes can be omitted.

    \egs \f$ \sigma_x \f$ (or equivalent: \f$ \sigma_{xx} \f$), \f$ D_y \f$, 
    \f$ S_{xz} \f$
	
Index notation   {#convention_indexnotation}
--------------

Apart from the notation related to the reference frames (cf. [above](@ref convention_reference_frame)),
 the VEF also uses other indices. 
These usually indicate:
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
<tr><td> \f$ \|\mathbf{x}\| \f$ <td> Euclidian norm of vector \f$ \mathbf{x} \f$
<tr><td> \f$ \|\mathbf{X}\| \f$ <td> Euclidian norm of tensor \f$ \mathbf{X} \f$
<tr><td> \f$ \dot{x} \f$ <td> 1st time derivative of \f$ x \f$
<tr><td> \f$ \overline{x} \f$ <td> Volume-average of (local variable) \f$ x \f$
</table>

Configuration options and configuration fields   {#convention_configuration}
----------------------------------------------

The \ref page_expert_mode makes use of configuration files, containing configuration fields 
that are grouped in configuration options (their exact definition is given in 
\ref alamdmc_format_configoptions). Some conventions are defined in order to facilitate 
readability of the documentation concerned:

- Configuration field names are typeset with bold typewriter font.

  \eg  \conffield_typeset{example_field} is a field name
  
- A declaration of a configuration field (or: field declaration) always involves giving its [type] (@ref field_type). By convention, the type of the field is embraced within parentheses.

  \eg The declaration \confdef{example_field,string} declares a field of type `string`

- A configuration option consists of one or more fields. Field declarations that are
  merely seperated by spacing in the documentation, belong to the same 
  configuration option: they need to be provided on a single line of configuration file, 
  and in the same order.
  
  \par &emsp; Example: 
  \confdef{field1,integer} \confdef{field2,real} is a configuration option 
	consisting of two fields, the first of type `integer`, and the second of type `real`.<BR>
	An example of the corresponding line in the configuration file might be:
~~~~~{.cfg}
1 0.7
~~~~~
<BR>
	
- The consecutive configuration options that belong to a specific part
  of the configuration file, are ennumerated in the documentation. Note that numbering may restart for a following section
  of configuration file.

  \par &emsp; Example:
  A configuration file section may be documented as follows:  
    -# \confdef{option1field,integer} is a first option (consisting of a single field).
	-# \confdef{option2field1,string} \confdef{option2field1,string} is a second (consecutive) option
	(consisting of two fields).<BR>
	
- Mandatory configuration options are always numbered as: 1. ,2. , 3. etc. If a configuration option has a
  limited number of supported values, those values may be given in an unummerated and indented list.
  Value-dependent suboptions that depend on the value of previous field, are documented as a Roman-numerated list (a., b., c., etc.), and they are indented accordingly. 

  \par &emsp; Example: 
  A configuration file section may be documented as follows: 
	-# \confdef{op1f,integer} is mandatory; it has no value-dependent suboptions.
	-# \confdef{op2f,string} is mandatory. The field \conffield_typeset{op2f} has 3 supported values:	`xx`, `yy`, and `zz`.
	  - `xx`<BR>
  	    If \conffield{op2f}=`xx`, there are two value-dependent suboptions:
	    -# \confdef{xx_subop2af,real} ...
	    -# \confdef{xx_subop2bf,logical} ... 
	      - `True`<BR>
  	        ... 
	      - `False`<BR>
	        ... 
	  - `yy`<BR>
	    If \conffield{op2f}=`yy`, there are no suboptions.
	  - `zz`<BR>
	    If \conffield{op2f}=`zz`, there is one 
	    value-dependent suboption:
	    -# \confdef{zz_subop2af,integer} ...
	-# \confdef{op3f,real} is mandatory; it has no value-dependent suboptions.

