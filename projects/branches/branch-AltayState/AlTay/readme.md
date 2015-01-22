
New developments of the AlTay should adhere to code standarts defined below.

- Only free-format source code is allowed.

- Good indentation is crucial. Consistenlty use 4 spaces for indentation. Tabulators are not allowed as indenters.

- All keywords should be written using lowercase letters.

- Naming convention for various elements of the code should follow the rules:
	* names of modules: use a prefix which is a lowercase name (or abbreviation) of the library where the module belongs to.
	* names of types: you may use one of two conventions. 
		1) if name of the type does not contain a prefix from the module it is enclosed in, use camel-case E.g. type(EulerAngles)
		2) if the name of the type is prefixed by the name of the module where it is defined, you must use 
		   the name of the module literally. E.g. type(altayStateContainer) defined in the altayState module.
	* constants (parameters) defined in modules: it is advisable to use the same prefix for all public parameters defined in a given module. 
	  The prefix should has some link to the module name.
	* variables: In general, lowercase names should be used. 
	* preprocessor macros: use all-capitals, E.g. PEBP_ENABLED


- The code should conform at least with Fortran 2003 standard.

- In general, global state variables should be avoided. More specifically:
      * common blocks are _forbidden_,
      * module variables with "save" attribute are discouraged,
      * local variables with "save" attribute (either explicit or implicit)
        are _strongly discouraged_.

- Generally, every subroutine must return exit status. The exit status should be included as the last non-optional formal parameter. 
  A conventional name for the exit status formal parameter is "info".
  Named constansts from the criErrcodes modules must be used for setting its values.



