#ifdef MSG_GROUP_ERRORS
      850 format(/,'Note:',1X,A,/)
      860 format(/,'Warning:',1X,A,/)
      900 format(/,'Error:',1X,A,/)

      902 format('Error: Incorrect format of configuration file. Check',1X,A,1X,'specific section' ) 
      
      930 format('Error:',1X,A,1X,'Make sure that VEF_ROOT variable is set.')
      
      960 format('Error: Search for the corresponding strain rate failed.')
      980 format('Error: Cannot access the state of the multilevel model')
#endif
