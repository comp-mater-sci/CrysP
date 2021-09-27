! $Id: msgFormats.inc 3401 2020-07-24 16:44:37Z Matthias.Bonisch $
!
! NOTE:
! Message labels reserved for user's module usage:
! 1 -- 100
! > 1000
!
! Rulers and decorations
#ifdef MSG_GROUP_RULERS

      800 format(112('='))
      801 format(112('-'))
    
#endif

! Error messages
#ifdef MSG_GROUP_ERRORS
      850 format(/,'Note:',1X,A,/)
      860 format(/,'Warning:',1X,A,/)
      900 format(/,'Error:',1X,A,/)

      901 format('Error: Incorrect format of configuration file.', 1X, A)
      902 format('Error: Incorrect format of configuration file. Check',1X,A,1X,'specific section' ) 
      
      930 format('Error:',1X,A,1X,'Make sure that VEF_ROOT variable is set.')
      ! File access errors
      951 format('Error: Cannot open file ', A, ' for reading.')
      952 format('Error: Cannot open file ', A, ' for writing.')
      
      960 format('Error: Search for the corresponding strain rate failed.')
      970 format('Error: Cannot run the multilevel model')
      980 format('Error: Cannot access the state of the multilevel model')
#endif
