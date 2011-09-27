! $Id$
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

      901 format('Incorrect format of configuration file:', 1X, A)
	  902 format('Incorrect format of configuration file. Check',1X,A,1X,'specific section' ) 
	  
	  ! File access errors
	  951 format('Cannot open file ', A, ' for reading.')
	  952 format('Cannot open file ', A, ' for writing.')
	  
#endif
