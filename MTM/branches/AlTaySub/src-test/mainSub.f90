!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of first release: 2010-10-18
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!>    \file mainSub.f90 Test program for ALAMEL subroutine
!>    
!
#ifdef ALTAY_SUBROUTINE

program alamelSubTest
use altayConfig
use altaySub
use verifySub
use testTexAccess
implicit none


     
      ! Main program - preinitialization + shared configuration
      !
      ! Apply as many modifications to acnf as needed.
      acnf%output_prefix = 'example'
      acnf%jobtitle = 'example job'
      acnf%micros_fname = 'micro1.smt'
      ! configure slipsystem data
      !acnf%slipsystem%input_fname = 'bcc.pre'
      acnf%slipsystem%input_fname = 'fcc.pre'
      !acnf%slipsystem%input_fname = 'bcc2.pre'

      ! configure texture data
      ! >> small dataset
      !acnf%texture%input_fname='alum39.smt'
      ! >> typical dataset
      acnf%texture%input_fname='A612LM.SMT'
      acnf%texture%input_type = 1
      !
      !acnf%texture%input_fname= 'example_0.CUR'
      !acnf%texture%input_type = 2
      !acnf%texture%input_fname= 'example_0.CUB'
      !acnf%texture%input_type = 3
      acnf%texture%block_id = 0
      
      ! call MMM test
      ! call verifyMMMmode(modelAlamel)

      call verifyAltayExample()
      
      
      ! call testTexAccessModules()
      
      ! call testSMTAccess()
      
end program

#endif     
