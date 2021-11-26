
There are several routines that are instrumented with RCM. Any call to these subroutines has be guarded by RCM_GUARD:
- SIMUL - done
- TAYLOR - done
- TAYLR1 - done
- Pancak2 - done
- TBH - done
- eigenv: (RCM dependent directly and via canoni)- done
- canoni - done
- STORE - done
- sliprat (indirectly RCM dependent via STORE) - done
- GETANG (indirectly RCM dependent  via eigenv) - done

