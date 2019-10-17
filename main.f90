PROGRAM nltest

IMPLICIT NONE

integer   i,k,imax,kmax,i1,k1,iwork,isave,px,nTab,LoD
integer   EOSmode,periodic,kmaxper,k1Old
integer   nstep,K_start_heat,x_start_heat,select_init,systemSolve
integer   turbmod,modifDiffTerm,modVF,profiling,isothermalBC,pressIsoThermal
real*8    Re,Pr,Qwall,CFL,Tw,dTwall
real*8    ctheta,Fr_1
real*8    alphac,alphak,alphae,alphav2

NAMELIST /input/ CFL, systemsolve, imax, K_start_heat, x_start_heat,      &
                 iwork, isave, nTab, nstep, LoD, periodic, Re, Pr, Qwall, &
                 isothermalBC, Tw, dTwall, pressIsoThermal, Fr_1, EOSmode, &
                 select_init, turbmod, ctheta, modVF, alphac,alphak,alphae, &
                 alphav2, modifDiffTerm



! ! Set some values
! iarr     = (/-16, 144, 0/)
! j        = 255
! ascalar  = 3.1415926535
! label    = 'Hello world'

! PRINT 100, iarr, j, ascalar, label

! OPEN (UNIT=10, FILE='nltest.dat', STATUS='UNKNOWN')
! WRITE (10, NML=qt)
! CLOSE (10)

! PRINT 100, iarr, j, ascalar, label

! ! Change the values
! iarr     = (/3457389, -1, -9999/)
! j        = 111
! ascalar  = 5e-12
! label    = 'Hello there'

! PRINT 100, iarr, j, ascalar, label

OPEN (UNIT=10, FILE='new_sample.nml', STATUS='OLD')
READ (10, NML=input)
CLOSE (10)

write(*,*) EOSmode

END PROGRAM nltest
