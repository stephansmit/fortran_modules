program hello
IMPLICIT none
include 'mpif.h'

! Fortran 90 users can (and should) use 
!     use mpi 
! instead of include 'mpif.h' if their MPI implementation provides a 
! mpi module. 
! include 'mpif.h' 

integer ierr, i, myrank, k1,i1, j,thefile, offset, status, nints,ind,px, size,fh,index
real(8), dimension(:,:),allocatable :: var1, var2, var3
real(8), dimension(:,:),allocatable :: nvar1, nvar2, nvar3
character(len=61), dimension(:), allocatable :: lines, lines2
real(8), dimension(:,:,:), allocatable :: allvars
integer(kind=MPI_OFFSET_KIND) disp 
integer k_min, k_max
character(len=61) :: line
character(len=60) test 

i1=2
k1=4
allocate(var1(0:i1,0:k1),var2(0:i1,0:k1),var3(0:i1,0:k1))
allocate(nvar1(0:i1,0:k1),nvar2(0:i1,0:k1),nvar3(0:i1,0:k1))



! allocate(lines2(0:(i1+1)*(k1)-1))


call MPI_INIT(ierr) 
call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr) 
call mpi_comm_size(MPI_COMM_WORLD,px,ierr)

do i = 0,i1
  do j = 0,k1
    var1(i,j) = myrank
    var2(i,j) = myrank
    var3(i,j) = myrank
  enddo 
enddo

call write_mpiio_formatted("testfile.dat",var1,var2,var3,i1,k1,myrank,px)

call MPI_FINALIZE(ierr) 

contains

subroutine write_mpiio_formatted(filename, var1, var2, var3, i1, k1,rank,px)
  implicit none 
  include "mpif.h"
  character(*),                  intent(IN) :: filename
  real(8), dimension(0:i1,0:k1), intent(IN) :: var1,var2,var3
  integer,                       intent(IN) :: i1,k1,rank,px
  integer nvar
  integer(kind=MPI_OFFSET_KIND) disp 
  character(len=61), dimension(:), allocatable :: lines, lines2
  nvar = 3
  index=1

  !first core write from 0 to k1-1
  if (myrank .eq. 0) then
    k_min = 0
    k_max = k1-1
    allocate(lines(1:(i1+1)*k1+1)) !+1 for header
    disp = 0
    size = ((i1+1)*(k1)+1)*(nvar*20+1)
    write(test,'(3(A20))') 'var1', 'var2', 'var3' !write the header
    write(line, '(A)') test // NEW_LINE("A")
    lines(index) = line
    index = index+1
  !last core write from 1 to k1
  else if (myrank .eq. px-1) then
    k_min = 1
    k_max = k1
    allocate(lines(1:(i1+1)*k1))
    size =           (i1+1)*(k1)*(nvar*20+1)
   disp = ((i1+1)*(k1)+1)*(nvar*20+1) + (myrank-1)*(i1+1)*(k1-1)*(nvar*20+1)
  !other core write from 1 to k1-1
  else
    k_min = 1
    k_max = k1-1
    allocate(lines(1:(i1+1)*(k1-1)))
    size =           (i1+1)*(k1-1)*(nvar*20+1)
    disp = ((i1+1)*(k1)+1)*(nvar*20+1) + (myrank-1)*(i1+1)*(k1-1)*(nvar*20+1)
  endif
  do i = 0,i1
    do j = k_min,k_max
      write(test,'(3(E20.12))') var1(i,j), var2(i,j), var3(i,j)
      write(line, '(A)') test // NEW_LINE("A")
      lines(index) = line
      index=index+1
    enddo
  enddo

  call MPI_FILE_OPEN(MPI_COMM_WORLD, filename,MPI_MODE_WRONLY + MPI_MODE_CREATE,MPI_INFO_NULL, fh, ierr) 
  call MPI_FILE_SET_VIEW(fh, disp, MPI_CHAR, MPI_CHAR, 'native', MPI_INFO_NULL, ierr) 
  call MPI_FILE_WRITE(fh, lines, size, MPI_CHAR,MPI_STATUS_IGNORE, ierr) 
  call MPI_FILE_CLOSE(fh, ierr) 
end subroutine write_mpiio_formatted

subroutine write_mpiio(filename, var1, var2, var3, i1, k1, rank)
  implicit none
  include "mpif.h"
  character(*),                  intent(IN) :: filename
  real(8), dimension(0:i1,0:k1), intent(IN) :: var1,var2,var3
  integer,                       intent(IN) :: i1,k1,rank
  real(8), dimension(0:i1,0:k1,1:3) :: tmpvars
  integer(kind=MPI_OFFSET_KIND) disp 
  integer :: fh

  tmpvars(:,:,1)=var1
  tmpvars(:,:,2)=var2
  tmpvars(:,:,3)=var3
  disp = rank * (1+k1)*(1+i1)*8*3

  call MPI_FILE_OPEN(MPI_COMM_WORLD, filename,MPI_MODE_WRONLY + MPI_MODE_CREATE,MPI_INFO_NULL, fh, ierr) 
  call MPI_FILE_SET_VIEW(fh, disp, MPI_REAL, MPI_REAL, 'native', MPI_INFO_NULL, ierr) 
  call MPI_FILE_WRITE(fh, tmpvars, (1+k1)*(1+i1)*3*2, MPI_REAL,MPI_STATUS_IGNORE, ierr) 
  call MPI_FILE_CLOSE(fh, ierr) 

end subroutine


subroutine read_mpiio(filen, var1, var2, var3, i1, k1, rank)
  implicit none
  include "mpif.h"
  character(*),       intent(IN) :: filen
  real(8), dimension(0:i1,0:k1), intent(OUT) :: var1,var2,var3
  integer,                 intent(IN) :: i1,k1,rank
  real(8), dimension(0:i1,0:k1,1:3) :: tmpvars
  integer(kind=MPI_OFFSET_KIND) disp 
  integer :: fh

  disp = rank * (1+k1)*(1+i1)*8*3
  call  MPI_FILE_OPEN(MPI_COMM_WORLD, filen,MPI_MODE_RDONLY,  MPI_INFO_NULL,  fh, ierr) 
  call  MPI_FILE_READ_AT(fh,  disp, tmpvars,  k1*i1*3*2, MPI_REAL,  status, ierr) 
  call  MPI_FILE_CLOSE(fh,  ierr) 
  var1=tmpvars(:,:,1)
  var2=tmpvars(:,:,2)
  var3=tmpvars(:,:,3)

  
end subroutine


end



! IMPLICIT NONE

! integer   i,k,imax,kmax,i1,k1,iwork,isave,px,nTab,LoD
! integer   EOSmode,periodic,kmaxper,k1Old
! integer   nstep,K_start_heat,x_start_heat,select_init,systemSolve
! integer   turbmod,modifDiffTerm,modVF,profiling,isothermalBC,pressIsoThermal
! real*8    Re,Pr,Qwall,CFL,Tw,dTwall
! real*8    ctheta,Fr_1
! real*8    alphac,alphak,alphae,alphav2

! NAMELIST /input/ CFL, systemsolve, imax, K_start_heat, x_start_heat,      &
!                  iwork, isave, nTab, nstep, LoD, periodic, Re, Pr, Qwall, &
!                  isothermalBC, Tw, dTwall, pressIsoThermal, Fr_1, EOSmode, &
!                  select_init, turbmod, ctheta, modVF, alphac,alphak,alphae, &
!                  alphav2, modifDiffTerm

! program test_get_command_argument
!   integer :: i
!   character(len=32) :: arg

!   i = 0
!   do
!     call get_command_argument(i, arg)
!     if (len_trim(arg) == 0) exit

!     write (*,*) trim(arg)
!     i = i+1
!   end do
! end program

! ! Set some values
! iarr     = (/-16, 144, 0/)
! j        = 255ma
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

! PRINT 100, iarr, j, ascalar, labelma

! OPEN (UNIT=10, FILE='new_sample.nml', STATUS='OLD')
! READ (10, NML=input)
! CLOSE (10)

! write(*,*) EOSmode

! END PROGRAM nltest
