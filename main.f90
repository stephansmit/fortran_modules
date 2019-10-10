program modular

  use mod_eosmodels
  implicit none

  include 'mpif.h'


  integer      ploc,ierr,istart,noutput, rank
  real*8       bulk,stress,stime,time1,time2,timer,time3,dif,adv


  real(8) value
  real(8) Re, Pr
  class(EOSModel), pointer :: test1
  class(EOSModel), pointer :: test2


  call cpu_time(time1)
  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD,rank,ierr)
  call mpi_comm_size(MPI_COMM_WORLD,ploc,ierr)

  Re = 200
  Pr = 200
  

  allocate(test1, source=IG_EOSModel(Re,Pr))
  allocate(test2, source=Table_EOSModel(Re,Pr,2000,"co2h_table.dat"))

  call test1%init()
  call test2%init()
  
  value = 0.01
  call test1%set(value,'L', value)
  write(*,*) value
  value = 0.01
  
  call test2%set(value,'L', value)
  write(*,*) value

  call mpi_finalize(ierr)

end program




! subroutine testbase(input)
!   use mod_types
!   implicit none
!   type(TDerivedClass) :: input
!   write(*,*) input%in2



! end subroutine testbase