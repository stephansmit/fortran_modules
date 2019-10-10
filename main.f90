program modular
  use mod_eosmodels
  implicit none
  include 'mpif.h'
  integer ploc,ierr, rank
  real(8) Re, Pr, output, value
  class(EOSModel), pointer :: eosmodel_ig
  class(EOSModel), pointer :: eosmodel_table


  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD,rank,ierr)
  call mpi_comm_size(MPI_COMM_WORLD,ploc,ierr)

  Re = 200
  Pr = 200
  
  allocate(eosmodel_ig,    source=IG_EOSModel(Re,Pr))
  allocate(eosmodel_table, source=Table_EOSModel(Re,Pr,2000,"co2h_table.dat"))


  call eosmodel_ig%init()
  call eosmodel_table%init()
  

  value = 0.01
  call eosmodel_ig%set(value,'L', output)
  write(*,*) output
  call eosmodel_table%set(value,'L', output)
  write(*,*) output

  call mpi_finalize(ierr)

end program




! subroutine testbase(input)
!   use mod_types
!   implicit none
!   type(TDerivedClass) :: input
!   write(*,*) input%in2



! end subroutine testbase