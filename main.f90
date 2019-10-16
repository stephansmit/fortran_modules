program modular
  use mod_classes
  implicit none
  include 'mpif.h'
  integer ploc,ierr, rank
  real(8) Re, Pr, output_ig,output_table, value
  class(BaseClass), allocatable :: eosmodel_ig
  
  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD,rank,ierr)
  call mpi_comm_size(MPI_COMM_WORLD,ploc,ierr)

  Re = 200
  Pr = 200
  ! value = 0.01  

  allocate(eosmodel_ig,    source=init_Implemented(Re,Pr))
  ! allocate(eosmodel_table, source=Table_EOSModel(Re,Pr,2000,"co2h_table.dat"))

  call eosmodel_ig%init()
  
  call eosmodel_ig%   set('L', output_ig)
  
  write(*,*) output_ig

  call mpi_finalize(ierr)

end program
