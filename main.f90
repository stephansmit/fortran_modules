program modular
  use mod_eosmodels
  implicit none
  include 'mpif.h'
  integer ploc,ierr, rank
  real(8) Re, Pr, output_ig,output_table, value
  class(EOSModel), pointer :: eosmodel_ig
  class(EOSModel), pointer :: eosmodel_table

  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD,rank,ierr)
  call mpi_comm_size(MPI_COMM_WORLD,ploc,ierr)

  Re = 200
  Pr = 200
  value = 0.01  

  allocate(eosmodel_ig,    source=IG_EOSModel(Re,Pr))
  allocate(eosmodel_table, source=Table_EOSModel(Re,Pr,2000,"co2h_table.dat"))

  call eosmodel_ig%init()
  call eosmodel_table%init()
  
  call eosmodel_ig%   set(value,'L', output_ig)
  call eosmodel_table%set(value,'L', output_table)

  write(*,*) output_ig, output_table

  call mpi_finalize(ierr)

end program
