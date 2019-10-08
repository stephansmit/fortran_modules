program modular
  use Test
  ! use TurbModels
  ! use TurbModels_SST
  ! use TurbModels_MK

  implicit none


  ! class(AbstractTurbModel), allocatable :: m
  ! allocate(MKTurbModel::m)

  !type(SSTTurbModel), allocatable :: m
  !type(MKTurbModel), allocatable :: m
  integer model
  model = 2
  call init(model)




  write(*,*) m%value()

end program
