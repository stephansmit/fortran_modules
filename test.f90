module Test

use TurbModels

implicit none
public 
  class(AbstractTurbModel), allocatable :: m

contains
  subroutine init(model)
    use TurbModels_SST
    use TurbModels_MK

    implicit none
    integer model
    if (model .eq. 1) then
      allocate(SSTTurbModel::m)
    else
      allocate(MKTurbModel::m)      
    endif
  
  end subroutine init

end module Test
