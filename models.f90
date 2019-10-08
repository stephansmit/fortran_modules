module Models

use EOSModels

implicit none
public 
  class(EOSModel), allocatable :: eosm

contains
  subroutine initEOSModel(model)
    use Table_EOSModels
    use IG_EOSModels

    implicit none
    integer model
    if (model .eq. 1) then
      allocate(IG_EOSModel::eosm)
    else
      allocate(Table_EOSModel::eosm)      
    endif
    call eosm%init()
  end subroutine initEOSModel

end module Models




