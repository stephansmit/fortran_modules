module Models

use EOSModels

implicit none
public 
  class(EOSModel), allocatable :: eosm

contains
  subroutine initEOSModel(model)
    ! use H2Table_EOSModels
    use Table_EOSModels
    use IG_EOSModels

    implicit none
    integer model
    if (model .eq. 1) then
      allocate(IG_EOSModel::eosm)
      call eosm%init()

    ! else if (model .eq. 2) then
    !   allocate(CO2Table_EOSModel::eosm)      
    else
      allocate(Table_EOSModel::eosm)   
      ! call eosm%set_table('co2h_table.dat', 2000) 
      call eosm%init()
    endif
  end subroutine initEOSModel

end module Models




