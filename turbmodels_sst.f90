module TurbModels_SST
  use TurbModels
  implicit none
  private

  type, extends(AbstractTurbModel), public :: SSTTurbModel
  contains 
    procedure :: value
  end type SSTTurbModel

contains
  function value(this) result(A)
    class(SSTTurbModel) :: this
    real :: A
    A = 2.0
  end function

end module TurbModels_SST