module TurbModels_MK
  use TurbModels
  implicit none
  private

  type, extends(AbstractTurbModel), public :: MKTurbModel
  contains 
    procedure :: value
  end type MKTurbModel

contains
  function value(this) result(A)
    class(MKTurbModel) :: this
    real :: A
    A = 4.0
  end function

end module TurbModels_MK



