module mod_eosmodels_ig
use mod_eosmodels
implicit none
 type, extends(EOSModel), public :: IG_EOSModel
  contains
    procedure :: init => initialize_ig
    procedure :: get => get_value_ig
  end type IG_EOSModel

contains

  subroutine initialize_ig(this)
    implicit none
    class(IG_EOSModel) :: this
  end subroutine

  function get_value_ig(this, x) result(y)
    implicit none
    class(IG_EOSModel) :: this
    real, intent(IN) :: x
    real :: y
    y = x*2.
  end function get_value_ig



end module mod_eosmodels_ig

