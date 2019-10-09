module mod_eosmodels_table
  use mod_eosmodels
  implicit none 

  type, extends(EOSModel), public :: Table_EOSModel
  integer ntab
  character(len=20) filename
  contains
    procedure :: init => initialize_table
    procedure :: get => get_value_table
  end type Table_EOSModel

contains

  subroutine initialize_table(this)
    implicit none
    class(Table_EOSModel) :: this
  end subroutine

  function get_value_table(this, x) result(y)
    implicit none
    class(Table_EOSModel) :: this
    real, intent(IN) :: x
    real :: y
    y = x*4.
  end function get_value_table


end module mod_eosmodels_table