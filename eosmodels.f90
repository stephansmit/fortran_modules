module mod_eosmodels
  implicit none

  type, abstract, public :: EOSModel
  contains
    procedure(init), deferred :: init
    procedure(get), deferred :: get
    procedure :: getfield
  end type EOSModel

  interface
    subroutine init(this)
      import :: EOSModel
      class(EOSModel) :: this
    end subroutine init

    function get( this, x ) result( y )
      import :: EOSModel
      class(EOSModel) :: this
      real, intent(IN) :: x
      real :: y
    end function get

  end interface

contains
  function getfield( this, x ) result( y )
    class(EOSModel) :: this
    real, intent(IN) :: x
    real :: y
    y = this%get(x) + this%get(x)
  end function getfield

 
end module mod_eosmodels
