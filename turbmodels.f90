module TurbModels
  
  type, abstract :: AbstractTurbModel
    contains
      procedure(getValue), deferred :: value
  end type AbstractTurbModel

  interface
    function getValue( this ) result( A )
      import :: AbstractTurbModel
      class(AbstractTurbModel) :: this
      real :: A
    end function getValue
  end interface

end module TurbModels



