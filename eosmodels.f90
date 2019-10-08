
module EOSModels
implicit none

  type, abstract :: EOSModel
  real, public :: A   
  
  contains
      procedure(initialize), deferred :: init
      
  end type EOSModel

  interface
    subroutine initialize( this )
      import :: EOSModel
      class(EOSModel) :: this
    end subroutine initialize
  end interface


end module EOSModels


module IG_EOSModels
use EOSModels
implicit none

  type, extends(EOSModel), public :: IG_EOSModel 
  contains 
      procedure :: init
  end type IG_EOSModel

contains  
  subroutine init(this) 
    class(IG_EOSModel) :: this
    this%A = 4.0
  end subroutine

end module IG_EOSModels



module Table_EOSModels
use EOSModels
implicit none

  type, extends(EOSModel), public :: Table_EOSModel
  contains 
      procedure :: init
  end type Table_EOSModel

contains  
  subroutine init(this) 
    class(Table_EOSModel) :: this
    this%A = 6.0
  end subroutine

end module Table_EOSModels