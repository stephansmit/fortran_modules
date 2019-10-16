module mod_classes
  implicit none


  !!!!!!!!!!!!!!!!!!!!!!!!!!
  !     Abstract class     !
  !!!!!!!!!!!!!!!!!!!!!!!!!!
  
  type, abstract, public :: BaseClass
  real(8) :: Re, Pr
  contains
    procedure(init_bc), deferred :: init
    procedure(set_bc), deferred :: set
  end type BaseClass

  interface
    subroutine init_bc(this)
      import :: BaseClass
      class(BaseClass) :: this
    end subroutine init_bc
    subroutine set_bc( this, string,output)
      import :: BaseClass
      class(BaseClass) :: this
      character(len=1), intent(IN) :: string
      real(8), intent(OUT) :: output
    end subroutine set_bc
  end interface


  !!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Second Abstract class  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!
  
  type, abstract, extends(BaseClass), public :: SecondBaseClass
  contains
    procedure(init_sbc), deferred :: init
    procedure(set_sbc), deferred :: set
  end type SecondBaseClass

  interface
    subroutine init_sbc(this)
      import :: SecondBaseClass
      class(SecondBaseClass) :: this
    end subroutine init_sbc
    subroutine set_sbc( this, string,output)
      import :: SecondBaseClass
      class(SecondBaseClass) :: this
      character(len=1), intent(IN) :: string
      real(8), intent(OUT) :: output
    end subroutine set_sbc
  end interface



  !!!!!!!!!!!!!!!!!!!!!!!!!!
  !       Table class      !
  !!!!!!!!!!!!!!!!!!!!!!!!!!
  
  type, extends(SecondBaseClass), public :: Implemented
  contains
    procedure :: init => initialize_imp
    procedure :: set => set_w_imp
   end type Implemented



contains
  ! type(SecondBaseClass) function init_SecondBaseClass(Re,Pr)
  !   real(8), intent(in) :: Re, Pr
  !   init_SecondBaseClass%Re = Re
  !   init_SecondBaseClass%Pr = Pr
  ! end function init_SecondBaseClass

  type(Implemented) function init_Implemented(Re,Pr)
    real(8), intent(in) :: Re, Pr
    init_Implemented%Re = Re
    init_Implemented%Pr = Pr
  end function init_Implemented

  !!!!!!!!!!!!!!!!!!!!!!!!!!
  !   Ideal Gas routines   !
  !!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine initialize_imp(this)
    implicit none
    class(Implemented) :: this
  end subroutine initialize_imp

  subroutine set_w_imp(this, string, output)
    implicit none
    class(Implemented) :: this
    character(len=1), intent(IN) :: string
    real(8), intent(OUT) :: output
    output = 4
  end subroutine set_w_imp


end module mod_classes
