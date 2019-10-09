! module mod_derivedtypes
!   use mod_types
!   implicit none

!   type, extends(TBaseClass) :: TDerivedClass
  
!   contains
!     procedure :: init => initialize_table
!     ! procedure :: set_constants => set_constants
!   end type TDerivedClass


!   ! type, extends(TDerivedClass) :: TDerivedClassCO2
!   ! contains
!   !   procedure :: init => initialize_co2
!   ! end type TDerivedClassCO2

!   ! type, extends(TDerivedClass) :: TDerivedClassH2
!   ! contains
!   !   procedure :: init => initialize_h2
!   ! end type TDerivedClassH2


! contains
!   subroutine initialize_table(this)
!     implicit none
!     class(TDerivedClass) :: this
!   end subroutine


!   ! subroutine set_constants(this, ntab, filename)
!   !   implicit none
!   !   class(TDerivedClass) :: this
!   !   integer, intent(in) :: ntab
!   !   character(len=20), intent(in) :: filename
!   !   this%filename = filename
!   !   this%ntab = ntab
!   ! end subroutine

  
!   ! subroutine initialize_co2(this)
!   !   implicit none
!   !   class(TDerivedClassCO2) :: this
!   !   call set_constants(this, 2000, "co2")
    
!   ! end subroutine

!   ! subroutine initialize_h2(this)
!   !   implicit none
!   !   class(TDerivedClassH2) :: this
!   !   call set_constants(this, 2000, "h2")
!   ! end subroutine



! end module mod_derivedtypes