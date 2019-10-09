
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





! module Table_EOSModels
! use EOSModels
! implicit none


!   type, extends(EOSModel), public :: Table_EOSModel
!   real(8), dimension(:), allocatable :: tempTab,rhoTab,betaTab, muTab,lamTab, cpTab,enthTab,lamocpTab, &
!                                         temp2Tab,rho2Tab,beta2Tab, mu2Tab,lam2Tab, cp2Tab,enth2Tab,lamocp2Tab
!   integer, public :: nTab 
!   Character(len =30), public :: tablefile
!   contains 
!       procedure :: init
!   end type Table_EOSModel

! contains  

!   subroutine init(this)
!     implicit none
!     class(Table_EOSModel) :: this
!     call allocateMem(this)
!     call readtable(this)
!     call calc_int_coeff(this)
!   end subroutine init


!   subroutine set_table(this, filename, ntab)
!     implicit none
!     class(Table_EOSModel) :: this
!     integer nTab
!     Character(len=30) filename
!     this%nTab = ntab
!     this%tablefile = 'co2h_table.dat'
!   end subroutine set_table


!   subroutine allocateMem(this) 
!     implicit none
!     class(Table_EOSModel) :: this
!     integer i
!     allocate(this%tempTab(1:this%nTab),this%rhoTab(1:this%nTab),this%betaTab(1:this%nTab), &
!              this%muTab(1:this%nTab),this%lamTab(1:this%nTab),this%cpTab(1:this%nTab), &
!              this%enthTab(1:this%nTab),this%lamocpTab(1:this%nTab),this%temp2Tab(1:this%nTab), &
!              this%rho2Tab(1:this%nTab),this%beta2Tab(1:this%nTab),this%mu2Tab(1:this%nTab), &
!              this%lam2Tab(1:this%nTab),this%cp2Tab(1:this%nTab),this%enth2Tab(1:this%nTab), &
!              this%lamocp2Tab(1:this%nTab))
!   end subroutine

  
!   subroutine readtable(this)
!     implicit none
!     integer i
!     class(Table_EOSModel) :: this
!     open(27,file=this%tablefile)
!     do i=1,this%nTab
!       read (27,*) this%tempTab(i),this%rhoTab(i),this%muTab(i),this%lamTab(i), &
!                   this%cpTab(i),this%enthTab(i),this%betaTab(i)
!       this%lamocpTab(i) = this%lamTab(i)/this%cpTab(i)
!     enddo
!     close(27)
!   end subroutine readtable


!   subroutine calc_int_coeff(this)
!     use mod_math
!     implicit none
!     class(Table_EOSModel) :: this
!     call spline(this%tempTab,this%enthTab,  this%nTab,this%enth2Tab)
!     call spline(this%enthTab,this%rhoTab,   this%nTab,this%rho2Tab)
!     call spline(this%enthTab,this%muTab,    this%nTab,this%mu2Tab)
!     call spline(this%enthTab,this%lamTab,   this%nTab,this%lam2Tab)
!     call spline(this%enthTab,this%cpTab,    this%nTab,this%cp2Tab)
!     call spline(this%enthTab,this%lamocpTab,this%nTab,this%lamocp2Tab)
!     call spline(this%enthTab,this%tempTab,  this%nTab,this%temp2Tab)
!     call spline(this%enthTab,this%betaTab,  this%nTab,this%beta2Tab)
!   end subroutine calc_int_coeff




! end module Table_EOSModels


! module CO2Table_EOSModels
!   use Table_EOSModels
!   implicit none

!     type, extends(Table_EOSModel), public :: CO2Table_EOSModel
!     contains  
!       procedure :: init
!     end type CO2Table_EOSModel

!   contains 

!     subroutine set_constants(this)
!       class(CO2Table_EOSModel) :: this
!       this%nTab = 2000
!       this%tablefile = 'pH2_2MPA_table.dat'
!     end subroutine set_constants




! end module CO2Table_EOSModels


! module H2Table_EOSModels
!   use Table_EOSModels
!   implicit none

!     type, extends(Table_EOSModel), public :: H2Table_EOSModel
!     contains 
!       procedure(initialize2) :: init
!     end type H2Table_EOSModel

!   contains 

!     subroutine set_constants(this)
!       class(H2Table_EOSModel) :: this
!       this%nTab = 2000
!       this%tablefile = 'pH2_2MPA_table.dat'
!     end subroutine set_constants

!     subroutine init(this)
!       class(H2Table_EOSModels) :: this
!       call set_constants(this)
!       call allocateMem(this)
!       call readtable(this)
!       call calc_int_coeff(this)
!     end subroutine init

! end module H2Table_EOSModels