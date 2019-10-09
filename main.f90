program modular
  use mod_eosmodels
  use mod_eosmodels_ig
  use mod_eosmodels_table


  implicit none


  class(EOSModel), pointer :: test1
  class(EOSModel), pointer :: test2


  allocate(test1, source=IG_EOSModel())
  allocate(test2, source=Table_EOSModel(1,"test"))

  call test1%init()
  call test2%init()
  
  write(*,*) test1%getfield(2.)
  write(*,*) test2%getfield(2.)
  
  ! allocate(test,source=TBaseClass())

  ! write(*,*) test%ntab
  ! allocate(test,source=TDerivedClass(1,'test'))

  ! write(*,*) test%filename
  !test%ntab = 2000

  ! call test%init()

  ! write(*,*) tes
  ! call testbase(test)
  
  ! write(*,*) test%get_in1()

end program




! subroutine testbase(input)
!   use mod_types
!   implicit none
!   type(TDerivedClass) :: input
!   write(*,*) input%in2



! end subroutine testbase