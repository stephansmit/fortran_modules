module mod_math

contains
!********************************************************************
!    spline (numerical recipes)
!********************************************************************
  subroutine spline(x, y, n, y2)
  implicit none
  integer   i, k, n, nmax
  parameter  (nmax=5000)
  real*8    yp1, ypn, x(n), y(n), y2(n), p, qn, sig, un, u(nmax)

  y2(1) = 0.
  u(1)  = 0.
  do i=2, n-1
     sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
     p=sig*y2(i-1)+2.
     y2(i)=(sig-1.)/p
     u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/ &
           (x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
  enddo

  qn=0.
  un=0.
  y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)

  do k=n-1, 1, -1
     y2(k)=y2(k)*y2(k+1)+u(k)
  enddo

  return
  end subroutine spline

end module mod_math