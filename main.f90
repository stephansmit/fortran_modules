program modular


  real(8), dimension(0:100) :: a1,b1,c1

  a1=1;b1=0;c1=2;
  open( 29, file = 'somedata.csv' )
  write(29, '(1a20)' ) 'Station', 'Mean salinity', 'Mean temperature'
  do i=0,100
    write(29, '(E20.6,E20.6,E20.6)' ) a1(i),b1(i),c1(i)
  enddo


  close(29)


end program
