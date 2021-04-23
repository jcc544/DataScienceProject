program CurveFit
  implicit none
  integer, parameter :: dp = selected_real_kind(15,300)
  integer :: in_unit = 30, out_unit = 50, istat, i, N, j
  character(len=100) :: errormsg
  
  real(kind=dp), dimension(:,:), allocatable :: data, fit
  real(kind=dp), dimension(2) :: a, nablaS
  real(kind=dp) :: S, lambda = 0.001_dp, old
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !First section - read the file and allocate arrays to hold data. Then allocated new array of same size to hold function prediction
  
  open(unit = in_unit, file = "pace_set.dat", status = "old", action = "read", iomsg = errormsg, iostat = istat)      
  if (istat /= 0) print *, errormsg
  open(unit = out_unit, file = "fit2.dat", status = "replace", action = "write", iomsg = errormsg, iostat = istat)      
  if (istat /= 0) print *, errormsg
  N = 0
  do i = 1, 10000000
    read(in_unit,*,end = 800)
    N = N + 1
  end do
  800 continue
  allocate(data(N,2))
  allocate(fit(N,2))
  rewind(in_unit)
  do i = 1, N
    read(in_unit, *) data(i, :)
  end do
  do i = 1, N
    fit(i, 1) = data(i, 1)
  end do
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Second section - fill the fit array with the model using the subroutine. This creates two arrays within which we can compare the data
  !Then calculate a value for the residual
  
  a(1) = 1_dp
  a(2) = 1_dp
  call model()
  call residual()
  call implicitdiff()
  write(out_unit,*)  0, a(1), a(2), S, dot_product(nablaS,nablaS)
  
  do j = 1, 10000
    old = S
    call update()
    call model()
    call residual()
    call implicitdiff()
    write(out_unit,*)  j, a(1), a(2), S, dot_product(nablaS,nablaS)
    if (abs(old - S) < 10.0_dp**(-4.0_dp)) exit
    
  end do  


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  contains
  
  subroutine model()
  do i = 1, N
    fit(i, 2) = a(1) * log10(fit(i,1)) + a(2)
  end do
  end subroutine model
  
  subroutine residual()
  S = 0
  do i = 1, N
    S = S + (abs(data(i,2) - fit(i,2)))**2
  end do
  S = S / N
  end subroutine residual
  
  subroutine implicitdiff()
  nablaS = 0
  do i = 1, N
    nablaS(1) = nablaS(1) + ((2.0_dp / N) * (data(i,2) - fit(i,2)) * (-1) * (log10(fit(i,1))))
    nablaS(2) = nablaS(2) + ((2.0_dp / N) * (data(i,2) - fit(i,2)) * (-1)) 
  end do
  end subroutine implicitdiff

  subroutine update()
  a(1) = a(1) - ((lambda) * nablaS(1))
  a(2) = a(2) - ((lambda) * nablaS(2))
  end subroutine update
  
end program CurveFit


