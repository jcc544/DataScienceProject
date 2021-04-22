program CurveFit
  implicit none
  integer, parameter :: dp = selected_real_kind(15,300)
  integer :: in_unit = 30, istat, i, lines
  character(len=100) :: errormsg
  
  real(kind=dp), dimension(:,:), allocatable :: data
  
  open(unit = in_unit, file = "simple_set.dat", status = "old", action = "read", iomsg = errormsg, iostat = istat)
  if (istat /= 0) print *, errormsg
  lines = 0
  do i = 1, 10000000
    read(in_unit,*,end = 800)
    lines = lines + 1
  end do
  800 continue
  allocate(data(2,lines))
  rewind(in_unit)
  do i = 1, lines
    read(in_unit, *) data(:,i)
  end do
  do i =1, lines
    print *, data(:,i)
  end do
end program CurveFit


