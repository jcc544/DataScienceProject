program part3
  implicit none
  integer, parameter :: dp = selected_real_kind(15,300)
  integer :: N, in_unit = 30, istat, i, a, j
  
  character(len=100) :: errormsg
  
  real(kind=dp) :: o, S
  real(kind=dp), dimension(3,2) :: b
  real(kind=dp), dimension(3,2) :: W1
  real(kind=dp), dimension(:,:), allocatable :: data
  real(kind=dp), dimension(:), allocatable :: output, yreal
  real(kind=dp), dimension(2) :: in
  real(kind=dp), dimension(3) :: hi, W2
  
  W1(1,1) = 0.4
  W1(1,2) = 0.2
  W1(2,1) = 0.1
  W1(2,2) = -0.2
  W1(3,1) = 0.5
  W1(3,2) = 0.8
  
  W2(1) = 0.2
  W2(2) = -0.9
  W2(3) = 0.1
  
  b(1,1) = 0
  b(2,1) = 0
  b(1,2) = 0
  b(2,2) = 0
  b(3,2) = 0
  
  
  open(unit = in_unit, file = "OR.dat", status = "old", action = "read", iomsg = errormsg, iostat = istat)      
  if (istat /= 0) print *, errormsg
  N = 0
  do i = 1, 10000000
    read(in_unit,*,end = 800)
    N = N + 1
  end do
  800 continue
  allocate(data(N,2))
  allocate(output(N))
  allocate(yreal(N))
  rewind(in_unit)
  do i = 1, N
    read(in_unit, *) data(i, :), yreal(i)
  end do  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  do a = 1, N
    in(:) = data(a,:)
    hi = matmul(W1,in)
    hi = hi + sum(b(:,1))
    do i = 1, 3
      hi(i) = sigmoid(hi(i))
    end do
    o = dot_product(W2,hi)
    o = o + sum(b(:,2))
    output(a) = sigmoid(o)
  end do
  S = 0
  do i = 1, N
    S = S + (output(i) - yreal(i))**2
  end do
  S = S / N
  print *, S
  
  contains 
  function sigmoid(x)
    real(kind=dp) :: sigmoid, x
    sigmoid = 1 / (1 + exp((-1) * x))
  end function sigmoid
end program part3
