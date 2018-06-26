program heatEquation

implicit none
integer :: n, i, m, j, p
real(8) :: l, k, tau, h
real(8), allocatable, dimension(:,:) :: t

open(unit = 100, file = "heat.dat")

n = 61
l = 1.0
k = 1.0
h = 1.0 / real(n)
tau = 0.0001

allocate(t(n,n))

t(:,1) = 0.0
t(:,n) = 0.0
t(1,:) = 0.0
t(1,31) = 1.0 / h


do i = 2, n
    
    do j = 2, (n - 1)
    
        t(i, j) = t(i - 1,j) + (k * tau / real(h ** 2.0)) * (t(i - 1, j + 1) + t(i - 1, j - 1) - 2.0 * t(i - 1, j))
    
    end do
    
end do

do p = 1, n
    write(100,*) t(p,:)
end do

close(100)

end program heatEquation
