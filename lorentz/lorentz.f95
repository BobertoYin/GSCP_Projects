program lorentz

implicit none
integer :: inc, i
real(8) :: tau, q, m
real(8), dimension(1:3) :: r, v, e, b, rtemp, rtemp2, rtemp3, vtemp, vtemp2, vtemp3, l, ltemp, ltemp2, ltemp3

open(unit = 100, file = "motion.dat")
open(unit = 200, file = "velocity.dat")

r = 0.0

v = 0.0
v(1) = 10.0

e(1) = 0.0
e(2) = 0.01
e(3) = 0.1

b = 0.0
b(3) = 0.1

q = 1.0
m = 1.0

inc = 50000
tau = 0.01

do i = 1, inc
    
    l = (q / m) * (e + cross(v,b))
    
    rtemp = r + 0.5 * tau * v
    vtemp = v + 0.5 * tau * l
    ltemp = (q / m) * (e + cross(vtemp,b))
    
    rtemp2 = r + 0.5 * tau * vtemp
    vtemp2 = v + 0.5 * tau * ltemp
    ltemp2 = (q / m) * (e + cross(vtemp2,b))
    
    rtemp3 = r + tau * vtemp2
    vtemp3 = v + tau * ltemp2
    ltemp3 = (q / m) * (e + cross(vtemp3,b))
    
    r = r + tau * (v + (2.0 * vtemp) + (2.0 * vtemp2) + vtemp3) / 6.0
    v = v + tau * (l + (2.0 * ltemp) + (2.0 * ltemp2) + ltemp3) / 6.0
    
    write(100,*) r(1), r(2), r(3)
    write(200,*) v(1), v(2), v(3)
    
end do

close(100)
close(200)

contains

function cross(V,B) result(k)
    implicit none
    real(8), dimension(3) :: V, B, k
    k(1) = V(2) * B(3) - V(3) * B(2)
    k(2) = -(V(1) * B(3) - V(3) * B(1))
    k(3) = V(1) * B(2) - V(2) * B(1)
end function cross

end program lorentz
