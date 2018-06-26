program orbit

implicit none
integer :: inc, i
real(8) :: pi, gm, tau, mass, time, kinetic, potential, total
real(8), dimension(1:3) :: r, v, a, rtemp, vtemp, atemp, rtemp2, vtemp2, atemp2, rtemp3, vtemp3, atemp3

open(unit=100, file="orbit.dat")
open(unit=200, file="kinetic_v_time.dat")
open(unit=300, file="potential_v_time.dat")
open(unit=400, file="total_v_time.dat")

pi = 4.0 * atan(1.0)
inc = 10000
tau = 1.0 / real(inc)
gm = 4 * (pi ** 2.0)
mass = 1.0
time = 0.0

r(1) = 1.0
r(2) = 0.0
r(3) = 0.0

write(100,*) r(1), r(2), r(3)

v(1) = 0.0
v(2) = 2.0 * pi
v(3) = 0.0

kinetic = 0.5 * mass * (norm2(v) ** 2.0)
potential = -gm / norm2(r)
total = kinetic + potential

do i = 1, inc - 1

    a = -(gm / (norm2(r) ** 3.0 )) * r
    
    rtemp = r + 0.5 * tau * v
    vtemp = v + 0.5 * tau * a
    atemp = -(gm / (norm2(rtemp) ** 3.0)) * rtemp
    
    rtemp2 = r + 0.5 * tau * vtemp
    vtemp2 = v + 0.5 * tau * atemp
    atemp2 = -(gm / (norm2(rtemp2) ** 3.0)) * rtemp2
    
    rtemp3 = r + tau * vtemp2
    vtemp3 = v + tau * atemp2
    atemp3 = -(gm / (norm2(rtemp3) ** 3.0)) * rtemp3
    
    r = r + (tau / 6.0) * (v + (2.0 * vtemp) + (2.0 * vtemp2) + vtemp3)
    v = v + (tau / 6.0) * (a + (2.0 * atemp) + (2.0 * atemp2) + atemp3)
    
    kinetic = 0.5 * mass * (norm2(v) ** 2.0)
    potential = -gm / norm2(r)
    total = kinetic + potential
    
    write(100,*) r(1), r(2), r(3)
    write(200,*) kinetic
    write(300,*) potential
    write(400,*) total
    
end do

close(100)
close(200)
close(300)
close(400)
 
end program orbit
