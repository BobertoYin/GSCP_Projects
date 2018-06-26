program projectileMotion

! this program plots a projectile in motion without air drag

implicit none
real(8), dimension(1:3) :: r, v, a
real(8) :: pi, inc, time, theta, g

open(unit = 100, file = "x.dat")
open(unit = 200, file = "velocity_vs_time.dat")
open(unit = 300, file = "time_vs_velocity_z.dat")

pi = 4.0 * atan(1.0)
inc = 0.01
time = 0.0
theta = 50.0 * (pi / 180.0)
g = 9.8

r(1) = 0.0
r(2) = 0.0
r(3) = 1.0

v(1) = 20.0 * cos(theta)
v(2) = 0.0
v(3) = 20.0 * sin(theta)

a(1) = 0.0
a(2) = 0.0
a(3) = -g

do while (r(3) > 0.0)
    v = v + inc * a
    r = r + inc * v
    time = time + inc
    
    write(100,*) r(1), r(2), r(3)
    write(200,*) v(1), v(2), v(3)
    write(300,*) time, v(3)
    
end do

close(100)
close(200)
close(300)

end program projectileMotion
