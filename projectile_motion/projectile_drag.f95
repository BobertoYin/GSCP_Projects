program projectileMotion

! this program plots a projectile in motion without air drag

implicit none
real(8), dimension(1:3) :: r, v, a, vold, drag
real(8) :: pi, inc, time, theta, g, speed, p, dragCo, xSec, radius, angle, vel, mass

open(unit = 100, file = "x_drag.dat")
open(unit = 200, file = "velocity_vs_time_drag.dat")
open(unit = 300, file = "time_vs_velocity_z_drag.dat")

pi = 4.0 * atan(1.0)
inc = 0.01
time = 0.0

write(*,*) "What is the launch angle in degrees?"
read(*,*) angle
theta = real(angle) * (pi / 180.0)
g = 9.8
p = 1.225
dragCo = 0.47 

write(*,*) "What is the radius of your projectile? (m)"
read(*,*) radius
xSec = pi * (radius ** 2.0)

r(1) = 0.0
r(2) = 0.0
write(*,*) "What is the initial height? (m)"
read(*,*) r(3)

write(*,*) "What is the initial velocity? (m/s)"
read(*,*) vel

write(*,*) "What is the initial mass? (kg)"
read(*,*) mass

v(1) = real(vel) * cos(theta)
v(2) = 0.0
v(3) = real(vel) * sin(theta)

a(1) = 0.0
a(2) = 0.0
a(3) = -g

speed = sqrt((v(1) ** 2) + (v(2) ** 2) + (v(3) ** 2)) 
drag = -0.5 * dragCo * xSec * p * speed * v / mass

do while (r(3) > 0.0)

    a(1) = 0.0
    a(2) = 0.0
    a(3) = -g

    speed = norm2(v)
    drag = -0.5 * dragCo * xSec * p * speed * v / mass
    a = a + drag
    time = time + inc
    vold = v
    v = v + inc * a
    r = r + inc*(v + vold) / (2.0)
    
    write(100,*) r(1), r(2), r(3)
    write(200,*) v(1), v(2), v(3)
    write(300,*) time, v(3)
    
end do

write(*,*) "The projectile hit the ground in ", time, " seconds"

close(100)
close(200)
close(300)

end program projectileMotion
