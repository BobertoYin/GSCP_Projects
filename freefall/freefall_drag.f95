program freefall

! this program creates graphs for height, velocity, and acceleration in respect to time for a free-fall problem with air drag

! declares variable types
implicit none
real(8) :: a, t, v, y, inc, v2, p, x_sec, drag_co, speed, drag

! sets our variables
a = -9.8
t = 0.0
inc = 1.0 / 100.0
p = 1.22
x_sec = 0.037
drag_co = 0.47

! prompts for initial height, velocity, and increment
write(*,*) "What is the initial height? (m)"
read(*,*) y

write(*,*) "What is the inital velocity? (Enter a negative value) (m/s)"
read(*,*) v

! prepares a data file for each graph
open(unit = 400, file = "drag_height_v_time.dat")
open(unit = 500, file = "drag_velocity_v_time.dat")
open(unit = 600, file = "drag_acceleration_v_time.dat")

! executes while object is above the ground
do while (y >= 0.0)
    
    ! increments time
    t = t + inc
    
    ! sets up air drag
    speed = abs(v)
    drag = -0.5 * drag_co * x_sec * p * speed * v
    
    ! changes acceleration
    a = -9.8 + drag
    
    ! holds old velocity
    v2 = v
    
    ! increments the velocity
    v = v + inc * a
    
    ! increments the height
    y = y + inc * ((v + v2) / 2)
    
    ! writes to their respective files
    write(400,*) t, y
    write(500,*) t, v
    write(600,*) t, a

end do

! closes the data files
close(400)
close(500)
close(600)

end program freefall
