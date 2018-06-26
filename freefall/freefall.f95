program freefall

! this program creates graphs for height, velocity, and acceleration in respect to time for a free-fall problem without air drag

! declares variable types
implicit none
real(8) :: a, t, v, y, inc 

! sets our variables
a = -9.8
t = 0.0
inc = 1.0 / 100.0

! prompts for initial height, velocity, and increment
write(*,*) "What is the initial height? (m)"
read(*,*) y

write(*,*) "What is the inital velocity? (Enter a negative value) (m/s)"
read(*,*) v

! prepares a data file for each graph
open(unit = 100, file = "height_v_time.dat")
open(unit = 200, file = "velocity_v_time.dat")
open(unit = 300, file = "acceleration_v_time.dat")

! executes while object is above the ground
do while (y >= 0.0)
    
    ! increments the height
    y = y + inc * v
    
    ! increments the velocity
    v = v + inc * a
    
    ! increments time
    t = t + inc
    
    ! writes to their respective files
    write(100,*) t, y
    write(200,*) t, v
    write(300,*) t, a

end do

! closes the data files
close(100)
close(200)
close(300)

end program freefall
