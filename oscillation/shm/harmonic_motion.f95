program harmonicMotion

implicit none
real, allocatable, dimension(:) :: x, velocity, acceleration, kineticEnergy, potentialEnergy, totalEnergy, time
integer :: i, n, cycles
real :: mass, k, pi, timeStep, period

pi = 4.0 * atan(1.0)
k = 8.24
mass = 0.254
period = 2 * pi * sqrt(mass / k)

write(*,*) "How many cycles would you like to plot? (Enter an integer)"
read(*,*) cycles
timeStep = 0.01
n = floor(cycles * period / timeStep)

allocate(x(n), velocity(n), acceleration(n), kineticEnergy(n), potentialEnergy(n), totalEnergy(n), time(n))

x(1) = 0.1
velocity(1) = 0.0
time(1) = 0.0
acceleration(1) = (-(k / mass)) * x(1)
kineticEnergy(1) = 0.5 * mass * (velocity(1) ** 2.0)
potentialEnergy(1) = 0.5 * k * (x(1) ** 2.0)
totalEnergy(1) = kineticEnergy(1) + potentialEnergy(1)
x(2) = x(1) + timeStep * velocity(1) + 0.5 * (timeStep ** 2.0) * acceleration(1)

do i = 2, n - 1

    acceleration(i) = (-(k / mass)) * x(i)
    x(i + 1) = 2 * x(i) - x(i - 1) + (timeStep ** 2.0) * acceleration(i)
    velocity(i) = (x(i + 1) - x(i - 1)) / (2.0 * timeStep)
    time(i + 1) = time(i) + timeStep
    kineticEnergy(i) = 0.5 * mass * (velocity(i) ** 2.0)
    potentialEnergy(i + 1) = 0.5 * k * (x(i + 1) ** 2.0)
    totalEnergy(i) = kineticEnergy(i) + potentialEnergy(i)
    
end do

open(unit = 100, file = "x_v_time.dat")
open(unit = 110, file = "velocity_v_time.dat")
open(unit = 120, file = "acceleration_v_time.dat")
open(unit = 130, file = "kinetic_energy_v_time.dat")
open(unit = 140, file = "potential_energy_v_time.dat")
open(unit = 150, file = "total_energy_v_time.dat")

do i = 1, n - 1

    write(100,*) time(i + 1), x(i + 1)
    write(110,*) time(i), velocity(i)
    write(120,*) time(i), acceleration(i)
    write(130,*) time(i), kineticEnergy(i)
    write(140,*) time(i + 1), potentialEnergy(i + 1)
    write(150,*) time(i), totalEnergy(i)
    
end do

close(100)
close(110)
close(120)
close(130)
close(140)
close(150)

end program harmonicMotion
