program simplePendulum

implicit none
real, allocatable, dimension(:) :: theta, omega, alpha, kineticEnergy, potentialEnergy, totalEnergy, time
integer :: i, n, cycles, initial
real :: mass, g, pi, timeStep, period, length

pi = 4.0 * atan(1.0)
g = 9.8
mass = 1.0
period = 2 * pi * sqrt(mass / g)

write(*,*) "How many cycles would you like to plot? (Enter an integer)"
read(*,*) cycles
timeStep = 0.01
n = floor(cycles * period / timeStep)

write(*,*) "What is the length from the rotational axis?"
read(*,*) length

write(*,*) "What is the initial angle measurement? (Degrees and integer)"
read(*,*) initial

allocate(theta(n), omega(n), alpha(n), kineticEnergy(n), potentialEnergy(n), totalEnergy(n), time(n))

theta(1) = (real(initial) * (pi / 180.0))
omega(1) = 0.0
time(1) = 0.0
alpha(1) = (-(g / length)) * sin(theta(1))
kineticEnergy(1) = 0.5 * mass * (length ** 2.0) * (omega(1) ** 2.0)
potentialEnergy(1) = mass * g * length * (1.0 - cos(theta(1)))
totalEnergy(1) = kineticEnergy(1) + potentialEnergy(1)
theta(2) = theta(1) + timeStep * omega(1) + 0.5 * (timeStep ** 2.0) * alpha(1)

do i = 2, n - 1

    alpha(i) = (-(g / length)) * sin(theta(i))
    theta(i + 1) = 2 * theta(i) - theta(i - 1) + (timeStep ** 2.0) * alpha(i)
    omega(i) = (theta(i + 1) - theta(i - 1)) / (2.0 * timeStep)
    time(i + 1) = time(i) + timeStep
    kineticEnergy(i) = 0.5 * mass * (length ** 2.0) * (omega(i) ** 2.0)
    potentialEnergy(i) = mass * g * length * (1.0 - cos(theta(i)))
    totalEnergy(i) = kineticEnergy(i) + potentialEnergy(i)
    
end do

open(unit = 100, file = "theta_v_time.dat")
open(unit = 110, file = "omega_v_time.dat")
open(unit = 120, file = "alpha_v_time.dat")

open(unit = 130, file = "kinetic_energy_v_time.dat")
open(unit = 140, file = "potential_energy_v_time.dat")
open(unit = 150, file = "total_energy_v_time.dat")

do i = 1, n - 1

    write(100,*) time(i + 1), theta(i + 1)
    write(110,*) time(i), omega(i)
    write(120,*) time(i), alpha(i)
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

end program simplePendulum
