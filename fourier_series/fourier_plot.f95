program fourier_sawtooth

implicit none
real(8) :: pi, x, y, inc
integer :: i, n

!initializes all variables
pi = 4.0 * atan(1.0)
x = 0.0
y = 0.0
inc = 2.0 * pi / 200.0

!sets up a data file
open(unit = 100, file = "sawtooth.dat")

!plots the piecewise sawtooth for 200 terms
do while (x <= 2.0 * pi)

	!follows sawtooth formula and sets f(x)
	if (x < pi) then
		y = x
		else
  		y = x - 2.0 * pi
	end if
    	
	!writes to the data file
	write(100,*) x, y
	
	!increments x
	x = x + inc
    
end do

!re-initialize variables from last do-while
x = 0.0

!sets up a second data file
open(unit = 200, file = "fourier.dat")

!prompts for the number of terms in the approximation
write(*,*) "How many terms do you want for your approximation?"
read(*,*) n

!fourier series
do while (x <= 2.0 * pi)

	!initializes y every loop
	y = 0.0

	!summation for f(x)
	do i = 1, n
		y = ((((-1.0) ** (float(i) + 1.0)) * 2.0) / float(i)) * sin(float(i) * x) + y
	end do
	
	!writes to the second data file
	write(200,*) x, y
	
	!increments x
	x = x + inc

end do

close(100)
close(200)

end program fourier_sawtooth
