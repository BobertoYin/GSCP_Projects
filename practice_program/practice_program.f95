program practice_program

!sets the round-off point
implicit none
integer(4) :: i, j
real(8) :: x, y, a, b

x = 50.0
y = 0.0

!create's a data file to hold the info
!unit specifier can't be 0, 5, 6
open(unit = 100, file = "practice_do_loop.dat")
open(unit = 110, file = "practice_while_loop.dat")

!fortran's for-loop
do i = 1, 10

	x = x + 10
	
	!fortran's if-statement
	if ((x > 100.0).and.(x < 130.0)) then
		y = 199.0
	elseif (x > 80.0) then
		y = 99.0
	else 
		y = 299.0
	end if

	write(100,*) "x = ", x

	write(100,*) "y = ", y

end do

!best to initialize variables!
a = 0.0
b = 0.0
j = 0
!fortran's while-loop
do while (j < 20)
	if (a >= 130) then
		exit
	end if	
	a = a + 10.0
	b = a
	j = j + 1
	write(110,*) a, b
end do

close(100)
close(110)

end program practice_program
