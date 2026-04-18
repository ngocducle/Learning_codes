! Computer program to find the GCD of two integers a and b 
! and to write the GCD in the form ax + by = (a,b) 

program Euclidean_algorithm 

implicit none 

integer :: a, b, x, y, gcd, temp, r, q, n 

write(*,'(A)') 'Euclidean algorithm: find the greatest common divisor of two integers a and b'
write(*,'(A)') 'and express the GCD in the form a*x+b*y'

! The values of a and b 
a = 20  
b = 13  

write(*,'(A,I0)') "a = ", a 
write(*,'(A,I0)') "b = ", b 

! Check if a != 0 or b != 0
if (a == 0 .or. b == 0) then
    write(*,'(A)') "Error: a = 0 or b = 0" 
    write(*,'(A)') "This program applies for non-zero integers a and b"
    write(*,'(A)') "Exit"
    stop 
end if 

! Make a and b positive
if (a < 0) then 
    a = -a 
end if 

if (b < 0) then 
    b = -b 
end if 

! Make a >= b 
if (a < b) then 
    temp = a 
    a = b 
    b = temp 
end if 

! Apply the Euclidean algorithm
n = 1 ! the step number 
gcd = b ! Initialize gcd 

do while (b > 0) 
    q = a / b 
    r = a - q*b 
    a = b 
    gcd = b 
    b = r 
    write(*,'(A,I0,A,I0,A,I0,A,I0)') "Step ",n,": a = ", a, ", b = ", b, ", q = ", q, ", r = ", r 
    n = n + 1 
end do ! WHILE-loop 

! Print the GCD 
write(*,'(A,I0)') "The GCD is ", gcd 

end program Euclidean_algorithm 