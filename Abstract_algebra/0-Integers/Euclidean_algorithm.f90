! Computer program to find the GCD of two integers a and b 
! and to write the GCD in the form ax + by = (a,b) 

program Euclidean_algorithm 

implicit none 

integer :: a, b, a0, b0, ap, bp, x, y, gcd, temp, r, q, n 

write(*,'(A)') 'Euclidean algorithm: find the greatest common divisor of two integers a and b'
write(*,'(A)') 'and express the GCD in the form a*x+b*y'

! The values of a and b 
a = 507885 
b = 60808 

write(*,'(A,I0)') "a = ", a 
write(*,'(A,I0)') "b = ", b 

! Check if a != 0 or b != 0
if (a == 0 .or. b == 0) then
    write(*,'(A)') "Error: a = 0 or b = 0" 
    write(*,'(A)') "This program applies for non-zero integers a and b"
    write(*,'(A)') "Exit"
    stop 
end if  

! Make a0 and b0 positive
a0 = abs(a)
b0 = abs(b)   

! Apply the Euclidean algorithm
n = 1 ! the step number 
gcd = b0 ! Initialize gcd 

do while (b0 > 0) 
    ! Calculate q and r  
    q = a0 / b0 
    r = a0 - q*b0  

    write(*,'(A,I0,A,I0,A,I0,A,I0)') "Step ",n,": a0 = ", a0, ", b0 = ", b0, ", q = ", q, ", r = ", r 

    ! Update a0 and b0  
    a0 = b0 
    gcd = b0 
    b0 = r
    n = n + 1 
end do ! WHILE-loop 

! Find x and y such that a*x + b*y = d = gcd(a,b) 
write(*,'(A)') "Find integers x and y such that " 
write(*,'(I0,A,I0,A,I0)') a, "*x + ", b, "*y = ", gcd 

! Find a' and b' such that a'*x+b'*y = d =gcd(a,b)
ap = a/gcd 
bp = b/gcd 

x = 0 
r = mod(1-ap*x,bp) 

write(*,'(A,I0,A,I0,A,I0,A,I0)') "Step ",x,": x = ", x, ", r = ", r

do while (r /= 0)
    x = x+1  
    r = mod(1-ap*x,bp)

    write(*,'(A,I0,A,I0,A,I0,A,I0)') "Step ",x,": x = ", x, ", r = ", r 
end do ! WHILE-loop  

y = (1-ap*x)/bp 

! Print the GCD  and x, y
write(*,'(A,I0,A,I0,A,I0)') "The GCD of ", a, " and ", b, " is ", gcd 
write(*,'(A,I0,A,I0)') " x = ", x, ", y = ", y 

end program Euclidean_algorithm 