! Computer program to find the GCD of two integers a and b 
! and to write the GCD in the form ax + by = (a,b) 
! using the Extended Euclidean Algorithm 

program Extended_Euclidean_algorithm

implicit none 

integer :: a,b,a0,b0,x0,y0,x1,y1,r0,r1 
integer :: xnew,ynew,rnew,q 
integer :: n  
integer :: d,x,y 

write(*,'(A)') "Extended Euclidean Algorithm: find the greatest common divisor of two integers a and b" 
write(*,'(A)') "and express the GCD in the form a*x+b*y" 

! The values of a and b 
a = 507885 
b = 60808 

write(*,'(A,I0)') "a = ", a 
write(*,'(A,I0)') "b = ", b 

! Make a0 and b0 positive
a0 = abs(a)
b0 = abs(b)

! Apply the Extended Euclidean Algorithm 
! initialize 
x0 = 1
y0 = 0
x1 = 0
y1 = 1 
r0 = a0 
r1 = b0 

! Start the algorithm 
n = 0 ! The step number  

do while (r1 /= 0)
    ! Euclidean algorithm 
    q = r0/r1 
    rnew = r0 - q*r1
    xnew = x0 - q*x1 
    ynew = y0 - q*y1 
    
    ! Update the values 
    r0 = r1 
    x0 = x1 
    y0 = y1 
    r1 = rnew 
    x1 = xnew 
    y1 = ynew 

    n = n+1 

    ! Print the step 
    write(*,'(A ,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0)') &
    & "Step ",n, ": r0 = ", r0, ", r1 = ",r1, ", q = ",q, &
    & " x0 = ", x0, ", x1 = ", x1, ", y0 = ", y0, ", y1 = ", y1 
end do ! WHILE-loop 

! The GCD 
d = r0 
x = x0 
y = y0 

! Print the GCD and x, y
write(*,'(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0)') "The GCD of ", a, " and ", b, " is ", d, & 
& " and it is in the form " ,a, "*x + ", b, "*y = " ,d, & 
& " with x = ", x, ", y = ", y


end program Extended_Euclidean_algorithm