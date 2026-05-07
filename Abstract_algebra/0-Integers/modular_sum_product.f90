program modular_sum_product

! Exercise 0.3.16 Dummit-Foote: Add and multiply modulo n 
! Here I use the division and subtraction to find the remainder 
! Instead of using the mod function  

use iso_fortran_env, only: int64 
implicit none

integer(int64) :: a, b, n, k, S, P

a = 63800389328_int64 
b = 39787456_int64 
n = 20192018_int64 

write(*,'(A,I0)') "a = ", a
write(*,'(A,I0)') "b = ", b
write(*,'(A,I0)') "n = ", n 

do while (a >= n)
    k = a/n 
    a = a - k*n 
end do

do while (b >= n)
    k = b/n 
    b = b - k*n
end do

write(*,'(A,I0)') "a mod n = ", a
write(*,'(A,I0)') "b mod n = ", b

S = a+b 

do while (S >= n)
    k = S/n 
    S = S - k*n 
end do

write(*,'(A,I0)') "a+b mod n = ", S

P = a*b 

do while (P >= n)
    k = P/n 
    P = P - k*n 
end do 

write(*,'(A,I0)') "a*b mod n = ", P 

end program modular_sum_product