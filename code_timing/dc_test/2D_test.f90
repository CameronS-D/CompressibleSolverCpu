program do_concurrent_test
    implicit none
    integer, parameter :: n = 5000
    integer :: i, j, m
    real(8) :: factorial
    real(8), allocatable, dimension(:, :) :: a_array
    allocate(a_array(n, n))

    write(*, *) "Performing calculations"

    factorial = 1.0d0
    do CONCURRENT (j=1:n, i=1:n)
        do m=1, j
            factorial = factorial * m
        enddo

        a_array(i, j) = factorial 
        factorial = 1.0d0
        do m=1, i
            factorial = factorial * m
        enddo

        a_array(i, j) = a_array(i, j) + factorial
    enddo

    write(*, *) "Complete."

    deallocate(a_array)

end program do_concurrent_test