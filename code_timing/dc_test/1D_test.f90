program do_concurrent_test
    implicit none
    integer, parameter :: n = 100000
    integer :: i, m
    real :: x
    real, allocatable, dimension(:) :: a_array, b_array, c_array
    allocate(a_array(n), b_array(n), c_array(n))

    write(*, *) "Performing calculations"
    do m = 1, 10000
        do CONCURRENT(i=1:n)
            x = real(m)
            a_array(i) = 0.
            b_array(i) = x * x * 2.0 * exp(i / SIN(i * 2 * 3.1415 / n))** 1.25 * cos(x)
            c_array(i) = exp(x) * 2.0 * sqrt(exp(cos(real(i / n + n / i) ** 3)) ) ** 3.4

            a_array(i) = b_array(i) / 5 + c_array(i) / 3
        enddo
    enddo

    write(*, *) "Complete."

    deallocate(a_array, b_array, c_array)

end program do_concurrent_test