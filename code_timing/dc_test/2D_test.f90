program do_concurrent_test
    implicit none
    integer, parameter :: n = 1000
    integer :: i, j, m
    real :: x
    real, allocatable, dimension(:, :) :: a_array, b_array, c_array
    allocate(a_array(n, n), b_array(n, n), c_array(n, n))

    write(*, *) "Performing calculations"
    do m = 1, 1000
        do CONCURRENT(j=1:n, i=1:n)
            x = real(m)
            a_array(i, j) = 0.
            b_array(i, j) = x * x * 2.0 * exp(i / SIN(j * 2 * 3.1415 / n))** 1.25 * cos(x)
            c_array(i, j) = exp(x) * 2.0 * sqrt(exp(cos(real(i / n + n / j) ** 3)) ) ** 3.4
            a_array(i, j) = b_array(i, j) / j + c_array(i, j) / i
            c_array(i, j) = b_array(i, j) - a_array(i, j) * j
            b_array(i, j) = c_array(i, j) * b_array(i, j) * c_array(i, j)
        enddo
    enddo

    write(*, *) "Complete."

    deallocate(a_array, b_array, c_array)

end program do_concurrent_test