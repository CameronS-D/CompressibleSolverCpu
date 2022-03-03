program do_concurrent_test
    implicit none
    integer, parameter :: n = 500
    integer :: i, j, k, m
    real :: x
    real, allocatable, dimension(:, :, :) :: a_array, b_array, c_array
    allocate(a_array(n, n, n), b_array(n, n, n), c_array(n, n, n))

    write(*, *) "Performing calculations"
    do m = 1, 10
        do CONCURRENT(k=1:n, j=1:n, i=1:n)
            x = real(m)
            a_array(k, j, i) = 0.
            b_array(k, j, i) = x * x * 2.0 * exp(i / SIN(j * 2 * 3.1415 / n))** 1.25 * cos(x) * k
            c_array(k, j, i) = exp(x) * 2.0 * sqrt(exp(cos(real(j / n + n / i) ** 3) * k) ) ** 3.4
            a_array(k, j, i) = b_array(k, j, i) * i / j + c_array(k, j, i) / k
        enddo
    enddo

    write(*, *) "Complete."

    deallocate(a_array, b_array, c_array)

end program do_concurrent_test