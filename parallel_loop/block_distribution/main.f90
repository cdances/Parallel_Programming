program mpi_loop
    ! Reference
    ! https://rc.fas.harvard.edu/wp-content/uploads/2013/03/MPI_Plamen_Krastev.pdf

    ! Modules

    use parallel_loop

    ! Variables
    implicit none
    integer, parameter :: N=100000
    integer, dimension(N) :: a

!    call serial_sum(N)

    call threaded_sum(N,a)



end program mpi_loop
