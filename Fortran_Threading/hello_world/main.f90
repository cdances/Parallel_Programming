program mpi_tutorial

    ! Modules
    use mpi
    use hello_world_mod 

    ! Variables
    implicit none
    integer :: rank, mpi_size, ierror!, tag, status(MPI_STATUS_SIZE)

    ! Initialize MPI
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, mpi_size, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)

    ! Main Funcion Calls
!    write(*,"(A,I11,A,I11)") "Hello World from node", rank, " of ", mpi_size
    call hello_world(rank,mpi_size)

    ! End MPI
    call MPI_FINALIZE(ierror)

end program mpi_tutorial
