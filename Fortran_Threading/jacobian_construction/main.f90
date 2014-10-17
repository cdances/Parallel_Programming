program jacob_construct

    ! Modules
    use mpi_tools
    use jacobian_construction

    ! Variables
    implicit none
    integer, parameter :: N=4
    integer :: i

    ! Initialize arrays
    call allocate_arrays(N)

    ! Initialize MPI
    call start_parallel()

    ! Parallel Section of the Code
    !---------------------------------------------------

    call calc_column()
    call calc_jacobian()

    !---------------------------------------------------
    call end_parallel()

    ! Print the Results
    if( rank == 0) then
        write(*,*) "Right Hand Side"

        do i=1,size(rhs)
            write(*,*) i,rhs(i)
        end do

!        write(*,*) "Jacobian"
        call print_jacobian()

    end if

    ! Delete arrays
    call deallocate_arrays()

end program jacob_construct
