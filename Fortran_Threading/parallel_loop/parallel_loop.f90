module parallel_loop
    ! Cyclic Technique
    ! or "Round-Robin"

    ! Modules
    use mpi

    ! Variables
    implicit none

    private
    public :: threaded_sum, shrinking_arrays

contains

subroutine shrinking_arrays(n)
    implicit none
    integer, intent(in) :: n
    integer :: i, ista, iend, total, total_sum
    integer :: rank, mpi_size, ierror!, tag, status(MPI_STATUS_SIZE)
    integer, allocatable :: a(:)

    ! Initialize MPI
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, mpi_size, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)

    call para_range(1, n, mpi_size, rank, ista, iend)

    if ( .not. allocated(a) ) allocate( a(ista:iend) )
    do i = ista, iend
        a(i) = i
    end do

    total = 0

    do i = ista, iend
        total = total + a(i)
    end do

    call MPI_REDUCE(total, total_sum, 1, MPI_INTEGER,MPI_SUM, 0,MPI_COMM_WORLD, ierror)

    if(rank == 0) write(*,*) "parallel total =",total_sum

    if ( allocated(a) ) deallocate(a)
    ! End MPI
    call MPI_FINALIZE(ierror)

    write(*,*) rank, ista, iend, total

end subroutine

subroutine threaded_sum(n,a)
    implicit none
    integer, intent(in) :: n
    integer, dimension(:) :: a
    integer :: i, total, total_sum


    integer :: rank, mpi_size, ierror!, tag, status(MPI_STATUS_SIZE)
    integer :: ista, iend ! For para_range

    ! Initialize MPI
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, mpi_size, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)

    call para_range(1, n, mpi_size, rank, ista, iend)

    do i = ista, iend
        a(i) = i
    end do

    total = 0

    do i = ista, iend
        total = total + a(i)
    end do

    call MPI_REDUCE(total, total_sum, 1, MPI_INTEGER,MPI_SUM, 0,MPI_COMM_WORLD, ierror)

    if(rank == 0) write(*,*) "parallel total =",total_sum

    ! End MPI
    call MPI_FINALIZE(ierror)

    write(*,*) rank, ista, iend, total

end subroutine threaded_sum

subroutine para_range( n1, n2, nprocs, irank, ista, iend)
!    integer, intent(in) :: n1, n2, nprocs, irank
!    integer, intent(in out) :: ista, iend

    integer :: n1 ! Lowest value of iteration variable
    integer :: n2 ! Highest value of iteration variable
    integer, intent(in) :: nprocs ! # cores
    integer, intent(in) :: irank ! Iproc (rank)
    integer :: ista ! Start of iterations for rank iproc
    integer :: iend ! End of iterations for rank iproc
    integer :: iwork1, iwork2

    iwork1 = ( n2 - n1 + 1 ) / nprocs
    iwork2 = MOD(n2 - n1 + 1, nprocs)
    ista = irank * iwork1 + n1 + MIN(irank, iwork2)
    iend = ista + iwork1 -1
    if ( iwork2 > irank ) iend = iend + 1
return
end subroutine para_range

end module parallel_loop
