module mpi_tools

    ! Modules
    use mpi

    ! Variables
    implicit none

    integer :: rank, mpi_size, ierror!, tag, status(MPI_STATUS_SIZE)

    public :: start_parallel, end_parallel, rank, mpi_size, ierror
contains

subroutine start_parallel()
    implicit none
    ! Initialize MPI
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, mpi_size, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
end subroutine start_parallel

subroutine end_parallel()
    implicit none
    ! End MPI
    call MPI_FINALIZE(ierror)
end subroutine

!subroutine print_1D_array(array)
!    implicit none
!    double precision, dimension(:), intent(in) :: array
!    !
!    integer :: i,j,N
!    N=size(array)
!
!    do i=1+rank,size(array),nproc
!        j=i+rank
!        if(N>j) write(*,*) j,array(j)
!    end do
!end subroutine

end module mpi_tools
