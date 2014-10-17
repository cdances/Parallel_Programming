module jacobian_construction

    ! Modules
    use mpi_tools

    ! Variables
    implicit none
    double precision, dimension(:,:), allocatable :: variable, jacobian
    double precision, dimension(:), allocatable :: rhs!, column

    double precision, parameter :: dt = 1.0
    double precision, parameter :: eps = 0.001
!    integer :: jac_count = 0
    private
    public :: rhs, jacobian, &  !column
              allocate_arrays, deallocate_arrays, &
              calc_column, calc_jacobian, &
              print_jacobian

contains

subroutine allocate_arrays(N)
    implicit none
    integer, intent(in)::N
    integer :: i

    if( .not. allocated(variable)) then
        allocate(variable(N,2))
        do i=1,N
            variable(i,1)=1.0
            variable(i,2)=1.0
        end do
    end if
    if( .not. allocated(jacobian)) allocate(jacobian(N,N))
!    if( .not. allocated(column)) allocate(column(N))
    if( .not. allocated(rhs)) allocate(rhs(N))

end subroutine allocate_arrays

subroutine deallocate_arrays()
    implicit none

    if( allocated(variable) ) deallocate(variable)
    if( allocated(jacobian) ) deallocate(jacobian)
!    if( allocated(column) ) deallocate(column)
    if( allocated(rhs) ) deallocate(rhs)

end subroutine deallocate_arrays

double precision function pde_mass(pos)
    implicit none
    integer, intent(in) :: pos

    pde_mass = ( variable(pos,2) - variable(pos,1) ) / dt - 3.0

end function pde_mass

subroutine calc_column()
    implicit none
!    double precision, dimension(:), intent(in out) :: array
    !
    integer :: i

    do i=1+rank,size(rhs),mpi_size
        rhs(i)=-1.0*pde_mass(i)
    end do

end subroutine

subroutine calc_jacobian()
    implicit none
    integer :: i,j,k, n
    double precision :: f_var

!    if( rank == 0 ) write(*,*) size(jacobian)
    n = size(jacobian,1)
    do k=0+rank,n*n-1,mpi_size
        j = mod(k,n)+1
        i = floor(real(k)/n) + 1
        f_var = pde_mass(i)

        variable(j,2) = variable(j,2) + eps
        jacobian(i,j)=(pde_mass(i)- f_var)/eps
        variable(j,2) = variable(j,2) - eps
        write(*,*) rank, k, i, j, rank, jacobian(i,j)
    end do
end subroutine

subroutine print_jacobian()
    implicit none
    integer :: i,j

    do i=1,size(jacobian,1)
        write(*,"(I5)",advance="no") i
        do j=1,size(jacobian,2)
            if( jacobian(i,j) /= 0.0 ) then
                write(*,"(I5,F11.5)",advance="no") j, jacobian(i,j)
            end if
        end do
        write(*,*)
    end do

end subroutine


end module jacobian_construction
