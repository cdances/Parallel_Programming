module hello_world_mod
    ! Modules
!    use mpi

    ! Variables
    implicit none

    private
    public :: hello_world

contains

subroutine hello_world(node,n_nodes)
    implicit none
    integer, intent(in) :: node, n_nodes

    write(*,"(A,I11,A,I11)") "Hello World from node", node, " of ", n_nodes
end subroutine hello_world

end module hello_world_mod 
