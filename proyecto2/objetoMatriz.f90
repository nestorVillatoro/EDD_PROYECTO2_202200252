module objetoMatriz
    use matrix_m
    implicit none

    type :: capaMatriz
        integer :: IdC
        type(matrix_t) :: Capa

        contains
        procedure :: MTXinicializar

    end type capaMatriz

    contains

    subroutine MTXinicializar(self, idC, capa)
        class(capaMatriz), intent(inout) :: self
        integer, intent(in) :: idC
        type(matrix_t), intent(in) :: capa
        
        self%IdC = idC
        self%Capa = capa
    end subroutine MTXinicializar


end module objetoMatriz