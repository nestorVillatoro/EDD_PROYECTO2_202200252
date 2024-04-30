module objetoPixeles
    implicit none
    type :: objpixeles
        integer :: capaID
        integer :: fila
        integer :: columna
        character(len=7) :: color

        contains
        procedure :: PXinicializar

    end type objpixeles

    contains

    subroutine PXinicializar(self, CapaID, Columna, Fila, Color)
        class(objpixeles), intent(inout) :: self
        integer, intent(in) :: CapaID, Columna, Fila
        character(len=10), intent(in) :: Color
        
        self%capaID = CapaID
        self%columna = Columna
        self%fila = Fila
        self%color = Color
    end subroutine PXinicializar


end module objetoPixeles