module objetoUsuarios
    implicit none 

    type:: objUsuarios
        character(len=:), allocatable:: nombre
        integer(kind=8), allocatable:: DPI
        character(len=:), allocatable:: password
        contains
        procedure :: Uinicializar
    end type objUsuarios
 
    contains

    subroutine Uinicializar(self, nombre, DPI, password)
        class(objUsuarios), intent(inout):: self
        character(len=*), intent(in):: nombre, password
        integer(kind=8), intent(in):: DPI

        self%nombre = nombre
        self%DPI = DPI
        self%password = password
    end subroutine Uinicializar 

end module objetoUsuarios