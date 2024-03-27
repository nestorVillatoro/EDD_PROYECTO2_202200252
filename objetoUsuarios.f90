module objetoUsuarios
    implicit none 

    type:: objUsuarios
        character(len=:), allocatable:: nombre
        character(len=:), allocatable:: DPI
        character(len=:), allocatable:: password

    end type objUsuarios
 
    contains

    subroutine Uinicializar(self, nombre, DPI, password)
        class(objUsuarios), intent(inout):: self
        character(len=*), intent(in):: nombre, DPI, password

        self%nombre = nombre
        self%DPI = DPI
        self%password = password
    end subroutine Uinicializar 

end module objetoUsuarios