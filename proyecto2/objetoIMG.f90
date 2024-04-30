module objetoImagen
    
    implicit none

    type :: objimagen
        integer :: id_imagen
        integer, dimension(:), allocatable :: listaCapasImg
        contains
        procedure :: inicializarIMG

    end type objimagen

    contains

    subroutine inicializarIMG(self, id, listaCapasImg)
        class(objimagen), intent(inout) :: self
        integer, intent(in) :: id
        integer, dimension(:), intent(in) :: listaCapasImg
        
        self%id_imagen = id
        self%listaCapasImg = listaCapasImg

    end subroutine inicializarIMG

end module objetoImagen