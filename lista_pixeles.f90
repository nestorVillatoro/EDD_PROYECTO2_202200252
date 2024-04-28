module lista_pixeles
    use objetoPixeles
    implicit none 
  
    type :: linked_list_pixeles
      type(node), pointer :: head => null() ! head of the list
  
      contains
        procedure :: push
        procedure :: verificarElementos
        procedure :: buscarPorID
    end type linked_list_pixeles
  
    type :: node
      type(objpixeles) :: value
      type(node), pointer :: next
    end type node 
  
    contains
  
    subroutine push(self, value)
      class(linked_list_pixeles), intent(inout) :: self
      type(objpixeles), intent(in) :: value
  
      type(node), pointer :: newNode
      allocate(newNode)
  
      newNode%value = value
      newNode%next => null()
  
      if (.not. associated(self%head)) then
        self%head => newNode
      else
        newNode%next => self%head
        self%head => newNode
      end if
  
      !print *, 'pushed: ', value%capaID, " ", value%fila, " ", value%columna, " ", value%color
    end subroutine push
  
  
    subroutine verificarElementos(self, verificarEstado)
      class(linked_list_pixeles), intent(inout) :: self
      logical::verificarEstado
      if (.not. associated(self%head)) then
        verificarEstado = .false.
      else
        verificarEstado = .true.
      end if
      end subroutine verificarElementos
    
    function buscarPorID(self, id_a_graficar, longitud) result(lista_objpixeles)
    
        integer, parameter :: max_longitud = 1000000
        class(linked_list_pixeles), intent(inout) :: self
        integer, intent(in) :: id_a_graficar
        integer :: longitud
        type(node), pointer :: current
        type(objpixeles) :: lista_objpixeles(max_longitud)

        current => self%head
        
        
        do while (associated(current))

            if(current%value%capaID .eq. id_a_graficar) then
              if (longitud < max_longitud) then
                lista_objpixeles(longitud+1) = current%value
                longitud = longitud+1
              end if
            end if
          current => current%next
        end do

    end function buscarPorID 
  end module lista_pixeles
  
    