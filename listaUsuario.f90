module lista_usuarios
    use objetoUsuarios
    implicit none 
  
    type :: linked_list_usuarios
      type(node), pointer :: head => null() ! head of the list
  
      contains
        procedure :: pushU
        procedure :: verificarElementos
        procedure :: verificarUsuario
        procedure :: imprimir_DPIdescendente
        procedure :: actualizar_usuario
        procedure :: eliminar_usuario
        procedure :: encontrar_usuario
    end type linked_list_usuarios
  
    type :: node
      type(objUsuarios) :: value
      type(node), pointer :: next
    end type node 
  
    contains
  
    subroutine pushU(self, value)
      class(linked_list_usuarios), intent(inout) :: self
      type(objusuarios), intent(in) :: value
  
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
    end subroutine pushU
  
  
    subroutine verificarElementos(self, verificarEstado)
      class(linked_list_usuarios), intent(inout) :: self
      logical::verificarEstado
      if (.not. associated(self%head)) then
        verificarEstado = .false.
      else
        verificarEstado = .true.
      end if
      end subroutine verificarElementos
    
    function verificarUsuario(lista, nombre, DPI, password) result(encontrado)
        class(linked_list_usuarios), intent(inout) :: lista
        character(len=60), intent(in) :: nombre, password
        integer(kind=8), intent(in) :: DPI
        logical :: encontrado
        type(node), pointer :: currentNode
        
        encontrado = .false.  
        
        if (.not. associated(lista%head)) then
            return  
        end if
        
        currentNode => lista%head  
        
        do while (associated(currentNode))
            if (currentNode%value%DPI == DPI .and. &
                trim(currentNode%value%nombre) == trim(nombre) .and. &
                trim(currentNode%value%password) == trim(password)) then
                encontrado = .true.  
                exit  
            end if
            currentNode => currentNode%next 
        end do
        
    end function verificarUsuario

    subroutine bubble_sort(arr, arr2, arr3)
      integer(kind=8), intent(inout) :: arr(:)
      character(len=60), intent(inout) :: arr2(:), arr3(:)
      integer :: i, j, n
      integer(kind=8) :: temp
      character(len=60) :: temp2, temp3
      
      n = size(arr)
      
      do i = 1, n-1
          do j = 1, n-i
              if (arr(j) < arr(j+1)) then
                  temp = arr(j)
                  temp2 = arr2(j)
                  temp3 = arr3(j)
                  arr(j) = arr(j+1)
                  arr2(j) = arr2(j+1)
                  arr3(j) = arr3(j+1)
                  arr(j+1) = temp
                  arr2(j+1) = temp2
                  arr3(j+1) = temp3

              end if
          end do
      end do
  end subroutine bubble_sort
  
  subroutine imprimir_DPIdescendente(lista)
      class(linked_list_usuarios), intent(in) :: lista
      type(node), pointer :: currentNode
      integer(kind=8), allocatable :: temp_dpi(:)
      character(len=60), allocatable :: temp_name(:), temp_contra(:)
      integer :: i, size

      currentNode => lista%head
      size = 0
      do while (associated(currentNode))
          size = size + 1
          currentNode => currentNode%next
      end do
      
      allocate(temp_dpi(size))
      allocate(temp_name(size))
      allocate(temp_contra(size))

      currentNode => lista%head
      i = 1
      do while (associated(currentNode))
          temp_dpi(i) = currentNode%value%DPI
          temp_name(i) = currentNode%value%nombre
          temp_contra(i) = currentNode%value%password
          currentNode => currentNode%next
          i = i + 1
      end do
      
      call bubble_sort(temp_dpi, temp_name, temp_contra)
      
      print *, "DPI en orden descendente:"
      do i = 1, size
          print *, temp_dpi(i), " N: ", trim(temp_name(i)), " C: ", trim(temp_contra(i))
      end do
      
  end subroutine imprimir_DPIdescendente
  
  subroutine actualizar_usuario(lista, DPI, nombre_nuevo, password_nueva)
    class(linked_list_usuarios), intent(inout) :: lista
    integer(kind=8), intent(in) :: DPI
    character(len=*), intent(in) :: nombre_nuevo, password_nueva
    type(node), pointer :: currentNode
    
    currentNode => lista%head
    
    do while (associated(currentNode))
        if (currentNode%value%DPI == DPI) then
            currentNode%value%nombre = nombre_nuevo
            currentNode%value%password = password_nueva
            return  
        end if
        currentNode => currentNode%next
    end do
    
    print *, "DPI no encontrado en la lista."
end subroutine actualizar_usuario

subroutine eliminar_usuario(lista, DPI)
  class(linked_list_usuarios), intent(inout) :: lista
  integer(kind=8), intent(in) :: DPI
  type(node), pointer :: currentNode, prevNode
  
  currentNode => lista%head
  prevNode => null()
  
  do while (associated(currentNode))
      if (currentNode%value%DPI == DPI) then
          
          if (associated(prevNode)) then
              prevNode%next => currentNode%next
              deallocate(currentNode)
          else
              
              lista%head => currentNode%next
              deallocate(currentNode)
          end if
          return  
      end if
      prevNode => currentNode
      currentNode => currentNode%next
  end do
  
  
  print *, "DPI no encontrado en la lista."
end subroutine eliminar_usuario

subroutine encontrar_usuario(lista, DPI)
  class(linked_list_usuarios), intent(inout) :: lista
  integer(kind=8), intent(in) :: DPI
  type(node), pointer :: currentNode, prevNode
  
  currentNode => lista%head
  prevNode => null()
  
  do while (associated(currentNode))
      if (currentNode%value%DPI == DPI) then
          print*, "Usuario encontrado!"
          print*, "DPI: ", currentNode%value%DPI
          print*, "Nombre: ", currentNode%value%nombre
          print*, "Password: ", currentNode%value%password
          return
      end if
      prevNode => currentNode
      currentNode => currentNode%next
  end do
  
  
  print *, "DPI no encontrado en la lista."
end subroutine encontrar_usuario



  end module lista_usuarios
  
    