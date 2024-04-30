module lista_Img
    use objetoImagen
    implicit none 
  
    type :: linked_list_img
      type(node), pointer :: head => null() ! head of the list
  
      contains
        procedure :: pushIMG
        procedure :: verificarElementos
        procedure :: imprimirTop5
    end type linked_list_img
  
    type :: node
      type(objimagen) :: value
      type(node), pointer :: next
    end type node 
  
    contains
  
    subroutine pushIMG(self, value)
      class(linked_list_img), intent(inout) :: self
      type(objimagen), intent(in) :: value
  
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
    end subroutine pushIMG
  
  
    subroutine verificarElementos(self, verificarEstado)
      class(linked_list_img), intent(inout) :: self
      logical::verificarEstado
      if (.not. associated(self%head)) then
        verificarEstado = .false.
      else
        verificarEstado = .true.
      end if
      end subroutine verificarElementos


    

      subroutine imprimirTop5(self)
        class(linked_list_img), intent(inout) :: self
      
        type(objimagen), allocatable :: vectorObjetos(:)
        type(node), pointer :: currentNode
        integer :: i, j, n
        integer :: idImagen
        integer, dimension(:), allocatable :: listaCapasImg
      
        ! Contamos la cantidad de objetos en la lista
        currentNode => self%head
        n = 0
        do while (associated(currentNode))
          n = n + 1
          currentNode => currentNode%next
        end do
      
        ! Asignamos tamaño al vector
        allocate(vectorObjetos(n))
      
        ! Llenamos el vector con los objetos de la lista
        currentNode => self%head
        do i = 1, n
          idImagen = currentNode%value%id_imagen
          listaCapasImg = currentNode%value%listaCapasImg
          call vectorObjetos(i)%inicializarIMG(idImagen, listaCapasImg)
          currentNode => currentNode%next
        end do
      
        ! Ordenamos el vector por la cantidad de elementos en listaCapasImg
        do i = 1, n-1
          do j = i+1, n
            if (size(vectorObjetos(i)%listaCapasImg) < size(vectorObjetos(j)%listaCapasImg)) then
              call swapObjetos(vectorObjetos(i), vectorObjetos(j))
            end if
          end do
        end do
      
        ! Imprimimos los primeros 5 elementos del vector ordenado
        print *, "Top 5 objetos con más elementos en listaCapasImg:"
        do i = 1, min(5, n)
          print *, "ID de la imagen: ", vectorObjetos(i)%id_imagen, ", Cantidad de elementos: ",&
           size(vectorObjetos(i)%listaCapasImg)
        end do
      
        deallocate(vectorObjetos)
      
      contains
      
        subroutine swapObjetos(obj1, obj2)
          type(objimagen), intent(inout) :: obj1, obj2
          type(objimagen) :: temp
      
          temp = obj1
          obj1 = obj2
          obj2 = temp
        end subroutine swapObjetos
      
      end subroutine imprimirTop5
      

  end module lista_Img
  
    