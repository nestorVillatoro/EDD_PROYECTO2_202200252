module abb_m
    use objetoMatriz
    implicit none 
    private
 
    type :: Node_t
        type(capaMatriz) :: value
        type(Node_t), pointer :: right => null()
        type(Node_t), pointer :: left => null()
    end type Node_t

    type, public :: abb
        type(Node_t), pointer :: root => null()

    contains
        procedure :: insert
        procedure :: delete
        procedure :: preorder
        procedure :: preorder2
        procedure :: inorder
        procedure :: inorder2
        procedure :: posorder
        procedure :: posorder2
        procedure :: graph
        procedure :: repHojas
        procedure :: profundidad
    end type abb

    contains    
    
    subroutine insert(self, val)
        class(abb), intent(inout) :: self
        type(capaMatriz), intent(in) :: val

        if (.not. associated(self%root)) then
            allocate(self%root)
            self%root%value = val
        else
            call insertRec(self%root, val)
        end if
    end subroutine insert
    recursive subroutine insertRec(root, val)
        type(Node_t), pointer :: root
        type(capaMatriz), intent(in) :: val
        
        if (val%IdC < root%value%IdC) then
            if (.not. associated(root%left)) then
                allocate(root%left)
                root%left%value = val
            else
                call insertRec(root%left, val)
            end if
        else if (val%IdC > root%value%IdC) then
            if (.not. associated(root%right)) then
                allocate(root%right)
                root%right%value = val
            else
                call insertRec(root%right, val)
            end if
        end if
    end subroutine insertRec

    subroutine delete(self, val)
        class(abb), intent(inout) :: self
        type(capaMatriz), intent(inout) :: val
    
        self%root => deleteRec(self%root, val)
    end subroutine delete

    recursive function deleteRec(root, value) result(res)
        type(Node_t), pointer :: root
        type(capaMatriz), intent(in) :: value
        type(Node_t), pointer :: res
        type(Node_t), pointer :: temp

        if (.not. associated(root)) then
            res => root
            return
        end if

        if (value%IdC < root%value%IdC) then
            root%left => deleteRec(root%left, value)
        else if (value%IdC > root%value%IdC) then
            root%right => deleteRec(root%right, value)
        else
            
            if (.not. associated(root%left)) then
                temp => root%right
                deallocate(root)
                res => temp
                return
            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
                res => temp
                return
            else
                call getMajorOfMinors(root%left, temp)
                root%value = temp%value
                root%left => deleteRec(root%left, temp%value)
            end if
        end if

        res => root
    end function deleteRec


    subroutine repHojas(self)
        class(abb), intent(in) :: self
        
        integer :: num_hojas
        
        num_hojas = 0
        call repHojasRec(self%root, 10, num_hojas)

    end subroutine repHojas
    
    recursive subroutine repHojasRec(node, unit_num, num_hojas)
        type(Node_t), pointer :: node
        integer :: unit_num
        integer :: num_hojas
        character(len=20) :: str_idcapa 
        if (associated(node)) then
            call repHojasRec(node%left, unit_num, num_hojas)
            if (.not. associated(node%left) .and. .not. associated(node%right)) then
                write(str_idcapa, '(I0)') node%value%IdC
                print*, "capa ID: ", str_idcapa
            end if
            call repHojasRec(node%right, unit_num, num_hojas)
        end if
    end subroutine repHojasRec
    

    recursive subroutine getMajorOfMinors(root, major)
        type(Node_t), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinors(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinors

    subroutine preorder(self)
        class(abb), intent(in) :: self
        
        call preorderRec(self%root)
        write(*, '()')
    end subroutine preorder
    recursive subroutine preorderRec(root)
        type(Node_t), pointer, intent(in) :: root

        if(associated(root)) then
            ! RAIZ - IZQ - DER
            write(*, '(I0 A)', advance='no') root%value%IdC, " - "
            call preorderRec(root%left)
            call preorderRec(root%right)
        end if
    end subroutine preorderRec

    function preorder2(self, cantCapas) result(listaID)
        class(abb), intent(in) :: self
        integer, intent(in):: cantCapas
        integer, allocatable:: listaID(:)

        allocate(listaID(cantCapas))
        
        call preorderRec2(self%root, cantCapas, listaID)
        write(*, '()')
    end function preorder2
    recursive subroutine preorderRec2(root, cantCapas, listaID)
        type(Node_t), pointer, intent(in) :: root
        integer, intent(in):: cantCapas
        integer, allocatable:: listaID(:)
        integer :: cont = 1

        if(associated(root) .and. cont <= cantCapas) then
            ! RAIZ - IZQ - DER
            write(*, '(I0 A)', advance='no') root%value%IdC, " - "
            listaID(cont) = root%value%IdC
            cont = cont +1 
            call preorderRec2(root%left, cantCapas, listaID)
            call preorderRec2(root%right, cantCapas, listaID)
        end if
    end subroutine preorderRec2

    subroutine inorder(self)
        class(abb), intent(in) :: self
        
        call inordenRec(self%root)
        print *, ""
    end subroutine inorder
    recursive subroutine inordenRec(root)
        type(Node_t), pointer, intent(in) :: root

        if(associated(root)) then
            ! IZQ - RAIZ - DER
            call inordenRec(root%left)
            write(*, '(I0 A)', advance='no') root%value%IdC, " - "
            call inordenRec(root%right)
        end if
    end subroutine inordenRec

    function inorder2(self, cantCapas) result(listaID)
        class(abb), intent(in) :: self
        integer, intent(in):: cantCapas
        integer, allocatable:: listaID(:)

        allocate(listaID(cantCapas))
        
        call inordenRec2(self%root, cantCapas, listaID)
        print *, ""
    end function inorder2
    recursive subroutine inordenRec2(root, cantCapas, listaID)
        type(Node_t), pointer, intent(in) :: root
        integer, intent(in):: cantCapas
        integer, allocatable:: listaID(:)
        integer :: cont = 1

        if(associated(root) .and. cont <= cantCapas) then
            ! IZQ - RAIZ - DER
            call inordenRec2(root%left, cantCapas, listaID)
            if(cont <= cantCapas) then
            write(*, '(I0 A)', advance='no') root%value%IdC, " - "
            listaID(cont) = root%value%IdC
            cont = cont+1
            end if
            call inordenRec2(root%right, cantCapas, listaID)
        end if
    end subroutine inordenRec2

    subroutine posorder(self)
        class(abb), intent(in) :: self
        
        call posordenRec(self%root)
        print *, ""
    end subroutine posorder
    recursive subroutine posordenRec(root)
        type(Node_t), pointer, intent(in) :: root

        if(associated(root)) then
            ! IZQ - DER - RAIZ
            call posordenRec(root%left)
            call posordenRec(root%right)
            write(*, '(I0 A)', advance='no') root%value%IdC, " - "
        end if
    end subroutine posordenRec

    function posorder2(self, cantCapas) result(listaID)
        class(abb), intent(in) :: self
        integer, intent(in):: cantCapas
        integer, allocatable:: listaID(:)

        allocate(listaID(cantCapas))

        call posordenRec2(self%root, cantCapas, listaID)
        print *, ""
    end function posorder2
    recursive subroutine posordenRec2(root, cantCapas, listaID)
        type(Node_t), pointer, intent(in) :: root
        integer, intent(in):: cantCapas
        integer, allocatable:: listaID(:)
        integer :: cont = 1

        if(associated(root)) then
            ! IZQ - DER - RAIZ
            call posordenRec2(root%left, cantCapas, listaID)
            call posordenRec2(root%right, cantCapas, listaID)
            if(cont <= cantCapas)then
            write(*, '(I0 A)', advance='no') root%value%IdC, " - "
            listaID(cont) = root%value%IdC
            cont = cont+1
            end if
        end if
    end subroutine posordenRec2

    function buscarPorID(self, id_a_graficar, longitud) result(lista_objpixeles)
    
        integer, parameter :: max_longitud = 1000000
        class(abb), intent(inout) :: self
        integer, intent(in) :: id_a_graficar
        integer :: longitud
        type(node_t), pointer :: current
        type(capaMatriz) :: lista_objpixeles(max_longitud)

        current => self%root
        
        
        do while (associated(current))

            if(current%value%IdC .eq. id_a_graficar) then
              if (longitud < max_longitud) then
                lista_objpixeles(longitud+1) = current%value
                longitud = longitud+1
              end if
            end if
          current => current%left
        end do

    end function buscarPorID 

    subroutine profundidad(self, listaProf)
        class(abb), intent(in) :: self
        integer, dimension(:,:), allocatable, intent(out) :: listaProf
        integer :: cantNode, currentID
    
        cantNode = contNode(self%root)
        allocate(listaProf(cantNode, 2))  
    
        currentID = 1  
        call profundidadRec(self%root, listaProf, 1, currentID)
        
    end subroutine profundidad
    
    recursive subroutine profundidadRec(node, listaProf, depth, currentID)
        type(Node_t), pointer :: node
        integer, dimension(:,:), intent(out) :: listaProf
        integer :: depth, currentID
    
        if (associated(node)) then
            call profundidadRec(node%left, listaProf, depth + 1, currentID)
            
            listaProf(currentID, 1) = node%value%IdC
            listaProf(currentID, 2) = depth
            currentID = currentID + 1
            
            call profundidadRec(node%right, listaProf, depth + 1, currentID)
        end if
    end subroutine profundidadRec

    recursive function contNode(root) result(num_nodes)
        type(Node_t), pointer :: root
        integer :: num_nodes
    
        num_nodes = 0
        if (associated(root)) then
            num_nodes = 1 + contNode(root%left) + contNode(root%right)
        end if
    end function contNode

    subroutine graph(self, filename)
        class(abb), intent(in) :: self
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dotStructure
        character(len=:), allocatable :: createNodes
        character(len=:), allocatable :: linkNodes
        
        createNodes = ''
        linkNodes = ''

        dotStructure = "digraph G{" // new_line('a')
        dotStructure = dotStructure // "node [shape=circle];" // new_line('a')

        if (associated(self%root)) then
            call RoamTree(self%root, createNodes, linkNodes)
        end if
        
        dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
        call write(filename, dotStructure)
        print *, "Archivo actualizado existosamente."
    end subroutine graph
    recursive subroutine RoamTree(current, createNodes, linkNodes)
        type(Node_t), pointer :: current
        character(len=:), allocatable, intent(inout) :: createNodes, linkNodes
        character(len=20) :: address, str_value

        if (associated(current)) then
            ! SE OBTIENE INFORMACION DEL NODO ACTUAL
          address = get_address(current)
          write(str_value, '(I0)') current%Value%IdC
          createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_value) // '"];' // new_line('a')
          ! VIAJAMOS A LA SUBRAMA IZQ
          if (associated(current%Left)) then
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address(current%Left)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "L"];' // new_line('a')
    
          end if
          ! VIAJAMOS A LA SUBRAMA DER
          if (associated(current%Right)) then
            address = get_address(current)
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address(current%Right)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "R"];' // new_line('a')
          end if
    
          call RoamTree(current%Left, createNodes, linkNodes)
          call RoamTree(current%Right, createNodes, linkNodes)
        end if
    end subroutine RoamTree
    subroutine write(filename, code)
        character(len=*), intent(in) :: code, filename
        character(len=:), allocatable :: dot_filename, png_filename
        
        ! Agregar extensiones
        dot_filename = trim(filename) // ".dot"
        png_filename = trim(filename) // ".png"
        
        open(10, file="graph/"//dot_filename, status='replace', action='write')
        write(10, '(A)') trim(code)
        close(10)

        ! Genera la imagen PNG
        call system("dot -Tpng graph/"// dot_filename //" -o graph/" // png_filename)
    end subroutine write

    function get_address(node) result(address)
        !class(matrix_t), intent(in) :: self
        type(Node_t), pointer :: node
        character(len=20) :: address
        ! integer 8
        integer*8 :: i
    
        i = loc(node) ! get the address of x
        ! convert the address to string
        write(address, 10) i 
        10 format(I0)
    
    end function get_address

end module abb_m