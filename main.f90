program main
use objetoUsuarios
use json_module
use matrix_m
use abb_m 
use objetoMatriz
implicit none 
character(len=60) :: name, name2, contStr
integer :: DPI, DPI2
character(len=60) :: password, password2
integer :: opsAdmin, cont
character(len=200) :: file_path

!capas
logical:: found
type(json_core) :: jsonc
type(json_value), pointer :: listaPointer, attributePointer
type(json_value), pointer :: idPointer, var1, var2
integer size, i, fila, columna, id_capa, j, size2
character(:), allocatable :: color
character(len=500)::str_num_fila, str_num_columna, str_num_idcapa

type(json_file) :: json
!capas
type(abb) :: tree
type(capaMatriz) :: capaMTX
type(matrix_t) :: mtx

cont = 0

do while(.true.)
    print*, "--------------------LOGIN-------------------"
    print*, "|  .- Nombre compeleto                     |"
    !read(*,*) name
    print*, "|  .- DPI                                  |"
    !read(*,*) DPI 
    print*, "|  .- Password                             |"
    !read(*,*) password
    print*, "--------------------------------------------"


    if((name.eq."admin") .and. (password.eq."EDD2024")) then
        do while(.true.)
            print*, "-----------------MENU ADMIN-----------------"
            print*, "|  1. Cargar usuarios                      |"
            print*, "|  2. Cerrar sesion                        |"
            print*, "--------------------------------------------"
            print*, "Ingrese una opcion: "
            read(*,*) opsAdmin
            select case(opsAdmin)
            case(1) 
                name2 = "username"
                DPI2 = 12345
                password2 = "megumin:3"
            case(2)
                print*, "Cerrando sesion..."
                exit
            end select


        enddo
    end if


    !if((name.eq.name2).and.(DPI.eq.DPI2).and.(password.eq.password2)) then
    if(.true.) then
        do while(.true.)
            print*, "------------------MENU USER-----------------"
            print*, "|  1. Cargar capas                         |"
            print*, "|  2. Graficar Arbol ABB                   |"
            print*, "|  3. Cerrar sesion                        |"
            print*, "--------------------------------------------"
            print*, "Ingrese una opcion: "
            read(*,*) opsAdmin
            select case(opsAdmin)
            case(1) 
                cont = cont + 1
                call mtx%init()
                print*, "Ingrese la ruta dela rchivo a cargar: "
                read(*,*) file_path

                call json%initialize()
       
                call json%load(filename=trim(file_path))

                call json%info('',n_children=size)
                call json%get_core(jsonc)
                call json%get('', listaPointer, found) 

                do i = 1, size
                    call jsonc%get_child(listaPointer, i, idPointer, found)

                    call jsonc%get_child(idPointer, 'id_capa', attributePointer, found)
                    call jsonc%get(attributePointer, id_capa)

                    call jsonc%get_child(idPointer, 'pixeles', attributePointer, found)
        
                    call jsonc%info(attributePointer,n_children=size2)

                    do j = 1, size2
                        call jsonc%get_child(attributePointer, j, var1, found)

                        call jsonc%get_child(var1, 'fila', var2, found)
                        call jsonc%get(var2, fila)

                        call jsonc%get_child(var1, 'columna', var2, found)
                        call jsonc%get(var2, columna)

                        call jsonc%get_child(var1, 'color', var2, found)
                        call jsonc%get(var2, color)
                        write(str_num_columna, '(I0)') columna
                        write(str_num_fila, '(I0)') fila
                        write(str_num_idcapa, '(I0)') id_capa
                        
                        !print *, "id_capa ",  trim(str_num_idcapa)
                        !print *, 'Fila: ', trim(str_num_fila), ' Columna: ', trim(str_num_columna), ' Color: ', color
                        call mtx%add(fila, columna, color) 
                        !print*, ""

                    end do
                    call capaMTX%MTXinicializar(id_capa, mtx)
                    call tree%insert(capaMTX)

                end do
                call json%destroy()
                call mtx%create_dot()
                write(contStr, '(I0)') cont
                call system("dot -Gnslimit=2 -Tpng graph/graphImg.dot -o graph/Imagen" // trim(contStr) //".png")
                call system("start graph/Imagen" // trim(contStr) //".png")
            case(2)
                call tree%graph("ABBGraph")
                call system("start graph/ABBGraph.png")
            case(3)
                print*, "Cerrando sesion..."
                exit
            end select


        enddo
    end if

enddo
end program main 