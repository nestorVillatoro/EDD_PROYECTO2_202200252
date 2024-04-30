program main
use objetoUsuarios
use json_module
use objetoImagen
use ModuloAVL 
use matrix_m
use abb_m 
use objetoMatriz
use objetoPixeles
use lista_pixeles
use lista_usuarios
use BTree
use lista_Img
implicit none 
character(len=60) :: name, contStr, password, name_cambiar, password_cambiar
integer :: opsAdmin, cont, id_a_graficar, longitud, opsin, cantCapas, abc, opsRepAdm, opsRepUser, xx
integer(kind=8) :: dpiCentero, DPI, DPI_cambiar
character(len=200) :: file_path
type(abb) :: tree 
integer, parameter :: max_longitud = 1000000
type(objpixeles) :: lista_objpixeles(max_longitud)
type(objpixeles) :: datoPixel
type(capaMatriz) :: capaMTX
type(matrix_t) :: mtx
type(objUsuarios) :: user
type(linked_list_pixeles) :: llpixeles
logical :: verificar_continuar
character(len=1) :: opsCapa
type(linked_list_usuarios) :: llusuarios
logical :: encontrado
integer, allocatable:: listaIDcapas(:)
type(linked_list_img) :: listaI
integer, dimension(:,:), allocatable :: listaProf
!capas
logical:: found
type(json_core) :: jsonc
type(json_value), pointer :: listaPointer, attributePointer
type(json_value), pointer :: idPointer, var1, var2
integer :: size, i, fila, columna, id_capa, j, size2
character(:), allocatable :: color
character(len=500)::str_num_fila, str_num_columna, str_num_idcapa

type(json_file) :: json
!capas

!clientes
logical:: foundc
type(json_core) :: jsoncc
type(json_value), pointer :: listaPointerC, clientePointerC, attributePointerC
integer sizeC, iC
character(:), allocatable :: dpiC, nombre_clienteC, passwordC

type(json_file) :: jsonCl
!clientes

!img
type(objimagen) :: objImg
type(Tree_t) :: avl
logical:: foundI
type(json_core) :: jsonccc
type(json_value), pointer :: listaIMG, imagenP, atributoP, capasDeImg, capaIP
integer :: sizeI, iI, jI, IDIMGS, numCI, capaIMG
integer, dimension(:), allocatable :: listaCapasImg


type(json_file) :: jsonIMG
!img
type(BtreeNode), pointer :: arbolUsuarios => null()


cont = 0

do while(.true.)
    print*, "--------------------LOGIN-------------------"
    print*, "|  .- Nombre compeleto                     |"
    read(*,*) name
    print*, "|  .- DPI                                  |"
    read(*,*) DPI 
    print*, "|  .- Password                             |"
    read(*,*) password
    print*, "--------------------------------------------"

 
    if((name.eq."admin") .and. (password.eq."EDD2024")) then
        do while(.true.)
            print*, "-----------------MENU ADMIN-----------------"
            print*, "|  1. Cargar usuarios                      |"
            print*, "|  2. Cargar usuario independiente         |"
            print*, "|  3. Modificar usuario                    |"
            print*, "|  4. Eliminar usuario                     |"
            print*, "|  5. Reportes                             |"
            print*, "|  6. Cerrar sesion                        |"
            print*, "--------------------------------------------"
            print*, "Ingrese una opcion: "
            read(*,*) opsAdmin
            select case(opsAdmin)
            case(1) 
                print*, "Ingrese la ruta de la rchivo a cargar: "
                read(*,*) file_path

                call jsonCl%initialize()
                call jsonCl%load(filename = trim(file_path))
                call jsonCl%info("", n_children=sizeC) 

                call jsonCl%get_core(jsoncc)
                call jsonCl%get("", listaPointerC, foundc)
                do iC = 1, sizeC 
                    call jsoncc%get_child(listaPointerC, iC, clientePointerC, found=foundc)

                    call jsoncc%get_child(clientePointerC, "dpi", attributePointerC, foundc)
                    if(foundc) then
                        call jsoncc%get(attributePointerC, dpiC)
                    end if

                    call jsoncc%get_child(clientePointerC, "nombre_cliente", attributePointerC, foundc)
                    if(foundc) then
                        call jsoncc%get(attributePointerC, nombre_clienteC)
                    end if

                    call jsoncc%get_child(clientePointerC, "password", attributePointerC, foundc)
                    if(foundc) then
                        call jsoncc%get(attributePointerC, passwordC)
                    end if

                    !print*, "DPI: ", trim(dpiC), " Nombre Cliente: ", trim(nombre_clienteC), " Password: ", trim(passwordC)
                    read(dpiC, "(I13)") dpiCentero
                    call user%Uinicializar(nombre_clienteC, dpiCentero, passwordC)  
                    call insertB(dpiCentero, nombre_clienteC, passwordC, arbolUsuarios)
                    call llusuarios%pushU(user)
                end do
                call llusuarios%imprimir_DPIdescendente()
                call jsonCl%destroy()

            case(2)
                print*, "DPI del usuario a agregar: "
                read(*,*) DPI_cambiar
                print*, "Nombre del usuario a agregar: "
                read(*,*) name_cambiar
                print*, "Contrasenia del usuario a agregar: "
                read(*,*) password_cambiar
                call user%Uinicializar(name_cambiar, DPI_cambiar, password_cambiar)  
                call insertB(DPI_cambiar, name_cambiar, password_cambiar, arbolUsuarios)
                call llusuarios%pushU(user)
                call llusuarios%imprimir_DPIdescendente()
            case(3)
                call llusuarios%imprimir_DPIdescendente()
                print*, "DPI del usuario a cambiar: "
                read(*,*) DPI_cambiar
                print*, "Nuevo nombre: "
                read(*,*) name_cambiar
                print*, "Nueva contraseña: "
                read(*,*) password_cambiar
                call llusuarios%actualizar_usuario(DPI_cambiar, name_cambiar, password_cambiar)
                call llusuarios%imprimir_DPIdescendente()
            case(4)
                call llusuarios%imprimir_DPIdescendente()
                print*, "DPI del usuario a eliminar: "
                read(*,*) DPI_cambiar
                call llusuarios%eliminar_usuario(DPI_cambiar)
                call llusuarios%imprimir_DPIdescendente()

            case(5)
                print*, "---------------MENU REPORTES----------------"
                print*, "|  1. Buscar Usuario Específico            |"
                print*, "|  2. Listar Usuarios                      |"
                print*, "|  3. Regresar                             |"
                print*, "--------------------------------------------"
                read(*,*) opsRepAdm
                if(opsRepAdm.eq.1) then
                    print*, "DPI del usuario a buscar: "
                    read(*,*) DPI_cambiar
                    call llusuarios%encontrar_usuario(DPI_cambiar)
                else if(opsRepAdm.eq.2) then
                    call llusuarios%imprimir_DPIdescendente()
                else if(opsRepAdm.eq.3) then
                    print*, "Regresando..."
                else
                    print*, "Opcion no valida!"
                end if
            case(6)
                print*, "Cerrando sesion..."
                exit
            end select
 

        enddo
    end if

    encontrado = llusuarios%verificarUsuario(name, DPI, password)  
    if(encontrado) then
    !if(.true.) then
        do while(.true.)
            print*, "------------------MENU USER-----------------"
            print*, "|  1. Cargar capas                         |"
            print*, "|  2. Graficar Arbol ABB                   |"
            print*, "|  3. Graficar por capa                    |"
            print*, "|  4. Graficar limitado                    |"
            print*, "|  5. Cargar imagenes                      |"
            print*, "|  6. Graficar Arbol AVL                   |"
            print*, "|  7. Reportes                             |"
            print*, "|  8. Cerrar sesion                        |"
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
                        call datoPixel%PXinicializar(id_capa, columna, fila, color)
                        call llpixeles%push(datoPixel)
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
                verificar_continuar = .true.
                longitud = 0
                do while(verificar_continuar)
                    print*, "Capas dispoibles: "
                    call tree%inorder()
                    print*, "Ingrese la capa que desea graficar: "
                    read(*,*) id_a_graficar
                    lista_objpixeles =  llpixeles%buscarPorID(id_a_graficar, longitud)
                    print*, "Desea continuar agregando capas para graficar? Y/N"
                    read(*,*) opsCapa
                    if (opsCapa.eq."N") then
                        verificar_continuar = .false.
                    end if
                end do
                call mtx%init()
                do i = 1, longitud
                    call mtx%add(lista_objpixeles(i)%fila, lista_objpixeles(i)%columna, lista_objpixeles(i)%color)
                end do
                call mtx%create_dot()
                call system("dot -Gnslimit=2 -Tpng graph/graphImg.dot -o graph/ImagenCapa.png")
                call system("start graph/ImagenCapa.png") 
               

            case(4)
                cantCapas = 0
                print*, "------------------OPCIONES------------------"
                print*, "|  1. inorder                              |"
                print*, "|  2. preorder                             |"
                print*, "|  3. posorder                             |"
                print*, "|  4. regresar                             |"
                print*, "--------------------------------------------"
                read(*,*) opsin
                if(opsin.eq.1)then
                    print*, "Capas inorder:"
                    call tree%inorder()
                    print*, "Cuantas capas desea graficar?"
                    read(*,*) cantCapas
                    allocate(listaIDcapas(cantCapas)) 
                    listaIDcapas= tree%inorder2(cantCapas)
                    longitud = 0
                    do abc = 1, cantCapas
                    lista_objpixeles =  llpixeles%buscarPorID(listaIDcapas(abc), longitud)
                    end do
                    call mtx%init()
                    do i = 1, longitud
                        call mtx%add(lista_objpixeles(i)%fila, lista_objpixeles(i)%columna, lista_objpixeles(i)%color)
                    
                    end do
                    call mtx%create_dot()
                    
                    call system("dot -Gnslimit=2 -Tpng graph/graphImg.dot -o graph/ImagenInorder.png")
                    call system("start graph/ImagenInorder.png")
                    deallocate(listaIDcapas)

                else if (opsin.eq.2) then
                    print*, "Capas preorder:"
                    call tree%preorder()
                    print*, "Cuantas capas desea graficar?"
                    read(*,*) cantCapas
                    allocate(listaIDcapas(cantCapas))
                    listaIDcapas= tree%preorder2(cantCapas) 
                    longitud = 0
                    do abc = 1, cantCapas
                    lista_objpixeles =  llpixeles%buscarPorID(listaIDcapas(abc), longitud)
                    end do
                    call mtx%init()
                    do i = 1, longitud
                        call mtx%add(lista_objpixeles(i)%fila, lista_objpixeles(i)%columna, lista_objpixeles(i)%color)
                    
                    end do
                    call mtx%create_dot()
                    
                    call system("dot -Gnslimit=2 -Tpng graph/graphImg.dot -o graph/ImagenPreorder.png")
                    call system("start graph/ImagenPreorder.png")
                    deallocate(listaIDcapas)

                else if (opsin.eq.3) then
                    print*, "Capas posorder:"
                    call tree%posorder()
                    print*, "Cuantas capas desea graficar?"
                    read(*,*) cantCapas
                    allocate(listaIDcapas(cantCapas))
                    listaIDcapas= tree%posorder2(cantCapas) 
                    longitud = 0
                    do abc = 1, cantCapas
                    lista_objpixeles =  llpixeles%buscarPorID(listaIDcapas(abc), longitud)
                    end do
                    call mtx%init()
                    do i = 1, longitud
                        call mtx%add(lista_objpixeles(i)%fila, lista_objpixeles(i)%columna, lista_objpixeles(i)%color)
                    
                    end do
                    call mtx%create_dot()
                    
                    call system("dot -Gnslimit=2 -Tpng graph/graphImg.dot -o graph/ImagenPosorder.png")
                    call system("start graph/ImagenPosorder.png")
                    deallocate(listaIDcapas)

                else if (opsin.eq.4) then

                else
                    print*, "opcion no valida"
                end if

            case(5)
                print*, "Ingrese la ruta del archivo a cargar: "
                read(*,*) file_path


                call jsonIMG%initialize()
                call jsonIMG%load(filename=trim(file_path))
                call jsonIMG%info(' ', n_children=sizeI)
                call jsonIMG%get_core(jsonccc)
                call jsonIMG%get('', listaIMG, foundI)

                do iI = 1, sizeI
                    call jsonccc%get_child(listaIMG, iI, imagenP, found=foundI) 
                    call jsonccc%get_child(imagenP, "id", atributoP, found=foundI)
                    if(foundI) then
                        call jsonccc%get(atributoP, IDIMGS)
                    end if
                    call jsonccc%get_child(imagenP, "capas", capasDeImg, found=foundI) 
                    call jsonccc%info(capasDeImg, n_children = numCI)
                    allocate(listaCapasImg(numCI))
                    do jI = 1, numCI
                        call jsonccc%get_child(capasDeImg, jI, capaIP, found=foundI)
                        call jsonccc%get(capaIP, capaIMG)
                        listaCapasImg(jI) =  capaIMG
                    end do 
                    call objImg%inicializarIMG(IDIMGS, listaCapasImg)
                    call avl%insert(objImg)
                    call listaI%pushIMG(objImg)
                    deallocate(listaCapasImg)
                end do
                
            case(6)
                call avl%GenerateGraph()
                call system("start graph/AVLgraph.png")
            case(7)
                print*, "Top 5 Imagenes con mas Capas"
                    call listaI%imprimirTop5() 

                print*, " "
                print*, "Todas las Capas que son Hojas"
                    call tree%repHojas()
                print*," "
                print*, "Profundidad de Árbol de Capas"
                print*, " "
                print*, "Listar Capas"
                print*, "----------------Inorder----------------"
                call tree%inorder()
                print*, "---------------Preorder----------------"
                call tree%preorder()
                print*, "---------------Postorder---------------"
                call tree%posorder()
            case(8)
                print*, "Cerrando sesion..."
                exit
            end select


        enddo

    else if ((name.eq."admin") .and. (password.eq."EDD2024")) then

    else
        print*, "Nombre, DPI o contraseña incorrecta/s"
    end if

enddo
end program main 