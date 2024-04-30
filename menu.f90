program menu
    use hash_table_m
    use json_module
    implicit none 
    character(len=60) :: name, password
    integer :: opsAdmin, opsCargaDeARchivos
    character(len=200) :: file_path
    type(HashTable) :: table 
    ! tecnicos ------------------------------------------------------------------------
    logical :: found
    type(json_core) :: jsonc
    type(json_value), pointer :: listaPointer, clientePointer, attributePointer
    integer :: size, i
    character(:), allocatable :: nombre, apellido, genero, direccion, telefono, dpi
    type(json_file) :: json
    !tecnicos ------------------------------------------------------------------------

    do while(.true.) 
        print*, "--------------------LOGIN-------------------"
        print*, "|  .- User                                 |"
        read(*,*) name
        print*, "|  .- Password                             |"
        read(*,*) password
        print*, "--------------------------------------------"
    
     
        if((name.eq."EDD1S2024") .and. (password.eq."ProyectoFase3")) then
            do while(.true.)
                print*, "-----------------MENU ADMIN-----------------"
                print*, "|  1. Carga de archivos                    |"
                print*, "|  2. Sucursales                           |"
                print*, "|  3. Reportes                             |"
                print*, "|  4. Cerrar sesion                        |"
                print*, "--------------------------------------------"
                print*, "Ingrese una opcion: "
                read(*,*) opsAdmin
                select case(opsAdmin)
                case(1) 
                    print*, "-------------CARGA DE ARCHIVOS--------------"
                    print*, "|  1. Sucursales                           |"
                    print*, "|  2. Rutas                                |"
                    print*, "--------------------------------------------"
                    print*, "Ingrese una opcion: "
                    read(*,*) opsCargaDeARchivos
                    if(opsCargaDeARchivos.eq.1) then
                        print*, "---------------SUCURSALES---------------"
                        print*, "Ingrese la ruta de la rchivo a cargar: "
                        read(*,*) file_path

                        call json%initialize()
                        call json%load(filename = trim(file_path))
                        call json%info("", n_children=size) 

                        call json%get_core(jsonc)
                        call json%get("", listaPointer, found)
                        do i = 1, size
                            call jsonc%get_child(listaPointer, i, clientePointer, found=found)

                            call jsonc%get_child(clientePointer, "dpi", attributePointer, found)
                            call jsonc%get(attributePointer, dpi)

                            call jsonc%get_child(clientePointer, "nombre", attributePointer, found)
                            call jsonc%get(attributePointer, nombre)

                            call jsonc%get_child(clientePointer, "apellido", attributePointer, found)
                            call jsonc%get(attributePointer, apellido)

                            call jsonc%get_child(clientePointer, "genero", attributePointer, found)
                            call jsonc%get(attributePointer, genero)

                            call jsonc%get_child(clientePointer, "direccion", attributePointer, found)
                            call jsonc%get(attributePointer, direccion)

                            call jsonc%get_child(clientePointer, "telefono", attributePointer, found)
                            call jsonc%get(attributePointer, telefono)

                            print*, "DPI: ", trim(dpi), ", Nombre: ", trim(nombre), ", Apellido: ", trim(apellido), &
                                    ", Género: ", trim(genero), ", Dirección: ", trim(direccion), ", Teléfono: ", trim(telefono)

                        end do
                        call json%destroy()
                    else if(opsCargaDeARchivos.eq.2)then
                        print*, "------------------RUTAS-----------------"
                    else 
                        print*, "Opcion no valida"
                    end if
                
                case(2)
                    print*, "---------------SUCURSALES---------------"
                case(3)
                    print*, "----------------REPORTES----------------"
                case(4)
                    print*, "Cerrando sesion..."
                    exit         
                end select
     
    
            enddo
        end if
    
        !encontrado = llusuarios%verificarUsuario(name, DPI, password)  
        !if(encontrado) then
        if(.true.) then
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
            enddo
    
        else if ((name.eq."EDD1S2024") .and. (password.eq."ProyectoFase3")) then

        else
            print*, "Usuario o contraseña incorrecta/s"
        end if
    
    enddo
    end program menu 