program main
    use json_module 
    implicit none
    logical:: found
    type(json_core) :: jsonc
    type(json_value), pointer :: listaPointer, clientePointer, attributePointer
    integer size, i 
    character(:), allocatable :: dpi, nombre_cliente, password
    character(len=200) :: file_path
    type(json_file) :: json

    print*, "Ingrese la ruta dela rchivo a cargar: "
    read(*,*) file_path

    call json%initialize()
    call json%load(filename = trim(file_path))
    call json%info("", n_children=size) 

    call json%get_core(jsonc)
    call json%get("", listaPointer, found)
    do i = 1, size 
        call jsonc%get_child(listaPointer, i, clientePointer, found=found)

        call jsonc%get_child(clientePointer, "dpi", attributePointer, found)
        if(found) then
            call jsonc%get(attributePointer, dpi)
        end if

        call jsonc%get_child(clientePointer, "nombre_cliente", attributePointer, found)
        if(found) then
            call jsonc%get(attributePointer, nombre_cliente)
        end if

        call jsonc%get_child(clientePointer, "password", attributePointer, found)
        if(found) then
            call jsonc%get(attributePointer, password)
        end if

        print*, "DPI: ", trim(dpi), " Nombre Cliente: ", trim(nombre_cliente), " Password: ", trim(password)
    end do
    call json%destroy()
end program main
