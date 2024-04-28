program main
    use json_module 
    implicit none
    logical:: foundc
    type(json_core) :: jsoncc
    type(json_value), pointer :: listaPointerC, clientePointerC, attributePointerC
    integer sizeC, iC 
    character(:), allocatable :: dpiC, nombre_clienteC, passwordC
    character(len=200) :: file_path
    type(json_file) :: jsonC

    print*, "Ingrese la ruta dela rchivo a cargar: "
    read(*,*) file_path

    call jsonC%initialize()
    call jsonC%load(filename = trim(file_path))
    call jsonC%info("", n_children=sizeC) 

    call jsonC%get_core(jsoncc)
    call jsonC%get("", listaPointerC, foundc)
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

        print*, "DPI: ", trim(dpiC), " Nombre Cliente: ", trim(nombre_clienteC), " Password: ", trim(passwordC)
    end do
    call jsonC%destroy()
end program main
