program b
    use json_module  
    implicit none 
    logical :: found
    type(json_core) :: jsonc
    type(json_value), pointer :: listaPointer, clientePointer, attributePointer
    integer :: size, i
    character(:), allocatable :: nombre, apellido, genero, direccion, telefono, dpi
    type(json_file) :: json


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

end program b
