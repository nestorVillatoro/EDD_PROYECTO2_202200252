program main
    use matrix_m
    use json_module
    use iso_c_binding 
    implicit none 
    character(len=200) :: file_path

    logical:: found
    type(json_core) :: jsonc
    type(json_value), pointer :: listaPointer, attributePointer
    type(json_value), pointer :: idPointer, var1, var2
    integer size, i, fila, columna, id_capa, j, size2
    character(:), allocatable :: color
    character(len=500)::str_num_fila, str_num_columna, str_num_idcapa

    type(json_file) :: json


    type(matrix_t) :: mtx
  
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
                        
                        print *, "id_capa",  trim(str_num_idcapa)
                        print *, 'Fila: ', trim(str_num_fila), ' Columna: ', trim(str_num_columna), ' Color: ', color
                        call mtx%add(fila, columna, color) 
                        print*, ""

                    end do

                end do
                call json%destroy()

    
      

    !call mtx%print()
    call mtx%create_dot()
    
  end program main