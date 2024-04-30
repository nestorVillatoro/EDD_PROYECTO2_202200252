module modUser
    !use module_abbtree_layers
    implicit none

    type user
    character(:), allocatable :: name
    integer(kind=8), allocatable :: DPI
    character(:), allocatable :: password
    contains
    end type
    contains
end module  modUser