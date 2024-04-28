module BTree
    implicit none

      ! Order 5
    integer, parameter :: MAXI = 4, MINI = 2

    type nodeptr
        type (BTreeNode), pointer :: ptr => null()
    end type nodeptr

    type BTreeNode
        integer(kind=8) :: val(0:MAXI+1)
        character(len=100) :: btNAME
        character(len=100) :: btPASS
        integer :: num = 0
        type(nodeptr) :: link(0:MAXI+1)
    end type BTreeNode
    
contains

subroutine insertB(val, btNAME, btPASS, root)
    integer(kind=8), intent(in) :: val
    character(len=*), intent(in) :: btNAME
    character(len=*), intent(in) :: btPASS
    integer(kind=8) :: i
    type(BTreeNode), pointer :: root
    type(BTreeNode), pointer :: child
    allocate(child)
    if (setValue(val, btNAME, btPASS, i, root, child)) then
            root => createNode(i, btNAME, btPASS, root, child)
    end if
end subroutine insertB

recursive function setValue(val, btNAME, btPASS, pval, node, child) result(res)
    integer(kind=8), intent(in) :: val
    integer(kind=8), intent(inout) :: pval
    character(len=*), intent(in) :: btNAME
    character(len=*), intent(in) :: btPASS
    type(BTreeNode), pointer, intent(inout) :: node
    type(BTreeNode), pointer, intent(inout) :: child
    type(BTreeNode), pointer :: newnode        
    integer :: pos
    logical :: res
    allocate(newnode)
    if (.not. associated(node)) then            
            pval = val
            child => null()
            res = .true.
            return
    end if
    if (val < node%val(1)) then
            pos = 0
    else
            pos = node%num
            do while (val < node%val(pos) .and. pos > 1) 
            pos = pos - 1
            end do
            if (val == node%val(pos)) then
                print *, "Duplicates are not permitted"
                res = .false.
                return
            end if
    end if
    if (setValue(val, btNAME, btPASS, pval, node%link(pos)%ptr, child)) then
            if (node%num < MAXI) then
                call insertNode(pval, btNAME, btPASS, pos, node, child)
            else
                call splitNode(pval, btNAME, btPASS, pval, pos, node, child, newnode)
                child => newnode
                res = .true.
            return
        end if
    end if
    res = .false.
end function setValue

subroutine insertNode(val, btNAME, btPASS, pos, node, child)
    integer(kind=8), intent(in) :: val
    integer, intent(in) :: pos
    character(len=*), intent(in) :: btNAME
    character(len=*), intent(in) :: btPASS
    type(BTreeNode), pointer, intent(inout) :: node
    type(BTreeNode), pointer, intent(in) :: child
    integer :: j
    j = node%num
    do while (j > pos)
            node%val(j + 1) = node%val(j)
            node%link(j + 1)%ptr => node%link(j)%ptr
            j = j - 1
    end do
    node%btNAME = btNAME
    node%btPASS = btPASS
    node%val(j + 1) = val
    node%link(j + 1)%ptr => child
    node%num = node%num + 1
end subroutine insertNode

subroutine splitNode(val, btNAME, btPASS, pval, pos, node, child, newnode)
    integer(kind=8), intent(in) :: val
    integer, intent(in) :: pos
    character(len=*), intent(in) :: btNAME
    character(len=*), intent(in) :: btPASS
    integer(kind=8), intent(inout) :: pval
    type(BTreeNode), pointer, intent(inout) :: node,  newnode
    type(BTreeNode), pointer, intent(in) ::  child
    integer :: median, i, j
    if (pos > MINI) then
            median = MINI + 1
    else
            median = MINI
    end if
    if (.not. associated(newnode)) then
        allocate(newnode)
    do i = 0, MAXI
                newnode%link(i)%ptr => null()
        enddo
    end if
    j = median + 1
    do while (j <= MAXI)
            newnode%val(j - median) = node%val(j)
            newnode%link(j - median)%ptr => node%link(j)%ptr
            j = j + 1
    end do
    node%num = median
    newnode%num = MAXI - median
    if (pos <= MINI) then
            call insertNode(val, btNAME, btPASS, pos, node, child)
    else
            call insertNode(val, btNAME, btPASS, pos - median, newnode, child)
    end if        
    pval = node%val(node%num)        
    newnode%link(0)%ptr => node%link(node%num)%ptr
    node%num = node%num - 1
end subroutine splitNode

function createNode(val, btNAME, btPASS, root, child) result(newNode)
    integer(kind=8), intent(in) :: val
    character(len=*), intent(in) :: btNAME
    character(len=*), intent(in) :: btPASS
    type(BTreeNode), pointer, intent(in) :: root
    type(BTreeNode), pointer, intent(in) :: child
    type(BTreeNode), pointer :: newNode
    integer :: i
    allocate(newNode)
    newNode%btNAME = btNAME
    newNode%btPASS = btPASS
    newNode%val(1) = val
    newNode%num = 1
    newNode%link(0)%ptr => root
    newNode%link(1)%ptr => child
    do i = 2, MAXI
            newNode%link(i)%ptr => null()
    end do
end function createNode

recursive subroutine traversal(inNode)
    type(BTreeNode), pointer, intent(in) :: inNode
    integer :: i
    if (associated(inNode)) then
            write (*, '(A)', advance='no') ' [ '
            i = 0
            do while (i < inNode%num)
                write (*,'(1I14)', advance='no') inNode%val(i+1)
                i = i + 1
            end do
            do i = 0, inNode%num
                call traversal(inNode%link(i)%ptr)    
            end do
            write (*, '(A)', advance='no') ' ] '
    end if
end subroutine traversal

recursive subroutine graphBTree(this,myNode)
        class(BTreeNode ), intent(in) :: this
        type(BTreeNode), pointer, intent(in) :: myNode
        type(BTreeNode), pointer :: current
        character(:),allocatable :: path
        !type(ordinal_user), pointer :: user
        integer :: i,file
        character(200) :: node1,casteo
        path= "treeUsers.dot"
        open(file, file=path, status="replace")
        write(file, *) 'digraph G {'
        
        if (.not. associated(myNode)) then
            write(file, *) '"empty" [label="Empty papers", shape=box];'
        else
            write (node1,'(I13)') myNode%val(1)
            do i = 1, myNode%num-1
                !user => myNode%val(i+1)
                !if (associated(user)) then
                    casteo=""
                    write (casteo,'(I13)') myNode%val(i+1)
                    print *, "casteo", trim(adjustl(casteo))
                    node1=trim(adjustl(node1))//","//trim(adjustl(casteo))
                !end if
            end do
            write(file, *) " ",'"Node', trim(adjustl(node1)), '" [label="', trim(adjustl(node1)),'"];'
            
            do i = 0, myNode%num
                if (associated(myNode%link(i)%ptr)) then
                    call graphrec(myNode%link(i)%ptr,node1,file)
                else
                    !write (,) "no hay mas nodos"
                end if 
            end do
        end if
        write(file, *) '}'
        close(file)
        call execute_command_line(trim("dot -Tpng treeUsers.dot -o treeUsers.png"))
        call system('Start treeUsers.png')
    end subroutine graphBTree

    recursive subroutine graphrec(myNode,node1,file)
        type(BTreeNode), pointer, intent(in) :: myNode
        type(BTreeNode), pointer :: current
        character(200), intent(inout) :: node1
        character(200) :: node2
        integer, intent(inout) :: file
        character(15) :: casteo
        integer :: i

        
        if (associated(myNode)) then
            write (node2,'(I13)') myNode%val(1)
            print *, "entro"
            do i = 1, myNode%num-1
                casteo=""
                write (casteo,'(I13)') myNode%val(i+1)
                node2=trim(adjustl(node2))//","//trim(adjustl(casteo))
            end do
            print *, trim(adjustl(node2))
            print *, "tamanio de node", myNode%num
            write(file, *) " ",'"Node', trim(adjustl(node2)), '" [label="', trim(adjustl(node2)),'"];'
            
            write(file, *) " ",'"Node', trim(adjustl(node1)), '" -> "Node', trim(adjustl(node2)), '";'
            do i = 0, myNode%num
                call graphrec(myNode%link(i)%ptr,node2,file)
            end do
        end if
    end subroutine graphrec

end module BTree