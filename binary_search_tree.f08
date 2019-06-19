module bst_mod
    implicit none

    abstract interface
        logical function cmp_func(a, b)
            integer, intent(in) :: a, b
        end function
    end interface

    type bst_node
        integer :: value
        type(bst_node), pointer :: left, right, parent
        integer :: order

    end type bst_node

    type(bst_node), target :: NIL

    type bst
        integer :: size
        type(bst_node), pointer :: root
        type(bst_node), pointer :: node_min, node_max
        
    contains

        procedure :: insert
        procedure :: search
        procedure :: remove

    end type bst

    interface bst
        module procedure new_bst
    end interface bst

    private :: bst_node

contains

    function new_bst()
        implicit none
        type(bst) :: new_bst

        new_bst%size = 0
        new_bst%root => null()
        new_bst%node_min => null()
        new_bst%node_max => null()

    end function new_bst

    function predecessor(self)
        implicit none
        class(bst_node), pointer :: predecessor
        class(bst_node), target, intent(inout) :: self

        type(bst_node), pointer :: cur

        cur => self
        if (associated(cur, NIL)) predecessor => NIL

    end function predecessor

    function successor(self)
        implicit none
        class(bst_node), pointer :: successor
        class(bst_node), intent(inout) :: self

        successor => self%left
        
    end function successor

end module bst_mod