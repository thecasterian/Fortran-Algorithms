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

    type bst
    private
        integer :: size
        type(bst_node), pointer :: root
        type(bst_node), pointer :: node_min, node_max
        
    contains

        procedure :: insert
        procedure :: search
        generic :: remove => remove_iterator, remove_integer

        procedure, private :: remove_iterator, remove_integer

    end type bst

    type bst_iterator
        type(bst_node), pointer :: ptr

    end type bst_iterator

    interface bst
        module procedure new_bst
    end interface bst

    type(bst_node), target :: NIL

    private :: bst_node
    private :: new_bst

contains

    function new_bst()
        implicit none
        type(bst) :: new_bst

        new_bst%size = 0
        new_bst%root => null()
        new_bst%node_min => null()
        new_bst%node_max => null()

    end function new_bst

    function insert(self, a)
        implicit none
        type(bst_iterator) :: insert
        class(bst), intent(inout) :: self
        integer, intent(in) :: a

    end function insert

    function search(self, a)
        implicit none
        type(bst_iterator) :: search
        class(bst), intent(inout) :: self
        integer, intent(in) :: a
    
    end function search

    function remove_iterator(self, it)
        implicit none
        type(bst_iterator) :: remove_iterator
        class(bst), intent(inout) :: self
        class(bst_iterator), intent(in) :: it

    end function remove_iterator

    function remove_integer(self, a)
        implicit none
        type(bst_iterator) :: remove_integer
        class(bst), intent(inout) :: self
        integer, intent(in) :: a

    end function remove_integer

end module bst_mod

program test
    use bst_mod

    implicit none
    type(bst) :: tree
    type(bst_iterator) :: it

    tree = bst()
    it = tree%insert(5)

end program test