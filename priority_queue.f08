module priority_queue_mod
    implicit none
    ! Default v_size
    integer, parameter :: DEFAULT_SIZE = 16

    abstract interface
        logical function cmp_func(a, b)
            integer, intent(in) :: a, b
        end function
    end interface

    type priority_queue
    private
        integer :: n
        integer :: v_size
        integer, dimension(:), pointer :: v
        procedure(cmp_func), pointer, nopass :: cmp_ptr
    
    contains

        procedure :: push
        procedure :: top
        procedure :: pop
        procedure, private :: doubling
        final :: del_pq

    end type priority_queue

    interface priority_queue
        module procedure :: new_pq
    end interface priority_queue

    private :: DEFAULT_SIZE
    private :: cmp_default

contains

    type(priority_queue) function new_pq(cmp_ptr)
    ! Default constructor for priority queue
        implicit none
        procedure(cmp_func), pointer, optional :: cmp_ptr

        new_pq%n = 0
        new_pq%v_size = DEFAULT_SIZE
        allocate(new_pq%v(DEFAULT_SIZE))

        new_pq%cmp_ptr => cmp_default
        if (present(cmp_ptr)) new_pq%cmp_ptr => cmp_ptr

    end function new_pq

    subroutine push(self, a)
    ! Push an element into the priority queue
    ! a : an element to be pushed
        implicit none
        class(priority_queue), intent(inout) :: self
        integer, intent(in) :: a

        integer :: i
        integer :: tmp

        ! If v is full, extend it
        if (self%n == self%v_size) call self%doubling

        self%n = self%n + 1
        self%v(self%n) = a

        i = self%n
        do
            if (i == 1) exit
            if (.not. self%cmp_ptr(self%v(i/2), self%v(i))) exit
            tmp = self%v(i/2)
            self%v(i/2) = self%v(i)
            self%v(i) = tmp
            i = i / 2
        end do

    end subroutine push

    integer function top(self)
    ! Get the maximum element of priotiry queue
        implicit none
        class(priority_queue), intent(in) :: self

        if (self%n == 0) then
            top = 0
        else
            top = self%v(1)
        end if

    end function top

    subroutine pop(self)
    ! Pop the maximum element of priority queue
        implicit none
        class(priority_queue), intent(inout) :: self

        integer :: i, l
        integer :: tmp

        if (self%n == 0) return

        self%v(1) = self%v(self%n)
        self%n = self%n - 1

        i = 1
        do while (2*i <= self%n)
            l = i
            if (self%cmp_ptr(self%v(i), self%v(2*i))) l = 2*i
            if (2*i+1 <= self%n) then
                if (self%cmp_ptr(self%v(l), self%v(2*i+1))) l = 2*i+1
            end if
            if (l == i) exit
            tmp = self%v(i)
            self%v(i) = self%v(l)
            self%v(l) = tmp
            i = l
        end do

    end subroutine pop

    subroutine doubling(self)
        implicit none
        class(priority_queue), intent(inout) :: self
        integer, dimension(:), pointer :: new_v
        integer :: i

        allocate(new_v(self%v_size * 2))
        do i = 1, self%v_size
            new_v(i) = self%v(i)
        end do
        deallocate(self%v)
        self%v_size = self%v_size * 2
        self%v = new_v

    end subroutine doubling

    subroutine del_pq(self)
        implicit none
        type(priority_queue), intent(inout) :: self

        if (associated(self%v)) deallocate(self%v)

    end subroutine del_pq

    logical function cmp_default(a, b)
        implicit none
        integer, intent(in) :: a, b

        cmp_default = a < b

    end function cmp_default

end module priority_queue_mod

program test
! A simple program to test the priority queue
    use priority_queue_mod

    implicit none
    type(priority_queue) :: pq
    procedure(cmp_func), pointer :: cmp_ptr
    logical, external :: cmp

    integer, dimension(10) :: a = [5, 10, 3, 1, 4, 6, 8, 7, 9, 2]
    integer :: i

    cmp_ptr => cmp
    pq = priority_queue(cmp_ptr)
    
    do i = 1, 10
        call pq%push(a(i))
    end do

    do i = 1, 10
        write(*, *) pq%top()
        call pq%pop()
    end do

end program test

logical function cmp(a, b)
! Using this cmp function, the minimum element has maximum priority
    implicit none
    integer, intent(in) :: a, b

    cmp = a > b

end function cmp