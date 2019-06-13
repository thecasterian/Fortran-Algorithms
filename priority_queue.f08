module priority_queue_mod
    implicit none
    ! Default v_size
    integer, parameter :: DEFAULT_SIZE = 16

    type priority_queue
    private
        integer :: n
        integer :: v_size
        integer, dimension(:), pointer :: v
    
    contains

        procedure :: push
        procedure :: top
        procedure :: pop
        procedure, private :: doubling

    end type priority_queue

    interface priority_queue
        module procedure :: new_pq
    end interface priority_queue

    private :: DEFAULT_SIZE

contains

    type(priority_queue) function new_pq()
    ! Default constructor for priority queue
        implicit none

        new_pq%n = 0
        new_pq%v_size = DEFAULT_SIZE
        allocate(new_pq%v(DEFAULT_SIZE))

    end function new_pq

    subroutine push(self, a)
    ! Push a into the priority queue
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
            if (self%v(i/2) >= self%v(i)) exit
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
            if (self%v(2*i) > self%v(i)) l = 2*i
            if (2*i+1 <= self%n) then
                if (self%v(2*i+1) > self%v(l)) l = 2*i+1
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

end module priority_queue_mod

program test
! A simple program to test the priority queue
    use priority_queue_mod

    implicit none
    type(priority_queue) :: pq
    integer, dimension(10) :: a = [5, 10, 3, 1, 4, 6, 8, 7, 9, 2]
    integer :: i

    pq = priority_queue()
    
    do i = 1, 10
        call pq%push(a(i))
    end do

    do i = 1, 10
        write(*, *) pq%top()
        call pq%pop()
    end do

end program test