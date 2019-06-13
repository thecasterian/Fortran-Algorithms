module introsort_mod
    implicit none
    integer, parameter :: INSERTION_LIMIT = 16

    public :: introsort
    private :: introsort_helper, insertion_sort, heapsort
    private :: partition, max_heapify

contains

    subroutine introsort(a, n, cmp)
    ! The main subroutine of introsort
    ! a : an array to be sorted
    ! n : length of a
    ! cmp : the function that recieves two elements of a
    !       and return whether the first is "less than"
    !       the second
        implicit none
        integer, dimension(:), target, intent(inout) :: a
        integer, intent(in) :: n
        logical, external :: cmp

        integer, dimension(:), pointer :: ptr_a
        integer :: tmp, max_level

        tmp = n / 2
        max_level = 0
        do while (tmp > 0)
            max_level = max_level + 2
            tmp = tmp / 2
        end do

        ptr_a => a
        call introsort_helper(ptr_a, n, max_level, cmp)

    end subroutine

    recursive subroutine introsort_helper(a, n, level, cmp)
        implicit none
        integer, dimension(:), pointer, intent(in) :: a
        integer, intent(in) :: n, level
        logical, external :: cmp

        integer :: q
        integer, dimension(:), pointer :: a_right

        if (n <= INSERTION_LIMIT) then
            call insertion_sort(a, n, cmp)
            return
        end if
        if (level == 0) then
            call heapsort(a, n, cmp)
            return
        end if

        q = partition(a, n, cmp)
        call introsort_helper(a, q-1, level-1, cmp)
        a_right => a(q+1:)
        call introsort_helper(a_right, n-q, level-1, cmp)

    end subroutine

    subroutine insertion_sort(a, n, cmp)
        implicit none
        integer, dimension(:), pointer, intent(in) :: a
        integer, intent(in) :: n
        logical, external :: cmp

        integer :: i, j
        integer :: tmp

        do i = 2, n
            tmp = a(i)
            j = i - 1
            do while (j >= 1)
                if (.not. cmp(tmp, a(j))) exit
                a(j+1) = a(j)
                j = j - 1
            end do
            a(j+1) = tmp
        end do

    end subroutine

    subroutine heapsort(a, n, cmp)
        implicit none
        integer, dimension(:), pointer, intent(in) :: a
        integer, value :: n
        logical, external :: cmp

        integer :: i
        integer :: tmp

        do i = n/2, 1, -1
            call max_heapify(a, n, i, cmp)
        end do
        do i = n, 2, -1
            tmp = a(1)
            a(1) = a(i)
            a(i) = tmp
            n = n - 1
            call max_heapify(a, n, 1, cmp)
        end do

    end subroutine

    integer function partition(a, n, cmp)
        implicit none
        integer, dimension(:), pointer, intent(in) :: a
        integer, intent(in) :: n
        logical, external :: cmp

        integer :: i, j
        integer :: x, f, m, l, tmp

        ! median of three
        f = a(1)
        m = a(n/2)
        l = a(n)
        if ((.not. cmp(m, f) .and. .not. cmp(l, m)) .or. &
            (.not. cmp(m, l) .and. .not. cmp(f, m))) then
            tmp = a(n/2)
            a(n/2) = a(n)
            a(n) = tmp
        end if
        if ((.not. cmp(f, m) .and. .not. cmp(l, f)) .or. &
            (.not. cmp(f, l) .and. .not. cmp(m, f))) then
            tmp = a(1)
            a(1) = a(n)
            a(n) = tmp
        end if

        i = 0
        x = a(n)
        do j = 1, n-1
            if (.not. cmp(x, a(j))) then
                i = i + 1
                tmp = a(i)
                a(i) = a(j)
                a(j) = tmp
            end if
        end do
        tmp = a(i+1)
        a(i+1) = a(n)
        a(n) = tmp
        partition = i+1

    end function

    subroutine max_heapify(a, n, i, cmp)
        implicit none
        integer, dimension(:), pointer, intent(in) :: a
        integer, intent(in) :: n
        integer, value :: i
        logical, external :: cmp

        integer :: l, tmp

        do while (2*i <= n)
            l = i
            if (cmp(a(i), a(2*i))) l = 2*i
            if (2*i+1 <= n) then
                if (cmp(a(l), a(2*i+1))) l = 2*i+1
            end if
            if (l == i) exit
            tmp = a(i)
            a(i) = a(l)
            a(l) = tmp
            i = l
        end do

    end subroutine

end module introsort_mod

program test
! A simple program to test the introsort function
    use introsort_mod

    implicit none
    integer, dimension(1000000) :: a
    integer :: n, i
    logical, external :: cmp

    read(*, *) n
    do i = 1, n
        read(*, *) a(i)
    end do

    call introsort(a, n, cmp)

    do i = 1, n
        write(*, '(i0)') a(i)
    end do

end program test

logical function cmp(a, b)
! Using this cmp function, the array is sorted in decreasing order
    implicit none
    integer, intent(in) :: a, b

    cmp = a > b

end function
