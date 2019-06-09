module introsort_mod
    implicit none
    integer, parameter :: INSERTION_LIMIT = 16

    public :: introsort
    private :: introsort_helper, insertion_sort, heapsort
    private :: partition, max_heapify, build_heap, swap

contains

    subroutine introsort(a, n)
        implicit none
        integer, dimension(:), target, intent(inout) :: a
        integer, intent(in) :: n

        integer, dimension(:), pointer :: ptr_a
        integer :: tmp, max_level

        tmp = n / 2
        max_level = 0
        do while (tmp > 0)
            max_level = max_level + 2
            tmp = tmp / 2
        end do

        ptr_a => a
        call introsort_helper(ptr_a, n, max_level)

    end subroutine

    recursive subroutine introsort_helper(a, n, level)
        implicit none
        integer, dimension(:), pointer, intent(in) :: a
        integer, intent(in) :: n, level

        integer :: q
        integer, dimension(:), pointer :: a_right

        if (n <= INSERTION_LIMIT) then
            call insertion_sort(a, n)
            return
        end if
        if (level == 0) then
            call heapsort(a, n)
            return
        end if

        q = partition(a, n)
        call introsort_helper(a, q-1, level-1)
        a_right => a(q+1:)
        call introsort_helper(a_right, n-q, level-1)

    end subroutine

    subroutine insertion_sort(a, n)
        implicit none
        integer, dimension(:), pointer, intent(in) :: a
        integer, intent(in) :: n

        integer :: i, j, tmp

        do i = 2, n
            tmp = a(i)
            j = i - 1
            do while (j >= 1 .and. a(j) > tmp)
                a(j+1) = a(j)
                j = j - 1
            end do
            a(j+1) = tmp
        end do

    end subroutine

    subroutine heapsort(a, n)
        implicit none
        integer, dimension(:), pointer, intent(in) :: a
        integer, value :: n

        integer :: i

        call build_heap(a, n)
        do i = n, 2, -1
            call swap(a(1), a(i))
            n = n - 1
            call max_heapify(a, n, 1)
        end do

    end subroutine

    integer function partition(a, n)
        implicit none
        integer, dimension(:), pointer, intent(in) :: a
        integer, intent(in) :: n

        integer :: i, j, x, f, m, l

        ! median of three
        f = a(1)
        m = a(n/2)
        l = a(n)
        if ((f <= m .and. m <= l) .or. (l <= m .and. m <= f)) then
            call swap(a(n/2), a(n))
        end if
        if ((m <= f .and. f <= l) .or. (l <= f .and. f <= m)) then
            call swap(a(1), a(n))
        end if

        i = 0
        x = a(n)
        do j = 1, n-1
            if (a(j) <= x) then
                i = i + 1
                call swap(a(i), a(j))
            end if
        end do
        call swap(a(i+1), a(n))
        partition = i+1

    end function

    subroutine max_heapify(a, n, i)
        implicit none
        integer, dimension(:), pointer, intent(in) :: a
        integer, intent(in) :: n
        integer, value :: i

        integer :: l

        do while (2*i <= n)
            l = i
            if (a(2*i) > a(i)) l = 2*i
            if (2*i+1 <= n .and. a(2*i+1) > a(l)) l = 2*i+1
            if (l == i) exit
            call swap(a(i), a(l))
            i = l
        end do

    end subroutine

    subroutine build_heap(a, n)
        implicit none
        integer, dimension(:), pointer, intent(in) :: a
        integer, intent(in) :: n

        integer :: i

        do i = n/2, 1, -1
            call max_heapify(a, n, i)
        end do

    end subroutine

    subroutine swap(a, b)
        implicit none
        integer, intent(inout) :: a, b

        integer :: tmp

        tmp = a
        a = b
        b = tmp

    end subroutine

end module introsort_mod

program test
    use introsort_mod

    implicit none
    integer, dimension(1000000) :: a
    integer :: n, i

    read(*, *) n
    do i = 1, n
        read(*, *) a(i)
    end do

    call introsort(a, n)

    do i = 1, n
        write(*, '(i0)') a(i)
    end do

end program test
