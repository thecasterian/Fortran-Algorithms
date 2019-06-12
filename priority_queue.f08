module priority_queue_mod
    implicit none

    type priority_queue
        
    end type priority_queue

end module priority_queue_mod

program test
    use priority_queue_mod

    implicit none
    integer :: n

    read(*, *) n
    write(*, *) n

end program test