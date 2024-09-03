! This process is the SLOW process

program main

   use, intrinsic :: iso_fortran_env
   use mpi_f08

   implicit none

   integer(kind=int32) :: ierror     ! Error code
   integer(kind=int32) :: rank       ! MPI rank of the current process
   integer(kind=int32) :: num_proc   ! Total processes in the current job
   integer(kind=int32) :: my_value   ! The value of the current process
   integer(kind=int32) :: ot_value   ! The value of the other process
   integer, dimension(1:1) :: send_array
   integer, dimension(1:1) :: recv_array

   ! Variables linked to the testing of the MPI request
   type(MPI_Request) :: request       ! Request for receiving asynchronously data
   logical           :: comm_complete ! Tells us if the communication is finished 
   type(MPI_Status)  :: status        ! Ignore this

   ! Initial value here, we could communicate but ... nevermind
   my_value = 1
   
   ! Init and getting comm-size and rank
   call MPI_Init(ierror)

   call MPI_Comm_size(MPI_COMM_WORLD, num_proc, ierror)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)


   ! In this program we just wait for the other program to feed us information
   do
      ! We make a blocking receive for the new value of the other program
      call MPI_Recv(recv_array, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, status, ierror)

      ! We do some processing on this stuff
      ot_value = recv_array(1)
      if (ot_value == -1) exit ! Leaving the loop
      call sleep(5) ! we wait 5 seconds to show the process is slower than the other
      my_value = my_value + ot_value

      write(6,*) 'Process 1 : Updated value, my_value = ', my_value
      
      ! Non-blocking send
      send_array(1) = my_value
      call MPI_ISend(send_array, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, request, ierror)

      ! And now we wait for it to be completed
      call MPI_Wait(request, status, ierror)
   end do
   
   call MPI_Barrier(MPI_COMM_WORLD, ierror)

   ! And we finish
   call MPI_Finalize(ierror)

end program
