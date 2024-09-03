! This process is the FAST process, it is going to iterate a lot waiting for the new value of the slow process

program main

   use mpi_f08

   implicit none

   integer(kind=int32) :: ierror     ! Error code
   integer(kind=int32) :: rank       ! MPI rank of the current process
   integer(kind=int32) :: num_proc   ! Total processes in the current job
   integer(kind=int32) :: iteration  ! Current iteration 
   integer(kind=int32) :: my_value   ! The value of the current process
   integer(kind=int32) :: ot_value   ! The value of the other process
   integer, dimension(1:1) :: send_array
   integer, dimension(1:1) :: recv_array

   ! Variables linked to the testing of the MPI request
   type(MPI_Request) :: request       ! Request for receiving asynchronously data
   logical           :: comm_complete ! Tells us if the communication is finished 
   type(MPI_Status)  :: status        ! Ignore this

   
   ! Init and getting comm-size and rank
   call MPI_Init(ierror)

   call MPI_Comm_size(MPI_COMM_WORLD, num_proc, ierror)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)


   ! First step we initialize everything
   my_value = 1
   ot_value = 1
   should_communicate = .true.

   ! We send the first value -> This is a blocking communication, we wait for the other process to acknowledge
   ! the reception.
   send_array(1) = my_value
   call MPI_Send(send_array, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, ierror)
   
   ! We ask for the updated value from the other process
   ! This is a non-blocking communication, the code will not wait and will keep doing stuff
   call MPI_Recv(recv_array, 1, MPI_INTEGER, 1, 1, MPI_COMM_WORLD, recv_request, ierror)

   ! Now we iterate
   do i=1, 1000
      ! Testing if the reception is ready
      ! note that this could also be embedded in a if (iteration % test_frequency == 0) block
      ! to avoid testing too often
      ! The second argument of the method tells us if the data is ready to be read in the buffer
      call MPI_Test(request, comm_complete, status, ierror)
      if (comm_complete) then
         ! And we send the updated value
         send_array(1) = my_value
         call MPI_Send(send_array, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, ierror)

         write(6,*) 'Process 0 : Received new values, updating. ot_value = ', ot_value
      end if

      ! Updating value, doing some work
      my_value = my_value + ot_value
      write(6,*) 'Process 0 : my_value = ', my_value
      call sleep(1)
   end do
   

end program
