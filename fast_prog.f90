! This process is the FAST process, it is going to iterate a lot waiting for the new value of the slow process

program main

   use, intrinsic :: iso_fortran_env
   use mpi_f08

   implicit none

   integer(kind=int32) :: ierror     ! Error code
   integer(kind=int32) :: rank       ! MPI rank of the current process
   integer(kind=int32) :: num_proc   ! Total processes in the current job
   integer(kind=int32) :: i          ! Current iteration 
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

   ! We send the first value -> This is a blocking communication, we wait for the other process to acknowledge
   ! the reception.
   send_array(1) = my_value
   call MPI_Send(send_array, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, ierror)
   
   ! We ask for the updated value from the other process
   ! This is a non-blocking communication, the code will not wait and will keep doing stuff
   call MPI_IRecv(recv_array, 1, MPI_INTEGER, 1, 1, MPI_COMM_WORLD, request, ierror)

   ! Now we iterate
   do i=1, 100
      ! Testing if the reception is ready
      ! note that this could also be embedded in a if (iteration % test_frequency == 0) block
      ! to avoid testing too often
      ! The second argument of the method tells us if the data is ready to be read in the buffer
      call MPI_Test(request, comm_complete, status, ierror)
      if (comm_complete) then
         ! We copy the value received in the main variables
         ot_value = recv_array(1)
         
         ! And we send the updated value (still a blocking comm)
         send_array(1) = my_value
         call MPI_Send(send_array, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, ierror)

         write(6,*) 'Process 0 : Received new values, updating. ot_value = ', ot_value

         ! And waiting for a new reception
         call MPI_IRecv(recv_array, 1, MPI_INTEGER, 1, 1, MPI_COMM_WORLD, request, ierror)
      end if

      ! Updating value, doing some work
      write(6,*) 'Process 0 : new value : ', my_value, ' + ', ot_value, ' = ', my_value + ot_value
      my_value = my_value + ot_value
      call sleep(1)
   end do

   ! We send a special instruction to end the program to the other program
   send_array(1) = -1
   call MPI_Send(send_array, 1, MPI_INTEGER, 1, 2, MPI_COMM_WORLD, ierror)

   ! We wait for everyone to be there
   call MPI_Barrier(MPI_COMM_WORLD, ierror)

   ! And we finish
   call MPI_Finalize(ierror)

end program
