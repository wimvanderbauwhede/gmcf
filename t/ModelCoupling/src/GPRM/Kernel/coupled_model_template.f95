subroutine main_routine(sys, tile, model) ! This replaces 'program main'
! Lines marked with ! gprm-coupler / ! end gprm-coupler are additions for coupling

! gprm-coupler
  use gprmCoupler

  integer(8) , intent(In) :: sys
  integer(8) , intent(In) :: tile
  integer , intent(In) :: model
! end gprm-coupler

! gprm-coupler
  integer :: sync_done, pre_post, destination, request_id
  integer, parameter :: GPRM_VAR_NAME_1=1,GPRM_VAR_NAME_2=2, DEST_1=1, DEST_2=2
  type(gprmPacket) :: packet
! end gprm-coupler

  integer :: t,t_start,t_stop,t_step
  real(kind=4), dimension(32) :: var_name_1, var_name_2
  t_start=0
  t_stop = 10
  t_step = 1

! gprm-coupler
  ! Init amongst other things gathers info about the time loops, maybe from a config file, need to work this out in detail:
  call gprmInitCoupler(sys,tile)
! end gprm-coupler

  ! (Do stuff) ...

  do t = t_start,t_stop,t_step


! gprm-coupler
  ! Sync all models by requesting all timesteps, block until all received, and sending your timestep to all who ask, until everyone has asked? 
  ! Is this possible? I think it is OK: 
  ! Start by sending N-1 requests; read from the FIFO, 
  ! block if there is nothing there. 
  ! You'll get requests and/or data. For every request, send data; keep going until you've sent data to all and received data from all. 
  ! I don't think this will deadlock.

  ! Sync will synchronise simulation time steps but also handle any pending requests
  !$GMC sync(t)
  sync_done=0
  do while(sync_done == 0)
    call gprmSync(model_id,t,sync_done)
    ! if sync is not done, means we had to break out to send data for some request
    if (sync_done == 0) then
      select case (gprmRequests(model_id)%data_id) ! <code for the variable var_name, GPRM-style>
        case (GPRM_VAR_NAME_1)
            call gprmSend1DFloatArray(var_name_1, size(var_name_1), gprmRequests(model)%destination,t)
      end select
    end if
  end do
  ! TODO: Now I should process all data packets received during sync

  ! Now the producer should block until it gets a request
!  call gprmWaitFor(REQDATA)

  ! The consumer should request any required pre- or post- computation values, block until you get them.
  call gprmRequestFloatArray(var_name_1, size(var_name_1), DEST_2, PRE, t,BLOCKING)
  ! I wonder if a better way is to first request them all, then block?
  ! Basically, it means:
  call gprmRequestFloatArray(GPRM_VAR_NAME_1, DEST_1, PRE, t)
  call gprmRequestFloatArray(GPRM_VAR_NAME_2, DEST_2, PRE, t)
  ! and then wait for them to arrive, but it means we need to tell the WaitFor call how many packets to expect:
  call gprmWaitFor(RESPDATA, 2)
  ! and then we read them
  do while (<data queue is not empty>)
    call gprmShiftPendingData(packet)
    ! read a packet
    select case (packet%data_id) ! <code for the variable var_name, GPRM-style>
        case (GPRM_VAR_NAME_1)
            call gprmRead1DFloatArray()
        ! ...
        end select
  end do

  call gprmRequestFloatArray(var_name_1, size(var_name_1), )

  end do
  ! Send any requested pre-computation values to the requester(s)
  call gprmGetNumberOfRequests(n_pending_requestss)
  do request_id=1,n_pending_requestss
   call gprmShiftPendingRequest(packet)
    pre_post = packet%pre_post
    destination = packet%destination
    if (pre_post == PRE) then
      select case (packet%data_id) ! <code for the variable var_name, GPRM-style>
        case (GPRM_VAR_NAME_1)
          ! Note that in principle some condition could apply, e.g. only send a value if the time is smaller/greater than t_i, or if the value is smaller/greater than some set value, or even, if we received
          call gprmSend1DFloatArray(var_name_1, size(var_name_1), destination,t)
!        ...
      end select
    end if
  end do

  ! Request any required pre- or post- computation values, block until you get them
  ! Doing this here is safer: all pending requests for PRE data will have been honoured.
  call gprmRequest1DFloatArray(var_name_2, size(var_name_2), DEST_1, POST,t)
! end gprm-coupler

  ! The actual computations follow here
  ! (Do stuff) ...

  ! Send any requested post-computation values to the requester(s)  
 
    call gprmSend1DFloatArray(var_name_2, size(var_name_2), DEST_1, t)
!    call gprmSendData(var1, var2, ..., varN,POST, t)

  end do

  ! (Do stuff) ...

  ! Maybe need to do some clean-up

end subroutine

