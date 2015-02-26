#define GMCF_DEBUG
module gmcfAPImodel1
    use gmcfAPI

    implicit none

    integer :: model1_id
    integer :: sync_done, has_packets, fifo_empty
    integer ::  t_sync, t_sync_prev, t_sync_step, t_inter, t_model1
    integer, parameter :: GMCF_VAR_NAME_1=1,GMCF_VAR_NAME_2=2, DEST_1=1, DEST_2=2
    type(gmcfPacket) :: packet

    save

contains

    subroutine gmcfInitModel1(sys,tile,m_id)
        integer(8), intent(In) :: sys
        integer(8), intent(In) :: tile
        integer, intent(In) :: m_id
        model1_id=m_id
        call gmcfInitCoupler(sys,tile, model1_id)
    end subroutine gmcfInitModel1

    subroutine gmcfSyncModel1(t, var_name_1, var_name_2)
        integer , intent(In) :: t
        real(kind=4), dimension(128), intent(In) :: var_name_1
        real(kind=4), dimension(128,128,128), intent(In) :: var_name_2
        t_model1 = t
        t_sync = t / t_sync_step
        t_inter = mod(t,t_sync_step)
        ! Sync will synchronise simulation time steps but also handle any pending requests
        !$GMC sync(t)
        if (t_sync == t_sync_prev+1) then

        sync_done=0
        do while(sync_done == 0)
            call gmcfSync(model1_id,t_sync,sync_done)
            print *, "FORTRAN MODEL1 AFTER gmcfSync()"
            ! if sync is not done, means we had to break out to send data for some request
            if (sync_done == 0) then
            print *, "FORTRAN MODEL1 SYNC NOT DONE!"
                select case (gmcfDataRequests(model1_id)%data_id) ! <code for the variable var_name, GMCF-style>
                    case (GMCF_VAR_NAME_1)
                        call gmcfSend1DFloatArray(model1_id,var_name_1, shape(var_name_1), GMCF_VAR_NAME_1,gmcfDataRequests(model1_id)%source,PRE,t_sync)
                    case (GMCF_VAR_NAME_2)
                        call gmcfSend3DFloatArray(model1_id,var_name_2, shape(var_name_2), GMCF_VAR_NAME_2, gmcfDataRequests(model1_id)%source,PRE,t_sync)
                end select
            end if
            print *, "FORTRAN MODEL1", model1_id," sync loop ",t,"..."
        end do
        print *, "FORTRAN MODEL1", model1_id," syncing DONE for time step ",t
        end if ! t_sync

    end subroutine gmcfSyncModel1

    subroutine gmcfPreModel1(var_name_1,var_name_1_prev,var_name_2,var_name_2_prev)
        real(kind=4), dimension(128), intent(InOut) :: var_name_1, var_name_1_prev
        real(kind=4), dimension(128,128,128), intent(InOut) :: var_name_2, var_name_2_prev
        if (t_sync == t_sync_prev+1) then
            ! So now we can do some work. Let's suppose model1 is the LES, and it requests data from model2, WRF.
            ! First overwrite the *prev vars with the current vars
             var_name_1_prev = var_name_1
             var_name_2_prev = var_name_2

            ! The data requested consists of 1-D and a 3-D array of floats
            print *, "FORTRAN MODEL1: sending DREQ 1 from",model1_id,'to',DEST_2
            call gmcfRequestData(model1_id,GMCF_VAR_NAME_1, size(var_name_1), DEST_2, PRE, t_sync) ! check if PRE/POST makes some sense here
            print *, "FORTRAN MODEL1: sending DREQ 2 from",model1_id,'to',DEST_2
            call gmcfRequestData(model1_id,GMCF_VAR_NAME_2, size(var_name_2), DEST_2, POST, t_sync)

            call gmcfWaitFor(model1_id,RESPDATA, DEST_2, 2)
                    print *, "FORTRAN MODEL1: got 2 DRESPs ..."

            ! and then we read them
            call gmcfHasPackets(model1_id,RESPDATA,has_packets)
            do while (has_packets==1)
                call gmcfShiftPending(model1_id,DEST_2,RESPDATA,packet,fifo_empty)
                ! read a packet
                select case (packet%data_id) ! <code for the variable var_name, GMCF-style>
                    case (GMCF_VAR_NAME_1)
                        call gmcfRead1DFloatArray(var_name_1,shape(var_name_1), packet)
                    case (GMCF_VAR_NAME_2)
                        call gmcfRead3DFloatArray(var_name_2,shape(var_name_2), packet)
                end select
                call gmcfHasPackets(model1_id,RESPDATA,has_packets)
            end do
            print *, "FORTRAN MODEL1: DONE reading DRESP into vars, ready to compute ..."
            t_sync_prev = t_sync
        end if ! of t_sync

    end subroutine gmcfPreModel1

!    subroutine gmcfPostModel1()
!    end subroutine gmcfPostModel1

end module gmcfAPImodel1
