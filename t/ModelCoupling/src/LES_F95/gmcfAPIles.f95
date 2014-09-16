#define GMCF_DEBUG
module gmcfAPIles
    use gmcfAPI

    implicit none

    integer :: les_id
    integer :: sync_done, has_packets, fifo_empty
    integer ::  t_sync, t_sync_prev, t_sync_step, t_inter, t_les
    integer, parameter :: GMCF_VAR_NAME_1=1,GMCF_VAR_NAME_2=2, DEST_1=1, DEST_2=2
    type(gmcfPacket) :: packet

    save

contains

    subroutine gmcfInitLes(sys,tile,m_id)
        integer(8), intent(In) :: sys
        integer(8), intent(In) :: tile
        integer, intent(In) :: m_id
        print *, "FORTRAN LES gmcfInitLes()"
        les_id=m_id
        t_les = 0
        t_sync_prev = -1
        t_sync = t_les
        t_sync_step = 20
        call gmcfInitCoupler(sys,tile, les_id)
    end subroutine gmcfInitLes

    subroutine gmcfSyncLes ! (var_name_1, var_name_2)
!        integer , intent(In) :: t
!        real(kind=4), dimension(128), intent(In) :: var_name_1
!        real(kind=4), dimension(128,128,128), intent(In) :: var_name_2

        t_sync = t_les / t_sync_step
        t_inter = mod(t_les,t_sync_step)
        t_les = t_les + 1
        ! Sync will synchronise simulation time steps but also handle any pending requests
        !$GMC sync(t)
        print *, "FORTRAN LES BEFORE gmcfSync()",t_les, t_sync, t_sync_prev
        if (t_sync == t_sync_prev+1) then
        print *, "FORTRAN LES BEFORE gmcfSync()"
        sync_done=0
        do while(sync_done == 0)
            call gmcfSync(les_id,t_sync,sync_done)
            print *, "FORTRAN LES AFTER gmcfSync()"
            ! if sync is not done, means we had to break out to send data for some request
            if (sync_done == 0) then
            print *, "FORTRAN LES SYNC NOT DONE!"
!                select case (gmcfDataRequests(les_id)%data_id) ! <code for the variable var_name, GMCF-style>
!                    case (GMCF_VAR_NAME_1)
!                        call gmcfSend1DFloatArray(les_id,var_name_1, shape(var_name_1), GMCF_VAR_NAME_1,gmcfDataRequests(les_id)%source,PRE,t_sync)
!                    case (GMCF_VAR_NAME_2)
!                        call gmcfSend3DFloatArray(les_id,var_name_2, shape(var_name_2), GMCF_VAR_NAME_2, gmcfDataRequests(les_id)%source,PRE,t_sync)
!                end select
            end if
            print *, "FORTRAN LES", les_id," sync loop ",t_les,"..."
        end do
        print *, "FORTRAN LES", les_id," syncing DONE for time step ",t_les
        end if ! t_sync
        ! WV: FIXME! DEBUG ONLY! This should be done in gmcfPreLes even if it's the only thing that gets done there!
        t_sync_prev = t_sync
    end subroutine gmcfSyncLes
#if 0
    subroutine gmcfPreLes(var_name_1,var_name_1_prev,var_name_2,var_name_2_prev)
        real(kind=4), dimension(128), intent(InOut) :: var_name_1, var_name_1_prev
        real(kind=4), dimension(128,128,128), intent(InOut) :: var_name_2, var_name_2_prev
        if (t_sync == t_sync_prev+1) then
            ! So now we can do some work. Let's suppose les is the LES, and it requests data from model2, WRF.
            ! First overwrite the *prev vars with the current vars
             var_name_1_prev = var_name_1
             var_name_2_prev = var_name_2

            ! The data requested consists of 1-D and a 3-D array of floats
            print *, "FORTRAN LES: sending DREQ 1 from",les_id,'to',DEST_2
            call gmcfRequestData(les_id,GMCF_VAR_NAME_1, size(var_name_1), DEST_2, PRE, t_sync) ! check if PRE/POST makes some sense here
            print *, "FORTRAN LES: sending DREQ 2 from",les_id,'to',DEST_2
            call gmcfRequestData(les_id,GMCF_VAR_NAME_2, size(var_name_2), DEST_2, POST, t_sync)

            call gmcfWaitFor(les_id,RESPDATA, DEST_2, 2)
                    print *, "FORTRAN LES: got 2 DRESPs ..."

            ! and then we read them
            call gmcfHasPackets(les_id,RESPDATA,has_packets)
            do while (has_packets==1)
                call gmcfShiftPending(les_id,RESPDATA,packet,fifo_empty)
                ! read a packet
                select case (packet%data_id) ! <code for the variable var_name, GMCF-style>
                    case (GMCF_VAR_NAME_1)
                        call gmcfRead1DFloatArray(var_name_1,shape(var_name_1), packet)
                    case (GMCF_VAR_NAME_2)
                        call gmcfRead3DFloatArray(var_name_2,shape(var_name_2), packet)
                end select
                call gmcfHasPackets(les_id,RESPDATA,has_packets)
            end do
            print *, "FORTRAN LES: DONE reading DRESP into vars, ready to compute ..."
            t_sync_prev = t_sync
        end if ! of t_sync

    end subroutine gmcfPreLes
#endif
!    subroutine gmcfPostLes()
!    end subroutine gmcfPostLes

end module gmcfAPIles
