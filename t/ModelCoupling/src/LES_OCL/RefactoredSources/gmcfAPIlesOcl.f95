!#define GMCF_VERBOSE
module gmcfAPIlesOcl
    use gmcfAPI

    implicit none

    integer :: les_id
    integer :: sync_done, has_packets, fifo_empty
    integer ::  t_sync, t_sync_prev, t_sync_step, t_inter, t_les
    integer, parameter :: GMCF_VAR_NAME_1=1,GMCF_VAR_NAME_2=2, DEST_1=1, DEST_2=2
    integer, parameter :: LES_WRF_IP=108,LES_WRF_JP=108,LES_WRF_KP=27
    integer, parameter :: LES_IP=100,LES_JP=100,LES_KP=26
    type(gmcfPacket) :: packet
    real(4), dimension(4,LES_WRF_JP,LES_WRF_KP) :: wind_profile_wrf, wind_profile_wrf_prev ! AD-HOC!

    save

contains

    subroutine gmcfInitLesOcl(sys,tile,m_id)
        integer(8), intent(In) :: sys
        integer(8), intent(In) :: tile
        integer, intent(In) :: m_id
#ifdef GMCF_VERBOSE
        print *, "GMCF API LES_OCL  gmcfInitLesOcl()"
#endif
        les_id=m_id
        t_les = 0
        t_sync_prev = -1
        t_sync = t_les
        t_sync_step = 20
        call gmcfInitCoupler(sys,tile, les_id)
    end subroutine gmcfInitLesOcl

    subroutine gmcfSyncLesOcl ! (var_name_1, var_name_2)
!        integer , intent(In) :: t
!        real(kind=4), dimension(128), intent(In) :: var_name_1
!        real(kind=4), dimension(128,128,128), intent(In) :: var_name_2

        t_sync = t_les / t_sync_step
        t_inter = mod(t_les,t_sync_step)
        t_les = t_les + 1
        ! Sync will synchronise simulation time steps but also handle any pending requests
        !$GMC sync(t)
#ifdef GMCF_VERBOSE
        print *, "GMCF API LES_OCL  BEFORE gmcfSync()",t_les, t_sync, t_sync_prev
#endif
        if (t_sync == t_sync_prev+1) then
#ifdef GMCF_VERBOSE
        print *, "GMCF API LES_OCL  BEFORE gmcfSync()"
#endif
        sync_done=0
        do while(sync_done == 0)
            call gmcfSync(les_id,t_sync,sync_done)
#ifdef GMCF_VERBOSE
            print *, "GMCF API LES_OCL  AFTER gmcfSync()"
#endif
            ! if sync is not done, means we had to break out to send data for some request
!            if (sync_done == 0) then
!            print *, "GMCF API LES_OCL  SYNC NOT DONE!"
!                select case (gmcfDataRequests(les_id)%data_id) ! <code for the variable var_name, GMCF-style>
!                    case (GMCF_VAR_NAME_1)
!                        call gmcfSend1DFloatArray(les_id,var_name_1, shape(var_name_1), GMCF_VAR_NAME_1,gmcfDataRequests(les_id)%source,PRE,t_sync)
!                    case (GMCF_VAR_NAME_2)
!                        call gmcfSend3DFloatArray(les_id,var_name_2, shape(var_name_2), GMCF_VAR_NAME_2, gmcfDataRequests(les_id)%source,PRE,t_sync)
!                end select
!            end if
#ifdef GMCF_VERBOSE
            print *, "GMCF API LES_OCL ", les_id," sync loop ",t_les,"..."
#endif
        end do
#ifdef GMCF_VERBOSE
        print *, "GMCF API LES_OCL ", les_id," syncing DONE for time step ",t_les
#endif
        end if ! t_sync
        ! WV: FIXME! DEBUG ONLY! This should be done in gmcfPreLesOcl even if it's the only thing that gets done there!
!        t_sync_prev = t_sync
    end subroutine gmcfSyncLesOcl

    subroutine gmcfPreLesOcl ! (var_name_1,var_name_1_prev,var_name_2,var_name_2_prev)
!        real(kind=4), dimension(128), intent(InOut) :: var_name_1, var_name_1_prev
!        real(kind=4), dimension(128,128,128), intent(InOut) :: var_name_2, var_name_2_prev
        if (t_sync == t_sync_prev+1) then
            ! So now we can do some work. Let's suppose les is the LES, and it requests data from model2, WRF.
            ! First overwrite the *prev vars with the current vars

             wind_profile_wrf_prev = wind_profile_wrf

            ! The data requested consists of 1-D and a 3-D array of floats
!            print *, "GMCF API LES_OCL : sending DREQ 1 from",les_id,'to',DEST_2
!            call gmcfRequestData(les_id,GMCF_VAR_NAME_1, size(var_name_1), DEST_2, PRE, t_sync) ! check if PRE/POST makes some sense here
#ifdef GMCF_VERBOSE
            print *, "GMCF API LES_OCL : sending DREQ 2 from",les_id,'to',DEST_2
#endif
            call gmcfRequestData(les_id,GMCF_VAR_NAME_2, size(wind_profile_wrf), DEST_2, POST, t_sync)

            call gmcfWaitFor(les_id,RESPDATA, DEST_2, 1)
#ifdef GMCF_VERBOSE
                    print *, "GMCF API LES_OCL : got 1 DRESP ..."
#endif

            ! and then we read them
            call gmcfHasPackets(les_id,RESPDATA,has_packets)
            do while (has_packets==1)
                call gmcfShiftPending(les_id,RESPDATA,packet,fifo_empty)
                ! read a packet
                select case (packet%data_id) ! <code for the variable var_name, GMCF-style>
!                    case (GMCF_VAR_NAME_1)
!                        call gmcfRead1DFloatArray(var_name_1,shape(var_name_1), packet)
                    case (GMCF_VAR_NAME_2)
                        call gmcfRead3DFloatArray(wind_profile_wrf,shape(wind_profile_wrf), packet)
                end select
                call gmcfHasPackets(les_id,RESPDATA,has_packets)
            end do
            print *, "GMCF API LES_OCL : SANITY: ",sum(wind_profile_wrf)
#ifdef GMCF_VERBOSE
            print *, "GMCF API LES_OCL : DONE reading DRESP into vars, ready to compute ..."
#endif
            t_sync_prev = t_sync
            print *, "LES time step: ",t_les, "SYNC time step: ",t_sync
!        else
!            print *, "GMCF API LES_OCL : skipping time step ",t_les, t_sync
        end if ! of t_sync

    end subroutine gmcfPreLesOcl

    subroutine gmcfInterpolateWindprofileLesOcl(wind_profile_interp)
    ! The values of LES_WRF_IP,LES_WRF_JP,LES_WRF_KP must be known at code generation time!
        real(4), dimension(LES_WRF_JP,LES_WRF_KP), intent(InOut) :: wind_profile_interp  ! AD-HOC!
        integer :: j,k,jj,kk
        do jj=3,LES_WRF_JP-4 ! LES_WRF_JP spans -3 .. 104 so we need j=4,103 if we ignore bounds, 3,105 if we include 1 point
            j = jj - 3
            k = kk + 1
            do kk=0,LES_WRF_KP-1 ! this is 1 .. 27 and there are 26 layers so we have no bound for -1 and 0 from WRF
            ! I suppose that would be soil anyway ...
                wind_profile_interp(j,k) = ( wind_profile_wrf_prev(1,jj,kk)*t_inter + wind_profile_wrf(1,jj,kk)*(t_sync_step-t_inter) ) / t_sync_step
            end do
        end do
    end subroutine gmcfInterpolateWindprofileLesOcl

!    subroutine gmcfPostLesOcl()
!    end subroutine gmcfPostLesOcl
    subroutine gmcfFinishedLesOcl
        call gmcfFinished(les_id)
    end subroutine gmcfFinishedLesOcl

end module gmcfAPIlesOcl
