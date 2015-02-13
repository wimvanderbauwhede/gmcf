! While this module is specific to the coupled system, it is generic with respect to the individual models.
! Needs refining, esp. the way we block on calls
! Also, maybe I want to use pragmas to insert the coupling code
! Also, we don't need model_id and per-model arrays because if my reasoning is correct, the module memory space will be per-thread

! To try this out I need to create a small C++ program that spawns two threads and in each thread we access the variables from a module

! According to http://stackoverflow.com/questions/23743716/simultaneous-calls-of-a-method-in-a-fortran-dll-with-module-variables-in-c-shar
! The variables in the module will be shared after all, so I need to keep the current approach.
! Seems to me that we could actually use this to make sharing easy ...
!#define GMCF_DEBUG
module gmcfAPI
    use gmcfConfiguration

    implicit none

    integer(8) :: sba_sys ! There is only one System
    integer(8), dimension(NMODELS) :: sba_tile ! I think this needs to be an array

    integer, parameter :: PRE = 0,POST = 1, BLOCKING=1, NON_BLOCKING=0
    integer, parameter :: REQDATA=6, REQTIME=7, RESPDATA=8, RESPTIME=9, FIN=10, ACKDATA=11
    integer(8), parameter :: ONE = 1
!    integer, parameter :: NMODELS = 2 ! FIXME: use MACRO. In GPRM NSERVICES is a const UINT but I can easily change that.

    type gmcfPacket
        integer :: source
        integer :: destination
        integer :: type
        integer :: timestamp
        integer :: pre_post
        integer :: data_id
        integer(8) :: data_sz
        integer(8) :: data_ptr
    end type gmcfPacket

    type(gmcfPacket), dimension(NMODELS) :: gmcfDataRequests
    !        type(gmcfPacket), dimension(NMODELS) :: gmcfData
    integer, dimension(NMODELS,NMODELS) :: sync_status
    ! Need to define status. I can think of init, working, finished, error
    integer, dimension(NMODELS) :: gmcfStatus = 0
    integer, dimension(NMODELS) :: sync_counter = NMODELS-1

    save

contains

    subroutine gmcfInitCoupler(sysptr,tileptr,model_id)
        integer(8), intent(In) :: sysptr
        integer(8), intent(In) :: tileptr
        integer, intent(In) :: model_id
        sba_sys= sysptr
        sba_tile(model_id)  = tileptr
    end subroutine gmcfInitCoupler

    subroutine gmcfSync(model_id,time,sync_done)
        integer, intent(In) :: time, model_id
        integer, intent(InOut) :: sync_done
        ! Start by sending N-1 requests for the simulation time (all except yourself)
        integer :: dest,requester, recvfrom, recvtime, fifo_empty, sync_irq

        type(gmcfPacket) :: packet
        sync_counter(model_id) = NMODELS-1
        ! Send requests for timestamps to all others
        do dest=1,NMODELS
            sync_status(model_id,dest)=0
            if (dest /= model_id) then
                call gmcfSendTimeRequest(model_id,dest,time)
            end if
        end do
        ! Read from the FIFO, block if there is nothing there.
        sync_done = 0
        sync_irq = 0
        do while (sync_done == 0 .and. sync_irq == 0)
            call gmcfReadFromFifo(model_id,packet,fifo_empty) ! this is a blocking call
            ! You'll get requests and/or data.
            select case (packet%type)
                ! For every request, send data;
                case (REQDATA)
                    ! If it's a request for data, send the current values
                    ! send requested data. So I need access to all data in here!
                    ! As long as we don't send the data, we will not receive the time stamp from that model
                    ! So we must send immediately
                    ! I guess we could break out of the sync,
                    ! send the data and then go back to the sync
                    ! If we store the control variables in an array in the module, that could work
                    gmcfDataRequests(model_id)=packet
                    sync_irq = 1
                case (REQTIME)
#ifdef GMCF_DEBUG
                    print *, "FORTRAN API SYNC: GOT A REQTIME PACKET ",   packet%type ," from ", packet%source, " to", packet%destination, ' timestamp=',packet%timestamp
#endif
! I received a request for timestamp, send a RESPTIME to the sender
                    requester = packet%source
                    call gmcfSendTimestamp(model_id,requester,time)
                case (RESPTIME)
#ifdef GMCF_DEBUG
                    print *, "FORTRAN API SYNC: GOT A RESPTIME PACKET ",   packet%type ," from ", packet%source, " to", packet%destination, ' timestamp=',packet%timestamp
#endif
                    ! If the data is a timestamp, check the sender address and value
                    ! receive time, check
                    recvfrom = packet%source
                    recvtime = packet%timestamp
                    if (recvtime == time .and. sync_status(model_id,recvfrom)==0) then
                        sync_status(model_id,recvfrom)=1
                        sync_counter(model_id) = sync_counter(model_id) - 1
                        if ( sync_counter(model_id) == 0) then
                            sync_done = 1
                        end if
                    end if
                case (RESPDATA)
#ifdef GMCF_DEBUG
                    print *, "FORTRAN API SYNC: GOT A RESPDATA PACKET ",   packet%type ," from ", packet%source, " to", packet%destination, ' timestamp=',packet%timestamp
#endif
                    ! If it's data, presumably this means there is some non-blocking request for it
                    ! receive data. Again, requires access to all data ...
                    ! But we can actually defer this until after the sync,
                    ! Just put these data packets in the data fifo
                    call gmcfPushPending(model_id, packet)
                    ! Then update the current values after the sync
                case (FIN)
#ifdef GMCF_DEBUG
                    print *, "FORTRAN API SYNC: GOT A FIN PACKET:",   packet%type ," from ", packet%source, " to", packet%destination
#endif
                    ! We need to check here if all others are done, or at least all models communicating with this one.
                    if ( sum( gmcfConnectivityMatrix(model_id, 1:NMODELS ) ) == NMODELS-1 ) then
                        sync_done = 1
                    end if
                case default
                    print *, "FORTRAN API SYNC: GOT AN UNEXPECTED PACKET:",   packet%type ," from ", packet%source, " to", packet%destination
            end select
        end do

        ! keep going until you've sent data to all and received data from all.

    end subroutine gmcfSync

    ! This is a blocking call on the main FIFO, no muxing
    subroutine gmcfReadFromFifo(model_id,packet,fifo_empty)
        integer, intent(In) :: model_id
        type(gmcfPacket), intent(Out) :: packet
        integer, intent(Out) :: fifo_empty
        integer :: source, destination, packet_type, timestamp, pre_post, data_id
        integer(8) :: data_sz, data_ptr

        ! So here we need to call into GPRM
        call gmcfreadfromfifoc(sba_sys, sba_tile(model_id), source, destination, packet_type, timestamp, pre_post, data_id, data_sz, data_ptr, fifo_empty)
#ifdef GMCF_DEBUG
        print *,'FORTRAN API: AFTER gmcfreadfromfifoc'
#endif
        packet%type=packet_type
        packet%source=source
        packet%destination=destination
        packet%timestamp=timestamp
        packet%pre_post=pre_post
        packet%data_id=data_id
        packet%data_sz=data_sz
        packet%data_ptr=data_ptr

    end subroutine gmcfReadFromFifo

    subroutine gmcfSendTimeRequest(model_id,dest, timestamp)
        integer, intent(In) :: model_id,dest, timestamp
        call gmcfsendpacketc(sba_sys, sba_tile(model_id), model_id, dest, REQTIME, 0, PRE, timestamp,ONE,0)
    end subroutine gmcfSendTimeRequest

    subroutine gmcfSendTimestamp(model_id,requester, timestamp)
        integer, intent(In) :: model_id,requester, timestamp
        call gmcfsendpacketc(sba_sys, sba_tile(model_id), model_id, requester, RESPTIME, 0, PRE, timestamp,ONE, 0)
    end subroutine gmcfSendTimestamp

    subroutine gmcfWaitFor(model_id,packet_type,sender, npackets)
        integer, intent(In) :: model_id,packet_type,sender, npackets
        ! What we do here is listen for packets and demux them, probably using a C function
        ! We do this until there is a packet in the fifo for packet_type
#ifdef GMCF_DEBUG
        print *,"FORTRAN API: gmcfWaitFor(",packet_type,npackets,")"
#endif
        call gmcfwaitforpacketsc(sba_sys, sba_tile(model_id), packet_type,sender, npackets)
        ! After this, packets will be in their respective queues, with the packet_type queue guaranteed containing at least one packet
    end subroutine
    ! This checks if there are any packets of a given type from a given source
    subroutine gmcfHasPacketsFrom(model_id, src_model_id, packet_type, has_packets)
        integer, intent(In) :: model_id, src_model_id, packet_type
        integer, intent(Out) :: has_packets
        call gmcfcheckfifoc(sba_sys, sba_tile(model_id),src_model_id, packet_type,has_packets);
    end subroutine gmcfHasPackets
! This checks if there are any packets of a given type from any source
    subroutine gmcfHasPackets(model_id, packet_type, has_packets)
        integer, intent(In) :: model_id, src_model_id, packet_type
        integer, intent(Out) :: has_packets
        call gmcfcheckfifosc(sba_sys, sba_tile(model_id), packet_type,has_packets);
    end subroutine gmcfHasPackets


    subroutine gmcfShiftPending(model_id, src_model_id, packet_type,packet,fifo_empty)
        integer, intent(In) :: model_id, src_model_id, packet_type
        type(gmcfPacket), intent(Out) :: packet
        integer, intent(Out) :: fifo_empty
        integer :: source, destination, timestamp, pre_post, data_id
        integer(8) :: data_sz, data_ptr

        call gmcfshiftpendingc(sba_sys, sba_tile(model_id), src_model_id, packet_type,source, destination, timestamp, pre_post, data_id, data_sz, data_ptr, fifo_empty)
        packet%type=packet_type
        packet%source=source
        packet%destination=destination
        packet%timestamp=timestamp
        packet%pre_post=pre_post
        packet%data_id=data_id
        packet%data_sz=data_sz
        packet%data_ptr=data_ptr
    end subroutine gmcfShiftPending

    subroutine gmcfPushPending(model_id, packet)
        integer, intent(In) :: model_id
        type(gmcfPacket), intent(In) :: packet
        call gmcfpushpendingc(sba_sys, sba_tile(model_id), packet%type,packet%source, packet%destination, packet%timestamp, packet%pre_post, packet%data_id, packet%data_sz, packet%data_ptr)
    end subroutine gmcfPushPending

    subroutine gmcfRequestData(model_id,data_id, data_sz, dest, pre_post, timestamp)
        integer, intent(In) :: model_id, data_id, dest, pre_post, timestamp
        integer, intent(In) :: data_sz
        integer(8) :: data_sz_64b
        data_sz_64b = data_sz
        call gmcfsendpacketc(sba_sys, sba_tile(model_id), model_id, dest, REQDATA, data_id, pre_post, timestamp,data_sz_64b,0)
    end subroutine gmcfRequestData
    ! same for other types and sizes, as in OpenCL wrapper

    subroutine gmcfSend1DFloatArray(model_id, array, sz, data_id, destination, pre_post,time)
        integer, dimension(1), intent(In):: sz
        integer,intent(In) :: data_id
        real(kind=4),dimension(sz(1)), intent(In) :: array

        integer, intent(In) :: model_id, destination,pre_post,time
        integer(8) :: sz1d
        sz1d = size(array)
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfSend1DFloatArray: SANITY:",sum(array)
#endif
        call gmcfsendarrayc(sba_sys, sba_tile(model_id), data_id, model_id, destination, pre_post,  time, sz1d,array)
    end subroutine gmcfSend1DFloatArray

    subroutine gmcfSend2DFloatArray(model_id, array, sz, data_id, destination, pre_post,time)
        integer, dimension(2), intent(In):: sz
        integer,intent(In) :: data_id
        real(kind=4),dimension(sz(1), sz(2)), intent(In) :: array
        integer, intent(In) :: model_id, destination,pre_post,time
        integer(8) :: sz1d
        sz1d = size(array)
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfSend2DFloatArray: SANITY:",sum(array)
#endif
        call gmcfsendarrayc(sba_sys, sba_tile(model_id), data_id, model_id, destination, pre_post,  time, sz1d,array)
    end subroutine gmcfSend2DFloatArray

    subroutine gmcfSend3DFloatArray(model_id, array, sz, data_id, destination, pre_post,time)
        integer, dimension(3), intent(In):: sz
        integer,intent(In) :: data_id
        real(kind=4),dimension(sz(1), sz(2), sz(3)), intent(In) :: array
        integer, intent(In) :: model_id, destination,pre_post,time
        integer(8) :: sz1d
        sz1d = size(array)
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfSend3DFloatArray: SANITY:",sum(array)
#endif
        call gmcfsendarrayc(sba_sys, sba_tile(model_id), data_id, model_id, destination, pre_post,  time, sz1d,array)
    end subroutine gmcfSend3DFloatArray

    subroutine gmcfSend3DIntegerArray(model_id, array, sz, data_id, destination, pre_post,time)
        integer, dimension(3), intent(In):: sz
        integer,intent(In) :: data_id
        integer,dimension(sz(1), sz(2), sz(3)), intent(In) :: array
        integer, intent(In) :: model_id, destination,pre_post,time
        integer(8) :: sz1d
        sz1d = size(array)
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfSend3DFloatArray: SANITY:",sum(array)
#endif
        call gmcfsendarrayc(sba_sys, sba_tile(model_id), data_id, model_id, destination, pre_post,  time, sz1d,array)
    end subroutine gmcfSend3DIntegerArray

   subroutine gmcfRead1DFloatArray(array, sz, packet) ! This will have to be a wrapper around a C function
        ! read data packets from the data fifo and update the variables
        ! We do the same as in OpenCL
        ! Then we get the pointer from the packet and cast it
        type(gmcfPacket) :: packet
        integer(8):: ptr, ptr_sz
        integer(8) :: sz1d
        integer, dimension(1):: sz
       real(kind=4), dimension(sz(1)) :: array
       real(kind=4), dimension(size(array)):: array1d
!        real, pointer, dimension(:) :: array
!        real, pointer, dimension(:) :: array1d

        sz1d = size(array)
!        allocate( array1d(sz1d) )
!        allocate( array(sz1d) )
        ptr = packet%data_ptr
        ptr_sz = packet%data_sz
        if (ptr_sz /= sz1d) then
            print *, 'WARNING: size of read array',ptr_sz,'does not match size of target', sz1d
        end if
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfRead1DFloatArray: PTR:",ptr
#endif
        call gmcffloatarrayfromptrc(ptr,array1d,sz1d)
        call gmcfsendpacketc(sba_sys, sba_tile(packet%destination), packet%destination, packet%source, ACKDATA, packet%data_id, PRE, -1 ,ONE, 0)
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfRead1DFloatArray: SANITY:" , array1d(1)
        print *, "FORTRAN API gmcfRead1DFloatArray: SANITY:" , sum(array1d)
#endif
        array = reshape(array1d,shape(array))
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfRead1DFloatArray: SANITY:",sum(array)
#endif
    end subroutine

    subroutine gmcfRead2DFloatArray(array, sz, packet)
        type(gmcfPacket) :: packet
        integer(8):: ptr, ptr_sz
        integer(8) :: sz1d
        integer, dimension(2):: sz
        real(kind=4),dimension(sz(1), sz(2)) :: array
        real(kind=4), dimension(size(array)):: array1d
!
!        real, pointer, dimension(:,:,:) :: array
!        real, pointer, dimension(:) :: array1d
        sz1d = size(array1d) ! product(sz)
        ptr = packet%data_ptr
        ptr_sz = packet%data_sz
        if (ptr_sz /= sz1d) then
            print *, 'WARNING: size of read array',ptr_sz,'does not match size of target', sz1d
        end if
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfRead3DFloatArray: PTR:",ptr
#endif
        call gmcffloatarrayfromptrc(ptr,array1d,sz1d) ! This ugly function will simply cast the ptr and return it as the array1d
        call gmcfsendpacketc(sba_sys, sba_tile(packet%destination), packet%destination, packet%source, ACKDATA, packet%data_id, PRE, -1 ,ONE, 0)
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfRead2DFloatArray: SANITY:",array1d(1)
        print *, "FORTRAN API gmcfRead2DFloatArray: SANITY:",sum(array1d)
#endif
        array = reshape(array1d,shape(array))
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfRead2DFloatArray: SANITY:",sum(array)
#endif
    end subroutine gmcfRead2DFloatArray

    subroutine gmcfRead3DFloatArray(array, sz, packet)
        type(gmcfPacket) :: packet
        integer(8):: ptr, ptr_sz
        integer(8) :: sz1d
        integer, dimension(3):: sz
        real(kind=4),dimension(sz(1), sz(2), sz(3)) :: array
        real(kind=4), dimension(size(array)):: array1d
!
!        real, pointer, dimension(:,:,:) :: array
!        real, pointer, dimension(:) :: array1d
        sz1d = size(array1d) ! product(sz)
        ptr = packet%data_ptr
        ptr_sz = packet%data_sz
        if (ptr_sz /= sz1d) then
            print *, 'WARNING: size of read array',ptr_sz,'does not match size of target', sz1d
        end if
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfRead3DFloatArray: PTR:",ptr
#endif
        call gmcffloatarrayfromptrc(ptr,array1d,sz1d) ! This ugly function will simply cast the ptr and return it as the array1d
        call gmcfsendpacketc(sba_sys, sba_tile(packet%destination), packet%destination, packet%source, ACKDATA, packet%data_id, PRE, -1 ,ONE, 0)
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfRead3DFloatArray: SANITY:",array1d(1)
        print *, "FORTRAN API gmcfRead3DFloatArray: SANITY:",sum(array1d)
#endif
        array = reshape(array1d,shape(array))
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfRead3DFloatArray: SANITY:",sum(array)
#endif
    end subroutine
   
       subroutine gmcfRead3DIntegerArray(array, sz, packet)
        type(gmcfPacket) :: packet
        integer(8):: ptr, ptr_sz
        integer(8) :: sz1d
        integer, dimension(3):: sz
        integer,dimension(sz(1), sz(2), sz(3)) :: array
        integer, dimension(size(array)):: array1d
!
!        real, pointer, dimension(:,:,:) :: array
!        real, pointer, dimension(:) :: array1d
        sz1d = size(array1d) ! product(sz)
        ptr = packet%data_ptr
        ptr_sz = packet%data_sz
        if (ptr_sz /= sz1d) then
            print *, 'WARNING: size of read array',ptr_sz,'does not match size of target', sz1d
        end if
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfRead3DIntegerArray: PTR:",ptr
#endif
        call gmcfintegerarrayfromptrc(ptr,array1d,sz1d) ! This ugly function will simply cast the ptr and return it as the array1d
        call gmcfsendpacketc(sba_sys, sba_tile(packet%destination), packet%destination, packet%source, ACKDATA, packet%data_id, PRE, -1 ,ONE, 0)
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfRead3DIntegerArray: SANITY:",array1d(1)
        print *, "FORTRAN API gmcfRead3DIntegerArray: SANITY:",sum(array1d)
#endif
        array = reshape(array1d,shape(array))
#ifdef GMCF_DEBUG
        print *, "FORTRAN API gmcfRead3DIntegerArray: SANITY:",sum(array)
#endif
    end subroutine
    
    subroutine gmcfGetTileId(tile, tile_id)
        integer(8), intent(in) :: tile
        integer, intent(out) :: tile_id
        call gmcfgettileidc(tile, tile_id)
    end subroutine gmcfGetTileId

    subroutine gmcfFinished(model_id)
        integer, intent(In) :: model_id
        ! Start by sending N-1 requests for the simulation time (all except yourself)
        integer :: dest
        gmcfStatus(model_id)=FIN
        ! Send requests for timestamps to all others
        do dest=1,NMODELS
            if (dest /= model_id) then!
                 call gmcfsendpacketc(sba_sys, sba_tile(model_id), model_id, dest, FIN, 0, PRE, -1 ,ONE, 0)
            end if
        end do
    end subroutine gmcfFinished

    subroutine gmcfAddToSet(model_id,set_id,src_model_id)
        integer, intent(In) :: model_id, set_id, src_model_id
        gmcfaddtosetc_(sba_tile(model_id), set_id, src_model_id);
    end subroutine gmcfAddToSet

    subroutine gmcfRemoveFromSet(model_id,set_id,src_model_id)
        integer, intent(In) :: model_id, set_id, src_model_id
        gmcfremovefromsetc_(sba_tile(model_id), set_id, src_model_id);
    end subroutine gmcfRemoveFromSet

    subroutine gmcfSetIsEmpty(model_id,set_id, is_empty)
        integer, intent(In) :: model_id, set_id, is_empty
        gmcfsetisemptyc_(sba_tile(model_id), set_id, is_empty);
    end subroutine gmcfSetIsEmpty

    subroutine gmcfSetContains(model_id,set_id,src_model_id, contains)
        integer, intent(In) :: model_id, set_id, src_model_id, contains
        gmcfsetcontainsc_(sba_tile(model_id), set_id, model_id, contains); !returns #entries for model_id
    end subroutine gmcfSetContains

    subroutine gmcfSetSize(model_id,set_id,set_size)
        integer, intent(In) :: model_id, set_id, set_size
        gmcfsetsizec_(sba_tile(model_id), set_id, set_size);
    end subroutine gmcfSetSize

end module gmcfAPI
