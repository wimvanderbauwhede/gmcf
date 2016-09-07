module gmcfConfiguration
    implicit none
    integer, dimension(NMODELS,NMODELS) :: gmcfConnectivityMatrix = reshape( (/  0, 1,  1, 0 /) , shape(gmcfConnectivityMatrix) )
    integer, parameter :: OCEAN_MODEL = 1 
    integer, parameter :: ATMOSPHERE_MODEL = 2

end module gmcfConfiguration
