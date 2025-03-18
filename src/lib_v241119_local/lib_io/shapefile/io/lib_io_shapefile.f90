module lib_io_shapefile
  implicit none
  private
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  public :: shp_
  public :: shp_entity_
  public :: shp_part_

  public :: dbf_
  public :: dbf_field_
  public :: dbf_record_
  public :: dbf_value_

  public :: SHPOpen
  public :: SHPClose
  public :: SHPGetInfo
  public :: SHPGetEntityInfo
  public :: SHPGetEntityData
  public :: SHPGetAll
  public :: SHPClean

  public :: DBFOpen
  public :: DBFClose
  public :: DBFGetInfo
  public :: DBFGetRecord
  public :: DBFGetAll
  public :: DBFClean
  !-------------------------------------------------------------
  ! Public Types
  !-------------------------------------------------------------
  type shp_part_
    integer(4) :: id = 0
    integer(4) :: nVertices = 0
    real(8), pointer :: x(:), & !(nVertices)
                        y(:), &
                        z(:), &
                        m(:)
  end type

  type shp_entity_
    integer(4) :: id = 0
    integer(4) :: nVertices = 0
    integer(4) :: nParts = 0
    type(shp_part_), pointer :: part(:)  !(nParts)
    real(8) :: xmin = 0.d0, &
               xmax = 0.d0, &
               ymin = 0.d0, &
               ymax = 0.d0, &
               zmin = 0.d0, &
               zmax = 0.d0, &
               mmin = 0.d0, &
               mmax = 0.d0
    logical(4) :: measureIsUsed = .false.
  end type

  type shp_
    integer(4) :: nEntities = 0
    type(shp_entity_), pointer :: entity(:)  !(nEntities)
    integer(4) :: SHPType = 0
    character(16) :: SHPTypeName = ''
    real(8) :: minBound(4) = (/0.d0, 0.d0, 0.d0, 0.d0/), &
               maxBound(4) = (/0.d0, 0.d0, 0.d0, 0.d0/)
  end type

  integer, parameter :: clen_title = 64

  type dbf_field_
    character(1)          :: typeChar = ''
    character(7)          :: typeName = ''
    character(clen_title) :: title    = ''
    integer(4)            :: width    = 0
    integer(4)            :: decimals = 0
  end type

  type dbf_value_
    character(:), pointer :: s
    integer(4)            :: i = 0
    real(8)               :: d = 0.d0
  end type

  type dbf_record_
    type(dbf_value_), pointer :: val(:)  !(nFields)
  end type

  type dbf_
    integer(4) :: nFields = 0
    integer(4) :: nRecords = 0
    type(dbf_field_) , pointer :: field(:)
    type(dbf_record_), pointer :: rec(:)
  end type
  !-------------------------------------------------------------
  ! Interfaces of Public Procedures
  !-------------------------------------------------------------
  interface SHPGetInfo
    module procedure SHPGetInfo__structure
    module procedure SHPGetInfo__components
  end interface

  interface SHPGetEntityInfo
    module procedure SHPGetEntityInfo__structure
    module procedure SHPGetEntityInfo__components
  end interface

  interface DBFGetInfo
    module procedure DBFGetInfo__structure
    module procedure DBFGetInfo__components
  end interface
  !-------------------------------------------------------------
  ! Intefaces for External Procedures
  !-------------------------------------------------------------
  interface 
    subroutine c_shpopen(f)
      character(*), pointer :: f
    end subroutine

    subroutine c_shpclose()
    end subroutine

    subroutine c_shpgetinfo(&
        nEntities, SHPType, SHPTypeName, nameLength, minBound, maxBound)
      integer(4)  , pointer :: nEntities
      integer(4)  , pointer :: SHPType
      character(*), pointer :: SHPTypeName
      integer(4)  , pointer :: nameLength
      real(8)     , pointer :: minBound(:), maxBound(:)
    end subroutine

    subroutine c_shpgetentityinfo(&
        iEntity, nVertices, nParts, measureIsUsed, &
        xmin, xmax, ymin, ymax, zmin, zmax, mmin, mmax, stat)
      integer(4), pointer :: iEntity
      integer(4), pointer :: nVertices
      integer(4), pointer :: nParts
      logical(4), pointer :: measureIsUsed
      real(8)   , pointer :: xmin, xmax, ymin, ymax, &
                             zmin, zmax, mmin, mmax
      integer(4), pointer :: stat
    end subroutine

    subroutine c_shpgetpanpartstart(iEntity, panPartStart, stat)
      integer(4), pointer :: iEntity
      integer(4), pointer :: panPartStart(:)
      integer(4), pointer :: stat
    end subroutine

    subroutine c_shpgetdata(iEntity, x, y, z, m, stat)
      integer(4), pointer :: iEntity
      real(8)   , pointer :: x(:), y(:), z(:), m(:)
      integer(4), pointer :: stat
    end subroutine

    subroutine c_dbfopen(f, stat)
      character(*), pointer :: f
      integer(4), pointer :: stat
    end subroutine

    subroutine c_dbfclose()
    end subroutine

    subroutine c_dbfgetfieldcount(nFields)
      integer(4), pointer :: nFields
    end subroutine

    subroutine c_dbfgetrecordcount(nRecords)
      integer(4), pointer :: nRecords
    end subroutine

    subroutine c_dbfgetfieldinfo(&
        iField, &
        typeChar, typeName, typeNameLength, &
        title, titleLength, width, decimals)
      integer(4)  , pointer :: iField
      character(*), pointer :: typeChar
      character(*), pointer :: typeName
      integer(4)  , pointer :: typeNameLength
      character(*), pointer :: title
      integer(4)  , pointer :: titleLength
      integer(4)  , pointer :: width
      integer(4)  , pointer :: decimals
    end subroutine

    subroutine c_dbfgetrecord(&
        iRecord, iField, typeChar, &
        recslen, recs, reci, recd)
      integer(4)  , pointer :: iRecord
      integer(4)  , pointer :: iField
      character(*), pointer :: typeChar
      integer(4)  , pointer :: recslen
      character(:), pointer :: recs
      integer(4)  , pointer :: reci
      real(8)     , pointer :: recd
    end subroutine 
  end interface
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine SHPOpen(f)
  implicit none
  character(*), intent(in) :: f

  character(len_trim(f)), pointer :: f_

  allocate(f_)
  f_ = trim(f)

  call c_shpopen(f_)

  deallocate(f_)
end subroutine SHPOpen
!===============================================================
!
!===============================================================
subroutine SHPClose()
  implicit none

  call c_shpclose()
end subroutine SHPClose
!===============================================================
!
!===============================================================
subroutine SHPGetInfo__structure(shp)
  implicit none
  type(shp_), intent(out) :: shp

  call SHPGetInfo__components(&
         shp%nEntities, shp%SHPType, shp%SHPTypeName, shp%minBound, shp%maxBound)
end subroutine SHPGetInfo__structure
!===============================================================
!
!===============================================================
subroutine SHPGetInfo__components(nEntities, SHPType, SHPTypeName, minBound, maxBound)
  implicit none
  integer(4)  , intent(out), optional :: nEntities
  integer(4)  , intent(out), optional :: SHPType
  character(*), intent(out), optional :: SHPTypeName
  real(8)     , intent(out), optional :: minBound(:)  !(4)
  real(8)     , intent(out), optional :: maxBound(:)  !(4)
  integer(4)   , pointer :: nEntities_
  integer(4)   , pointer :: SHPType_
  character(16), pointer :: SHPTypeName_
  integer(4)   , pointer :: nameLength_
  real(8)      , pointer :: minBound_(:), &
                            maxBound_(:)

  allocate(nEntities_    , &
           SHPType_    , &
           SHPTypeName_, &
           nameLength_   , &
           minBound_(4)  , &
           maxBound_(4))
  nEntities_  = 0
  SHPType_ = 0
  SHPTypeName_ = ''
  minBound_(:) = 0.d0
  maxBound_(:) = 0.d0

  call c_shpgetinfo(nEntities_, SHPType_, SHPTypeName_, nameLength_, minBound_, maxBound_)

  if( present(SHPType) ) SHPType = SHPType_
  if( present(SHPTypeName) )then
    SHPTypeName = SHPTypeName_
    if( len(SHPTypeName) > nameLength_ )then
      SHPTypeName(nameLength_+1:) = ''
    endif
  endif

  if( present(nEntities)   ) nEntities   = nEntities_
  if( present(SHPType)     ) SHPType     = SHPType_
  if( present(SHPTypeName) ) SHPTypeName = SHPTypeName_
  if( present(minBound)    ) minBound(:) = minBound_(:)
  if( present(maxBound)    ) maxBound(:) = maxBound_(:)

  deallocate(nEntities_, &
             SHPType_, &
             SHPTypeName_, &
             nameLength_, &
             minBound_, &
             maxBound_)
end subroutine SHPGetInfo__components
!===============================================================
!
!===============================================================
subroutine SHPGetEntityInfo__structure(iEntity, entity)
  implicit none
  integer(4)       , intent(in)  :: iEntity
  type(shp_entity_), intent(out) :: entity

  if( entity%id == 0 )then
    entity%id = iEntity
  else
    if( entity%id /= iEntity )then
      write(0,*) '*** @ lib_io_shapefile SHPGetEntityInfo__structure ***'
      write(0,*) '*** Entity index mismatch.'
      write(0,*) 'iEntity  :', iEntity
      write(0,*) 'entity%id:', entity%id
      stop
    endif
  endif

  call SHPGetEntityInfo__components(&
           iEntity, &
           nVertices=entity%nVertices, nParts=entity%nParts, measureIsUsed=entity%measureIsUsed, &
           xmin=entity%xmin, xmax=entity%xmax, &
           ymin=entity%ymin, ymax=entity%ymax, &
           zmin=entity%zmin, zmax=entity%zmax, &
           mmin=entity%mmin, mmax=entity%mmax)
end subroutine SHPGetEntityInfo__structure
!===============================================================
!
!===============================================================
subroutine SHPGetEntityInfo__components(&
    iEntity, &
    nVertices, nParts, measureIsUsed, &
    xmin, xmax, ymin, ymax, zmin, zmax, mmin, mmax)
  implicit none
  integer(4), intent(in) :: iEntity
  integer(4), intent(out), optional :: nVertices
  integer(4), intent(out), optional :: nParts
  logical(4), intent(out), optional :: measureIsUsed
  real(8)   , intent(out), optional :: xmin, xmax, &
                                    ymin, ymax, &
                                    zmin, zmax, &
                                    mmin, mmax
  integer(4), pointer :: iEntity_
  integer(4), pointer :: nVertices_
  integer(4), pointer :: nParts_
  logical(4), pointer :: measureIsUsed_
  real(8)   , pointer :: xmin_, xmax_, &
                         ymin_, ymax_, &
                         zmin_, zmax_, &
                         mmin_, mmax_
  integer(4), pointer :: stat

  allocate(iEntity_)
  iEntity_ = iEntity - 1

  allocate(nVertices_, &
           nParts_, &
           measureIsUsed_, &
           xmin_, xmax_, &
           ymin_, ymax_, &
           zmin_, zmax_, &
           mmin_, mmax_)
  allocate(stat)

  call c_shpgetentityinfo(&
           iEntity_, &
           nVertices_, nParts_, measureIsUsed_, &
           xmin_, xmax_, ymin_, ymax_, zmin_, zmax_, mmin_, mmax_, stat)

  if( stat /= 0 )then
    write(0,*) '*** @ lib_io_shapefile SHPGetEntityInfo__components'
    write(0,*) 'An error occured in function c_shpgetentityinfo'
    stop
  endif

  if( present(nVertices)     ) nVertices     = nVertices_
  if( present(nParts)        ) nParts        = nParts_
  if( present(measureIsUsed) ) measureIsUsed = measureIsUsed_
  if( present(xmin) ) xmin = xmin_
  if( present(xmax) ) xmax = xmax_
  if( present(ymin) ) ymin = ymin_
  if( present(ymax) ) ymax = ymax_
  if( present(zmin) ) zmin = zmin_
  if( present(zmax) ) zmax = zmax_
  if( present(mmin) ) mmin = mmin_
  if( present(mmax) ) mmax = mmax_

  deallocate(iEntity_, &
             nVertices_, &
             nParts_, &
             measureIsUsed_, &
             xmin_, &
             xmax_, &
             ymin_, &
             ymax_, &
             zmin_, &
             zmax_, &
             mmin_, &
             mmax_, &
             stat)
end subroutine SHPGetEntityInfo__components
!===============================================================
!
!===============================================================
subroutine SHPGetEntityData(iEntity, entity)
  implicit none
  integer(4)       , intent(in)    :: iEntity
  type(shp_entity_), intent(inout) :: entity
  type(shp_part_)  , pointer :: part
  integer(4)       , pointer :: iEntity_
  integer(4)       , pointer :: iPart
  integer(4)       , pointer :: panPartStart(:)
  integer(4)       , pointer :: stat
  real(8)   , pointer :: x(:), &
                         y(:), &
                         z(:), &
                         m(:)
  integer(4) :: iP0
  !-------------------------------------------------------------
  ! Set or check entity id.
  !-------------------------------------------------------------
  if( entity%id == 0 )then
    entity%id = iEntity
  else
    if( entity%id /= iEntity )then
      write(0,*) '*** @ lib_io_shapefile SHPGetEntityData ***'
      write(0,*) 'Entity index mismatch.'
      write(0,*) 'iEntity  :', iEntity
      write(0,*) 'entity%id:', entity%id
      stop
    endif
  endif
  !-------------------------------------------------------------
  ! Get panPartStart.
  !-------------------------------------------------------------
  allocate(iEntity_)
  iEntity_ = iEntity - 1

  call SHPGetEntityInfo__structure(iEntity, entity)

  allocate(panPartStart(entity%nParts))
  allocate(stat)

  call c_shpgetpanpartstart(iEntity_, panPartStart, stat)

  if( stat /= 0 ) stop

  allocate(entity%part(entity%nParts))
  allocate(iPart)
  do iPart = 1, entity%nParts-1
    entity%part(iPart)%nVertices = panPartStart(iPart+1) - panPartStart(iPart)
  enddo
  entity%part(entity%nParts)%nVertices = entity%nVertices - panPartStart(entity%nParts)

  deallocate(panPartStart)
  !-------------------------------------------------------------
  ! Read coordinate data.
  !-------------------------------------------------------------
  allocate(x(entity%nVertices), &
           y(entity%nVertices), &
           z(entity%nVertices))
  if( entity%measureIsUsed )then
    allocate(m(entity%nVertices))
  else
    allocate(m(1))
  endif

  call c_shpgetdata(iEntity_, x, y, z, m, stat)

  if( stat /= 0 ) stop

  iP0 = 0
  do iPart = 1, entity%nParts
    part => entity%part(iPart)
    allocate(part%x(part%nVertices), &
             part%y(part%nVertices), &
             part%z(part%nVertices))
    part%x(:) = x(iP0+1:iP0+part%nVertices)
    part%y(:) = y(iP0+1:iP0+part%nVertices)
    part%z(:) = z(iP0+1:iP0+part%nVertices)

    if( entity%measureIsUsed )then
      allocate(part%m(part%nVertices))
      part%m(:) = m(iP0+1:iP0+part%nVertices)
    else
      allocate(part%m(1))
      part%m(:) = 0.d0
    endif

    iP0 = iP0 + part%nVertices
  enddo
  !-------------------------------------------------------------
  ! Deallocate.
  !-------------------------------------------------------------
  deallocate(iEntity_, &
             iPart, &
             stat)
  deallocate(x, y, z, m)
end subroutine SHPGetEntityData
!===============================================================
!
!===============================================================
subroutine SHPGetAll(shp)
  implicit none
  type(shp_), intent(out) :: shp
  integer(4) :: iEntity

  call SHPGetInfo__structure(shp)

  allocate(shp%entity(shp%nEntities))
  do iEntity = 1, shp%nEntities
    call SHPGetEntityData(iEntity, shp%entity(iEntity))
  enddo
end subroutine SHPGetAll
!===============================================================
!
!===============================================================
subroutine SHPClean(shp)
  implicit none
  type(shp_), intent(out) :: shp

  shp%nEntities = 0
  nullify(shp%entity)
  shp%SHPType = 0
  shp%SHPTypeName = ''
  shp%minBound(:) = 0.d0
  shp%maxBound(:) = 0.d0
end subroutine SHPClean
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine DBFOpen(f)
  implicit none
  character(*), intent(in) :: f

  character(len_trim(f)), pointer :: f_
  integer(4), pointer :: stat

  allocate(f_)
  f_ = trim(f)

  allocate(stat)

  call c_dbfopen(f_, stat)

  if( stat /= 0 )then
    write(0,*) '*** @ lib_io_shapefile DBFOpen ***'
    write(0,*) 'An error occured while opening dbf file: '//trim(f)
    stop
  endif

  deallocate(stat)
  deallocate(f_)
end subroutine DBFOpen
!===============================================================
!
!===============================================================
subroutine DBFClose()
  implicit none

  call c_dbfclose()
end subroutine DBFClose
!===============================================================
!
!===============================================================
subroutine DBFGetInfo__structure(dbf)
  implicit none
  type(dbf_), intent(out) :: dbf
  integer(4)      , pointer :: iField
  type(dbf_field_), pointer :: field
  character(1)         , pointer :: typeChar
  character(7)         , pointer :: typeName
  character(clen_title), pointer :: title
  integer(4)           , pointer :: width
  integer(4)           , pointer :: decimals
  integer(4), pointer :: typeNameLength
  integer(4), pointer :: titleLength

  call DBFGetInfo__components(dbf%nFields, dbf%nRecords)

  allocate(dbf%field(dbf%nFields))

  allocate(dbf%rec(dbf%nRecords))

  allocate(iField)
  allocate(typeChar, &
           typeName, &
           title, &
           width, &
           decimals)
  allocate(typeNameLength, &
           titleLength)

  do iField = 0, dbf%nFields-1
    call c_dbfgetfieldinfo(&
           iField, &
           typeChar, typeName, typeNameLength, &
           title, titleLength, width, decimals)
    field => dbf%field(iField+1)
    field%typeChar = typeChar
    field%typeName = typeName(:typeNameLength)
    field%title    = title(:titleLength)
    field%width    = width
    field%decimals = decimals
  enddo

  deallocate(iField, &
             typeChar, &
             typeName, &
             title, &
             width, &
             decimals, &
             typeNameLength, &
             titleLength)
end subroutine DBFGetInfo__structure
!===============================================================
!
!===============================================================
subroutine DBFGetInfo__components(&
    nFields, nRecords, &
    typeChar, typeName, title, width, decimals)
  implicit none
  integer(4)  , intent(out), optional :: nFields
  integer(4)  , intent(out), optional :: nRecords
  character(*), pointer    , optional :: typeChar(:)
  character(*), pointer    , optional :: typeName(:)
  character(*), pointer    , optional :: title(:)
  integer(4)  , pointer    , optional :: width(:)
  integer(4)  , pointer    , optional :: decimals(:)
  integer(4), pointer :: nFields_
  integer(4), pointer :: nRecords_
  integer(4), pointer :: iField
  character(1), pointer :: typeChar_
  character(7), pointer :: typeName_
  character(clen_title), pointer :: title_
  integer(4)           , pointer :: width_
  integer(4)           , pointer :: decimals_
  integer(4), parameter :: typeNameLengthAll = 7
  integer(4), pointer   :: typeNameLength
  integer(4), pointer   :: titleLength

  allocate(nFields_, &
           nRecords_)

  call c_dbfgetfieldcount(nFields_)

  call c_dbfgetrecordcount(nRecords_)

  if( present(nFields)  ) nFields  = nFields_
  if( present(nRecords) ) nRecords = nRecords_

  if( nFields_ == 0 ) return

  if( .not. ( present(typeChar) .or. &
              present(typeName) .or. &
              present(title) .or. &
              present(width) .or. &
              present(decimals) ) ) return

  if( present(typeChar) )then
    if( associated(typeChar) ) deallocate(typeChar)
    allocate(typeChar(nFields_))
  endif

  if( present(typeName) )then
    if( associated(typeName) ) deallocate(typeName)
    allocate(typeName(nFields_))
  endif

  if( present(title) )then
    if( associated(title) ) deallocate(title)
    allocate(title(nFields_))
  endif

  if( present(width) )then
    if( associated(width) ) deallocate(width)
    allocate(width(nFields_))
  endif

  if( present(decimals) )then
    if( associated(decimals) ) deallocate(decimals)
    allocate(decimals(nFields_))
  endif

  allocate(iField)
  allocate(typeChar_, &
           typeName_, &
           title_, &
           width_, &
           decimals_)
  allocate(typeNameLength, &
           titleLength)

  do iField = 0, nFields_-1
    call c_dbfgetfieldinfo(&
           iField, &
           typeChar_, typeName_, typeNameLength, &
           title_, titleLength, width_, decimals_)
    typeName_ = typeName_(:typeNameLength)
    title_    = title_(:titleLength)

    if( present(typeChar) ) typeChar(iField+1) = typeChar_
    if( present(typeName) ) typeName(iField+1) = typeName_
    if( present(title)    ) title(iField+1)    = title_
    if( present(width)    ) width(iField+1)    = width_
    if( present(decimals) ) decimals(iField+1) = decimals_
  enddo

  deallocate(nFields_, &
             nRecords_, &
             iField, &
             typeChar_, &
             typeName_, &
             title_, &
             width_, &
             decimals_, &
             typeNameLength, &
             titleLength)
end subroutine DBFGetInfo__components
!===============================================================
!
!===============================================================
subroutine DBFGetRecord(iRecord, field, rec)
  implicit none
  integer(4)       , intent(in)  :: iRecord
  type(dbf_field_) , intent(in)  :: field(:)
  type(dbf_record_), intent(out) :: rec
  integer(4)            :: nFields
  integer(4)  , pointer :: iField
  integer(4)  , pointer :: iRecord_
  character(1), pointer :: typeChar
  integer(4)  , pointer :: recslen
  character(:), pointer :: recs
  integer(4)  , pointer :: reci
  real(8)     , pointer :: recd

  nFields = size(field)

  allocate(iRecord_)
  iRecord_ = iRecord - 1

  allocate(rec%val(nFields))

  allocate(recslen)

  allocate(iField)
  do iField = 1, nFields
    if( field(iField)%typeChar == 'C' )then
      recslen = field(iField)%width
      allocate(character(recslen) :: rec%val(iField)%s)
      rec%val(iField)%s = ''
    endif
  enddo

  allocate(character(maxval(field(:)%width)*2) :: recs)
  allocate(reci, &
           recd)
  allocate(typeChar)

  do iField = 0, nFields-1
    typeChar = field(iField+1)%typeChar
    call c_dbfgetrecord(iRecord_, iField, typeChar, &
                        recslen, recs, reci, recd)
    if( recslen > 0 )then
      allocate(character(recslen) :: rec%val(iField+1)%s)
      rec%val(iField+1)%s = recs(:recslen)
    endif
    rec%val(iField+1)%i = reci
    rec%val(iField+1)%d = recd
  enddo

  deallocate(iField, &
             iRecord_, &
             typeChar, &
             recslen, &
             recs, &
             reci, &
             recd)
end subroutine DBFGetRecord
!===============================================================
!
!===============================================================
subroutine DBFGetAll(dbf)
  implicit none
  type(dbf_), intent(inout) :: dbf
  integer(4) :: iRecord

  call DBFClean(dbf)

  call DBFGetInfo__structure(dbf)

  do iRecord = 1, dbf%nRecords
    call DBFGetRecord(iRecord, dbf%field, dbf%rec(iRecord))
  enddo
end subroutine DBFGetAll
!===============================================================
!
!===============================================================
subroutine DBFClean(dbf)
  implicit none
  type(dbf_), intent(out) :: dbf

  dbf%nFields  = 0
  dbf%nRecords = 0
  nullify(dbf%field)
  nullify(dbf%rec)
end subroutine DBFClean
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine slice_str(str, n)
  implicit none
  character(*), intent(inout) :: str
  integer(4)  , intent(in) :: n
  character(n) :: str_

  str_ = str(:n)
  str = str_
end subroutine slice_str
!===============================================================
!
!===============================================================
end module lib_io_shapefile
