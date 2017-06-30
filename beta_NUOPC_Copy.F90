#define FILENAME "beta_NUOPC_Copy.F90"
#define MODNAME "beta_NUOPC_Copy"
#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)

module beta_NUOPC_Copy
  use ESMF
  use NUOPC

  implicit none

  private

  public :: NUOPC_CopyFieldToArray
  public :: NUOPC_CopyArrayToField

  interface NUOPC_CopyFieldToArray
    module procedure NUOPC_CopyFieldToR42D
    module procedure NUOPC_CopyFieldToR82D
  end interface

  interface NUOPC_CopyArrayToField
    module procedure NUOPC_CopyR42DtoField
    module procedure NUOPC_CopyR82DtoField
  end interface

  character(len=ESMF_MAXSTR) :: logMsg

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Copy ESMF Field to FORTRAN Array
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NUOPC_CopyFieldToR42D"

  subroutine NUOPC_CopyFieldToR42D(srcField,dstArray,localDe,rc)
   ! ARGUMENTS
    type(ESMF_Field),intent(in)       :: srcField
    real(ESMF_KIND_R4),intent(inout)  :: dstArray(:,:)
    integer,intent(in),optional       :: localDe
    integer,intent(out),optional      :: rc

    ! LOCAL VARIABLES
    real(ESMF_KIND_R4),pointer        :: srcArrayR4(:,:)
    real(ESMF_KIND_R8),pointer        :: srcArrayR8(:,:)
    type(ESMF_TypeKind_Flag)          :: typekind
    integer                           :: rank
    integer                           :: localDeCount
    integer(ESMF_KIND_I8)             :: srcSize, dstSize

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(srcField,localDeCount=localDeCount, &
      typekind=typekind, rank=rank, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (.NOT.present(localDe)) then
      if (localDeCount .gt. 1) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, &
          msg=METHOD//": missing localDe.", rcToReturn=rc)
        return
      endif
    endif

    if (rank .eq. 2) then
      if (typekind .eq. ESMF_TYPEKIND_R4) then
        call ESMF_FieldGet(srcField,localDe=localDe,farrayPtr=srcArrayR4,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        srcSize=size(srcArrayR4,kind=ESMF_KIND_I8)
        dstSize=size(dstArray,kind=ESMF_KIND_I8)
        if (srcSize /= dstSize) then
          write (logMsg,"(A,I0,A,I0)") &
            ": srcSize /= dstSize: ", srcSize," /= ",dstSize
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
            msg=METHOD//trim(logMsg), rcToReturn=rc)
          return
        endif
        dstArray=srcArrayR4
      elseif (typekind .eq. ESMF_TYPEKIND_R8) then
        call ESMF_LogWrite(METHOD//": loss of precision R8 to R4.", &
          ESMF_LOGMSG_WARNING)
        call ESMF_FieldGet(srcField,localDe=localDe,farrayPtr=srcArrayR8,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        srcSize=size(srcArrayR8,kind=ESMF_KIND_I8)
        dstSize=size(dstArray,kind=ESMF_KIND_I8)
        if (srcSize /= dstSize) then
          write (logMsg,"(A,I0,A,I0)") &
            ": srcSize /= dstSize: ", srcSize," /= ",dstSize
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
            msg=METHOD//trim(logMsg), rcToReturn=rc)
          return
        endif
        dstArray=srcArrayR8
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_IMPL, &
          msg=METHOD//": typekind is not supported.", rcToReturn=rc)
        return
      endif
    else
      call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_IMPL, &
        msg=METHOD//": rank is not supported.", rcToReturn=rc)
      return
    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "NUOPC_CopyFieldToR82D"

  subroutine NUOPC_CopyFieldToR82D(srcField,dstArray,localDe,rc)
   ! ARGUMENTS
    type(ESMF_Field),intent(in)       :: srcField
    real(ESMF_KIND_R8),intent(inout)  :: dstArray(:,:)
    integer,intent(in),optional       :: localDe
    integer,intent(out),optional      :: rc

    ! LOCAL VARIABLES
    real(ESMF_KIND_R4),pointer        :: srcArrayR4(:,:)
    real(ESMF_KIND_R8),pointer        :: srcArrayR8(:,:)
    type(ESMF_TypeKind_Flag)          :: typekind
    integer                           :: rank
    integer                           :: localDeCount
    integer(ESMF_KIND_I8)             :: srcSize, dstSize

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(srcField,localDeCount=localDeCount, &
      typekind=typekind, rank=rank, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (.NOT.present(localDe)) then
      if (localDeCount .gt. 1) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, &
          msg=METHOD//": missing localDe.", rcToReturn=rc)
        return
      endif
    endif

    if (rank .eq. 2) then
      if (typekind .eq. ESMF_TYPEKIND_R4) then
        call ESMF_FieldGet(srcField,localDe=localDe,farrayPtr=srcArrayR4,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        srcSize=size(srcArrayR4,kind=ESMF_KIND_I8)
        dstSize=size(dstArray,kind=ESMF_KIND_I8)
        if (srcSize /= dstSize) then
          write (logMsg,"(A,I0,A,I0)") &
            ": srcSize /= dstSize: ", srcSize," /= ",dstSize
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
            msg=METHOD//trim(logMsg), rcToReturn=rc)
          return
        endif
        dstArray=srcArrayR4
      elseif (typekind .eq. ESMF_TYPEKIND_R8) then
        call ESMF_FieldGet(srcField,localDe=localDe,farrayPtr=srcArrayR8,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        srcSize=size(srcArrayR8,kind=ESMF_KIND_I8)
        dstSize=size(dstArray,kind=ESMF_KIND_I8)
        if (srcSize /= dstSize) then
          write (logMsg,"(A,I0,A,I0)") &
            ": srcSize /= dstSize: ", srcSize," /= ",dstSize
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
            msg=METHOD//trim(logMsg), rcToReturn=rc)
          return
        endif
        dstArray=srcArrayR8
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_IMPL, &
          msg=METHOD//": typekind is not supported.", rcToReturn=rc)
        return
      endif
    else
      call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_IMPL, &
        msg=METHOD//": rank is not supported.", rcToReturn=rc)
      return
    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NUOPC_CopyR42DtoField"

  subroutine NUOPC_CopyR42DtoField(srcArray,dstField,localDe,rc)
   ! ARGUMENTS
    real(ESMF_KIND_R4),intent(in)     :: srcArray(:,:)
    type(ESMF_Field),intent(inout)    :: dstField
    integer,intent(in),optional       :: localDe
    integer,intent(out),optional      :: rc

    ! LOCAL VARIABLES
    real(ESMF_KIND_R4),pointer        :: dstArrayR4(:,:)
    real(ESMF_KIND_R8),pointer        :: dstArrayR8(:,:)
    type(ESMF_TypeKind_Flag)          :: typekind
    integer                           :: rank
    integer                           :: localDeCount
    integer(ESMF_KIND_I8)             :: srcSize, dstSize

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(dstField,localDeCount=localDeCount, &
      typekind=typekind, rank=rank, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (.NOT.present(localDe)) then
      if (localDeCount .gt. 1) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, &
          msg=METHOD//": missing localDe.", rcToReturn=rc)
        return
      endif
    endif

    if (rank .eq. 2) then
      if (typekind .eq. ESMF_TYPEKIND_R4) then
        call ESMF_FieldGet(dstField,localDe=localDe,farrayPtr=dstArrayR4,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        srcSize=size(srcArray,kind=ESMF_KIND_I8)
        dstSize=size(dstArrayR4,kind=ESMF_KIND_I8)
        if (srcSize /= dstSize) then
          write (logMsg,"(A,I0,A,I0)") &
            ": srcSize /= dstSize: ", srcSize," /= ",dstSize
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
            msg=METHOD//trim(logMsg), rcToReturn=rc)
          return
        endif
        dstArrayR4=srcArray
      elseif (typekind .eq. ESMF_TYPEKIND_R8) then
        call ESMF_FieldGet(dstField,localDe=localDe,farrayPtr=dstArrayR8,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        srcSize=size(srcArray,kind=ESMF_KIND_I8)
        dstSize=size(dstArrayR8,kind=ESMF_KIND_I8)
        if (srcSize /= dstSize) then
          write (logMsg,"(A,I0,A,I0)") &
            ": srcSize /= dstSize: ", srcSize," /= ",dstSize
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
            msg=METHOD//trim(logMsg), rcToReturn=rc)
          return
        endif
        dstArrayR8=srcArray
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_IMPL, &
          msg=METHOD//": typekind is not supported.", rcToReturn=rc)
        return
      endif
    else
      call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_IMPL, &
        msg=METHOD//": rank is not supported.", rcToReturn=rc)
      return
    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NUOPC_CopyR82DtoField"

  subroutine NUOPC_CopyR82DtoField(srcArray,dstField,localDe,rc)
   ! ARGUMENTS
    real(ESMF_KIND_R8),intent(in)     :: srcArray(:,:)
    type(ESMF_Field),intent(inout)    :: dstField
    integer,intent(in),optional       :: localDe
    integer,intent(out),optional      :: rc

    ! LOCAL VARIABLES
    real(ESMF_KIND_R4),pointer        :: dstArrayR4(:,:)
    real(ESMF_KIND_R8),pointer        :: dstArrayR8(:,:)
    type(ESMF_TypeKind_Flag)          :: typekind
    integer                           :: rank
    integer                           :: localDeCount
    integer(ESMF_KIND_I8)             :: srcSize, dstSize

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(dstField,localDeCount=localDeCount, &
      typekind=typekind, rank=rank, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (.NOT.present(localDe)) then
      if (localDeCount .gt. 1) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, &
          msg=METHOD//": missing localDe.", rcToReturn=rc)
        return
      endif
    endif

    if (rank .eq. 2) then
      if (typekind .eq. ESMF_TYPEKIND_R4) then
        call ESMF_LogWrite(METHOD//": loss of precision R8 to R4.", &
          ESMF_LOGMSG_WARNING)
        call ESMF_FieldGet(dstField,localDe=localDe,farrayPtr=dstArrayR4,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        srcSize=size(srcArray,kind=ESMF_KIND_I8)
        dstSize=size(dstArrayR4,kind=ESMF_KIND_I8)
        if (srcSize /= dstSize) then
          write (logMsg,"(A,I0,A,I0)") &
            ": srcSize /= dstSize: ", srcSize," /= ",dstSize
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
            msg=METHOD//trim(logMsg), rcToReturn=rc)
          return
        endif
        dstArrayR4=srcArray
      elseif (typekind .eq. ESMF_TYPEKIND_R8) then
        call ESMF_FieldGet(dstField,localDe=localDe,farrayPtr=dstArrayR8,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        srcSize=size(srcArray,kind=ESMF_KIND_I8)
        dstSize=size(dstArrayR8,kind=ESMF_KIND_I8)
        if (srcSize /= dstSize) then
          write (logMsg,"(A,I0,A,I0)") &
            ": srcSize /= dstSize: ", srcSize," /= ",dstSize
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
            msg=METHOD//trim(logMsg), rcToReturn=rc)
          return
        endif
        dstArrayR8=srcArray
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_IMPL, &
          msg=METHOD//": typekind is not supported.", rcToReturn=rc)
        return
      endif
    else
      call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_IMPL, &
        msg=METHOD//": rank is not supported.", rcToReturn=rc)
      return
    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

end module
