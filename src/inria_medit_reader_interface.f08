module inria_medit_reader_interface

    use, intrinsic :: iso_fortran_env , only: INT32

    implicit none

    private
    public  :: inria_medit_file_t



    logical, parameter, private :: DEFAULT_DATA_FIELD_AVAILABILITY = .false.
    !! Default value: whether the data file is available



    integer, parameter, private :: DEFAULT_MESH_VERSION_NUMBER = 0
    !! Default number: `MeshVersionFormatted`

    integer, parameter, private :: DEFAULT_STATEMENT_STAT_NUMBER = 0
    !! Default number: `IOSTAT` or `STAT`

    integer, parameter, private :: LEN_STATEMENT_STAT_MSG = 256
    !! Buffer size retaining `IOMSG` or `ERRMSG`

    integer, parameter, private :: LEN_TEXT_LINE = 512
    !! Buffer size retaining `IOMSG` or `ERRMSG`



    character(LEN_STATEMENT_STAT_MSG), parameter :: DEFAULT_STATEMENT_STAT_MSG = repeat(string=' ', ncopies=LEN_STATEMENT_STAT_MSG)
    !! Default value: `IOMSG` or `ERRMSG`



    type :: io_unit_t
    ! A `TYPE` to retain the unit number to read a file

        integer, private :: number
        !! Retaining the unit number to read a file

        contains

        procedure, pass, private :: close_file
        procedure, pass, private :: open_file
        procedure, pass, private :: output_unit_number
        procedure, pass, private :: rewind_position

        generic, private :: output_number => output_unit_number

    end type io_unit_t



    type :: statement_stat_t
    ! A `TYPE` to receive `IOSTAT` &  `IOMSG`
    ! A `TYPE` to receive   `STAT` & `ERRMSG`

        integer, private :: number
        !! Retaining `IOSTAT` or `STAT`

        character(len=LEN_STATEMENT_STAT_MSG), private :: msg
        !! Retaining `IOMSG` or `MSG`

        contains

        procedure, pass, public  :: is_OK
        procedure, pass, public  :: output_msg
        procedure, pass, public  :: output_stat
        procedure, pass, private :: reset_fields_statement_stat

        generic, private :: reset_fields => reset_fields_statement_stat

    end type statement_stat_t

    type(statement_stat_t), parameter :: DEFAULT_STATEMENT_STAT = &!
    &   statement_stat_t( &!
    &       number = DEFAULT_STATEMENT_STAT_NUMBER , &!
    &       msg    = DEFAULT_STATEMENT_STAT_MSG      &!
    &   )



    type, abstract :: data_field_t

        logical, private :: availability
        !! Whether this is data field is available
        !! .true.  for the     recorded data field
        !! .false. for the NOT recorded data field

        contains

        procedure,   pass, public  :: is_available
        procedure, nopass, private :: is_header_core
        procedure,   pass, private :: make_available
        procedure,   pass, private :: read_field
        procedure,   pass, private :: read_header_and_sub_int_data
        procedure,   pass, private :: reset_availability
        procedure,   pass, private :: reset_field
        procedure,   pass, private :: search_header

        procedure( is_header_abstract         ), pass, deferred, private :: is_header
        procedure( read_field_main_abstract   ), pass, deferred, private :: read_field_main
        procedure( reset_fields_main_abstract ), pass, deferred, private :: reset_fields_main

    end type data_field_t



    type, extends(data_field_t) :: mesh_version_t
    ! A `TYPE` to retain a data field named `MeshVersionFormatted`

        integer(INT32), private :: number
        !! A field to retain the mesh version number

        contains

        procedure, pass, private :: is_header             => is_header_mesh_version
        procedure, pass, private :: output_version_number
        procedure, pass, private :: read_field_main       => read_field_main_mesh_version
        procedure, pass, private :: reset_fields_main     => reset_fields_main_mesh_version

        generic, public :: output_number => output_version_number

    end type mesh_version_t



    type, extends(data_field_t), abstract :: allocatable_data_field_t

        integer(INT32), private :: num_of_items

        contains

    end type allocatable_data_field_t



    type :: inria_medit_file_t
    ! A `TYPE` to read INRIA MEDIT mesh file

        character(len=LEN_TEXT_LINE), private :: text_line
        !! Buffer of the read a single text line

        type(io_unit_t), public :: io_unit
        ! A field to retain the unit number to read a file

        type(statement_stat_t), public :: statement_stat = DEFAULT_STATEMENT_STAT
        !! A field to receive `IOSTAT` &  `IOMSG`
        !! A field to receive   `STAT` & `ERRMSG

        type(mesh_version_t), public :: mesh_version
        !! A field to retain a data field named `MeshVersionFormatted`

        contains

        procedure, pass, public  :: read_file
        procedure, pass, private :: reset_fields_inria_medit_file

        generic, private :: reset_fields => reset_fields_inria_medit_file

    end type inria_medit_file_t



    ! for `data_field_t`
    interface

        module pure elemental function is_available(data_field)

            class(data_field_t), intent(in) :: data_field
            !! A dummy argument for this FUNCTION

            logical :: is_available
            !! The return value of this FUNCTION

        end function is_available



        module pure elemental function is_header_abstract(data_field, string) result(is_header)

            class(data_field_t), intent(in) :: data_field
            !! A dummy argument for this FUNCTION

            character(len=*), intent(in) :: string
            !! A dummy argument for this FUNCTION
            !! The target string of the check

            logical :: is_header
            !! The return value of this FUNCTION

        end function is_header_abstract



        module pure elemental function is_header_core(string, string_header) result(is_header)

            character(len=*), intent(in) :: string
            !! A dummy argument for this FUNCTION
            !! The target string of the check

            character(len=*), intent(in) :: string_header
            !! A dummy argument for this FUNCTION
            !! The header being searching for

            logical :: is_header
            !! The return value of this FUNCTION

        end function is_header_core



        module subroutine make_available(data_field)

            class(data_field_t), intent(inout) :: data_field
            !! A dummy argument for this SUBROUTINE
            !! Retaining the read data

        end subroutine make_available



        module subroutine read_field(data_field, io_unit, text_line, statement_stat)

            class(data_field_t), intent(inout) :: data_field
            !! A dummy argument for this SUBROUTINE
            !! Retaining the read data

            type(io_unit_t), intent(in) :: io_unit
            !! A dummy argument for this SUBROUTINE
            !! Specify the unit number to read a file

            character(len=*), intent(inout) :: text_line
            !! A dummy argument for this SUBROUTINE
            !! Buffer of the read a single text line

            type(statement_stat_t), intent(inout) :: statement_stat
            !! A dummy argument for this SUBROUTINE
            !! Receive   `STAT` & `ERRMSG`
            !! Receive `IOSTAT` &  `IOMSG`

        end subroutine read_field



        module subroutine read_field_main_abstract(data_field, io_unit, text_line, statement_stat)

            class(data_field_t), intent(inout) :: data_field
            !! A dummy argument for this SUBROUTINE
            !! A instance to store the read data

            type(io_unit_t), intent(in) :: io_unit
            !! A dummy argument for this SUBROUTINE
            !! Specify the unit number to read a file

            character(len=*), intent(inout) :: text_line
            !! A dummy argument for this SUBROUTINE
            !! Buffer of the read a single text line

            type(statement_stat_t), intent(inout) :: statement_stat
            !! A dummy argument for this SUBROUTINE
            !! Receive   `STAT` & `ERRMSG`
            !! Receive `IOSTAT` &  `IOMSG`

        end subroutine read_field_main_abstract



        module subroutine read_header_and_sub_int_data(data_field, io_unit, text_line, sub_data, statement_stat)

            class(data_field_t), intent(inout) :: data_field
            !! A dummy argument for this SUBROUTINE
            !! A instance to store the read data

            type(io_unit_t), intent(in) :: io_unit
            !! A dummy argument for this SUBROUTINE
            !! Specify the unit number to read a file

            character(len=*), intent(inout) :: text_line
            !! A dummy argument for this SUBROUTINE
            !! Buffer of the read a single text line

            integer(INT32), intent(out) :: sub_data
            !! A dummy argument for this SUBROUTINE
            !! Receive the read sub data with the data field header

            type(statement_stat_t), intent(inout) :: statement_stat
            !! A dummy argument for this SUBROUTINE
            !! Receive   `STAT` & `ERRMSG`
            !! Receive `IOSTAT` &  `IOMSG`

        end subroutine read_header_and_sub_int_data



        module subroutine reset_availability(data_field)

            class(data_field_t), intent(inout) :: data_field
            !! A dummy argument for this SUBROUTINE

        end subroutine reset_availability



        module subroutine reset_field(data_field, statement_stat)

            class(data_field_t), intent(inout) :: data_field
            !! A dummy argument for this SUBROUTINE

            type(statement_stat_t), intent(inout) :: statement_stat
            !! A dummy argument for this SUBROUTINE
            !! Receive `STAT` & `ERRMSG`

        end subroutine reset_field



        module subroutine reset_fields_main_abstract(data_field, statement_stat)

            class(data_field_t), intent(inout) :: data_field
            !! A dummy argument for this SUBROUTINE

            type(statement_stat_t), intent(inout) :: statement_stat
            !! A dummy argument for this SUBROUTINE
            !! Receive `STAT` & `ERRMSG`

        end subroutine reset_fields_main_abstract



        module subroutine search_header(data_field, io_unit, text_line, statement_stat)

            class(data_field_t), intent(inout) :: data_field
            !! A dummy argument for this SUBROUTINE

            type(io_unit_t), intent(in) :: io_unit
            !! A dummy argument for this SUBROUTINE
            !! Specify the unit number to read a file

            character(len=*), intent(inout) :: text_line
            !! A dummy argument for this SUBROUTINE
            !! Buffer of the read a single text line

            type(statement_stat_t), intent(inout) :: statement_stat
            !! A dummy argument for this SUBROUTINE
            !! Receive `IOSTAT` & `IOMSG`

        end subroutine search_header

    end interface
    ! for `data_field_t`



    ! for `inria_medit_file_t`
    interface

        module subroutine read_file(inria_medit_file, file)

            class(inria_medit_file_t), intent(inout) :: inria_medit_file
            !! A dummy argument for this SUBROUTINE
            !! Container for retaining read mesh information

            character(len=*), intent(in) :: file
            !! A dummy argument for this SUBROUTINE
            !! The path of a file for reading

        end subroutine read_file



        module subroutine reset_fields_inria_medit_file(inria_medit_file)

            class(inria_medit_file_t), intent(inout) :: inria_medit_file
            !! A dummy argument for this SUBROUTINE
            !! Container for retaining read mesh information

        end subroutine reset_fields_inria_medit_file

    end interface
    ! for `inria_medit_file_t`



    ! for `io_unit_t`
    interface

        module pure elemental function output_unit_number(io_unit) result(unit_number)

            class(io_unit_t), intent(in) :: io_unit
            !! A dummy argument for this FUNCTION

            integer :: unit_number
            !! The return value of this FUNCTION

        end function output_unit_number



        module subroutine close_file(io_unit, statement_stat)

            class(io_unit_t), intent(in) :: io_unit
            !! A dummy argument for this SUBROUTINE
            !! Specify the device number (file) to close

            type(statement_stat_t), intent(inout) :: statement_stat
            !! A dummy argument for this SUBROUTINE
            !! Receive `IOSTAT` & `IOMSG`

        end subroutine close_file



        module subroutine open_file(io_unit, file, statement_stat)

            class(io_unit_t), intent(inout) :: io_unit
            !! A dummy argument for this SUBROUTINE
            !! Receive the device number (file) to open

            character(len=*), intent(in) :: file
            !! A dummy argument for this SUBROUTINE
            !! The path of a file for reading

            type(statement_stat_t), intent(inout) :: statement_stat
            !! A dummy argument for this SUBROUTINE
            !! Receive `IOSTAT` & `IOMSG`

        end subroutine open_file



        module subroutine rewind_position(io_unit, statement_stat)

            class(io_unit_t), intent(in) :: io_unit
            !! A dummy argument for this SUBROUTINE
            !! Specify the device number (file) to close

            type(statement_stat_t), intent(inout) :: statement_stat
            !! A dummy argument for this SUBROUTINE
            !! Receive `IOSTAT` & `IOMSG`

        end subroutine rewind_position

    end interface
    ! for `io_unit_t`



    ! for `mesh_version_t`
    interface

        module pure elemental function is_header_mesh_version(data_field, string) result(is_header)

            class(mesh_version_t), intent(in) :: data_field
            !! A dummy argument for this FUNCTION

            character(len=*), intent(in) :: string
            !! A dummy argument for this FUNCTION
            !! The target string of the check

            logical :: is_header
            !! The return value of this FUNCTION

        end function is_header_mesh_version



        module pure elemental function output_version_number(mesh_version) result(version_number)

            class(mesh_version_t), intent(in) :: mesh_version
            !! A dummy argument for this FUNCTION

            integer(INT32) :: version_number
            !! The return value of this FUNCTION

        end function



        module subroutine read_field_main_mesh_version(data_field, io_unit, text_line, statement_stat)

            class(mesh_version_t), intent(inout) :: data_field
            !! A dummy argument for this SUBROUTINE
            !! A instance to store the read data

            type(io_unit_t), intent(in) :: io_unit
            !! A dummy argument for this SUBROUTINE
            !! Specify the unit number to read a file

            character(len=*), intent(inout) :: text_line
            !! A dummy argument for this SUBROUTINE
            !! Buffer of the read a single text line

            type(statement_stat_t), intent(inout) :: statement_stat
            !! A dummy argument for this SUBROUTINE
            !! Receive   `STAT` & `ERRMSG`
            !! Receive `IOSTAT` &  `IOMSG`

        end subroutine read_field_main_mesh_version



        module subroutine reset_fields_main_mesh_version(data_field, statement_stat)

            class(mesh_version_t), intent(inout) :: data_field
            !! A dummy argument for this SUBROUTINE
            !! A instance to store the read data

            type(statement_stat_t), intent(inout) :: statement_stat
            !! A dummy argument for this SUBROUTINE
            !! Receive `STAT` & `ERRMSG`

        end subroutine reset_fields_main_mesh_version

    end interface
    ! for `mesh_version_t`



    ! for `statement_stat_t`
    interface

        module pure elemental function is_OK(statement_stat)

            class(statement_stat_t), intent(in) :: statement_stat
            !! A dummy argument for this FUNCTION

            logical :: is_OK
            !! The return value of this FUNCTION

        end function is_OK



        module pure elemental function output_msg(statement_stat) result(msg)

            class(statement_stat_t), intent(in) :: statement_stat
            !! A dummy argument for this FUNCTION

            character(len=LEN_STATEMENT_STAT_MSG) :: msg
            !! The return value of this FUNCTION

        end function output_msg



        module pure elemental function output_stat(statement_stat) result(stat)

            class(statement_stat_t), intent(in) :: statement_stat
            !! A dummy argument for this FUNCTION

            integer :: stat
            !! The return value of this FUNCTION

        end function output_stat



        module elemental subroutine reset_fields_statement_stat(statement_stat)

            class(statement_stat_t), intent(inout) :: statement_stat
            !! A dummy argument for this SUBROUTINE

        end subroutine reset_fields_statement_stat

    end interface
    ! for `statement_stat_t`



    interface is_iostat_end

        module pure elemental function is_iostat_end_statement_stat(statement_stat)

            type(statement_stat_t), intent(in) :: statement_stat
            !! A dummy argument for this FUNCTION

            logical :: is_iostat_end_statement_stat
            !! The return value of this FUNCTION

        end function is_iostat_end_statement_stat

    end interface is_iostat_end

end module inria_medit_reader_interface



submodule (inria_medit_reader_interface) data_field_implementation

    implicit none
    contains



    module procedure is_available
        is_available = data_field%availability
    end procedure is_available



    module procedure is_header_core

        integer :: index_
        !! A local variable for this PROCEDURE



        index_ = index(string=string(:), substring=' ')

        select case (index_)
            case(0)      ; is_header = ( string( :             ) .eq. string_header(:) )
            case default ; is_header = ( string( :(index_ - 1) ) .eq. string_header(:) )
        end select

    end procedure is_header_core



    module procedure make_available
        data_field%availability = .true.
    end procedure make_available



    module procedure read_field

        ! search the header of this data field

        call data_field%search_header( &!
            io_unit        = io_unit        , &!
            text_line      = text_line      , &!
            statement_stat = statement_stat   &!
        )

        if ( .not. statement_stat%is_OK() ) then

            ! disable this instance
            call data_field%reset_availability()

            return

        end if



        ! read the main contents of this data field

        call data_field%read_field_main( &!
            io_unit        = io_unit        , &!
            text_line      = text_line(:)   , &!
            statement_stat = statement_stat   &!
        )



        ! check the statement stat
        
        if ( statement_stat%is_OK() ) then

            ! make this instance available
            call data_field%make_available()

        else

            ! disable this instance
            call data_field%reset_availability()

        end if

    end procedure read_field



    module procedure read_header_and_sub_int_data

        integer :: index_space
        !! A local variable for this PROCEDURE



        index_space = index(string= trim( text_line(:) ) , substring=' ')



        select case(index_space)

            case(1:)

                read( &!
                    unit   = text_line( (index_space + 1): ) , &!
                    fmt    = *                               , &!
                    iostat = statement_stat%number           , &!
                    iomsg  = statement_stat%msg(:)             &!
                ) &!
                sub_data

            case default

                read( &!
                    unit   = io_unit%output_number() , &!
                    fmt    = *                       , &!
                    iostat = statement_stat%number   , &!
                    iomsg  = statement_stat%msg(:)     &!
                ) &!
                sub_data

        end select

    end procedure read_header_and_sub_int_data



    module procedure reset_availability
        data_field%availability = DEFAULT_DATA_FIELD_AVAILABILITY
    end procedure reset_availability



    module procedure reset_field

        call data_field%reset_availability()
        call data_field%reset_fields_main(statement_stat)

    end procedure reset_field



    module procedure search_header

        ! call `REWIND` statement

        call io_unit%rewind_position(statement_stat)

        if ( .not. statement_stat%is_OK() ) then
            return
        end if



        judge_text_lines: &!
        do

            ! try to read a single text line from the target mesh file

            read( &!
                unit   = io_unit%output_number() , &!
                fmt    = '(A)'                   , &!
                iostat = statement_stat%number   , &!
                iomsg  = statement_stat%msg(:)     &!
            ) &!
            text_line(:)

            if ( .not. statement_stat%is_OK() ) then
                return
            end if



            ! remove leading spaces

            text_line(:) = adjustl( text_line(:) )



            ! judge the read text line

            if ( data_field%is_header( trim( text_line(:) ) ) ) then
                return
            else
                cycle judge_text_lines
            end if

        end do &!
        judge_text_lines

    end procedure search_header

end submodule data_field_implementation



submodule (inria_medit_reader_interface) inria_medit_file_implementation

    implicit none
    contains



    module procedure read_file

        call inria_medit_file%reset_fields()



        ! try to open the target file

        call inria_medit_file%io_unit%open_file( &!
            file           = file                            , &!
            statement_stat = inria_medit_file%statement_stat   &!
        )

        if ( .not. inria_medit_file%statement_stat%is_OK() ) then
            return
        end if



        ! try to read a data field: `MeshVersionFormatted`

        call inria_medit_file%mesh_version%read_field( &!
            io_unit        = inria_medit_file%io_unit        , &!
            text_line      = inria_medit_file%text_line(:)   , &!
            statement_stat = inria_medit_file%statement_stat   &!
        )

        if ( .not. inria_medit_file%statement_stat%is_OK() ) then

            inria_medit_file%text_line(:) &!
            &   =  trim( inria_medit_file%text_line(:) )  &!
            &   // new_line('')                           &!
            &   // 'Failed to read a data field: `MeshVersionFormatted`'

            return

        end if



        ! try to close the read file

        call inria_medit_file%io_unit%close_file( &!
            statement_stat = inria_medit_file%statement_stat &!
        )

    end procedure read_file



    module procedure reset_fields_inria_medit_file

        call inria_medit_file%statement_stat%reset_fields()

        associate( statement_stat => inria_medit_file%statement_stat )

            call inria_medit_file%mesh_version%reset_field(statement_stat)

        end associate

    end procedure reset_fields_inria_medit_file

end submodule inria_medit_file_implementation



submodule (inria_medit_reader_interface) io_unit_implementation

    implicit none
    contains



    module procedure close_file

        close( &!
            unit   = io_unit%number        , &!
            iomsg  = statement_stat%msg(:) , &!
            iostat = statement_stat%number , &!
            status = 'keep'                  &!
        )

    end procedure close_file



    module procedure open_file

        open( &!
            file    = file                  , &!
            newunit = io_unit%number        , &!
            action  ='read'                 , &!
            form    = 'formatted'           , &!
            iomsg   = statement_stat%msg(:) , &!
            iostat  = statement_stat%number , &!
            status  = 'old'                   &! 
        )

    end procedure open_file


    module procedure output_unit_number
        unit_number = io_unit%number
    end procedure output_unit_number



    module procedure rewind_position

        rewind( &!
            unit   = io_unit%number         , &!
            iostat = statement_stat%number  , &!
            iomsg  = statement_stat%msg(:)    &!
        )

    end procedure rewind_position

end submodule io_unit_implementation



submodule (inria_medit_reader_interface) mesh_version_implementation

    implicit none

    character(len=*), parameter :: STR_HEADER = 'MeshVersionFormatted'
    !! The header string of this data field

    contains



    module procedure is_header_mesh_version
        is_header = data_field%is_header_core( string(:), STR_HEADER(:) )
    end procedure is_header_mesh_version



    module procedure output_version_number
        version_number = mesh_version%number
    end procedure output_version_number



    module procedure read_field_main_mesh_version

        associate( mesh_version => data_field )

            call mesh_version%read_header_and_sub_int_data( &!
                io_unit        = io_unit             , &!
                text_line      = text_line(:)        , &!
                sub_data       = mesh_version%number , &!
                statement_stat = statement_stat        &!
            )

        end associate

    end procedure read_field_main_mesh_version



    module procedure reset_fields_main_mesh_version

        associate( mesh_version => data_field )
            mesh_version%number = DEFAULT_MESH_VERSION_NUMBER
        end associate

        call statement_stat%reset_fields()

    end procedure reset_fields_main_mesh_version

end submodule mesh_version_implementation



submodule (inria_medit_reader_interface) statement_stat_implementation

    implicit none
    contains



    module procedure is_iostat_end_statement_stat
        is_iostat_end_statement_stat = is_iostat_end(statement_stat%number)
    end procedure is_iostat_end_statement_stat



    module procedure is_OK
        is_OK = (statement_stat%number .eq. 0)
    end procedure is_OK



    module procedure output_msg
        msg(:) = statement_stat%msg(:)
    end procedure output_msg



    module procedure output_stat
        stat = statement_stat%number
    end procedure output_stat



    module procedure reset_fields_statement_stat
        statement_stat%number = DEFAULT_STATEMENT_STAT%number
        statement_stat%msg(:) = DEFAULT_STATEMENT_STAT%msg(:)
    end procedure reset_fields_statement_stat

end submodule statement_stat_implementation
