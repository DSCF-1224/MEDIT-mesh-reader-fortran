module inria_medit_reader_interface

    implicit none

    private
    public  :: inria_medit_file_t



    integer, parameter, private :: DEFAULT_STATEMENT_STAT_NUMBER = 0
    !! Default number: `IOSTAT` or `STAT`

    integer, parameter, private :: LEN_STATEMENT_STAT_MSG = 256
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

    end type data_field_t



    type :: inria_medit_file_t
    ! A `TYPE` to read INRIA MEDIT mesh file

        type(io_unit_t), public :: io_unit
        ! A field to retain the unit number to read a file

        type(statement_stat_t), public :: statement_stat = DEFAULT_STATEMENT_STAT
        !! A field to receive `IOSTAT` &  `IOMSG`
        !! A field to receive   `STAT` & `ERRMSG`

        contains

        procedure, pass, public  :: read_file
        procedure, pass, private :: reset_fields_inria_medit_file

        generic, private :: reset_fields => reset_fields_inria_medit_file

    end type inria_medit_file_t



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

    end interface
    ! for `io_unit_t`



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



        ! try to close the read file

        call inria_medit_file%io_unit%close_file( &!
            statement_stat = inria_medit_file%statement_stat &!
        )

    end procedure read_file



    module procedure reset_fields_inria_medit_file

        call inria_medit_file%statement_stat%reset_fields()

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

end submodule io_unit_implementation



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
