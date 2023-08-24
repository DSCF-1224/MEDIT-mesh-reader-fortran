module inria_medit_reader_interface

    implicit none

    private
    public  :: inria_medit_file_t



    type :: inria_medit_file_t
    ! A `TYPE` to read INRIA MEDIT mesh file

        contains

        procedure, pass, public :: read_file

    end type inria_medit_file_t



    interface

        module subroutine read_file(inria_medit_file, file)

            class(inria_medit_file_t), intent(inout) :: inria_medit_file
            !! A dummy argument for this SUBROUTINE
            !! Container for retaining read mesh information

            character(len=*), intent(in) :: file
            !! A dummy argument for this SUBROUTINE
            !! The path of a file for reading

        end subroutine read_file

    end interface

end module inria_medit_reader_interface



submodule (inria_medit_reader_interface) inria_medit_file_implementation

    implicit none
    contains



    module procedure read_file
    end procedure read_file

end submodule
