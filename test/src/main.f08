program test

    use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

    use, non_intrinsic :: inria_medit_reader_interface



    implicit none



    type(inria_medit_file_t) :: mesh_file
    !! A variable for this PROGRAM



    call mesh_file%read_file('data/bamg_square_o.mesh')

    if ( .not. mesh_file%statement_stat%is_OK() ) then
        write( unit=ERROR_UNIT,  fmt='(I0)' )       mesh_file%statement_stat%output_stat()
        write( unit=ERROR_UNIT,  fmt='(A)'  ) trim( mesh_file%statement_stat%output_msg() )
    end if

end program test
