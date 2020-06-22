#!/bin/sh


# gen_put_var_interfaces.sh - Generate SMIOLf_put_var interfaces
# functions and the function entries for its interface block.

############################################################
#
# generate_put_get_var_interface
# 
# Generate the function interfaces for SMIOLf_put_var
#
# $1 - Variable type name for function name (int, real, real32 etc. )
#
# $2 - Fortran variable type (character, integer, real)
#
# $3 - Integer number of ranks (dimensions) for this interface (0, 1, 2 etc.)
#
# $4 - The iso_c_binding C variable needed to include and use for this variable
#
# $5 - The direction either 'put' or 'get'
#
############################################################
generate_put_get_var_interface()
{
    funcVarName=$1
    varType=$2 
    nRanks=$3
    rankName=$3"d"
    cType=$4
    direction=$5

    # Creating the function declaration and adding size(x) arguments, if any..
    printf "\n"
    printf "    integer function SMIOLf_%s_var_%s_%s(file, varname, decomp, buf" $direction $rankName $funcVarName
    
    i=1
    if [ $nRanks -ne 0 ]
    then
        #
        # If this is a multi-rank integer or real variable, then we need to include
        # a scalar variable that specifies the size of each rank so the array is an
        # explicit shape array, which is interpoerable with C in the 2003 Fortran 
        # Standard
        #
        if [ $varType != "character" ]
        then
            printf ", "
            while [ $i -lt `expr $nRanks + 1` ]
            do
                printf "size"$i
                if [ $i == `expr $nRanks` ]
                then
                    break 
                else
                    printf ", "
                fi
                i=`expr $i + 1`
            done
        fi
    fi
    printf ") result(ierr)\n"
    
    printf "\n"
    if [ $varType = "character"  ]
    then
        printf "        use iso_c_binding, only : c_char, c_loc, c_ptr, c_null_ptr, c_null_char\n"
    else
        printf "        use iso_c_binding, only : c_char, %s, c_loc, c_ptr, c_null_ptr, c_null_char\n" $cType
    fi

    printf "\n"
    printf "        implicit none\n"
    printf "\n"
    printf "        type(SMIOLf_file), target :: file\n"
    printf "        character(len=*), intent(in) :: varname\n"
    printf "        type(SMIOLf_decomp), pointer :: decomp\n"

    # Make size1, size2 ... sizeN if needed
    if [ $nRanks != 0 ]
    then
        if [ $varType != "character" ]
        then
            i=1
            while [ $i -lt `expr $nRanks + 1` ]
            do
                printf "        integer, intent(in) :: size%d\n" $i
                i=`expr $i + 1`
            done
        fi
    fi

    # Variable buf argument decleration
    if [ $varType != "character" ]
    then
        # integers and reals
        printf "        %s(kind=%s), " $varType $cType
    else
        # Characters
        printf "        character(len=*), "
    fi


    i=1
    if [ $nRanks != 0 ]
    then
        # Now add each size1, size2 ... sizeN to the dimension of the 
        # explict shaped array
        if [ $varType != "character" ]
        then # All variables exepct for character variables
            printf "dimension("
            while [ $i -lt `expr $nRanks + 1` ]
            do
                printf "size%d" $i
                if [ $i == `expr $nRanks` ]
                then
                    break 
                else
                    printf ", "
                fi
                i=`expr $i + 1`
            done
            printf "), "
        fi
    fi

    printf "intent(in), target :: buf\n"

    printf "\n"
    printf "        integer :: i\n"
    printf "        character(kind=c_char), dimension(:), pointer :: c_varname\n"
    printf "        type (c_ptr) :: c_file\n"
    printf "        type (c_ptr) :: c_decomp\n"
    printf "        type (c_ptr) :: c_buf\n"

    printf "\n"
    printf "        interface\n"
    printf "           function SMIOL_%s_var(file, varname, decomp, buf) result(ierr) bind(C, name='SMIOL_%s_var')\n" $direction $direction
    printf "                use iso_c_binding, only : c_ptr, c_char, c_int\n"
    printf "                type (c_ptr), value :: file\n"
    printf "                character (kind=c_char), dimension(*) :: varname\n"
    printf "                type (c_ptr), value :: decomp\n"
    printf "                type (c_ptr), value :: buf\n"
    printf "                integer (kind=c_int) :: ierr\n"
    printf "           end function\n"
    printf "       end interface\n"

    if [ $varType = "character" ]
    then
        printf "\n"
        printf "       !\n"
        printf "       ! Create an array of strings for when we are putting multidimensional strings.\n"
        printf "       ! NOTE: this generator only writes single strings. See below, where the Fortran string\n"
        printf "       ! is converted to a c string.\n"
        printf "       !\n"
        printf "       type string_ptr\n"
        printf "           character(kind=c_char), dimension(:), allocatable :: str\n"
        printf "       end type string_ptr\n"
        printf "\n"
        printf "       type (string_ptr), dimension(:), allocatable, target :: string\n"
    fi

    printf "\n"
    printf "       c_file = c_null_ptr\n"
    printf "       c_decomp = c_null_ptr\n"
    printf "       c_buf = c_null_ptr\n"

    printf "\n"
    printf "       !\n"
    printf "       ! file, decomp and buf are all targets, so no need to check if they are\n"
    printf "       ! associated or not\n"
    printf "       !\n"

    printf "       c_file = c_loc(file)\n"
    printf "       c_decomp = c_loc(decomp)\n"
    printf "\n"
    printf "       !\n"
    printf "       ! Convert variable name string\n"
    printf "       !\n"
    printf "       allocate(c_varname(len_trim(varname) + 1))\n"
    printf "       do i=1,len_trim(varname)\n"
    printf "           c_varname(i) = varname(i:i)\n"
    printf "       end do\n"
    printf "       c_varname(i) = c_null_char\n"
    printf "\n"

    # Convert string variable 
    if [ $varType = "character" ]
    then
        printf "       ! \n"
        printf "       ! Convert character variable string that is to be %s\n" $direction
        printf "       ! NOTE: This operation is only works upon a single string\n"
        printf "       ! \n"
        printf "       allocate(string(1))\n"
        printf "       allocate(string(1) %% str(len(buf)))\n"
        printf "\n"
        printf "       do i=1,len(buf)\n"
        printf "           string(1) %% str(i) = buf(i:i)\n"
        printf "       enddo\n"
    fi


    if [ $varType == "character" ]
    then
        printf "       c_buf = c_loc(string(1) %% str)\n"
    else
        printf "       c_buf = c_loc(buf)\n"
    fi

    printf "\n"
    printf "       ierr = SMIOL_%s_var(c_file, c_varname, c_decomp, c_buf)\n" $direction

    printf "\n"
    printf "       deallocate(c_varname)\n"

    printf "\n"
    printf "    end function SMIOLf_%s_var_%s_%s\n" $direction $rankName $funcVarName
    printf "\n"
}

############################################################
#
# generate_interface_block_entry()
#
# Generate the interface block entry for a SMIOLf_put_var function.
# i.e.:
#
#     `module procedure SMIOLf_DIRECTION_var_RANK_TYPE`
#
# $1 - Direction, either 'put' or 'get'
#
# $2 - Type name used in for the function name (int, real32, char etc.)
#
# $3 - Integer rank value for this function (0, 1, 2, etc.)
#
############################################################
generate_interface_block_entry()
{
    direction=$1
    typeName=$2
    rank=$3
    # Interfaces are:
    printf "         module procedure SMIOLf_%s_var_%sd_%s\n" $direction $rank $typeName
}



put_var_interfaceHeaderFile="smiolf_put_var_interface_headers.inc"
put_var_interfaceBodyFile="smiolf_put_var_interfaces.inc"

get_var_interfaceHeaderFile="smiolf_get_var_interface_headers.inc"
get_var_interfaceBodyFile="smiolf_get_var_interfaces.inc"

# Empty the files above if they exist
printf "" > $put_var_interfaceHeaderFile
printf "" > $put_var_interfaceBodyFile

printf "" > $get_var_interfaceHeaderFile
printf "" > $get_var_interfaceBodyFile

# Character - 0D
for d in 0
do
    generate_put_get_var_interface "char" "character" $d "c_char" "put" >> $put_var_interfaceBodyFile
    generate_interface_block_entry "put" "char" $d >> $put_var_interfaceHeaderFile

    generate_put_get_var_interface "char" "character" $d "c_char" "get" >> $get_var_interfaceBodyFile
    generate_interface_block_entry "get" "char" $d >> $get_var_interfaceHeaderFile
done

# Integer - 0D - 1D - 2D - 3D
for d in 0 1 2 3
do
    generate_put_get_var_interface "int" "integer" $d "c_int" "put" >> $put_var_interfaceBodyFile
    generate_interface_block_entry "put" "int" $d >> $put_var_interfaceHeaderFile

    generate_put_get_var_interface "int" "integer" $d "c_int" "get" >> $get_var_interfaceBodyFile
    generate_interface_block_entry "get" "int" $d >> $get_var_interfaceHeaderFile
done

# Real - 0D - 1D - 2D - 3D - 4D - 5D - 6D
for d in 0 1 2 3 5 6
do
    generate_put_get_var_interface "real32" "real" $d "c_float" "put" >> $put_var_interfaceBodyFile
    generate_interface_block_entry "put" "real32" $d >> $put_var_interfaceHeaderFile

    generate_put_get_var_interface "real32" "real" $d "c_float" "get" >> $get_var_interfaceBodyFile
    generate_interface_block_entry "get" "real32" $d >> $get_var_interfaceHeaderFile
done

# Double - 0D - 1D - 2D - 3D - 4D - 5D - 6D
for d in 0 1 2 3 5 6
do
    generate_put_get_var_interface "real64" "real" $d "c_double" "put" >> $put_var_interfaceBodyFile
    generate_interface_block_entry "put" "real64" $d >> $put_var_interfaceHeaderFile

    generate_put_get_var_interface "real64" "real" $d "c_double" "get" >> $get_var_interfaceBodyFile
    generate_interface_block_entry "get" "real64" $d >> $get_var_interfaceHeaderFile
done
