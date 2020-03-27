#!/bin/sh

############################################################
############################################################
# gen_put_var_interfaces.sh
#
# Generate SMIOLf_put_var interface functions and module 
# procedure headers.
############################################################
############################################################

############################################################
# generate_interface_function()
# 
# $1 Var name to use for the function
#
# $2 Fortran variable type
#
# $2 Will be the interger number of ranks (dimensions)
#
# $3 Will be the name of the ranks - i.e. 0d 1d 2d 3d etc.
#
# $4 Will be the c type needed to use
############################################################
generate_interface_function()
{
    funcVarName=$1
    varType=$2 
    nRanks=$3
    rankName=$4
    cType=$5

    # Creating the function declaration and adding size(x) arguments, if any..
    echo""
    echo -n "    integer function SMIOLf_put_var_RANK_TYPE(file, decomp, varname, buf" | sed "s/RANK/$rankName/" | sed "s/TYPE/$funcVarName/" 
    
    i=1
    if [ $nRanks -ne 0 ]
    then
        if [ $varType != "character" ]
        then
            echo -n ", "
            while [ $i -lt `expr $nRanks + 1` ]
            do
                echo -n "size"$i
                if [ $i == `expr $nRanks` ]
                then
                    break 
                else
                    echo -n ", "
                fi
                i=`expr $i + 1`
            done
        fi
    fi
    echo ") result(ierr)"

    
    echo ""
    echo "        use iso_c_binding, only : c_char, TYPE, c_loc, c_ptr, c_null_ptr, c_null_char" | sed "s/TYPE/$cType/"
    echo ""
    echo "        implicit none"
    echo ""
    echo "        type(SMIOLf_file), target :: file"
    echo "        type(SMIOLf_decomp), target :: decomp"
    echo "        character(len=*), intent(in) :: varname"

    # Make size1, size2 ... sizeN if needed
    if [ $nRanks != 0 ]
    then
        if [ $varType != "character" ]
        then
            i=1
            while [ $i -lt `expr $nRanks + 1` ]
            do
                echo "        integer, intent(in) :: size"$i
                i=`expr $i + 1`
            done
        fi
    fi

    # Variable buf definition 
    if [ $varType != "character" ]
    then
        echo -n "        TYPE(kind=CTYPENAME), "| sed "s/TYPE/$varType/" | sed "s/CTYPENAME/$cType/"
    else
        echo -n "        character(len=*), "
    fi


    i=1
    if [ $nRanks != 0 ]
    then
        if [ $varType != "character" ]
        then # All variables exepct for character variables
            echo -n "dimension("
            while [ $i -lt `expr $nRanks + 1` ]
            do
                echo -n "size"$i
                if [ $i == `expr $nRanks` ]
                then
                    break 
                else
                    echo -n ", "
                fi
                i=`expr $i + 1`
            done
            echo -n "), "
        fi
    fi

    if [ $varType == "character" ]
    then
        echo -n "dimension(:), "
    fi

    if [ $varType != "character" ]
    then
        echo "intent(in), target :: buf"
    else
        echo "intent(in) :: buf"
    fi

    echo ""
    echo "        integer :: i"
    if [ $varType = "character" ]
    then
        echo "        integer :: j"
    fi
    echo "        character(kind=c_char), dimension(:), pointer :: c_varname"
    echo "        type (c_ptr) :: c_file"
    echo "        type (c_ptr) :: c_decomp"

    if [ $varType != "character" ]
    then
        echo "        type (c_ptr) :: c_buf"
    else
        echo "        type (c_ptr), dimension(:), allocatable, target :: c_buf"
    fi
    

    echo ""
    echo "        interface"
    echo "           function SMIOL_put_var(file, decomp, varname, buf) result(ierr) bind(C, name='SMIOL_put_var')"
    echo "                use iso_c_binding, only : c_ptr, c_char, c_int"
    echo "                type (c_ptr), value :: file"
    echo "                type (c_ptr), value :: decomp"
    echo "                character (kind=c_char), dimension(*) :: varname"
    echo "                type (c_ptr), value :: buf"
    echo "                integer (kind=c_int) :: ierr"
    echo "           end function"
    echo "       end interface"

    if [ $varType = "character" ]
    then
        echo "       ! Used to store an array of pointers to character arrays"
        echo "       type string_ptr"
        echo "           character(kind=c_char), dimension(:), allocatable :: str"
        echo "       end type string_ptr"
        echo "" 
        echo "       type (string_ptr), dimension(:), allocatable, target :: strings"
    fi

    echo ""
    echo "       c_file = c_null_ptr"
    echo "       c_decomp = c_null_ptr"

    if [ $varType != "character" ]
    then
        echo "       c_buf = c_null_ptr"
    fi

    echo ""
    echo "       !"
    echo "       ! file, decomp and buf are all targets, so no need to check if they are"
    echo "       ! associated or not"
    echo "       !"

    echo "       c_file = c_loc(file)"
    echo "       c_decomp = c_loc(decomp)"
    if [ $varType != "character" ]
    then
        echo "       c_buf = c_loc(buf)"
    fi
    echo ""
    echo "       !"
    echo "       ! Convert variable name string"
    echo "       !"
    echo "       allocate(c_varname(len_trim(varname) + 1))"
    echo "       do i=1,len_trim(varname)"
    echo "           c_varname(i) = varname(i:i)"
    echo "       end do"
    echo "       c_varname(i) = c_null_char"

    if [ $varType = "character" ]
    then
        if [ $nRanks -ne 0 ]
        then
            echo "        allocate(c_buf(size(buf)))"
            echo "        allocate(strings(size(buf)))"
            echo ""
            echo "        do j=1,size(buf)"
        else
            echo "        allocate(c_buf(1))"
            echo "        allocate(strings(1))"
            echo ""
            echo "        do j=1,1"
        fi
        echo "            allocate(strings(j) % str(len_trim(buf(j))+1))"
        echo ""
        echo "            do i=1,len_trim(buf(j))"
        echo "                strings(j) % str(i) = buf(j)(i:i)"
        echo "            end do"
        echo "            strings(j) % str(i) = c_null_char"
        echo "            c_buf(j) = c_loc(strings(j) % str)"
        echo "        end do"
    fi


    echo ""
    echo "       ierr = SMIOL_put_var(c_file, c_decomp, c_varname, c_buf)"

    echo ""
    echo "       deallocate(c_varname)"

    echo ""
    echo "    end function SMIOLf_put_var_RANK_TYPE" | sed "s/RANK/$rankName/" | sed "s/TYPE/$funcVarName/"
    echo ""
}


############################################################
# generate_header_entry()
#
# $1 Will be the type for the function name
#
# $2 Will be the rank - i.e. 0d, 1d, 2d,
#
############################################################
generate_header_entry()
{
    typeName=$1
    rank=$2
    # Interfaces are:
    # module procedure SMIOLf_put_var_RANK_TYPE
    echo "         module procedure SMIOLf_put_var_RANK_TYPE" | sed "s/RANK/$rank/" | sed "s/TYPE/$typeName/"
}


# Automatically generate SMIOLf_put_var interfaces

interfaceHeaderFile="smiolf_put_var_interface_headers.inc"
interfaceBodyFile="smiolf_put_var_interfaces.inc"
# Empty the files above
echo -n "" > $interfaceHeaderFile
echo -n "" > $interfaceBodyFile

# Character - 0D
for d in 0
do
    generate_interface_function "char" "character" $d $d"d" "c_char" >> $interfaceBodyFile
    generate_header_entry "char" $d"d" >> $interfaceHeaderFile
done

# Integer - 0D - 1D - 2D - 3D
for d in 0 1 2 3
do
    generate_interface_function "int" "integer" $d $d"d" "c_int" >> $interfaceBodyFile
    generate_header_entry "int" $d"d" >> $interfaceHeaderFile
done

# Real - 0D - 1D - 2D - 3D - 4D - 5D - 6D
for d in 0 1 2 3 5 6
do
    generate_interface_function "real32" "real" $d $d"d" "c_float" >> $interfaceBodyFile
    generate_header_entry "real32" $d"d" >> $interfaceHeaderFile
done

# Double - 0D - 1D - 2D - 3D - 4D - 5D - 6D
for d in 0 1 2 3 5 6
do
    generate_interface_function "real64" "real" $d $d"d" "c_double" >> $interfaceBodyFile
    generate_header_entry "real64" $d"d" >> $interfaceHeaderFile
done
