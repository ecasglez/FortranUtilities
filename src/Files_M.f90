!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------
!
! MODULE: Files_M
!
! DESCRIPTION:
!> @brief Useful tools to manipulate files in Fortran programs.
!
! REVISION HISTORY:
! 16-06-2020 - Initial Version.
!
!> @author Emilio Castro.
!> @version 1.0.
!
!> @copyright See LICENSE file that comes with this distribution.
!--------------------------------------------------------------------

MODULE Files_M

   USE iso_c_binding

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: mkdir, cp, mv, rm, exists, is_directory, is_empty, is_regular_file, is_symlink, create_symlink

   INTERFACE
      FUNCTION c_createdir(dir, ign) RESULT(res) BIND(c,name='c_createdir')
         USE iso_c_binding
         CHARACTER(C_CHAR), VALUE :: dir
         LOGICAL(C_BOOL)          :: ign
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_createdir
      FUNCTION c_create_symlink(src, dest, ignoreErrors) RESULT(res) BIND(c,name='c_create_symlink')
         USE iso_c_binding
         IMPLICIT NONE
         CHARACTER(C_CHAR), VALUE :: src, dest
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_create_symlink
      FUNCTION c_copy_file(src, dest, ignoreErrors) RESULT(res) BIND(c,name='c_copy_file')
         USE iso_c_binding
         IMPLICIT NONE
         CHARACTER(C_CHAR), VALUE :: src, dest
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_copy_file
      FUNCTION c_move_file(src, dest, ignoreErrors) RESULT(res) BIND(c,name='c_move_file')
         USE iso_c_binding
         IMPLICIT NONE
         CHARACTER(C_CHAR), VALUE :: src, dest
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_move_file
      FUNCTION c_remove(fname, ignoreErrors) RESULT(res) BIND(c,name='c_remove')
         USE iso_c_binding
         IMPLICIT NONE
         CHARACTER(C_CHAR), VALUE :: fname
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_remove
      FUNCTION c_is_directory(fname, ignoreErrors) RESULT(res) BIND(c,name='c_is_directory')
         USE iso_c_binding
         CHARACTER(C_CHAR), VALUE :: fname
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_is_directory
      FUNCTION c_is_empty(fname, ignoreErrors) RESULT(res) BIND(c,name='c_is_empty')
         USE iso_c_binding
         CHARACTER(C_CHAR), VALUE :: fname
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_is_empty
      FUNCTION c_is_regular_file(fname, ignoreErrors) RESULT(res) BIND(c,name='c_is_regular_file')
         USE iso_c_binding
         CHARACTER(C_CHAR), VALUE :: fname
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_is_regular_file
      FUNCTION c_is_symlink(fname, ignoreErrors) RESULT(res) BIND(c,name='c_is_symlink')
         USE iso_c_binding
         CHARACTER(C_CHAR), VALUE :: fname
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_is_symlink
   END INTERFACE

   CONTAINS

      !--------------------------------------------------------------------
      ! DESCRIPTION:
      !> @brief Creates a directory.
      !
      ! REVISION HISTORY:
      ! 16-06-2020 - Initial Version.
      !
      !> @author Emilio Castro.
      !> @version 1.0.
      !> @param dir String name of the directory to be created.
      !> @param ignoreErrors. True to print a detailed description of the error message. 
      !> Optional parameter. Default is False.
      !> @return True if the process has been succesful. False in case of error.
      FUNCTION mkdir(dir,ignoreErrors) RESULT(res)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: dir
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         LOGICAL                      :: res
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_createdir(dir//C_NULL_CHAR, ign)
      END FUNCTION mkdir



      !--------------------------------------------------------------------
      ! DESCRIPTION:
      !> @brief Creates a symlink to a file or directory.
      !
      ! REVISION HISTORY:
      ! 16-06-2020 - Initial Version.
      !
      !> @author Emilio Castro.
      !> @version 1.0.
      !> @param src Name of the file or directory to be linked. 
      !> @param dest Name of the destination link..
      !> @param ignoreErrors. True to print a detailed description of the error message. 
      !> Optional parameter. Default is False.
      !> @return True if the process has been succesful. False in case of error.
      FUNCTION create_symlink(src, dest, ignoreErrors) RESULT(res)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: src, dest
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         LOGICAL                      :: res
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_create_symlink(src//C_NULL_CHAR, dest//C_NULL_CHAR, ign)
      END FUNCTION create_symlink



      !--------------------------------------------------------------------
      ! DESCRIPTION:
      !> @brief Copies a file or directory. Directories are copied recursively.
      !> Existing files are overwritten.
      !
      ! REVISION HISTORY:
      ! 16-06-2020 - Initial Version.
      !
      !> @author Emilio Castro.
      !> @version 1.0.
      !> @param src Name of the file to be copied. 
      !> @param dest Name of the destination file.
      !> @param ignoreErrors. True to print a detailed description of the error message. 
      !> Optional parameter. Default is False.
      !> @return True if the process has been succesful. False in case of error.
      FUNCTION cp(src, dest, ignoreErrors) RESULT(res)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: src, dest
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         LOGICAL                      :: res
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_copy_file(src//C_NULL_CHAR, dest//C_NULL_CHAR, ign)
      END FUNCTION cp



      !--------------------------------------------------------------------
      ! DESCRIPTION:
      !> @brief Moves or renames a file or directory. When moving a directory if the 
      !> destination is an already existing directory which is not empty an error is shown
      !> and nothing is done. The destination folder must be removed first using function rm.
      !
      ! REVISION HISTORY:
      ! 16-06-2020 - Initial Version.
      !
      !> @author Emilio Castro.
      !> @version 1.0.
      !> @param src Name of the file to be moved. 
      !> @param dest Name of the destination file.
      !> @param ignoreErrors. True to print a detailed description of the error message. 
      !> Optional parameter. Default is False.
      !> @return True if the process has been succesful. False in case of error.
      FUNCTION mv(src, dest, ignoreErrors) RESULT(res)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: src, dest
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         LOGICAL                      :: res
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_move_file(src//C_NULL_CHAR, dest//C_NULL_CHAR, ign)
      END FUNCTION mv



      !--------------------------------------------------------------------
      ! DESCRIPTION:
      !> @brief Removes a file or directory.
      !
      ! REVISION HISTORY:
      ! 16-06-2020 - Initial Version.
      !
      !> @author Emilio Castro.
      !> @version 1.0.
      !> @param fname Name of the file or directory to be removed.
      !> @param ignoreErrors. True to print a detailed description of the error message. 
      !> Optional parameter. Default is False.
      !> @return True if the process has been succesful. False in case of error.
      FUNCTION rm(fname, ignoreErrors) RESULT(res)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         LOGICAL                      :: res
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_remove(fname//C_NULL_CHAR, ign)
      END FUNCTION rm



      !--------------------------------------------------------------------
      ! DESCRIPTION:
      !> @brief Checks if a file or directory exists.
      !
      ! REVISION HISTORY:
      ! 16-06-2020 - Initial Version.
      !
      !> @author Emilio Castro.
      !> @version 1.0.
      !> @param fname Name of the file to be check for existence.
      !> @return True if the file exists. False otherwise.
      FUNCTION exists(fname) RESULT(res)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         LOGICAL                      :: res
         INQUIRE(FILE=fname,EXIST=res)
      END FUNCTION exists



      !--------------------------------------------------------------------
      ! DESCRIPTION:
      !> @brief Checks if a directory exists.
      !
      ! REVISION HISTORY:
      ! 16-06-2020 - Initial Version.
      !
      !> @author Emilio Castro.
      !> @version 1.0.
      !> @param fname Name of the directory to be checked. 
      !> @param ignoreErrors. True to print a detailed description of the error message. 
      !> Optional parameter. Default is False.
      !> @return True if fname is a directory. False otherwise.
      FUNCTION is_directory(fname, ignoreErrors) RESULT(res)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         LOGICAL                      :: res
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_is_directory(fname//C_NULL_CHAR, ign)
      END FUNCTION is_directory



      !--------------------------------------------------------------------
      ! DESCRIPTION:
      !> @brief Checks if a file is empty.
      !
      ! REVISION HISTORY:
      ! 16-06-2020 - Initial Version.
      !
      !> @author Emilio Castro.
      !> @version 1.0.
      !> @param fname Name of the file to be checked. 
      !> @param ignoreErrors. True to print a detailed description of the error message. 
      !> Optional parameter. Default is False.
      !> @return True if fname is an empty file. False otherwise.
      FUNCTION is_empty(fname, ignoreErrors) RESULT(res)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         LOGICAL                      :: res
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_is_empty(fname//C_NULL_CHAR, ign)
      END FUNCTION is_empty



      !--------------------------------------------------------------------
      ! DESCRIPTION:
      !> @brief Checks if a regular file exists: it is not a directory, symlink, etc.
      !
      ! REVISION HISTORY:
      ! 16-06-2020 - Initial Version.
      !
      !> @author Emilio Castro.
      !> @version 1.0.
      !> @param fname Name of the file to be checked. 
      !> @param ignoreErrors. True to print a detailed description of the error message. 
      !> Optional parameter. Default is False.
      !> @return True if fname is a regular file. False otherwise.
      FUNCTION is_regular_file(fname, ignoreErrors) RESULT(res)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         LOGICAL                      :: res
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_is_regular_file(fname//C_NULL_CHAR, ign)
      END FUNCTION is_regular_file



      !--------------------------------------------------------------------
      ! DESCRIPTION:
      !> @brief Checks if a symlink exists.
      !
      ! REVISION HISTORY:
      ! 16-06-2020 - Initial Version.
      !
      !> @author Emilio Castro.
      !> @version 1.0.
      !> @param fname Name of the symlink to be checked. 
      !> @param ignoreErrors. True to print a detailed description of the error message. 
      !> Optional parameter. Default is False.
      !> @return True if fname is a symlink. False otherwise.
      FUNCTION is_symlink(fname, ignoreErrors) RESULT(res)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         LOGICAL                      :: res
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_is_symlink(fname//C_NULL_CHAR, ign)
      END FUNCTION is_symlink

END MODULE Files_M


