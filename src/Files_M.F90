!--------------------------------------------------------------------
! FortranUtilities
!--------------------------------------------------------------------

MODULE FU_Files
   !! author: Emilio Castro.
   !! date: 16/06/2020.
   !! version: 1.0.
   !! license: MIT.
   !! summary: Useful tools to manipulate files in Fortran programs.
   !! Useful tools to manipulate files in Fortran programs.

   USE iso_c_binding

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: mkdir, cp, mv, rm, exists, is_directory, is_empty, is_regular_file
#ifdef LIN_CPP
   PUBLIC :: is_symlink, create_symlink
#endif
   PUBLIC :: filesep, is_path_absolute


#ifdef WIN_CPP
         CHARACTER,PARAMETER :: filesep = '\\'
         !! Path separator: '\' for Windows and '/' for Linux, MacOS and other OS.
#elif LIN_CPP
         CHARACTER,PARAMETER :: filesep = '/'
         !! Path separator: '\' for Windows and '/' for Linux, MacOS and other OS.
#else
         CHARACTER,PARAMETER :: filesep = '/'
         !! Path separator: '\' for Windows and '/' for Linux, MacOS and other OS.
#endif


   INTERFACE
      FUNCTION c_createdir(dir, ign) RESULT(res) BIND(c,name='c_createdir')
         USE iso_c_binding
         CHARACTER(C_CHAR), VALUE :: dir
         LOGICAL(C_BOOL)          :: ign
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_createdir
#ifdef LIN_CPP
      FUNCTION c_create_symlink(src, dest, ignoreErrors) RESULT(res) BIND(c,name='c_create_symlink')
         USE iso_c_binding
         IMPLICIT NONE
         CHARACTER(C_CHAR), VALUE :: src, dest
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_create_symlink
#endif
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
#ifdef LIN_CPP
      FUNCTION c_is_symlink(fname, ignoreErrors) RESULT(res) BIND(c,name='c_is_symlink')
         USE iso_c_binding
         CHARACTER(C_CHAR), VALUE :: fname
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_is_symlink
#endif
   END INTERFACE

   CONTAINS

      FUNCTION mkdir(dir,ignoreErrors) RESULT(res)
         !! author: Emilio Castro.
         !! date: 16/06/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Creates a directory.
         !! Creates a directory.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: dir
         !! Path and name of the directory to be created.
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! True to print a detailed description of the error message.
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if the process has been succesful. False in case of error.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_createdir(dir//C_NULL_CHAR, ign)
      END FUNCTION mkdir



#ifdef LIN_CPP
      FUNCTION create_symlink(src, dest, ignoreErrors) RESULT(res)
         !! author: Emilio Castro.
         !! date: 16/06/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Creates a symlink to a file or directory (Linux only).
         !! Creates a symlink to a file or directory (Linux only).
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: src
         !! Name of the file or directory to be linked. 
         CHARACTER(LEN=*), INTENT(IN) :: dest
         !! Name of the destination link.
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! True to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if the process has been succesful. False in case of error.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_create_symlink(src//C_NULL_CHAR, dest//C_NULL_CHAR, ign)
      END FUNCTION create_symlink
#endif



      FUNCTION cp(src, dest, ignoreErrors) RESULT(res)
         !! author: Emilio Castro.
         !! date: 16/06/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Copies a file or directory.
         !! Copies a file or directory. Directories are copied recursively.
         !! Existing files are overwritten.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: src
         !! Name of the file to be copied. 
         CHARACTER(LEN=*), INTENT(IN) :: dest
         !! Name of the destination file.
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! True to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if the process has been succesful. False in case of error.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_copy_file(src//C_NULL_CHAR, dest//C_NULL_CHAR, ign)
      END FUNCTION cp



      FUNCTION mv(src, dest, ignoreErrors) RESULT(res)
         !! author: Emilio Castro.
         !! date: 16/06/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Moves or renames a file or directory.
         !! Moves or renames a file or directory. When moving a directory if the 
         !! destination is an already existing directory which is not empty an error is shown
         !! and nothing is done. The destination folder must be removed first using function rm.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: src
         !! Name of the file to be moved. 
         CHARACTER(LEN=*), INTENT(IN) :: dest
         !! Name of the destination file.
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! True to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if the process has been succesful. False in case of error.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_move_file(src//C_NULL_CHAR, dest//C_NULL_CHAR, ign)
      END FUNCTION mv



      FUNCTION rm(fname, ignoreErrors) RESULT(res)
         !! author: Emilio Castro.
         !! date: 16/06/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Removes a file or directory.
         !! Removes a file or directory.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Name of the file or directory to be removed.
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! True to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if the process has been succesful. False in case of error.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_remove(fname//C_NULL_CHAR, ign)
      END FUNCTION rm



      FUNCTION exists(fname) RESULT(res)
         !! author: Emilio Castro.
         !! date: 16/06/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Checks if a file or directory exists.
         !! Checks if a file or directory exists.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Name of the file to be check for existence.
         LOGICAL                      :: res
         !! True if the file exists. False otherwise.
         INQUIRE(FILE=fname,EXIST=res)
      END FUNCTION exists



      FUNCTION is_directory(fname, ignoreErrors) RESULT(res)
         !! author: Emilio Castro.
         !! date: 16/06/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Checks if a directory exists.
         !! Checks if a directory exists.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Name of the directory to be checked. 
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! True to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if fname is a directory. False otherwise.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_is_directory(fname//C_NULL_CHAR, ign)
      END FUNCTION is_directory



      FUNCTION is_empty(fname, ignoreErrors) RESULT(res)
         !! author: Emilio Castro.
         !! date: 16/06/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Checks if a file is empty.
         !! Checks if a file is empty.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Name of the file to be checked. 
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! True to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if fname is an empty file. False otherwise.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_is_empty(fname//C_NULL_CHAR, ign)
      END FUNCTION is_empty



      FUNCTION is_regular_file(fname, ignoreErrors) RESULT(res)
         !! author: Emilio Castro.
         !! date: 16/06/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Checks if a regular file exists: it is not a directory, symlink, etc.
         !! Checks if a regular file exists: it is not a directory, symlink, etc.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Name of the file to be checked. 
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! True to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if fname is a regular file. False otherwise.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_is_regular_file(fname//C_NULL_CHAR, ign)
      END FUNCTION is_regular_file



#ifdef LIN_CPP
      FUNCTION is_symlink(fname, ignoreErrors) RESULT(res)
         !! author: Emilio Castro.
         !! date: 16/06/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Checks if a symlink exists (Linux only).
         !! Checks if a symlink exists (Linux only).
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Name of the symlink to be checked. 
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! True to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if fname is a symlink. False otherwise.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = ignoreErrors
         ELSE
            ign = .FALSE.
         END IF
         res = c_is_symlink(fname//C_NULL_CHAR, ign)
      END FUNCTION is_symlink
#endif


      ELEMENTAL FUNCTION is_path_absolute(fname) RESULT(res)
         !! author: Emilio Castro.
         !! date: 24/07/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Determines if a path is absolute or not
         !! Determines if a path is absolute or not. Returns True if path is absolute
         !! and False if path is relative.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Path to a file. It can be a single filename or an array with several filenames.
         LOGICAL                      :: res
         !! True if the path is absolute and false if the path is relative. If the input
         !! is an array, then the returned value will be an array.
#ifdef WIN_CPP
         res = INDEX(fname,':') == 2 .AND. ( &
            (IACHAR(fname) <= 90  .AND. IACHAR(fname) >= 65) .OR. &
            (IACHAR(fname) <= 122 .AND. IACHAR(fname) >= 97) )
#else
         res = INDEX(fname,filesep) == 1
#endif
      END FUNCTION is_path_absolute



END MODULE FU_Files


