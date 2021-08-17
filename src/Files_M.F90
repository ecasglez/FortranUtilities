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
   USE FU_Prec

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: mkdir, cp, mv, rm, exists, is_directory, is_empty, is_regular_file
#ifdef LIN_CPP
   PUBLIC :: is_symlink, create_symlink
#endif
   PUBLIC :: filesep, is_path_absolute, is_path_relative, extension, stem, filename, &
             parent_path, readMatrix, writeMatrix, replace_extension, replace_filename, &
             remove_filename


#ifdef WIN_CPP
         CHARACTER,PARAMETER :: filesep = ACHAR(92)
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
         CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: dir
         LOGICAL(C_BOOL),INTENT(IN) :: ign
         LOGICAL(C_BOOL)            :: res
      END FUNCTION c_createdir
#ifdef LIN_CPP
      FUNCTION c_create_symlink(src, dest, ignoreErrors) RESULT(res) BIND(c,name='c_create_symlink')
         USE iso_c_binding
         IMPLICIT NONE
         CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: src, dest
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_create_symlink
#endif
      FUNCTION c_copy_file(src, dest, ignoreErrors) RESULT(res) BIND(c,name='c_copy_file')
         USE iso_c_binding
         IMPLICIT NONE
         CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: src, dest
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_copy_file
      FUNCTION c_move_file(src, dest, ignoreErrors) RESULT(res) BIND(c,name='c_move_file')
         USE iso_c_binding
         IMPLICIT NONE
         CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: src, dest
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_move_file
      FUNCTION c_remove(fname, ignoreErrors) RESULT(res) BIND(c,name='c_remove')
         USE iso_c_binding
         IMPLICIT NONE
         CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: fname
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_remove
      FUNCTION c_exists(fname) RESULT(res) BIND(c,name='c_exists')
         USE iso_c_binding
         IMPLICIT NONE
         CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: fname
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_exists
      FUNCTION c_is_directory(fname, ignoreErrors) RESULT(res) BIND(c,name='c_is_directory')
         USE iso_c_binding
         CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: fname
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_is_directory
      FUNCTION c_is_empty(fname, ignoreErrors) RESULT(res) BIND(c,name='c_is_empty')
         USE iso_c_binding
         CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: fname
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_is_empty
      FUNCTION c_is_regular_file(fname, ignoreErrors) RESULT(res) BIND(c,name='c_is_regular_file')
         USE iso_c_binding
         CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: fname
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_is_regular_file
#ifdef LIN_CPP
      FUNCTION c_is_symlink(fname, ignoreErrors) RESULT(res) BIND(c,name='c_is_symlink')
         USE iso_c_binding
         CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: fname
         LOGICAL(C_BOOL)  , VALUE :: ignoreErrors
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_is_symlink
#endif
      FUNCTION c_is_absolute(fname) RESULT(res) BIND(c,name='c_is_absolute')
         USE iso_c_binding
         CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: fname
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_is_absolute
      FUNCTION c_is_relative(fname) RESULT(res) BIND(c,name='c_is_relative')
         USE iso_c_binding
         CHARACTER(C_CHAR), DIMENSION(*) :: fname
         LOGICAL(C_BOOL)          :: res
      END FUNCTION c_is_relative
      SUBROUTINE c_extension(fname) BIND(c,name='c_extension')
         USE iso_c_binding
         CHARACTER(C_CHAR),DIMENSION(*)   :: fname
      END SUBROUTINE c_extension
      SUBROUTINE c_replace_extension(fname,ext) BIND(c,name='c_replace_extension')
         USE iso_c_binding
         CHARACTER(C_CHAR),DIMENSION(*)   :: fname
         CHARACTER(C_CHAR),DIMENSION(*)   :: ext
      END SUBROUTINE c_replace_extension
      SUBROUTINE c_stem(fname) BIND(c,name='c_stem')
         USE iso_c_binding
         CHARACTER(C_CHAR),DIMENSION(*)   :: fname
      END SUBROUTINE c_stem
      SUBROUTINE c_filename(fname) BIND(c,name='c_filename')
         USE iso_c_binding
         CHARACTER(C_CHAR),DIMENSION(*)   :: fname
      END SUBROUTINE c_filename
      SUBROUTINE c_replace_filename(fname,newname) BIND(c,name='c_replace_filename')
         USE iso_c_binding
         CHARACTER(C_CHAR),DIMENSION(*)   :: fname
         CHARACTER(C_CHAR),DIMENSION(*)   :: newname
      END SUBROUTINE c_replace_filename
      SUBROUTINE c_remove_filename(fname) BIND(c,name='c_remove_filename')
         USE iso_c_binding
         CHARACTER(C_CHAR),DIMENSION(*)   :: fname
      END SUBROUTINE c_remove_filename
      SUBROUTINE c_parent_path(fname) BIND(c,name='c_parent_path')
         USE iso_c_binding
         CHARACTER(C_CHAR),DIMENSION(*)   :: fname
      END SUBROUTINE c_parent_path
   END INTERFACE


   INTERFACE readMatrix
      !! author: Emilio Castro.
      !! date: 01/12/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Reads a matrix from a file.
      !! Reads a matrix from a file. The file must have the proper format:
      !!
      !! First line includes the number of rows, the number of columns and a logical
      !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
      !!
      !! The second row can be a header or not.
      !!
      !! Then the matrix comes. The different columns must be separated using blanks.
      !!
      !! The file is then opened, read and closed. The return variable must have ALLOCATABLE attribute,
      !! and must not be allocated (the subroutine takes care of allocation but not about deallocation).
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! call readMatrix(filename, matrix)
      !!```
      !!
      !! Where:
      !!
      !! * `filename`: string of any length with the path to the file to read. If the file does not exist or
      !! it is empty a Fortran runtime error is raised. If the file does not have the proper format
      !! the behaviour is undefined.
      !! * `matrix`: an allocatable array of rank 2 of any of the integer or real kinds supported.
      !! It will be allocated automatically in the subroutine, but the user must deallocate matrix
      !! manually after use.
      !!
      !!### Example
      !!
      !! The following example program loads values from a file named matrix.txt:
      !!
      !!```Fortran
      !! PROGRAM readMatrixExample
      !!    USE FU_Files, ONLY: readMatrix
      !!    USE FU_Prec, ONLY: dp
      !!    IMPLICIT NONE
      !!    CHARACTER(LEN=:), ALLOCATABLE :: matrixFileName
      !!    REAL(KIND=dp), DIMENSION(:,:), ALLOCATABLE :: matrix
      !!    matrixFileName = 'matrix.txt'
      !!    CALL readMatrix(matrixFileName, matrix)
      !!    WRITE(*,*) matrix
      !!    DEALLOCATE(matrix)
      !! END PROGRAM readMatrixExample
      !!```
      !!
      !! File matrix.txt contains the following information:
      !!
      !!```
      !! 2 3 F 
      !! 1.1 1.2 1.3 
      !! 2.1 2.2 2.3
      !!```
      MODULE PROCEDURE readMatrix_i8
      MODULE PROCEDURE readMatrix_i16
      MODULE PROCEDURE readMatrix_i32
      MODULE PROCEDURE readMatrix_i64
      MODULE PROCEDURE readMatrix_sp
      MODULE PROCEDURE readMatrix_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE readMatrix_qp
#endif
   END INTERFACE readMatrix

   INTERFACE writeMatrix
      !! author: Emilio Castro.
      !! date: 01/12/2020.
      !! version: 1.0.
      !! license: MIT.
      !! summary: Writes a matrix to a file.
      !! Writes a matrix to a file. The file will have the following format:
      !!
      !! First line includes the number of rows, the number of columns and a logical
      !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
      !!
      !! The second row can be a header or not.
      !!
      !! Then the matrix comes. The different columns are separated using blanks.
      !!
      !! The file is then opened, written and closed.
      !!
      !!### Syntax
      !!
      !!```Fortran
      !! call writeMatrix(filename, matrix, header, formato)
      !!```
      !!
      !! Where:
      !!
      !! * `filename`: string of any length with the path to the file to write.
      !! * `matrix`: an array of rank 2 of any of the integer or real kinds supported.
      !! * `header`: optional character variable of any length to write in the second line
      !! of the file.
      !! * `formato`: optional character variable with the format to use for the numbers (without
      !! parenthesis.
      !!
      !!### Example
      !!
      !! The following example program writes values to a file named matrix.txt:
      !!
      !!```Fortran
      !! PROGRAM writeMatrixExample
      !!    USE FU_Files, ONLY: writeMatrix
      !!    USE FU_Prec, ONLY: dp
      !!    IMPLICIT NONE
      !!    CHARACTER(LEN=:), ALLOCATABLE :: matrixFileName
      !!    REAL(KIND=dp), DIMENSION(2,3) :: matrix
      !!    matrixFileName = 'matrix.txt'
      !!    matrix(1,1) = 1.1 
      !!    matrix(1,2) = 1.2 
      !!    matrix(1,3) = 1.3 
      !!    matrix(2,1) = 2.1 
      !!    matrix(2,2) = 2.2 
      !!    matrix(2,3) = 2.3 
      !!    CALL writeMatrix(matrixFileName, matrix, formato='F3.1')
      !! END PROGRAM writeMatrixExample
      !!```
      !!
      !! After execution file matrix.txt contains the following information:
      !!
      !!```
      !!        2           3 F
      !! 1.1 1.2 1.3
      !! 2.1 2.2 2.3
      !!```
      MODULE PROCEDURE writeMatrix_i8
      MODULE PROCEDURE writeMatrix_i16
      MODULE PROCEDURE writeMatrix_i32
      MODULE PROCEDURE writeMatrix_i64
      MODULE PROCEDURE writeMatrix_sp
      MODULE PROCEDURE writeMatrix_dp
#ifdef QPREC_FPP
      MODULE PROCEDURE writeMatrix_qp
#endif
   END INTERFACE writeMatrix


   CONTAINS

      FUNCTION mkdir(dir,ignoreErrors) RESULT(res)
         !! author: Emilio Castro.
         !! date: 16/06/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Creates a directory.
         !! Creates a directory.
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = mkdir(dir, ignoreErrors)
         !!```
         !!
         !! Where:
         !!
         !! * `dir`: Path and name of the directory to be created.
         !! * `ignoreErrors`: False to print a detailed description of the error message.
         !! Optional parameter. Default is False.
         !! 
         !! It returns True if the process has been succesful or False in case of error.
         !!
         !!### Example
         !!
         !! The following program creates folder tmp:
         !!
         !!```Fortran
         !! PROGRAM mkdirExample
         !!    USE FU_Files, ONLY: mkdir
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='tmp'
         !!    IF (mkdir(path)) THEN
         !!       WRITE(*,*) 'Success'
         !!    ELSE
         !!       WRITE(*,*) 'Error'
         !!    END IF
         !! END PROGRAM mkdirExample
         !!```

         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: dir
         !! Path and name of the directory to be created.
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! False to print a detailed description of the error message.
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if the process has been succesful. False in case of error.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = LOGICAL(ignoreErrors,KIND=C_BOOL)
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
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = create_symlink(src, dest, ignoreErrors)
         !!```
         !!
         !! Where:
         !!
         !! * `src`: Name of the file or directory to be linked.
         !! * `dest`: Name of the destination link
         !! * `ignoreErrors`: False to print a detailed description of the error message.
         !! Optional parameter. Default is False.
         !! 
         !! It returns True if the process has been succesful or False in case of error.
         !!
         !!### Example
         !!
         !! The following program creates folder tmp:
         !!
         !!```Fortran
         !! PROGRAM create_symlinkExample
         !!    USE FU_Files, ONLY: create_symlink
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: src, dest
         !!    src = 'file1'
         !!    dest = 'file2'
         !!    IF (create_symlink(src, dest)) THEN
         !!       WRITE(*,*) 'Success'
         !!    ELSE
         !!       WRITE(*,*) 'Error'
         !!    END IF
         !! END PROGRAM create_symlinkExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: src
         !! Name of the file or directory to be linked. 
         CHARACTER(LEN=*), INTENT(IN) :: dest
         !! Name of the destination link.
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! False to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if the process has been succesful. False in case of error.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = LOGICAL(ignoreErrors,KIND=C_BOOL)
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
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = cp(src, dest, ignoreErrors)
         !!```
         !!
         !! Where:
         !!
         !! * `src`: Name of the file or folder to copy.
         !! * `dest`: Name of the destination 
         !! * `ignoreErrors`: False to print a detailed description of the error message.
         !! Optional parameter. Default is False.
         !! 
         !! It returns True if the process has been succesful or False in case of error.
         !!
         !!### Example
         !!
         !! The following program copies file1 to file2:
         !!
         !!```Fortran
         !! PROGRAM cpExample
         !!    USE FU_Files, ONLY: cp
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path1, path2
         !!    path1='file1'
         !!    path2='file2'
         !!    IF (cp(path1, path2)) THEN
         !!       WRITE(*,*) 'Success'
         !!    ELSE
         !!       WRITE(*,*) 'Error'
         !!    END IF
         !! END PROGRAM cpExample
         !!```

         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: src
         !! Name of the file to be copied. 
         CHARACTER(LEN=*), INTENT(IN) :: dest
         !! Name of the destination file.
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! False to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if the process has been succesful. False in case of error.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = LOGICAL(ignoreErrors,KIND=C_BOOL)
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
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = mv(src, dest, ignoreErrors)
         !!```
         !!
         !! Where:
         !!
         !! * `src`: Name of the file or folder to move.
         !! * `dest`: Name of the destination 
         !! * `ignoreErrors`: False to print a detailed description of the error message.
         !! Optional parameter. Default is False.
         !! 
         !! It returns True if the process has been succesful or False in case of error.
         !!
         !!### Example
         !!
         !! The following program moves or renames file1 to file2:
         !!
         !!```Fortran
         !! PROGRAM mvExample
         !!    USE FU_Files, ONLY: mv
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path1, path2
         !!    path1='file1'
         !!    path2='file2'
         !!    IF (mv(path1, path2)) THEN
         !!       WRITE(*,*) 'Success'
         !!    ELSE
         !!       WRITE(*,*) 'Error'
         !!    END IF
         !! END PROGRAM mvExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: src
         !! Name of the file to be moved. 
         CHARACTER(LEN=*), INTENT(IN) :: dest
         !! Name of the destination file.
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! False to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if the process has been succesful. False in case of error.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = LOGICAL(ignoreErrors,KIND=C_BOOL)
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
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = rm(fname, ignoreErrors)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name of the file or folder to remove.
         !! * `ignoreErrors`: False to print a detailed description of the error message.
         !! Optional parameter. Default is False.
         !! 
         !! It returns True if the process has been succesful or False in case of error.
         !!
         !!### Example
         !!
         !! The following program removes file1:
         !!
         !!```Fortran
         !! PROGRAM rmExample
         !!    USE FU_Files, ONLY: rm
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1'
         !!    IF (rm(path)) THEN
         !!       WRITE(*,*) 'Success'
         !!    ELSE
         !!       WRITE(*,*) 'Error'
         !!    END IF
         !! END PROGRAM rmExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Name of the file or directory to be removed.
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! False to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if the process has been succesful. False in case of error.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = LOGICAL(ignoreErrors,KIND=C_BOOL)
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
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = exists(fname)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name of the file to be checked for existence.
         !! 
         !! It returns True if the file exists and False otherwhise.
         !!
         !!### Example
         !!
         !! The following program check if file1 exists:
         !!
         !!```Fortran
         !! PROGRAM existsExample
         !!    USE FU_Files, ONLY: exists
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1'
         !!    IF (exists(path)) THEN
         !!       WRITE(*,*) 'File exists'
         !!    ELSE
         !!       WRITE(*,*) 'File does not exist'
         !!    END IF
         !! END PROGRAM existsExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Name of the file to be check for existence.
         LOGICAL                      :: res
         !! True if the file exists. False otherwise.
         res = c_exists(fname//C_NULL_CHAR)
      END FUNCTION exists



      FUNCTION is_directory(fname, ignoreErrors) RESULT(res)
         !! author: Emilio Castro.
         !! date: 16/06/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Checks if a directory exists.
         !! Checks if a directory exists.
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = is_directory(fname, ignoreErrors)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name of the direcory to be checked.
         !! * `ignoreErrors`: False to print a detailed description of the error message.
         !! Optional parameter. Default is False.
         !! 
         !! It returns True if fname is a directory and false otherwise.
         !!
         !!### Example
         !!
         !! The following program checks the existance of a folder:
         !!
         !!```Fortran
         !! PROGRAM is_directoryExample
         !!    USE FU_Files, ONLY: is_directory
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='/tmp'
         !!    IF (is_directory(path)) THEN
         !!       WRITE(*,*) 'It is a directory'
         !!    ELSE
         !!       WRITE(*,*) 'It is not a directory'
         !!    END IF
         !! END PROGRAM is_directoryExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Name of the directory to be checked. 
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! False to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if fname is a directory. False otherwise.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = LOGICAL(ignoreErrors,KIND=C_BOOL)
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
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = is_empty(fname, ignoreErrors)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name of the file to be checked.
         !! * `ignoreErrors`: False to print a detailed description of the error message.
         !! Optional parameter. Default is False.
         !! 
         !! It returns True if fname is an empty file and false otherwise.
         !!
         !!### Example
         !!
         !! The following program checks the if a file is empty:
         !!
         !!```Fortran
         !! PROGRAM is_emptyExample
         !!    USE FU_Files, ONLY: is_empty
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1'
         !!    IF (is_empty(path)) THEN
         !!       WRITE(*,*) 'Empty'
         !!    ELSE
         !!       WRITE(*,*) 'Has data'
         !!    END IF
         !! END PROGRAM is_emptyExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Name of the file to be checked. 
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! False to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if fname is an empty file. False otherwise.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = LOGICAL(ignoreErrors,KIND=C_BOOL)
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
         !! summary: Checks if a regular file exists and it is not a directory, etc.
         !! Checks if a regular file exists and it is not a directory, etc. It follows symlinks.
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = is_regular_file(fname, ignoreErrors)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name of the file to be checked.
         !! * `ignoreErrors`: False to print a detailed description of the error message.
         !! Optional parameter. Default is False.
         !! 
         !! It returns True if fname is a regular file and false otherwise.
         !!
         !!### Example
         !!
         !! The following program checks the if a file is a regular file or not:
         !!
         !!```Fortran
         !! PROGRAM is_regular_fileExample
         !!    USE FU_Files, ONLY: is_regular_file
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1'
         !!    IF (is_regular_file(path)) THEN
         !!       WRITE(*,*) 'It is a file'
         !!    ELSE
         !!       WRITE(*,*) 'It is not a file'
         !!    END IF
         !! END PROGRAM is_regular_fileExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Name of the file to be checked. 
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! False to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if fname is a regular file. False otherwise.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = LOGICAL(ignoreErrors,KIND=C_BOOL)
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
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = is_symlink(fname, ignoreErrors)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name of the file to be checked.
         !! * `ignoreErrors`: False to print a detailed description of the error message.
         !! Optional parameter. Default is False.
         !! 
         !! It returns True if fname is a symlink and false otherwise.
         !!
         !!### Example
         !!
         !! The following program checks the if a file is a symlink or not:
         !!
         !!```Fortran
         !! PROGRAM is_symlinkExample
         !!    USE FU_Files, ONLY: is_symlink
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1'
         !!    IF (is_symlink(path)) THEN
         !!       WRITE(*,*) 'It is a symlink'
         !!    ELSE
         !!       WRITE(*,*) 'It is not a symlink'
         !!    END IF
         !! END PROGRAM is_symlinkExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Name of the symlink to be checked. 
         LOGICAL,OPTIONAL, INTENT(IN) :: ignoreErrors
         !! False to print a detailed description of the error message. 
         !! Optional parameter. Default is False.
         LOGICAL                      :: res
         !! True if fname is a symlink. False otherwise.
         LOGICAL(C_BOOL)              :: ign
         IF (PRESENT(ignoreErrors)) THEN
            ign = LOGICAL(ignoreErrors,KIND=C_BOOL)
         ELSE
            ign = .FALSE.
         END IF
         res = c_is_symlink(fname//C_NULL_CHAR, ign)
      END FUNCTION is_symlink
#endif


      FUNCTION is_path_absolute(fname) RESULT(res)
         !! author: Emilio Castro.
         !! date: 27/07/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Determines if a path is absolute or not
         !! Determines if a path is absolute or not. Returns True if path is absolute
         !! and False if path is relative.
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = is_path_absolute(fname)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name of the path to be checked.
         !! 
         !! It returns True if fname is an absolute path and false otherwise.
         !!
         !!### Example
         !!
         !! The following program checks the if some paths are absolute or not:
         !!
         !!```Fortran
         !! PROGRAM is_path_absoluteExample
         !!    USE FU_Files, ONLY: is_path_absolute
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1'
         !!    WRITE(*,*) path, is_path_absolute(path)
         !!    path='./file1'
         !!    WRITE(*,*) path, is_path_absolute(path)
         !!    path='/tmp/file1'
         !!    WRITE(*,*) path, is_path_absolute(path)
         !! END PROGRAM is_path_absoluteExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Path to a file.
         LOGICAL                      :: res
         !! True if the path is absolute and false if the path is relative.
         res = c_is_absolute(fname//C_NULL_CHAR)
      END FUNCTION is_path_absolute



      FUNCTION is_path_relative(fname) RESULT(res)
         !! author: Emilio Castro.
         !! date: 27/07/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Determines if a path is relative or not
         !! Determines if a path is relative or not. Returns True if path is relative
         !! and False if path is absolute.
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = is_path_relative(fname)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name of the path to be checked.
         !! 
         !! It returns True if fname is a relative path and false otherwise.
         !!
         !!### Example
         !!
         !! The following program checks the if some paths are relative or not:
         !!
         !!```Fortran
         !! PROGRAM is_path_relativeExample
         !!    USE FU_Files, ONLY: is_path_relative
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1'
         !!    WRITE(*,*) path, is_path_relative(path)
         !!    path='./file1'
         !!    WRITE(*,*) path, is_path_relative(path)
         !!    path='/tmp/file1'
         !!    WRITE(*,*) path, is_path_relative(path)
         !! END PROGRAM is_path_relativeExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Path to a file.
         LOGICAL                      :: res
         !! True if the path is relative and false if the path is absolute.
         res = c_is_relative(fname//C_NULL_CHAR)
      END FUNCTION is_path_relative



      FUNCTION extension(fname) RESULT(res)
         !! author: Emilio Castro.
         !! date: 27/07/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Determines the extension of a file.
         !! Determines the extension of a file given its name or path.
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = extension(fname)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name or path of the file the determine the extension.
         !! 
         !! It returns the extension of the file including the "dot". Empty path is
         !! returned if no extension is found.
         !!
         !!### Example
         !!
         !! The following program gets the extensions of some files
         !!
         !!```Fortran
         !! PROGRAM extensionExample
         !!    USE FU_Files, ONLY: extension
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1.txt'
         !!    WRITE(*,*) path, ' ', extension(path)
         !!    path='./file1.txt'
         !!    WRITE(*,*) path, ' ', extension(path)
         !!    path='/tmp/file1.txt'
         !!    WRITE(*,*) path, ' ', extension(path)
         !!    path='/tmp/file1'
         !!    WRITE(*,*) path, ' ', extension(path)
         !! END PROGRAM extensionExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Filename or path to a file.
         CHARACTER(LEN=:), ALLOCATABLE :: res
         !! Extension of the file including the "dot". Empty path is returned if no extension is found.
         CHARACTER(LEN=:, KIND = C_CHAR), ALLOCATABLE :: c_string
         c_string = fname//C_NULL_CHAR
         CALL c_extension(c_string)
         res = c_to_f(c_string)
      END FUNCTION extension


      FUNCTION replace_extension(fname, ext) RESULT(res)
         !! author: Emilio Castro.
         !! date: 21/01/2021.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Changes the extension of a filename.
         !! Changes the extension of a filename.
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = replace_extension(fname, ext)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name or path of the file.
         !! * `ext`: New extension for the name
         !! 
         !! It returns the fname value with the modified extension.
         !!
         !!### Example
         !!
         !! The following program changes the extensions of some filenames.
         !!
         !!```Fortran
         !! PROGRAM replace_extensionExample
         !!    USE FU_Files, ONLY: replace_extension
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1.txt'
         !!    WRITE(*,*) path, ' ', replace_extension(path, 'dat')
         !!    path='./file1.txt'
         !!    WRITE(*,*) path, ' ', replace_extension(path, 'dat')
         !!    path='/tmp/file1.txt'
         !!    WRITE(*,*) path, ' ', replace_extension(path, 'dat')
         !!    path='/tmp/file1.dat'
         !!    WRITE(*,*) path, ' ', replace_extension(path, '')
         !!    path='/tmp/file1'
         !!    WRITE(*,*) path, ' ', replace_extension(path, 'dat')
         !! END PROGRAM replace_extensionExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Filename or path to a file.
         CHARACTER(LEN=*), INTENT(IN) :: ext
         !! New extension for the fname.
         CHARACTER(LEN=:), ALLOCATABLE :: res
         !! The fname value with the modified extension.
         CHARACTER(LEN=:, KIND = C_CHAR), ALLOCATABLE :: c_string, c_string1
         c_string = fname//C_NULL_CHAR
         c_string1 = ext//C_NULL_CHAR
         CALL c_replace_extension(c_string,c_string1)
         res = c_to_f(c_string)
      END FUNCTION replace_extension


      FUNCTION stem(fname) RESULT(res)
         !! author: Emilio Castro.
         !! date: 06/08/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Determines the filename without the path and without the final extension given a path.
         !! Determines the filename without the path and without the final extension given a path.
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = stem(fname)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name or path of the file the determine the filename without the final extension and the path.
         !! 
         !! It returns the filename of the file without the extension and without path.
         !!
         !!### Example
         !!
         !! The following program gets the filenames of some files
         !!
         !!```Fortran
         !! PROGRAM stemExample
         !!    USE FU_Files, ONLY: stem
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1.txt'
         !!    WRITE(*,*) path, ' ', stem(path)
         !!    path='./file1.txt'
         !!    WRITE(*,*) path, ' ', stem(path)
         !!    path='/tmp/file1.txt'
         !!    WRITE(*,*) path, ' ', stem(path)
         !!    path='/tmp/file1'
         !!    WRITE(*,*) path, ' ', stem(path)
         !! END PROGRAM stemExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Filename or path to a file.
         CHARACTER(LEN=:), ALLOCATABLE :: res
         !! Filename without the final extension and without the path.
         !! If filename consists of an extension only, the extension is returned.
         CHARACTER(LEN=:, KIND = C_CHAR), ALLOCATABLE :: c_string
         c_string = fname//C_NULL_CHAR
         CALL c_stem(c_string)
         res = c_to_f(c_string)
      END FUNCTION stem


      FUNCTION filename(fname) RESULT(res)
         !! author: Emilio Castro.
         !! date: 06/08/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Determines the full filename given a path.
         !! Determines the full filename given a path.
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = filename(fname)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name or path of the file the determine the filename.
         !! 
         !! It returns the filename of the given path.
         !!
         !!### Example
         !!
         !! The following program gets the filenames of some files
         !!
         !!```Fortran
         !! PROGRAM filenameExample
         !!    USE FU_Files, ONLY: filename
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1.txt'
         !!    WRITE(*,*) path, ' ', filename(path)
         !!    path='./file1.txt'
         !!    WRITE(*,*) path, ' ', filename(path)
         !!    path='/tmp/file1.txt'
         !!    WRITE(*,*) path, ' ', filename(path)
         !!    path='/tmp/file1'
         !!    WRITE(*,*) path, ' ', filename(path)
         !! END PROGRAM filenameExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Filename or path to a file.
         CHARACTER(LEN=:), ALLOCATABLE :: res
         !! Full filename given in the path.
         CHARACTER(LEN=:, KIND = C_CHAR), ALLOCATABLE :: c_string
         c_string = fname//C_NULL_CHAR
         CALL c_filename(c_string)
         res = c_to_f(c_string)
      END FUNCTION filename


      FUNCTION replace_filename(fname, newname) RESULT(res)
         !! author: Emilio Castro.
         !! date: 21/01/2021.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Changes the filename of a path (keeping the path).
         !! Changes the filename of a path (keeping the path).
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = replace_filename(fname, newname)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name or path of the file.
         !! * `newname`: New name for the name filename (including extension).
         !! 
         !! It returns the fname value with the modified name.
         !!
         !!### Example
         !!
         !! The following program changes the filename of some paths.
         !!
         !!```Fortran
         !! PROGRAM replace_filenameExample
         !!    USE FU_Files, ONLY: replace_filename
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1.txt'
         !!    WRITE(*,*) path, ' ', replace_filename(path, 'file2')
         !!    path='./file1.txt'
         !!    WRITE(*,*) path, ' ', replace_filename(path, 'file2')
         !!    path='/tmp/file1.txt'
         !!    WRITE(*,*) path, ' ', replace_filename(path, 'file2')
         !!    path='/tmp/file1.dat'
         !!    WRITE(*,*) path, ' ', replace_filename(path, '')
         !!    path='/tmp/file1'
         !!    WRITE(*,*) path, ' ', replace_filename(path, 'file2')
         !! END PROGRAM replace_filenameExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Filename or path to a file.
         CHARACTER(LEN=*), INTENT(IN) :: newname
         !! New filename for the fname (including extension).
         CHARACTER(LEN=:), ALLOCATABLE :: res
         !! The fname value with the modified filename.
         CHARACTER(LEN=:, KIND = C_CHAR), ALLOCATABLE :: c_string, c_string1
         c_string = fname//C_NULL_CHAR
         c_string1 = newname//C_NULL_CHAR
         CALL c_replace_filename(c_string,c_string1)
         res = c_to_f(c_string)
      END FUNCTION replace_filename


      FUNCTION remove_filename(fname) RESULT(res)
         !! author: Emilio Castro.
         !! date: 21/01/2021.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Removes the filename from a path.
         !! Removes the filename from a path. It is similar to function
         !! [[parent_path]] but this one does not remove trailing path separators (if any).
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = remove_filename(fname)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name or path of the file.
         !! 
         !! It returns the fname value with the filename removed.
         !!
         !!### Example
         !!
         !! The following program removes the filename of some paths.
         !!
         !!```Fortran
         !! PROGRAM remove_filenameExample
         !!    USE FU_Files, ONLY: remove_filename
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1.txt'
         !!    WRITE(*,*) path, ' ', remove_filename(path)
         !!    path='./file1.txt'
         !!    WRITE(*,*) path, ' ', remove_filename(path)
         !!    path='/tmp/file1.txt'
         !!    WRITE(*,*) path, ' ', remove_filename(path)
         !!    path='/tmp/file1.dat'
         !!    WRITE(*,*) path, ' ', remove_filename(path)
         !!    path='/tmp/file1'
         !!    WRITE(*,*) path, ' ', remove_filename(path)
         !! END PROGRAM remove_filenameExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Filename or path to a file.
         CHARACTER(LEN=:), ALLOCATABLE :: res
         !! The fname value with the filename removed.
         CHARACTER(LEN=:, KIND = C_CHAR), ALLOCATABLE :: c_string
         c_string = fname//C_NULL_CHAR
         CALL c_remove_filename(c_string)
         res = c_to_f(c_string)
      END FUNCTION remove_filename


      FUNCTION parent_path(fname) RESULT(res)
         !! author: Emilio Castro.
         !! date: 06/08/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Determines the path to the parent directory given the path to a file.
         !! Determines the path to the parent directory given the path to a file. It is similar
         !! to function [[remove_filename]] but this one removes trailing path separators (if any).
         !!
         !!### Syntax
         !!
         !!```Fortran
         !! l = parent_path(fname)
         !!```
         !!
         !! Where:
         !!
         !! * `fname`: Name or path of the file.
         !! 
         !! It returns the path to the parent directory given the path to a file.
         !!
         !!### Example
         !!
         !! The following program returns the parent paths of some files.
         !!
         !!```Fortran
         !! PROGRAM parent_pathExample
         !!    USE FU_Files, ONLY: parent_path
         !!    IMPLICIT NONE
         !!    CHARACTER(LEN=:), ALLOCATABLE :: path
         !!    path='file1.txt'
         !!    WRITE(*,*) path, ' ', parent_path(path)
         !!    path='./file1.txt'
         !!    WRITE(*,*) path, ' ', parent_path(path)
         !!    path='/tmp/file1.txt'
         !!    WRITE(*,*) path, ' ', parent_path(path)
         !!    path='/tmp/file1.dat'
         !!    WRITE(*,*) path, ' ', parent_path(path)
         !!    path='/tmp/file1'
         !!    WRITE(*,*) path, ' ', parent_path(path)
         !! END PROGRAM parent_pathExample
         !!```
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: fname
         !! Filename or path to a file.
         CHARACTER(LEN=:), ALLOCATABLE :: res
         !! Path of the parent directory without final slash.
         CHARACTER(LEN=:, KIND = C_CHAR), ALLOCATABLE :: c_string
         c_string = fname//C_NULL_CHAR
         CALL c_parent_path(c_string)
         res = c_to_f(c_string)
      END FUNCTION parent_path





      SUBROUTINE readMatrix_i8(filename,res)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Reads a matrix from a file.
         !! Reads a matrix from a file. The file must have the proper format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns must be separated using blanks.
         !!
         !! The file is then opened, read and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to read the matrix from.
         INTEGER(KIND=i8), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: res
         !! Values read from the matrix. This output variable must be allocatable but
         !! must not be allocated prior to call readMatrix as it is allocated here. However
         !! deallocation must be done manually by the user when finishes using the information.
         INTEGER :: nrows, ncols, r, c, u
         LOGICAL :: header
         ! True if there is a line with a header to skip.

         INCLUDE 'Files_M/include_readMatrix.f90'

      END SUBROUTINE readMatrix_i8

      SUBROUTINE readMatrix_i16(filename,res)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Reads a matrix from a file.
         !! Reads a matrix from a file. The file must have the proper format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns must be separated using blanks.
         !!
         !! The file is then opened, read and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to read the matrix from.
         INTEGER(KIND=i16), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: res
         !! Values read from the matrix. This output variable must be allocatable but
         !! must not be allocated prior to call readMatrix as it is allocated here. However
         !! deallocation must be done manually by the user when finishes using the information.
         INTEGER :: nrows, ncols, r, c, u
         LOGICAL :: header
         ! True if there is a line with a header to skip.

         INCLUDE 'Files_M/include_readMatrix.f90'

      END SUBROUTINE readMatrix_i16

      SUBROUTINE readMatrix_i32(filename,res)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Reads a matrix from a file.
         !! Reads a matrix from a file. The file must have the proper format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns must be separated using blanks.
         !!
         !! The file is then opened, read and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to read the matrix from.
         INTEGER(KIND=i32), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: res
         !! Values read from the matrix. This output variable must be allocatable but
         !! must not be allocated prior to call readMatrix as it is allocated here. However
         !! deallocation must be done manually by the user when finishes using the information.
         INTEGER :: nrows, ncols, r, c, u
         LOGICAL :: header
         ! True if there is a line with a header to skip.

         INCLUDE 'Files_M/include_readMatrix.f90'

      END SUBROUTINE readMatrix_i32

      SUBROUTINE readMatrix_i64(filename,res)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Reads a matrix from a file.
         !! Reads a matrix from a file. The file must have the proper format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns must be separated using blanks.
         !!
         !! The file is then opened, read and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to read the matrix from.
         INTEGER(KIND=i64), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: res
         !! Values read from the matrix. This output variable must be allocatable but
         !! must not be allocated prior to call readMatrix as it is allocated here. However
         !! deallocation must be done manually by the user when finishes using the information.
         INTEGER :: nrows, ncols, r, c, u
         LOGICAL :: header
         ! True if there is a line with a header to skip.

         INCLUDE 'Files_M/include_readMatrix.f90'

      END SUBROUTINE readMatrix_i64

      SUBROUTINE readMatrix_sp(filename,res)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Reads a matrix from a file.
         !! Reads a matrix from a file. The file must have the proper format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns must be separated using blanks.
         !!
         !! The file is then opened, read and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to read the matrix from.
         REAL(KIND=sp), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: res
         !! Values read from the matrix. This output variable must be allocatable but
         !! must not be allocated prior to call readMatrix as it is allocated here. However
         !! deallocation must be done manually by the user when finishes using the information.
         INTEGER :: nrows, ncols, r, c, u
         LOGICAL :: header
         ! True if there is a line with a header to skip.

         INCLUDE 'Files_M/include_readMatrix.f90'

      END SUBROUTINE readMatrix_sp

      SUBROUTINE readMatrix_dp(filename,res)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Reads a matrix from a file.
         !! Reads a matrix from a file. The file must have the proper format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns must be separated using blanks.
         !!
         !! The file is then opened, read and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to read the matrix from.
         REAL(KIND=dp), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: res
         !! Values read from the matrix. This output variable must be allocatable but
         !! must not be allocated prior to call readMatrix as it is allocated here. However
         !! deallocation must be done manually by the user when finishes using the information.
         INTEGER :: nrows, ncols, r, c, u
         LOGICAL :: header
         ! True if there is a line with a header to skip.

         INCLUDE 'Files_M/include_readMatrix.f90'

      END SUBROUTINE readMatrix_dp

#ifdef QPREC_FPP
      SUBROUTINE readMatrix_qp(filename,res)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Reads a matrix from a file.
         !! Reads a matrix from a file. The file must have the proper format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns must be separated using blanks.
         !!
         !! The file is then opened, read and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to read the matrix from.
         REAL(KIND=qp), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: res
         !! Values read from the matrix. This output variable must be allocatable but
         !! must not be allocated prior to call readMatrix as it is allocated here. However
         !! deallocation must be done manually by the user when finishes using the information.
         INTEGER :: nrows, ncols, r, c, u
         LOGICAL :: header
         ! True if there is a line with a header to skip.

         INCLUDE 'Files_M/include_readMatrix.f90'

      END SUBROUTINE readMatrix_qp
#endif


      SUBROUTINE writeMatrix_i8(filename,matrix,header,formato)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Writes a matrix to a file.
         !! Writes a matrix to a file. The file will have the following format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns are separated using blanks.
         !!
         !! The file is then opened, written and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to write the matrix to.
         INTEGER(KIND=i8), DIMENSION(:,:), INTENT(IN) :: matrix
         !! Values of the matrix to write to the file.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: header
         !! Header to be writen in the second line.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: formato
         !! Format to use for the numbers without parenthesis. Example: I3
         INTEGER :: nrows, ncols, r, c, u

         INCLUDE 'Files_M/include_writeMatrix.f90'

      END SUBROUTINE writeMatrix_i8

      SUBROUTINE writeMatrix_i16(filename,matrix,header,formato)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Writes a matrix to a file.
         !! Writes a matrix to a file. The file will have the following format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns are separated using blanks.
         !!
         !! The file is then opened, written and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to write the matrix to.
         INTEGER(KIND=i16), DIMENSION(:,:), INTENT(IN) :: matrix
         !! Values of the matrix to write to the file.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: header
         !! Header to be writen in the second line.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: formato
         !! Format to use for the numbers without parenthesis. Example: I3
         INTEGER :: nrows, ncols, r, c, u

         INCLUDE 'Files_M/include_writeMatrix.f90'

      END SUBROUTINE writeMatrix_i16

      SUBROUTINE writeMatrix_i32(filename,matrix,header,formato)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Writes a matrix to a file.
         !! Writes a matrix to a file. The file will have the following format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns are separated using blanks.
         !!
         !! The file is then opened, written and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to write the matrix to.
         INTEGER(KIND=i32), DIMENSION(:,:), INTENT(IN) :: matrix
         !! Values of the matrix to write to the file.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: header
         !! Header to be writen in the second line.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: formato
         !! Format to use for the numbers without parenthesis. Example: I3
         INTEGER :: nrows, ncols, r, c, u

         INCLUDE 'Files_M/include_writeMatrix.f90'

      END SUBROUTINE writeMatrix_i32

      SUBROUTINE writeMatrix_i64(filename,matrix,header,formato)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Writes a matrix to a file.
         !! Writes a matrix to a file. The file will have the following format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns are separated using blanks.
         !!
         !! The file is then opened, written and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to write the matrix to.
         INTEGER(KIND=i64), DIMENSION(:,:), INTENT(IN) :: matrix
         !! Values of the matrix to write to the file.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: header
         !! Header to be writen in the second line.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: formato
         !! Format to use for the numbers without parenthesis. Example: I3
         INTEGER :: nrows, ncols, r, c, u

         INCLUDE 'Files_M/include_writeMatrix.f90'

      END SUBROUTINE writeMatrix_i64

      SUBROUTINE writeMatrix_sp(filename,matrix,header,formato)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Writes a matrix to a file.
         !! Writes a matrix to a file. The file will have the following format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns are separated using blanks.
         !!
         !! The file is then opened, written and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to write the matrix to.
         REAL(KIND=sp), DIMENSION(:,:), INTENT(IN) :: matrix
         !! Values of the matrix to write to the file.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: header
         !! Header to be writen in the second line.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: formato
         !! Format to use for the numbers without parenthesis. Example: F8.3
         INTEGER :: nrows, ncols, r, c, u

         INCLUDE 'Files_M/include_writeMatrix.f90'

      END SUBROUTINE writeMatrix_sp

      SUBROUTINE writeMatrix_dp(filename,matrix,header,formato)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Writes a matrix to a file.
         !! Writes a matrix to a file. The file will have the following format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns are separated using blanks.
         !!
         !! The file is then opened, written and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to write the matrix to.
         REAL(KIND=dp), DIMENSION(:,:), INTENT(IN) :: matrix
         !! Values of the matrix to write to the file.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: header
         !! Header to be writen in the second line.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: formato
         !! Format to use for the numbers without parenthesis. Example: F8.3
         INTEGER :: nrows, ncols, r, c, u

         INCLUDE 'Files_M/include_writeMatrix.f90'

      END SUBROUTINE writeMatrix_dp

#ifdef QPREC_FPP
      SUBROUTINE writeMatrix_qp(filename,matrix,header,formato)
         !! author: Emilio Castro.
         !! date: 01/12/2020.
         !! version: 1.0.
         !! license: MIT.
         !! summary: Writes a matrix to a file.
         !! Writes a matrix to a file. The file will have the following format:
         !!
         !! First line includes the number of rows, the number of columns and a logical
         !! indicating if the second line is a header line to be skipped. Example: 5 9 T.
         !!
         !! The second row can be a header or not.
         !!
         !! Then the matrix comes. The different columns are separated using blanks.
         !!
         !! The file is then opened, written and closed.
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: filename
         !! Filename of the file to write the matrix to.
         REAL(KIND=qp), DIMENSION(:,:), INTENT(IN) :: matrix
         !! Values of the matrix to write to the file.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: header
         !! Header to be writen in the second line.
         CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: formato
         !! Format to use for the numbers without parenthesis. Example: F8.3
         INTEGER :: nrows, ncols, r, c, u

         INCLUDE 'Files_M/include_writeMatrix.f90'

      END SUBROUTINE writeMatrix_qp
#endif








      ! Auxiliary functions


      FUNCTION c_to_f(c_string) RESULT(res)
         ! Auxiliary function used as interface to convert c strings to fortran strings
         IMPLICIT NONE
         CHARACTER(C_CHAR)              :: c_string
         CHARACTER(LEN=:), ALLOCATABLE  :: res
         INTEGER :: l !length
         l = 1
         DO WHILE (c_string(l:l) /= C_NULL_CHAR)
            l = l + 1
         END DO
         res = c_string(1:l-1)
      END FUNCTION c_to_f




END MODULE FU_Files


