/* Fortran Utilities
 *
 * MODULE: Files_M
 *
 * DESCRIPTION:
 * Useful tools to manipulate files in Fortran programs.
 *
 * REVISION HISTORY:
 * 16-06-2020 - Initial Version.
 *
 * AUTHOR: Emilio Castro.
 *
 * VERSION 1.0.
 *
 * Copyright: See LICENSE file that comes with this distribution.
 *            A portion of the file is released with a license CC BY-SA 4.0.
 *
 * NOTES: If compiler is gcc 8 the following compilation
 *        flag must be used: -lstdc++fs. From gcc 9 onwards
 *        it is not required.
*/

//Detecting if the compiler uses filesystem or experimental/filesystem.
#if defined(__cpp_lib_filesystem)
#   include <filesystem>
    namespace fs = std::filesystem;
#elif defined(__cpp_lib_experimental_filesystem)
#   include <experimental/filesystem>
    namespace fs = std::experimental::filesystem;
#elif !defined(__has_include)
#   include <experimental/filesystem>
    namespace fs = std::experimental::filesystem;
#elif __has_include(<filesystem>)
#   include <filesystem>
    namespace fs = std::filesystem;
#elif __has_include(<experimental/filesystem>)
#   include <experimental/filesystem>
    namespace fs = std::experimental::filesystem;
#else
#   error "Could not find filesystem nor experimental/filesystem support."
#endif

#include <cstring>

extern "C"
{
   bool c_createdir(const char *, const bool *);
}

bool c_createdir(const char * name, const bool * ignoreErrors)
{
   std::error_code ec;
   fs::create_directories(name, ec);
   if (not ignoreErrors && ec.value()!= 0)
   {
      printf("ERROR while creating directory '%s': %s\n",name, ec.message().c_str());
   }
   return (ec.value() == 0);
}

#ifdef LIN_CPP
extern "C"
{
   bool c_create_symlink(const char *, const char *, const bool *);
}

bool c_create_symlink(const char * src, const char * dest, const bool * ignoreErrors)
{
   std::error_code ec;
   if (fs::is_directory(src,ec))
   {
      fs::create_directory_symlink(src, dest, ec);
   }
   else
   {
      fs::create_symlink(src, dest, ec);
   }
   if (not ignoreErrors && ec.value()!= 0)
   {
      printf("ERROR while creating symlink '%s' with name '%s': %s\n",src, dest, ec.message().c_str());
   }
   return (ec.value() == 0);
}
#endif

extern "C"
{
   bool c_copy_file(const char *, const char *, const bool *);
}

bool c_copy_file(const char * src, const char * dest, const bool * ignoreErrors)
{
   std::error_code ec;
   fs::copy(src,dest,fs::copy_options::overwrite_existing | fs::copy_options::recursive,ec);
   if (not ignoreErrors && ec.value() != 0)
   {
      printf("ERROR while copying '%s' to '%s': %s\n",src, dest, ec.message().c_str());
   }
   return (ec.value() == 0);
}

extern "C"
{
   bool c_move_file(const char *, const char *, const bool *);
}

bool c_move_file(const char * src, const char * dest, const bool * ignoreErrors)
{
   std::error_code ec;
   fs::rename(src,dest,ec);
   if (not ignoreErrors && ec.value() != 0)
   {
      printf("ERROR while moving '%s' to '%s': %s\n",src, dest, ec.message().c_str());
   }
   return (ec.value() == 0);
}

extern "C"
{
   bool c_remove(const char *, const bool *);
}

bool c_remove(const char * fname, const bool * ignoreErrors)
{
   std::error_code ec;
   fs::remove_all(fname,ec);
   if (not ignoreErrors && ec.value() != 0)
   {
      printf("ERROR while removing '%s': %s\n",fname, ec.message().c_str());
   }
   return (ec.value() == 0);
}

extern "C"
{
   bool c_exists(const char *);
}

bool c_exists(const char * kkname)
{
   const fs::path& p = kkname;
   fs::file_status s = fs::file_status{};
   return (fs::status_known(s) ? fs::exists(s) : fs::exists(p));
}

extern "C"
{
   bool c_is_directory(const char *, const bool *);
}

bool c_is_directory(const char * name, const bool * ignoreErrors)
{
   bool res;
   std::error_code ec;
   res = fs::is_directory(name, ec);
   if (not ignoreErrors && ec.value() != 0)
   {
      printf("ERROR while using is_directory '%s': %s\n",name, ec.message().c_str());
   }
   return (res && ec.value() == 0);
}


extern "C"
{
   bool c_is_empty(const char *, const bool *);
}

bool c_is_empty(const char * name, const bool * ignoreErrors)
{
   bool res;
   std::error_code ec;
   res = fs::is_empty(name, ec);
   if (not ignoreErrors && ec.value() != 0)
   {
      printf("ERROR while using is_empty '%s': %s\n",name, ec.message().c_str());
   }
   return (res && ec.value() == 0);
}


extern "C"
{
   bool c_is_regular_file(const char *, const bool *);
}

bool c_is_regular_file(const char * name, const bool * ignoreErrors)
{
   bool res;
   std::error_code ec;
   res = fs::is_regular_file(name, ec);
   if (not ignoreErrors && ec.value() != 0)
   {
      printf("ERROR while using is_regular_file '%s': %s\n",name, ec.message().c_str());
   }
   return (res && ec.value() == 0);
}

#ifdef LIN_CPP
extern "C"
{
   bool c_is_symlink(const char *, const bool *);
}

bool c_is_symlink(const char * name, const bool * ignoreErrors)
{
   bool res;
   std::error_code ec;
   res = fs::is_symlink(name, ec);
   if (not ignoreErrors && ec.value() != 0)
   {
      printf("ERROR while using is_symlink '%s': %s\n",name, ec.message().c_str());
   }
   return (res && ec.value() == 0);
}
#endif





extern "C"
{
   bool c_is_absolute(const char *);
}


bool c_is_absolute(const char * name)
{
   return(fs::path(name).is_absolute());
}



extern "C"
{
   bool c_is_relative(const char *);
}


bool c_is_relative(const char * name)
{
   return(fs::path(name).is_relative());
}



extern "C"
{
   void c_extension(char *);
}


void c_extension(char * name)
{
   strcpy(name,fs::path(name).extension().string().c_str());
}


extern "C"
{
   void c_replace_extension(char *, char *);
}


void c_replace_extension(char * name, char * ext)
{
   strcpy(name,fs::path(name).replace_extension(ext).string().c_str());
}


extern "C"
{
   void c_stem(char *);
}


void c_stem(char * name)
{
   strcpy(name,fs::path(name).stem().string().c_str());
}


extern "C"
{
   void c_filename(char *);
}


void c_filename(char * name)
{
   strcpy(name,fs::path(name).filename().string().c_str());
}


extern "C"
{
   void c_replace_filename(char *, char *);
}


void c_replace_filename(char * name, char * newname)
{
   strcpy(name,fs::path(name).replace_filename(newname).string().c_str());
}


extern "C"
{
   void c_remove_filename(char *);
}


void c_remove_filename(char * name)
{
   strcpy(name,fs::path(name).remove_filename().string().c_str());
}


extern "C"
{
   void c_parent_path(char *);
}


void c_parent_path(char * name)
{
   strcpy(name,fs::path(name).parent_path().string().c_str());
}
